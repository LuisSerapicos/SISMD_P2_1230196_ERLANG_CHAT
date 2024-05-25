%%%-------------------------------------------------------------------
%%% @author Luis Serapicos
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. mai. 2024 23:08
%%%-------------------------------------------------------------------
-module(server).
-author("Luis Serapicos").

%% API
-export([start/1, loop/2]).

%% @doc Starts the server process and registers it under the given name.
%% @param Server The name of the server process.
%% @spec start(atom()) -> {ok, pid()}
start(Server) ->
  Pid = spawn(fun() -> init(Server) end),
  register(Server, Pid),
  {ok, Pid}.


%% @doc Initializes the server process.
%% @param Server The name of the server process.
%% @spec init(atom()) -> ok
%% If the state is restored, the server will notify the router that the state has been restored.
%% Global is used to send messages to the router, because the router is registered globally on another node.
init(Server) ->
  io:format("Server ~p started~n", [Server]),
  {Messages, Clients} = load_state(Server),
  global:send(router, {state_restored, self()}),
  loop(Server, {Messages, Clients}).


%% @doc The main loop of the server process.
%% @param State The current state of the server.
%% If the server receives a message to stop, it will exit with normal status.
%% If the server receives a message to connect a client, it will add the client to the list of clients.
%% If the server receives a message to leave a client, it will remove the client from the list of clients.
%% If the server receives a message from a client, it will save the message and update the state.
%% If the server receives a message to restore state, it will restore the state and update the state.
loop(Server, {Messages, Clients}) ->
  receive
    {restore_state, {NewMessages, NewClients}} ->
      io:format("Restoring state: Messages ~p, Clients ~p~n", [NewMessages, NewClients]),
      loop(Server, {NewMessages, NewClients});
    {From, get_pid} ->
      From ! {server, self()},
      loop(Server, {Messages, Clients});
    {From, {connect, Client}} ->
      NewClients = lists:usort([Client | Clients]), % Unique client names
      save_state(Server, {Messages, NewClients}),
      io:format("Client connected: ~p~n", [Client]),
      loop(Server, {Messages, NewClients});
    {From, {leave, Client}} ->
      NewClients = lists:delete(Client, Clients),
      save_state(Server, {Messages, NewClients}),
      io:format("Client left: ~p~n", [Client]),
      loop(Server, {Messages, NewClients});
    {From, Msg} ->
      io:format("Received ~p: ~p~n", [From, Msg]),
      io:format("Sending reply...~n"),
      From ! {self(), happy_to_receive_your_message},
      %% Save the message and update the state
      NewMessages = [Msg | Messages],
      save_state(Server, {NewMessages, Clients}),
      loop(Server, {NewMessages, Clients});
    {From, stop} ->
      io:format("Received from ~p message to stop!~n", [From]),
      From ! {self(), server_disconnect},
      exit(normal)
  end.


%% @doc Saves the state of the server to ETS.
%% @param State The state of the server.
%% @spec save_state({list(), list()}) -> ok
%% If the router is found, the function will send a message to the router to save the state.
save_state(ServerName, {Messages, Clients}) ->
  case global:whereis_name(router) of
    undefined ->
      io:format("Router not found, state not saved: Messages ~p, Clients ~p~n", [Messages, Clients]);
    Router ->
      Router ! {save_state, ServerName, {Messages, Clients}},
      io:format("State saved: Messages ~p, Clients ~p~n", [Messages, Clients])
  end.


%% @doc Loads the state of the server from ETS.
%% @spec load_state() -> {list(), list()}
%% If the router is not found, the function will retry every 5 seconds until the router is found.
%% If the router is found, the function will send a message to the router to load the state.
%% If the state is not received within 10 seconds, the function will initialize an empty state.
load_state(ServerName) ->
  case global:whereis_name(router) of
    undefined ->
      io:format("Router not found, retrying in 5 seconds~n"),
      timer:sleep(5000),
      load_state(ServerName);
    Router ->
      Router ! {load_state, self(), ServerName},
      receive
        {state, {Messages, Clients}} ->
          io:format("Loaded state: Messages ~p, Clients ~p~n", [Messages, Clients]),
          {Messages, Clients}
      after 10000 ->
        io:format("Timeout loading state, initializing empty state~n"),
        {[], []}
      end
  end.


