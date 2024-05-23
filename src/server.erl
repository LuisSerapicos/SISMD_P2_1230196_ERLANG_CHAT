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
-export([start/1, loop/1]).

start(Server) ->
  Pid = spawn(fun() -> init(Server) end),
  register(Server, Pid),
  {ok, Pid}.

init(Server) ->
  io:format("Server ~p started~n", [Server]),
  {Messages, Clients} = load_state(),
  %% Notify the router that state has been restored
  global:send(router, {state_restored, self()}),
  loop({Messages, Clients}).

loop({Messages, Clients}) ->
  receive
    {restore_state, {NewMessages, NewClients}} ->
      io:format("Restoring state: Messages ~p, Clients ~p~n", [NewMessages, NewClients]),
      loop({NewMessages, NewClients});
    {From, get_pid} ->
      From ! {server, self()},
      loop({Messages, Clients});
    {From, stop} ->
      io:format("Received from ~p message to stop!~n", [From]),
      From ! {self(), server_disconnect},
      exit(normal);
    {From, {connect, ClientName}} ->
      NewClients = lists:usort([ClientName | Clients]), % Ensure unique client names
      save_state({Messages, NewClients}),
      io:format("Client connected: ~p~n", [ClientName]),
      loop({Messages, NewClients});
    {From, Msg} ->
      io:format("Received ~p: ~p~n", [From, Msg]),
      io:format("Sending reply...~n"),
      From ! {self(), happy_to_receive_your_message},
      %% Save the message and update the state
      NewMessages = [Msg | Messages],
      save_state({NewMessages, Clients}),
      loop({NewMessages, Clients})
  end.

save_state({Messages, Clients}) ->
  case global:whereis_name(router) of
    undefined ->
      io:format("Router not found, state not saved: Messages ~p, Clients ~p~n", [Messages, Clients]);
    Router ->
      Router ! {save_state, {Messages, Clients}},
      io:format("State saved: Messages ~p, Clients ~p~n", [Messages, Clients])
  end.

load_state() ->
  case global:whereis_name(router) of
    undefined ->
      io:format("Router not found, retrying in 5 seconds~n"),
      timer:sleep(5000),
      load_state();
    Router ->
      Router ! {load_state, self()},
      receive
        {state, {Messages, Clients}} ->
          io:format("Loaded state: Messages ~p, Clients ~p~n", [Messages, Clients]),
          {Messages, Clients}
      after 10000 ->
        io:format("Timeout loading state, initializing empty state~n"),
        {[], []}
      end
  end.


