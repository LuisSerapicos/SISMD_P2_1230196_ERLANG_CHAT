%%%-------------------------------------------------------------------
%%% @author Luis Serapicos
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. mai. 2024 23:07
%%%-------------------------------------------------------------------
-module(router).
-author("Luis Serapicos").

%% API
-export([start/0, monitor_servers/1, loop/0, load_state_from_ets/1, save_state_to_ets/2]).

%% @doc Starts the router process and registers it globally.
%% @spec start() -> {ok, pid()}
%% Starts the router process and registers it globally, so it can be accessed from other nodes.
start() ->
  Pid = spawn_link(fun() -> init() end),
  global:register_name(router, Pid),
  {ok, Pid}.


%% @doc Initializes the router process.
%% @spec init() -> ok
%% Initializes an ETS table to store the state of the server.
init() ->
  ets:new(server_state, [named_table, public, set]),
  loop().


%% @doc Sends a message to the server to get its PID.
%% @param ServerName The name of the server process.
%% @param Node The name of the node the server is running on.
%% @spec get_server_pid({atom(), atom()}) -> pid()
get_server_pid({Server, Node}) ->
  io:format("Sending get_pid message to ~p on ~p~n", [Server, Node]),
  {Server, Node} ! {self(), get_pid},
  receive
    {server, ServerPid} ->
      io:format("Received PID ~p from ~p on ~p~n", [ServerPid, Server, Node]),
      ServerPid
  after
    10000 ->
      io:format("Timeout waiting for PID from ~p on ~p~n", [Server, Node]),
      {error, timeout}
  end.


%% @doc Monitors the server process on the specified node.
%% @param Server The server process.
%% @spec monitor_server({atom(), atom()}) -> ok
monitor_server(Server) ->
  monitor_server(Server, undefined).


%% @doc Monitors the server process on the specified node.
%% @param Server The server process.
%% @param Ref The reference to the monitor process.
%% @spec monitor_server({atom(), atom()}, undefined | reference()) -> ok
monitor_server({Server, Node}, Ref) ->
  io:format("Monitoring ~p on ~p~n", [Server, Node]),
  ServerPid = get_server_pid({Server, Node}),
  case ServerPid of
    {error, timeout} ->
      io:format("Failed to get the PID of the server ~p~n", [Server]);
    _ ->
      io:format("Got PID ~p for ~p on ~p, starting to monitor~n", [ServerPid, Server, Node]),
      case Ref of
        undefined ->
          %% No monitor exists, create a new one
          NewRef = erlang:monitor(process, {Server, Node}),
          monitor_loop(Server, Node, NewRef);
        _ ->
          %% A monitor already exists, do not create a new one
          monitor_loop(Server, Node, Ref)
      end
  end.


%% @doc Monitors the server process.
%% @param ServerName The name of the server process.
%% @param Node The name of the node the server is running on.
%% @param Ref The reference to the monitor process.
%% @spec monitor_loop(atom(), atom(), reference()) -> ok
%% Make RPC call to start a new server instance on the specified node.
monitor_loop(Server, Node, Ref) ->
  receive
    {'DOWN', Ref, process, _, Reason} ->
      io:format("Server ~p exited with reason ~p~n", [Server, Reason]),
      {Messages, Clients} = load_state_from_ets(Server),  %% Load the state from ETS
      {ok, NewServerPid} = rpc:call(Node, server, start, [Server]),
      NewServerPid ! {restore_state, {Messages, Clients}}, %% Send the restore state message to the new server
      receive
        {state_restored, NewServerPid} ->
          erlang:unlink(NewServerPid), %% Unlink the server process before it exits
          monitor_server({Server, Node}, Ref)
      end;
    _ ->
      monitor_loop(Server, Node, Ref)
  end.


%% @doc Monitors the server processes on the specified nodes.
%% @param Servers The list of server processes.
%% @spec monitor_servers([{atom(), atom()}]) -> ok
monitor_servers(Servers) ->
  lists:foreach(fun(Server) -> spawn(fun() -> monitor_server(Server) end) end, Servers).


%% @doc The main loop of the router process.
%% This function waits for messages and handles them as they arrive.
%% If the router receives a message to save the state, it will save the state to ETS.
%% If the router receives a message to load the state, it will load the state from ETS and send it to the requester.
loop() ->
  receive
    {save_state, Server, {Messages, Clients}} ->
      save_state_to_ets(Server, {Messages, Clients}),
      loop();
    {load_state, From, Server} ->
      {Messages, Clients} = load_state_from_ets(Server),
      From ! {state, {Messages, Clients}},
      loop();
    _ ->
      loop()
  end.


%% @doc Saves the state of the server to ETS.
%% @param ServerName The name of the server process.
%% @param State The state of the server to save.
%% @spec save_state_to_ets(atom(), {list(), list()}) -> ok
%% Saves the state of the server to ETS.
save_state_to_ets(Server, {Messages, Clients}) ->
  ets:insert(server_state, {Server, {Messages, Clients}}),
  io:format("State saved to ETS for ~p: Messages ~p, Clients ~p~n", [Server, Messages, Clients]).

%% @doc Loads the state of the server from ETS.
%% @param ServerName The name of the server process.
%% @spec load_state_from_ets(atom()) -> {list(), list()}
%% If the state is found in the ETS table, it will return the state, otherwise it will return an empty state.
load_state_from_ets(Server) ->
  case ets:lookup(server_state, Server) of
    [{_, {Messages, Clients}}] ->
      io:format("Loaded state from ETS for ~p: Messages ~p, Clients ~p~n", [Server, Messages, Clients]),
      {Messages, Clients};
    [] ->
      io:format("No state found in ETS for ~p, initializing empty state~n", [Server]),
      {[], []}
  end.

