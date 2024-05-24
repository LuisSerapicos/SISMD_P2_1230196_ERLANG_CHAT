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
-export([start/0, monitor_server/1, loop/0, load_state_from_ets/0, save_state_to_ets/1]).

%% @doc Starts the router process and registers it globally.
%% @spec start() -> {ok, pid()}
%% Starts the router process and registers it globally, so it can be accessed from other nodes.
start() ->
  Pid = spawn_link(fun() -> init() end),
  global:register_name(router, Pid), %% Register the router globally
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
get_server_pid({ServerName, Node}) ->
  io:format("Sending get_pid message to ~p on ~p~n", [ServerName, Node]),
  {ServerName, Node} ! {self(), get_pid},
  receive
    {server, ServerPid} ->
      io:format("Received PID ~p from ~p on ~p~n", [ServerPid, ServerName, Node]),
      ServerPid
  after
    10000 ->
      io:format("Timeout waiting for PID from ~p on ~p~n", [ServerName, Node]),
      {error, timeout}
  end.


%% @doc Monitors the server process on the specified node.
%% @param ServerName The name of the server process.
%% @param Node The name of the node the server is running on.
%% @spec monitor_server({atom(), atom()}) -> ok
monitor_server({ServerName, Node}) ->
  io:format("Monitoring ~p on ~p~n", [ServerName, Node]),
  ServerPid = get_server_pid({ServerName, Node}),
  case ServerPid of
    {error, timeout} ->
      io:format("Failed to get the PID of the server ~p~n", [ServerName]);
    _ ->
      io:format("Got PID ~p for ~p on ~p, starting to monitor~n", [ServerPid, ServerName, Node]),
      Ref = erlang:monitor(process, {ServerName, Node}),
      monitor_loop(ServerName, Node, Ref)
  end.


%% @doc Monitors the server process.
%% @param ServerName The name of the server process.
%% @param Node The name of the node the server is running on.
%% @param Ref The reference to the monitor process.
%% @spec monitor_loop(atom(), atom(), reference()) -> ok
%% Make RPC call to start a new server instance on the specified node.
monitor_loop(ServerName, Node, Ref) ->
  receive
    {'DOWN', Ref, process, _, Reason} ->
      io:format("Server ~p exited with reason ~p~n", [ServerName, Reason]),
      {Messages, Clients} = load_state_from_ets(),  %% Load the state from ETS
      {ok, NewServerPid} = rpc:call(Node, server, start, [ServerName]),
      NewServerPid ! {restore_state, {Messages, Clients}}, %% Send the restore state message to the new server
      receive
        {state_restored, NewServerPid} ->
          monitor_server({ServerName, Node})
      end;
    _ ->
      monitor_loop(ServerName, Node, Ref)
  end.


%% @doc The main loop of the router process.
%% This function waits for messages and handles them as they arrive.
%% If the router receives a message to save the state, it will save the state to ETS.
%% If the router receives a message to load the state, it will load the state from ETS and send it to the requester.
loop() ->
  receive
    {save_state, {Messages, Clients}} ->
      save_state_to_ets({Messages, Clients}),
      loop();
    {load_state, From} ->
      {Messages, Clients} = load_state_from_ets(),
      From ! {state, {Messages, Clients}},
      loop();
    _ ->
      loop()
  end.


%% @doc Saves the state of the server to ETS.
%% @param State The state of the server to save.
%% @spec save_state({list(), list()}) -> ok
%% Saves the state of the server to ETS.
save_state_to_ets({Messages, Clients}) ->
  ets:insert(server_state, {state, {Messages, Clients}}),
  io:format("State saved to ETS: Messages ~p, Clients ~p~n", [Messages, Clients]).


%% @doc Loads the state of the server from ETS.
%% @spec load_state_from_ets() -> {list(), list()}
%% If the state is found in the ETS table, it will return the state, otherwise it will return an empty state.
load_state_from_ets() ->
  case ets:lookup(server_state, state) of
    [{state, {Messages, Clients}}] ->
      io:format("Loaded state from ETS: Messages ~p, Clients ~p~n", [Messages, Clients]),
      {Messages, Clients};
    [] ->
      io:format("No state found in ETS, initializing empty state~n"),
      {[], []}
  end.

