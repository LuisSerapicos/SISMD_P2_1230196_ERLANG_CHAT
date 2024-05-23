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

start() ->
  Pid = spawn_link(fun() -> init() end),
  global:register_name(router, Pid), %% Register the router globally
  {ok, Pid}.

init() ->
  ets:new(server_state, [named_table, public, set]),
  loop().

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

get_server_pid({ServerName, Node}) ->
  io:format("Sending get_pid message to ~p on ~p~n", [ServerName, Node]),
  {ServerName, Node} ! {self(), get_pid},
  receive
    {server, Pid} ->
      io:format("Received PID ~p from ~p on ~p~n", [Pid, ServerName, Node]),
      Pid
  after
    10000 ->
      io:format("Timeout waiting for PID from ~p on ~p~n", [ServerName, Node]),
      {error, timeout}
  end.

monitor_loop(ServerName, Node, Ref) ->
  receive
    {'DOWN', Ref, process, _, Reason} ->
      io:format("Server ~p exited with reason ~p~n", [ServerName, Reason]),
      %% Load state from ETS
      {Messages, Clients} = load_state_from_ets(),
      %% Start a new server instance on the specified node
      {ok, NewServerPid} = rpc:call(Node, server, start, [ServerName]),
      %% Send the restore state message to the new server
      NewServerPid ! {restore_state, {Messages, Clients}},
      %% Restart monitoring
      receive
        {state_restored, NewServerPid} ->
          %% Restart monitoring
          monitor_server({ServerName, Node})
      end;
    _ ->
      monitor_loop(ServerName, Node, Ref)
  end.

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

save_state_to_ets({Messages, Clients}) ->
  ets:insert(server_state, {state, {Messages, Clients}}),
  io:format("State saved to ETS: Messages ~p, Clients ~p~n", [Messages, Clients]).

load_state_from_ets() ->
  case ets:lookup(server_state, state) of
    [{state, {Messages, Clients}}] ->
      io:format("Loaded state from ETS: Messages ~p, Clients ~p~n", [Messages, Clients]),
      {Messages, Clients};
    [] ->
      io:format("No state found in ETS, initializing empty state~n"),
      {[], []}
  end.
