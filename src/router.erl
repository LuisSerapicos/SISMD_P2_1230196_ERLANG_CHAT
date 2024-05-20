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
-export([start/0, get_server_pid/1, monitor_server/1, loop/0]).


start() ->
  Pid = spawn_link(fun() -> loop() end),
  register(router, Pid).

get_server_pid({ServerName, Node}) ->
  io:format("Sending get_pid message to ~p on ~p~n", [ServerName, Node]),
  {ServerName, Node} ! {self(), get_pid},
  receive
    {server, Pid} ->
      io:format("Received PID ~p from ~p on ~p~n", [Pid, ServerName, Node]),
      Pid
  after
    5000 ->
      io:format("Timeout waiting for PID from ~p on ~p~n", [ServerName, Node]),
      {error, timeout}  % waits for 5 seconds
  end.

monitor_server({ServerName, Node}) ->
  io:format("Monitoring ~p on ~p~n", [ServerName, Node]),
  ServerPid = get_server_pid({ServerName, Node}),
  case ServerPid of
    {error, timeout} ->
      io:format("Failed to get the PID of the server ~p~n", [ServerName]);
    _ ->
      io:format("Got PID ~p for ~p on ~p, starting to monitor~n", [ServerPid, ServerName, Node]),
      Ref = erlang:monitor(process, {ServerName, Node}),
      receive
        {'DOWN', Ref, process, _, Reason} ->
          io:format("Server ~p exited with reason ~p~n", [ServerName, Reason]),
          start(),
          loop()
      end
  end.

loop() ->
  receive
    {From, get_pid} ->
      From ! {router, whereis(server)},
      loop();
    {From, monitor} ->
      process_flag(trap_exit, true),
      link(whereis(server)),
      loop();
    {'EXIT', Pid, Reason} ->
      io:format("Server process exited with reason ~p~n", [Reason]),
      start(),
      loop()
  end.

