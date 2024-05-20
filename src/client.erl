%%%-------------------------------------------------------------------
%%% @author Luis Serapicos
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. mai. 2024 23:07
%%%-------------------------------------------------------------------
-module(client).
-author("Luis Serapicos").

%% API
-export([start/1,add_remote/1,send_msg/4,stop_client/1]).

%% @doc Starts the client process.
%% @param Client The name to register the client process under.
%% @spec start(atom()) -> ok
start(Client) ->
  Pid = spawn_link(fun() -> loop() end),
  register(Client, Pid).

%% @doc Adds a remote machine to the client's list of known machines.
%% @param RemoteMachine The name of the remote machine to add.
%% @spec add_remote(atom()) -> pong | pang
add_remote(RemoteMachine) ->
  net_adm:ping(RemoteMachine).

%% @doc Sends a message to a server on a remote machine.
%% @param Client The name of the client process.
%% @param Server The name of the server process.
%% @param RemoteMachine The name of the remote machine.
%% @param Message The message to send.
%% @spec send_msg(atom(), atom(), atom(), any()) -> ok
send_msg(Client, stop, RemoteMachine, _Message) ->
  case whereis(Client) of
    undefined ->
      io:format("No process registered under ~p~n", [Client]);
    _Pid ->
      Client ! {stop, RemoteMachine}
  end;
send_msg(Client, Server, RemoteMachine, Message) ->
  case whereis(Client) of
    undefined ->
      io:format("No process registered under ~p~n", [Client]);
    _Pid ->
      Client ! {send, Server, RemoteMachine, Message}
  end.

%% @doc Stops the client process.
%% @param Client The name of the client process.
%% @spec stop_client(atom()) -> ok
stop_client(Client) ->
  Client ! {stop_client}.

%% @doc The main loop of the client process.
%% This function waits for messages and handles them as they arrive.
%% If a 'send' message is received, the client process sends the message to the specified server on the specified remote machine and then waits for a response.
%% If a 'stop_client' message is received, the client process stops.
loop() ->
  receive
    {send,Server,RemoteMachine,Message} ->
      {Server,RemoteMachine} ! {self(),Message},
      receive
        {_,Reply} -> io:format("Received from server: ~p~n",[Reply])
      end,
      loop();
    {stop_client} ->
      io:format("Cliente exiting...")
  end.
