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
-export([start/1, add_remote/1, send_msg/4, leave/1, connect/2]).

%% @doc Starts the client process and registers it under the given name.
%% @param Client The name to register the client process under.
%% @spec start(atom()) -> atom()
start(Client) ->
  Pid = spawn_link(fun() -> loop() end),
  register(Client, Pid),
  {ok, Client}.

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

%% @doc Sends a leave message to the server.
%% @param Client The name of the client process.
%% @spec leave(atom()) -> ok
leave(Client) ->
  case whereis(Client) of
    undefined ->
      io:format("No process registered under ~p~n", [Client]);
    _Pid ->
      Client ! {leave}
  end.

%% @doc Connects the client to the server on the specified node.
%% @param Server The name of the server process.
%% @param Node The name of the node the server is running on.
%% @spec connect(atom(), atom()) -> ok
connect(Server, Node) ->
  {ok, ClientName} = start(client),
  case whereis(ClientName) of
    undefined ->
      io:format("Client not registered~n");
    _Pid ->
      {Server, Node} ! {ClientName, {connect, ClientName}},
      io:format("Client ~p connecting to server ~p on node ~p~n", [ClientName, Server, Node])
  end.

%% @doc The main loop of the client process.
%% This function waits for messages and handles them as they arrive.
%% If a 'send' message is received, the client process sends the message to the specified server on the specified remote machine and then waits for a response.
%% If a 'stop_client' message is received, the client process stops.
loop() ->
  receive
    {send, Server, RemoteMachine, Message} ->
      {Server, RemoteMachine} ! {self(), Message},
      receive
        {_, Reply} -> io:format("Received from server: ~p~n", [Reply])
      end,
      loop();
    {stop_client} ->
      io:format("Client exiting..."),
      exit(normal);
    {leave} ->
      io:format("Client leaving...~n"),
      exit(normal);
    {connect, Server} ->
      loop()
  end.
