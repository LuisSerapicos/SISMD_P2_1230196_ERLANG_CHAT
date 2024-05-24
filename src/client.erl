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
-export([start/1, send_msg/4, leave/3, connect/3]).

%% @doc Starts the client process and registers it under the given name.
%% @param Client The name of the client process.
%% @spec start(atom()) -> {ok, atom()}
start(Client) ->
  Pid = spawn_link(fun() -> loop() end),
  register(Client, Pid),
  {ok, Client}.


%% @doc Sends a message to a server on a remote machine.
%% @param Client The name of the client process.
%% @param Server The name of the server process.
%% @param Node The name of the remote machine.
%% @param Message The message to send.
%% @spec send_msg(atom(), atom(), atom(), any()) -> ok
send_msg(Client, Server, Node, Message) ->
  case whereis(Client) of
    undefined ->
      io:format("Client ~p not registered~n", [Client]);
    _Pid ->
      Client ! {send, Server, Node, Message}
  end.


%% @doc Sends a leave message to the server.
%% @param Server The name of the server process.
%% @param Node The name of the node the server is running on.
%% @param ClientName The name of the client.
%% @spec leave(atom(), atom(), atom()) -> ok
leave(Server, Node, ClientName) ->
  case whereis(ClientName) of
    undefined ->
      io:format("Client ~p not registered~n", [ClientName]);
    _Pid ->
      {Server, Node} ! {ClientName, {leave, ClientName}},
      io:format("Client ~p left the server ~p on node ~p~n", [ClientName, Server, Node])
  end.


%% @doc Connects the client to the server on the specified node.
%% @param Server The name of the server process.
%% @param Node The name of the node the server is running on.
%% @param ClientName The name of the client.
%% @spec connect(atom(), atom(), atom()) -> ok
connect(Server, Node, ClientName) ->
  {ok, ClientName} = start(ClientName),
  case whereis(ClientName) of
    undefined ->
      io:format("Client ~p not registered~n", [ClientName]);
    _Pid ->
      {Server, Node} ! {ClientName, {connect, ClientName}},
      io:format("Client ~p connected to the server ~p on node ~p~n", [ClientName, Server, Node])
  end.


%% @doc The main loop of the client process.
%% This function waits for messages and handles them as they arrive.
%% If a 'send' message is received, the client process sends the message to the specified server on the specified remote machine and then waits for a response.
%% If a 'leave' message is received, the client process sends a leave message to the server and then exits.
%% If a 'connect' message is received, the client process connects to the specified server on the specified remote machine.
loop() ->
  receive
    {send, Server, Node, Message} ->
      {Server, Node} ! {self(), Message},
      receive
        {_, Reply} -> io:format("Message received from server: ~p~n", [Reply])
      end,
      loop();
    {leave} ->
      io:format("Client leaving...~n"),
      exit(normal);
    {connect, Server} ->
      loop()
  end.

