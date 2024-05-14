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
-export([start/1]).

%% @doc Starts the server process.
%% @param Server The name to register the server process under.
%% @spec start(atom()) -> ok
start(Server) -> register(Server,spawn(fun() -> loop() end)).

%% @doc The main loop of the server process.
%% This function waits for messages and handles them as they arrive.
%% If a 'stop' message is received, the server process sends a 'server_disconnect' message back to the sender and then stops.
%% If any other message is received, the server process sends a 'happy_to_receive_your_message' message back to the sender and then continues waiting for more messages.
loop() ->
  receive
    {From, stop} ->
      io:format("Received from ~p message to stop!~n",[From]),
      From ! {self(),server_disconnect};
    {From, Msg} ->
      io:format("Received ~p: ~p~n",[From,Msg]),
      io:format("Sending reply...~n"),
      From ! {self(),happy_to_receive_your_message},
      loop()
  end.
