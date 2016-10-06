-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{serverName = ServerName}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, Request) ->
  Response = "default",
  NewSt = St,
  case Request of
    connect ->
      % set the response
      Response = " Connected to shire";
      % NewSt = St#client_st{connected=true};
    disconnect ->
      0;
    {join, Channel} ->
      0;
    {leave, Channel} ->
      0;
    {msg_from_GUI, Channel, Msg} ->
      0;
    {nick, Nick} ->
      0
    end,
    % io:fwrite("Server received: ~p~n", [Request]),
    % Response = "hi!",
    % io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, NewSt}.
