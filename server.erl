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
  NewSt = St,
  % io:fwrite("Request ~p~n", [Request]),
  case Request of
    connect ->
      % set the response
      Response = "connected";
      % change the state
    disconnect ->
      Response = "disconnected";
    {join, Channel} ->
      Response = "joined";
    {leave, Channel} ->
      Response = "left";
    {msg_from_GUI, Channel, Msg} ->
      Response = "Connected to shire";
    {nick, Nick} ->
      Response = "Changed nickname"
    end,
    {reply, Response, NewSt}.
