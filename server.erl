-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{serverName = ServerName, channels = []}.

initial_cState(ChatroomName, ServerName) ->
  #chatroom_st{name = ChatroomName, clients = [], serverName = ServerName}.

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
      case lists:any(fun(e) -> e == Channel end, St#server_st.channels) of
        true -> 0;
        false -> 0
      end,
      Response = "joined";
      % check if a chatroom already exists
      % pass in initial_cState & chatroom name & handle_chat
      % have to spawn a new thread to make a new chatroom
    {leave, Channel} ->
      Response = "left";
    {msg_from_GUI, Channel, Msg} ->
      Response = "Connected to shire";
    {nick, Nick} ->
      Response = "Changed nickname"
    end,
    {reply, Response, NewSt}.

handle_chat(St, Request) ->
  0.
