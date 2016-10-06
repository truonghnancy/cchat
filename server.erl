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
  % io:fwrite("Request ~p~n", [Request]),
  case Request of
    connect ->
      Response = "connected",
      NewSt = St;
    disconnect ->
      Response = "disconnected",
      NewSt = St;
    {join, Channel} ->
      ChannelAtom = list_to_atom(Channel),
      case lists:any(fun(e) -> e == Channel end, St#server_st.channels) of
        true -> % it does exist
          % call genserver:request with the channel name to add the client to client list
          Response = genserver:request(ChannelAtom, {addClient, ChannelAtom}),
          NewSt = St;
        false -> % it does NOT exist yet
          genserver:start(ChannelAtom, initial_cState(ChannelAtom, St#server_st.serverName), fun handle_chat/2),
          NewSt = St#server_st{channels = channels ++ [ChannelAtom]},
          Response = "joined"
      end;
    {leave, Channel} ->
      Response = "left",
      NewSt = St;
    {msg_from_GUI, Channel, Msg} ->
      Response = "Connected to shire",
      NewSt = St;
    {nick, Nick} ->
      Response = "Changed nickname",
      NewSt = St
    end,
    {reply, Response, NewSt}.

handle_chat(St, Request) ->
  case Request of
    {addClient, ChannelAtom} ->
      case lists:any(fun(e) -> e == Channel end, St#server_st.channels) of;
    _ ->
      0
  end,
  {reply, Response, NewSt}.
