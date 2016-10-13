-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, connected = false, chatrooms = [], nickname = Nick }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
  ServerAtom = list_to_atom(Server),
    case St#client_st.connected of
      true ->
        {reply, {error, user_already_connected, "Client is already connected to server!"}, St};
      false ->
        Response = (catch genserver:request(ServerAtom, {connect, St#client_st.nickname})),
        io:fwrite("Client received: ~p~n", [Response]),
        case Response of
          {'EXIT', "Timeout"} -> {reply, {error, server_not_reached, "Server could not be reached!"}, St};
          {'EXIT', _} -> {reply, {error, server_not_reached, "Server does not exist!"}, St};
          "connected" ->
            NewSt = St#client_st{connected=true, serverAtom = ServerAtom},
            {reply, ok, NewSt};
          nick_taken ->
            NewSt = St,
            {reply, {error, nick_taken, "Other client with same nickname is already connected to server!"}, St}
        end
    end;
% return error after timeout

%% Disconnect from server
handle(St, disconnect) ->
    case St#client_st.connected of
      false ->
        {reply, {error, user_not_connected, "Client is not connected to a server!"}, St};
      true ->
        if
          length(St#client_st.chatrooms) > 0 -> {reply, {error, leave_channels_first, "Leave channels before disconnecting!"}, St};
          true ->
            Response = genserver:request(St#client_st.serverAtom, {disconnect, St#client_st.nickname}),
            io:fwrite("Client received: ~p~n", [Response]),
            case Response of
              "Timeout" -> {reply, {error, server_not_reached, "Server could not be reached!"}, St};
              "disconnected" ->
                NewSt = St#client_st{connected=false, serverAtom = ""},
                {reply, ok, NewSt}
            end
        end
    end;
    % {reply, ok, St} ;

% Join channel
handle(St, {join, Channel}) ->
    %% TODO: Remeber to add the client nickname when sending request to server
    case St#client_st.connected of
      true ->
        Response = genserver:request(St#client_st.serverAtom, {join, Channel, St#client_st.nickname, self()}),
        case Response of
          user_already_joined ->
            {reply, {error, user_already_joined, "User already joined this chatroom"}, St};
          joined ->
            NewSt = St#client_st{chatrooms = St#client_st.chatrooms ++ [Channel]},
            {reply, ok, NewSt}
        end;
      false ->
        {reply, {error, user_not_connected, "Connect to a server first"}, St}
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    case St#client_st.connected of
      true ->
        Response = genserver:request(St#client_st.serverAtom, {leave, Channel, St#client_st.nickname, self()}),
        case Response of
          user_not_joined ->
            {reply, {error, user_not_joined, "User has not yet joined this chatroom"}, St};
          "left" ->
            NewSt = St#client_st{chatrooms = lists:delete(Channel, St#client_st.chatrooms)},
            {reply, ok, NewSt}
        end;
      false ->
        {reply, {error, user_not_joined, "Connect to a server first"}, St}
    end;


% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
   case St#client_st.connected of
     true ->
      spawn(fun() -> genserver:request(St#client_st.serverAtom, {msg_from_GUI, Channel, Msg, St#client_st.nickname, self()}) end),
%      case Response of
%        user_not_joined ->
%          {reply, {error, user_not_joined, "User has not yet joined this chatroom"}, St};
%        "success" ->
%          {reply, ok, St}
%      end;
    false ->
      {reply, {error, user_not_joined, "Connect to a server first"}, St}
  end;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nickname, St};

%% Change nick
handle(St, {nick, Nick}) ->
    case St#client_st.connected of
      false ->
        NewState = St#client_st{nickname = Nick},
        {reply, ok, NewState};
      true ->
        {reply, {error, user_already_connected, "Cant't change nickname because user is already connected"}, St}
    end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
