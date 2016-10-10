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
        Response = genserver:request(ServerAtom, {connect, St#client_st.nickname}),
        io:fwrite("Client received: ~p~n", [Response]),
        case Response of
          "Timeout" -> {reply, {error, server_not_reached, "Server could not be reached!"}, St};
          "connected" ->
            NewSt = St#client_st{connected=true, serverAtom = ServerAtom},
            {reply, ok, NewSt};
          user_already_connected ->
            NewSt = St,
            {reply, {error, user_already_connected, "Other client with same nickname is already connected to server!"}, St}
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
    % {reply, ok, St} ;
    %% TODO: Remeber to add the client nickname when sending request to server
    Response = genserver:request(St#client_st.serverAtom, {join, Channel, St#client_st.nickname}),
    case Response of
      user_already_joined ->
        {reply, {error, user_already_joined, "User already joined this chatroom"}, St};
      _ ->
        NewSt = St#client_st{chatrooms = St#client_st.chatrooms ++ [Channel]},
        {reply, ok, NewSt}
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

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
