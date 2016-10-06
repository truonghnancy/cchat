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
        Response = genserver:request(ServerAtom, connect),
        io:fwrite("Client received: ~p~n", [Response]),
        case Response of
          "Timeout" -> {reply, {error, server_not_reached, "Server could not be reached!"}, St};
          "connected" ->
            NewSt = St#client_st{connected=true},
            {reply, ok, NewSt}
        end
    end;
% return error after timeout

%% Disconnect from server
handle(St, disconnect) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Join channel
handle(St, {join, Channel}) ->
    % {reply, ok, St} ;
    %% TODO: Remeber to add the client nickname when sending request to server
    {reply, {error, not_implemented, "Not implemented"}, St} ;

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
    % {reply, "nick", St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
