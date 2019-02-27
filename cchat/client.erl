-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
  gui, % atom of the GUI process
  nick, % nick/username of the client
  server, % atom of the chat server
  channels
}).

-record(channel_st, {
  name, % Channelname
  members
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
  catch genserver:request(ServerAtom,{change_nick,Nick,Nick}),
  #client_st{
    gui = GUIAtom,
    nick = Nick,
    server = ServerAtom
  }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
  Result = (catch genserver:request(St#client_st.server, {join, Channel, self()})),
  case Result of

    joined -> {reply, ok, St};
    error ->
      {reply, {error, user_already_joined, "The user has already joined"},St};
    {'EXIT',_} ->
      {reply, {error,server_not_reached,"No resp from server"},St}

  end;


% Leave channel
handle(St, {leave, Channel}) ->
      Result = (catch genserver:request(list_to_atom(Channel), {leave, self()})),
      case Result of
        left -> {reply, ok, St};
        error ->
          {reply, {error, user_not_joined, "The user has not joined the channel"},St};
        {'EXIT',_} ->
          {reply, {error,server_not_reached,"No respons from server"},St}


      end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
  case catch genserver:request(list_to_atom(Channel), {message_send, St#client_st.nick, Msg, self()}) of
    message_send ->
      {reply, ok, St};
    error ->
      {reply, {error, user_not_joined, "User has not joined this channel"}, St};
    {'EXIT', _} ->
      {reply, {error, server_not_reached, "No respon from server."}, St}
  end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
  {reply, St#client_st.nick, St};

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
  case catch genserver:request(St#client_st.server, {change_nick, NewNick, St#client_st.nick}) of
    changed -> {reply, ok, St#client_st{nick = NewNick}};
    error -> {reply, {error,nick_taken, "The nick has already been taken"},St}
  end;


% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
  gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
  {reply, ok, St};

% Quit client via GUI
handle(St, quit) ->
  % Any cleanup should happen here, but this is optional
  {reply, ok, St};

% Catch-all for any unhandled requests
handle(St, Data) ->
  {reply, {error, not_implemented, "Client does not handle this command"}, St}.
