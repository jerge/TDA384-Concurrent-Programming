-module(serverWOGen).
-export([start/1,stop/1]).

-record(server_state, {
    users = [], % All users in the server TODO Do we need this?
    channels = #{} % All channels mapped to the users in the channel
}).

-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

-record(user, {
    pid, nick
}).
% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    InitialState = #server_state{},
    Pid = spawn(fun () -> loop(ServerAtom, InitialState) end),
    % - Register this process to ServerAtom
    register(ServerAtom, Pid),
    % - Return the process ID
    Pid.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.

loop(ServerAtom, State) ->
    receive 
        % Pattern match the Join command
        {From, GPid, Channel, Nick, join} ->
            Channels = State#server_state.channels,
            Users = State#server_state.users,
            User = #user{pid = GPid, nick = Nick},
            case maps:is_key(Channel,Channels) of 
                false -> 
                    NewChannels = maps:put(Channel,[User],Channels),
                    From ! {true},
                    loop(ServerAtom,#server_state{users = Users, channels = NewChannels});
                true ->
                    UsersInChannel = maps:get(Channel,Channels),
                    InChannel = lists:member(User,UsersInChannel),
                    if 
                        InChannel ->
                            From ! {false};
                            %loop(ServerAtom,State);
                        true ->
                            NewChannels = maps:put(Channel,[User]++UsersInChannel,Channels),
                            From ! {true},
                            loop(ServerAtom,#server_state{users = Users, channels = NewChannels})
                    end


                    %loop(ServerAtom,#server_state{users = Users, channels = Channels})
            end;

        % Pattern math the Leave command
        {From, GPid, Channel, Nick, leave} ->
            Channels = State#server_state.channels,
            Users = State#server_state.users,
            User = #user{pid = GPid, nick = Nick},
            UsersInChannel = maps:get(Channel,Channels),
            InChannel = lists:member(User,UsersInChannel),
            if
                InChannel ->
                    From ! {true},
                    NewChannels = maps:put(Channel,lists:delete(User,UsersInChannel),Channels),
                    loop(ServerAtom,#server_state{users = Users, channels = NewChannels});
                true ->
                    From ! {false},
                    loop(ServerAtom,State)

            end;

        {From, GPid, Channel, Nick, Msg, message_send} ->
            Channels = State#server_state.channels,
            User = #user{pid = GPid, nick = Nick},
            UsersInChannel = maps:get(Channel,Channels),
            InChannel = lists:member(User,UsersInChannel),
            if
                InChannel ->
                    lists:map(
                        (fun (Receiver) -> client:handle(#client_st{gui = Receiver#user.pid}, {message_receive, Channel, Receiver#user.nick, Msg}) end),
                        UsersInChannel),
                    From ! {true};
                true ->
                    From ! {false}
            end,
            loop(ServerAtom,State)
    end.

