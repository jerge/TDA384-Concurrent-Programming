-module(server).
-export([start/1,stop/1]).

-record(server_state, {
    users = [], % All users in the server
    channels = #{} % All channels mapped to the users in the channel
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
        %NewChannels = State#server_state.channels,
        %NewUsers = State#server_state.users,
        % Join
        {From, GPid, Channel, Nick} ->
            Channels = State#server_state.channels,
            Users = State#server_state.users,
            case maps:is_key(Channel,Channels) of 
                false -> 
                    NewChannels = maps:put(Channel,[#user{pid = GPid, nick = Nick}],Channels),
                    From ! {true},
                    loop(ServerAtom,#server_state{users = Users, channels = NewChannels});
                true ->
                    loop(ServerAtom,#server_state{users = Users, channels = Channels})
                end
            %loop(ServerAtom,#server_state{users = State#server_state.users, channels = Channels});
            %loop(ServerAtom,State)
        %stop -> 
        %    true
        end.

