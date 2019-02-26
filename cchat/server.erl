-module(server).
-export([start/1,stop/1]).


-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

-record(channel_st, {
    name, % Channelname
    members
}).

-record(user, {
    pid, nick
}).
% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    genserver:start(ServerAtom,[],fun handle/2).


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    % :request to stop_channels and :stop the genserver
    not_implemented.

%State is the list of all Channels
% 2 cases. Either you want to join channels or you want to stop everything
handle(Channels, {join, Ch, Client}) ->
     %If the channel exists :request for the Client to join the channel
     %or :start the server with the :cases function and the state corresponding
     %to all members of the channel and add it to the channel lists
    case lists:member(Ch,Channels) of
        true ->
            Result = (catch genserver:request(list_to_atom(Ch),{join, Client})),
            case Result of
                joined -> {reply,joined,Channels};
                user_already_joined -> {reply,failed,Channels}
            end;
        false ->
            Name = list_to_atom(Ch),
            genserver:start(Name,#channel_st{ name = Name, members = [Client]},fun cases/2),
            {reply,joined,[Ch | Channels]}
    end.

    % :reply in both cases
%handle(Channels, stop_channels) ->
    % Call :stop function on all channels

    % :reply
%    not_implemented.

%State is the Members of the channel
cases(ChannelState, Command) ->
    case Command of
        % Pattern match the Join command
        {join, Client} ->
            % If in channel :reply with fail otherwise
            % Append Client to the Members list and :reply with success
            case lists:member(Client,ChannelState#channel_st.members) of
                true -> {reply,user_already_joined,ChannelState};
                false -> {reply,joined,ChannelState#channel_st{members = [Client | ChannelState#channel_st.members]}}
            end;

        % Pattern match the Leave command
        {leave,Client} ->
            % If not in channel :reply with fail
            % If in channel remove the Client from the Members list and :reply with success
            case lists:member(Client,ChannelState#channel_st.members) of
                false -> {reply,user_not_joined,ChannelState};
                true -> {reply, left,ChannelState#channel_st{members = lists:delete(Client,ChannelState#channel_st.members)}}
            end;
        % Pattern match the Message_send command
        {message_send, Nick, Msg, Pid} ->
            case lists:member(Pid, ChannelState#channel_st.members) of
                true ->
                    spawn( fun() ->
                        [genserver:request(Receivers, {message_receive, ChannelState#channel_st.name, Nick, Msg})
                            || Receivers <- ChannelState#channel_st.members, Receivers =/= Pid]
                           end),
                    {reply, message_send, ChannelState};
                false ->
                    {reply, error, ChannelState}
            end
        %{message_send,Sender,Nick,Msg} ->
        %    % If the Sender is not in the channel :reply with fail
        %    % Otherwise :request a message_receive to all clients, % Can be threaded
        %    io:fwrite("msgsend"),
        %    case lists:member(Sender,ChannelState#channel_st.members) of
        %        false -> {reply, user_not_joined, ChannelState};
        %        true ->
        %            spawn(
        %                %fun () -> [io:fwrite("hej") || User <- ChannelState#channel_st.members, User =/= Sender] end),
        %                fun () -> [genserver:request(User,{message_receive, ChannelState#channel_st.name, Nick, Msg}) || User <- ChannelState#channel_st.members, User =/= Sender] end),
        %            {reply,sent,ChannelState}
        %    end

    end.
%   not_implemented.

