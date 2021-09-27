
-module(gms1).
-export([start/1, start/2]).

% initialize the first process
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) -> leader(Id, Master, [], [Master]).

% join to the group
start(Id, Grp) -> Self = self(), {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader|Slaves], Group} -> 
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    end.


% The leader keeps:
% Id: a unique name, of the node, only used for debugging
% Master: the process identifier of the application layer
% Slaves: an ordered list of the process identifiers of all slaves in the group
% Group: a list of all application layer processes in the group

leader(Id, Master, Slaves, Group) ->
    receive
        % a message either from its own master or from a peer node.
        {mcast, Msg} ->
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        % a message, from a peer or the master, that is a request from a node to join the group.
        {join, Wrk, Peer} ->
            % add the new process
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop -> ok
    end.

slave(Id, Master, Leader, Slaves, Group) ->
    % monotor the Leader
    erlang:monitor(process, Leader),
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {"DOWN", _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group);
        stop -> ok
    end.

% Broadcast
% send a message to each process in a list
bcast(Id, Message, Slaves) -> lists:foreach(fun(Pid) -> Pid ! Message  end, Slaves).

% Election
% select the first node in its lists of peers and elect this as the leader.
election(Id, Master, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
end.
