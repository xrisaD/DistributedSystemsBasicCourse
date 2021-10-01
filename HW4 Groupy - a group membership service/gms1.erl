
-module(gms1).
-export([start/1, start/2]).

% initialize the first process
% Since it is the only node in the
% group it will of course be the leader of the group
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) -> leader(Id, Master, [], [Master]).

% join to the group
start(Id, Grp) -> Self = self(), {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    % send message to a node in the group
    Grp ! {join, Master, Self},
    % wait for the invitation
    receive
        {view, [Leader|Slaves], Group} -> 
            Master ! {view, Group},
            % join as
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
            % broadcast the message to all slaves
            bcast(Id, {msg, Msg}, Slaves),
            % send the message to master
            Master ! Msg,
            % wait for the next message
            leader(Id, Master, Slaves, Group);
        % a message, from a peer or the master, that is a request from a node to join the group.
        {join, Wrk, Peer} ->
            % add the new process
            % we add the new node at the end of the list of peers
            % we want the new node to be the last one to see the view message that we send out.
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            % broadcast a view message to all slaves
            % {view, Slaves and Master, Group}
            % all slaves will have the updated view
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            % wait for the next message
            leader(Id, Master, Slaves2, Group2);
        stop -> ok
    end.

% main responsibility to forward messages
slave(Id, Master, Leader, Slaves, Group) ->
    receive
        % messages for the Leader
        {mcast, Msg} ->
            % send multicast message to the leader
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            % send join message to the leader
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        % messages for the Master
        {msg, Msg} ->
            % send multicast message to the master
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} ->
            % send view message to the master
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop -> ok
    end.

% Broadcast
% send a message to each process in a list
bcast(Id, Message, Slaves) -> 
    lists:foreach(fun(Pid) -> Pid ! Message  end, Slaves).
