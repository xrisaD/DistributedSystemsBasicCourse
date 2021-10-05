
-module(gms4).
-export([start/1, start/2]).

-define(timeout, 1000).
-define(timeout2, 10).
-define(arghh, 1000).
-define(prob, 10).
-define(times, 4).

% initialize the first process
% Since it is the only node in the
% group it will of course be the leader of the group
start(Id) ->
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Self) end)}.

init(Id, Master) -> leader(Id, Master, 0, [], [Master]).

% join to the group
start(Id, Grp) -> Self = self(), {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    % send message to a node in the group
    Grp ! {join, Master, Self},
    % wait for the invitation
    receive
        {view, N, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            % monitor the leader
            erlang:monitor(process, Leader),
            % join as a slave
            slave(Id, Master, Leader,  N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
        % they leader may is dead and he won't reply
        after ?timeout ->
            Master ! {error, "no reply from leader"}
    end.


% The leader keeps:
% Id: a unique name, of the node, only used for debugging
% Master: the process identifier of the application layer
% Slaves: an ordered list of the process identifiers of all slaves in the group
% Group: a list of all application layer processes in the group

leader(Id, Master, N, Slaves, Group) ->
    receive
        % a message either from its own master or from a peer node.
        {mcast, Msg} ->
            % send the message to master
            Master ! Msg,
            % broadcast the message to all slaves
            bcast2(Id, {msg, N, Msg}, Slaves),
            % wait for the next message
            leader(Id, Master, N + 1, Slaves, Group);
        % a message, from a peer or the master, that is a request from a node to join the group.
        {join, Wrk, Peer} ->
            % add the new process
            % we add the new node at the end of the list of peers
            % we want the new node to be the last one to see the view message that we send out.
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            % broadcast a view message to all slaves
            % all slaves will have the updated view
            bcast1(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            % wait for the next message
            leader(Id, Master, N + 1, Slaves2, Group2);
        stop -> ok
    end.

% main responsibility to forward messages
slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        
        % messages for the Leader from the Master
        {mcast, Msg} ->
            % send multicast message to the leader
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            % send join message to the leader
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);

        % messages for the Master from the Leader
        {{msg, I, _}, Qref} when I < N ->
             % ack this message
            Leader ! {ack, Qref},
            % discard this message
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {{msg, N, Msg}, Qref} ->
             % ack this message
            Leader ! {ack, Qref},
            % send multicast message to the master
            Master ! Msg,
            % save the last message from the leader
            slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
        {view, N, [Leader|Slaves2], Group2} ->
            % send view message to the master
            Master ! {view, Group2},
            % save the last message from the leader
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
        stop -> ok;
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group)
    end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),  
    % the process will select the first node in its lists of peers and elect this as the leader.
    case Slaves of
        % the process finds itself being the first node
        [Self|Rest] ->
            % it will become the leader
            % broadcast the last message received to all the other nodes
            % in case the leader has crashed before the last message has been sent to all the nodes
            bcast1(Id, Last, Rest),
            % broadcast the new view
            bcast1(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            % start leader
            leader(Id, Master, N+1, Rest, Group);
        [Leader|Rest] ->
            % monitor the new leader
            erlang:monitor(process, Leader),
            % start slave
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

% Broadcast
% send a message to each process in a list

% simple broadcast
bcast1(Id, Msg, Nodes) ->
    lists:foreach(fun(Node) -> 
        Node ! Msg
        end, Nodes).


% simulate lost messages
bcast2(Id, Msg, Nodes) ->
    % create a unique reference for each message
    Result = lists:foldl(fun(Node, Res) -> 
        Qref = make_ref(),
        Res ++ [{Qref, Node}] 
        end, [], Nodes),
    % broadcast the message
    bcast3(Id, Msg, Result, ?times).

% broadcast to the given nodes
% the goal is to send them a message with the same Qref
% so if we received an old reply from them it will be also ok
% we will be sure that they have received the message
bcast3(Id, Msg, Result, Times) ->
    send_messages(Result, Msg),
    wait_for_acks(Id, Msg, Result, Times-1).


% send the messages
send_messages(Result, Msg) -> 
        lists:foreach(fun({Qref, Node}) -> send_or_not(Node, Msg, Qref) end, Result).
% simulate lost message byt not sending the message with a specific probability
send_or_not(Node, Msg, Qref) -> 
    case rand:uniform(?prob) of
        ?prob -> dont_send;
            _ -> Node ! {Msg, Qref} % send
    end.

% wait for acks from the receivers
wait_for_acks(_, _, [], _) -> ok, io:format("ALL nodes received the message~n");
wait_for_acks(_, _, _, 0) -> stop, io:format("TIMES=0~n");
wait_for_acks(Id, Msg, Result, Times) -> 
    receive
        {ack, Qref} ->
            Cond = (lists:keyfind(Qref, 1, Result) == false),
            % delete the Qref that we received
            if 
                Cond -> wait_for_acks(Id, Msg, Result, Times);
                true -> % we just received the ack, we don't want to wait for it again
                        Result2 = lists:keydelete(Qref, 1, Result), 
                        wait_for_acks(Id, Msg, Result2, Times)
            end
    after ?timeout2 -> 
        % % if we have received all acks we will stop
        % Cond = length(Result) > 0,
        % % bcast to the nodes that we haven't received a reply from
        % if Cond -> bcast3(Id, Msg, Result, Times);
        %    true -> stop
        % end
        bcast3(Id, Msg, Result, Times)
    end.

