-module(node1).
-export([start/1, start/2]).

-define(Timeout, 1000).
-define(Stabilize, 1000).

% First version
% handle a growing ring

% Start (first node)
start(Id) -> start(Id, nil).

% Start (Connect to a ring).
start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    % a node starts with null predecessor
    Predecessor = nil,
    % create or connect to a ring
    {ok, Successor} = connect(Id, Peer),
    % stabilize
    schedule_stabilize(),
    % start node
    node(Id, Predecessor, Successor).

% The first node will have itself as a predecessor
connect(Id, nil) -> {ok, {Id, self()}};

connect(Id, Peer) ->
    % send a key message to the know Peer
    % we need to know its key
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            % receive our predecessor's key and send it back to the init function
            {ok, {Skey, Peer}}
        after ?Timeout -> io:format("Time out: no response~n",[])
    end.

% when a node is created
% send stabilize message to yourself after some time
schedule_stabilize() -> timer:send_interval(?Stabilize, self(), stabilize).

node(Id, Predecessor, Successor) ->
    receive
        % a peer needs to know our key
        {key, Qref, Peer} ->
            % send the key to the Peer
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor);
        % a new node informs us of its existence
        {notify, New} ->
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        % a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        % our successor informs us about its predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        % will be send by schedule stabilize
        stabilize ->
            % io:format("Successor: ~p~n", [Successor]),
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        % added for debugging purposes
        info -> 
            io:format("Predecessor: ~p~nSuccessor~p~n", [Predecessor, Successor]),
             node(Id, Predecessor, Successor);
        % Probe messages
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor)
    end.


% send a request message to its successor.
stabilize({_, Spid}) -> Spid ! {request, self()}.

% Pred: our successor's current predecessor 
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            % notify the sucessor about our existence
            Spid ! {notify, {Id, self()}}, Successor;
        {Id, _} ->
            % it is pointing back to us we don’t have to do anything.
            Successor;
        {Skey, _} ->
            % it is pointing to itself we should of course notify it about our existence
            Spid ! {notify, {Id, self()}}, Successor;
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                     %  we should place ourselves behind the predecessor
                    Xpid ! {request, self()},
                    Pred; % the predecessor will become our sucessor
                false -> 
                    % slide in between the two nodes
                    % inform our successor of our existence.
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.


request(Peer, Predecessor) ->
    case Predecessor of
        nil -> Peer ! {status, nil};
        {Pkey, Ppid} -> Peer ! {status, {Pkey, Ppid}}
    end.

% return our predecessor
notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil -> {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true -> {Nkey, Npid};
                false -> Predecessor
            end
    end.


% Probe functions: for testing purposes

create_probe(Id, Successor) -> 
    {_, Spid} = Successor,
    T = erlang:system_time(micro_seconds),
    Spid ! {probe, Id, [Id], T}.

remove_probe(T, Nodes) -> 
    Now = erlang:system_time(micro_seconds),
    TotalTime = Now - T,
    io:format("T: ~p~nNodes:~p~n", [TotalTime, Nodes]).

forward_probe(Ref, T, Nodes, Id, Successor) -> 
    {_, Spid} = Successor, 
    Spid ! {probe, Ref, Nodes ++ [Id], T}.