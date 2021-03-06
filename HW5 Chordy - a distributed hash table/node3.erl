-module(node3).
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

% monitor and demonitor
monitor(Pid) -> erlang:monitor(process, Pid).
drop(nil) -> ok;
drop(Ref) -> erlang:demonitor(Ref, [flush]).

init(Id, Peer) ->
    % a node starts with null predecessor
    Predecessor = nil,
    % create or connect to a ring
    {ok, Successor, Next} = connect(Id, Peer),
    % stabilize
    schedule_stabilize(),
    % start node
    node(Id, Predecessor, Successor, storage:create(), Next).

% The first node will have itself as a predecessor
connect(Id, nil) ->S =self(), {ok, {Id, nil, S}, {Id, S}};

connect(Id, Peer) ->
    % send a key message to the know Peer
    % we need to know its key
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            % receive our predecessor's key and send it back to the init function
            {ok, {Skey, nil, Peer}, nil}
        after ?Timeout -> io:format("Time out: no response~n",[])
    end.

% when a node is created
% send stabilize message to yourself after some time
schedule_stabilize() -> timer:send_interval(?Stabilize, self(), stabilize).

node(Id, Predecessor, Successor, Store, Next) ->
    receive
        % a peer needs to know our key
        {key, Qref, Peer} ->
            % send the key to the Peer
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store, Next);
        % a new node informs us of its existence
        {notify, New} ->
            {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, UpdatedStore, Next);
        % the predecessor needs to know our predecessor and our successor
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        % our successor informs us about its predecessor (and about its successor)
        {status, Pred, Nx} ->
            {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
            node(Id, Predecessor, Succ, Store, Nxt);
        % will be send by schedule stabilize
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store, Next);
        % added for debugging purposes
        info -> 
            io:format("ID: ~p~nPredecessor: ~p~nSuccessor~p~nStore~p~nNext:~p~n", [Id, Predecessor, Successor, Store, Next]),
             node(Id, Predecessor, Successor, Store, Next);
        % Probe messages
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
        % add
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);
        % lookup
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        % delegate responsibility.
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);
        % monitor both predecessor and successor
        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);
        stop -> stop
    end.


% demonitor:

% if our predecessor crashed
down(Ref, {_, Ref, _}, Successor, Next) -> 
    drop(Ref), 
    {nil, Successor, Next};

% if our successor crashed
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    drop(Ref),
    Nref = monitor(Npid),
    % inform our new successor of our existance
    Npid ! {notify, {Nkey, Npid}}, 
    {Predecessor, {Nkey, Nref, Npid}, nil}.

% storage functions
add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _,Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            % return the updated store
            storage:add(Key, Value, Store);
        false ->
            % send message to our successor
            Spid ! {add, Key, Value, Qref, Client},
            % the store won't be updated
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
    true ->
        Result = storage:lookup(Key, Store),
        % send to the requester
        Client ! {Qref, Result};
    false ->
        {_, Spid} = Successor,
        % send message to our successor
        Spid ! {lookup, Key, Qref, Client}
    end.

% send a request message to its successor.
stabilize({Skey, _, Spid}) -> Spid ! {request, self()}.

% Pred: our successor's current predecessor 
stabilize(Pred, Id, Successor, Nx) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil -> 
            % notify the successor about our existence
            Spid ! {notify, {Id, self()}},
            Ref = monitor(Spid),
            {{Skey, Ref, Spid}, Nx};
        {Id, _, _} ->
            % it is pointing back to us we don???t have to do anything.
            {Successor, Nx};
        {Skey, _, _} ->
            % it is pointing to itself we should of course notify it about our existence
            Ref = monitor(Spid),
            Spid ! {notify, {Id, self()}}, 
            {{Skey, Ref, Spid}, Nx};
        {Xkey, _, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    %  we should place ourselves behind the predecessor
                    Xpid ! {request, self()},
                    % demonitor the previous successor
                    drop(Sref),
                    % the predecessor will become our successor
                    % so our previous successor will become the next successor
                    % because our new successor is the previous predecessor it is already monitored
                    {Pred, {Skey, Spid}}; 
                false -> 
                    % slide in between the two nodes
                    % inform our successor of our existence.
                    Spid ! {notify, {Id, self()}},
                    Ref = monitor(Spid),
                    {{Skey, Ref, Spid}, Nx}
            end
    end.

request(Peer, Predecessor, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Predecessor of
        nil -> Peer ! {status, nil, {Skey, Spid}};
        {Pkey, Pref, Ppid} -> Peer ! {status, {Pkey, Pref, Ppid}, {Skey, Spid}}
    end.


% return our predecessor and the updated store
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        % in case we didn't have a predecessor this is our new predecessor
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Ref = monitor(Npid),
            {{Nkey, Ref, Npid}, Keep};
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    Keep = handover(Id, Store, Nkey, Npid),
                    % demonitor the previous predecessor
                    drop(Pref),
                    % monitor the new predecessor
                    Ref = monitor(Npid),
                    {{Nkey, Ref, Npid}, Keep};
                false -> {Predecessor, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    % we keep the entries betweeen (Pkey, Id], where Pkey is the predecessor's id
    {Keep, Rest} = storage:split(Nkey, Id, Store),
    % we handover the rest of the entries which aren't ours to our successor
    Npid ! {handover, Rest},
    Keep.

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