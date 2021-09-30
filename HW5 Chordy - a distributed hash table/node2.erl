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
    node(Id, Predecessor, Successor, storage:create()).

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

node(Id, Predecessor, Successor, Store) ->
    receive
        % a peer needs to know our key
        {key, Qref, Peer} ->
            % send the key to the Peer
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        % a new node informs us of its existence
        {notify, New} ->
            Pred = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Store);
        % a predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        % our successor informs us about its predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        % will be send by schedule stabilize
        stabilize ->
            % io:format("Successor: ~p~n", [Successor]),
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
        % added for debugging purposes
        info -> 
            io:format("Predecessor: ~p~nSuccessor~p~n", [Predecessor, Successor]),
             node(Id, Predecessor, Successor, Store);
        % Probe messages
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
        % add
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        % lookup
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        % delegate responsibility.
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged)
    end.

% storage functions

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case 0 of
    true ->
        Client ! {Qref, ok},
        storage:add(Key, Value, Store);
    false ->
        % send message to our succesor
        Spid ! {add, Key, Value, Qref, Client},
        % the store won't be updated
        Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case 0 of
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


% return our predecessor and the updated store
notify({Nkey, Npid}, Id, Predecessor, Store) ->
case Predecessor of
    nil ->
        Keep = handover(Id, Store, Nkey, Npid),
        {{Nkey, Npid}, Keep};
    {Pkey, _} ->
        case key:between(Nkey, Pkey, Id) of
            true -> 
                Keep = handover(Id, Store, Nkey, Npid),
                {{Nkey, Npid}, Keep};
            false -> {Predecessor,Store}
        end
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    % handover to the new predecessor the rest of the entries 
    % we keep the entries betweeen (Pkey, Id]
    % here the current node is the predecessor so (Id, Nkey)
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