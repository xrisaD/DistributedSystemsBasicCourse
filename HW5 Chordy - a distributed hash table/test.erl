-module(test).

-compile(export_all).

-define(Timeout, 1000).
-define(Timeout2, 10000).

run_first_experiment(ExistingKeys, NewKeys, FirstNode) ->
    % First Experiment: One machine handles 4000 lookups  
    io:format("Experiment 1:~n~nOne machine with 4000 lookups~n"),
    Exp1Keys = ExistingKeys ++ ExistingKeys ++ NewKeys ++ NewKeys,
    check(Exp1Keys, FirstNode, true).

run_second_experiment(ExistingKeys, NewKeys, FirstNode, ThreeNodes) ->
    % Second Experiment: 4 machine handles 1000 lookups each
    io:format("Experiment 2:~n4 machines with 1000 lookups each~n"),
    T1 = check(ExistingKeys, FirstNode, false),
    T2 = check(ExistingKeys, lists:nth(1, ThreeNodes), false),
    T3 = check(ExistingKeys, lists:nth(2, ThreeNodes), false),
    T4 = check(ExistingKeys, lists:nth(3, ThreeNodes), false),
    Total = T1 + T2 + T3 + T4,
    io:format("Total: ~p~n", [Total]).

% Experiment: the number of experiment, one or two
% M: the number of nodes we want to be created
run1() -> 
    N = 1000,
    % 4 machines building the ring
    % 4 machines testing the performance

    % create the ring
    {FirstNode, ThreeNodes} = createRing(3),
    timer:sleep(20000),
    % add all keys to the first node
    {ExistingKeys, NewKeys} = add_all_keys(N, FirstNode),
    timer:sleep(1000),

    run_first_experiment(ExistingKeys, NewKeys, FirstNode),
    run_second_experiment(ExistingKeys, NewKeys, FirstNode, ThreeNodes).

run2(NumOfPairs, NumOfMachines) -> 
    % create the ring
    {FirstNode, MNodes} = createRing(NumOfMachines-1),
    timer:sleep(?Timeout2),
    % add all keys
    {ExistingKeys, _} = add_all_keys(NumOfPairs, FirstNode),
    timer:sleep(?Timeout2),
    check(ExistingKeys, FirstNode, true).

createRing(M) -> 
    % add 1 node to the ring
    FirstNode = start(node2),
    % add M extra nodes to the ring
    MNodes = start(node2, M, FirstNode),
    {FirstNode, MNodes}.

add_all_keys(N, FirstNode) ->
    % add N keys, all starting from the first node
    ExistingKeys = keys(N),
    add(ExistingKeys, FirstNode),
    % generate N keys
    NewKeys = keys(N),
    {ExistingKeys, NewKeys}.


%% Starting up a set of nodes is made easier using this function.

start(Module) ->
    Id = key:generate(), 
    apply(Module, start, [Id]).


start(Module, P) ->
    Id = key:generate(), 
    apply(Module, start, [Id, P]).    

start(_, 0, _) ->
    [];

start(Module, N, P) ->
    [start(Module, P)] ++ start(Module, N-1, P).

%% The functions add and lookup can be used to test if a DHT works.

add(Key, Value , P) ->
    Q = make_ref(),
    P ! {add, Key, Value, Q, self()},
    receive 
	{Q, ok} ->
	   ok
	after ?Timeout ->
	    {error, "timeout"}
    end.

lookup(Key, Node) ->
    Q = make_ref(),
    Node ! {lookup, Key, Q, self()},
    receive 
	{Q, Value} ->
	    Value
    after ?Timeout ->
	    {error, "timeout"}
    end.


%% This benchmark can be used for a DHT where we can add and lookup
%% key. In order to use it you need to implement a store.

% returns a list with N random keys
keys(N) ->
    lists:map(fun(_) -> key:generate() end, lists:seq(1,N)).

% for each Key in Keys add it to the ring by sending it to P
add(Keys, P) ->
    lists:foreach(fun(K) -> add(K, gurka, P) end, Keys).

% measure lookup time for all Keys
check(Keys, P, Print) ->
    T1 = now(),
    {Failed, Timeout} = check(Keys, P, 0, 0),
    T2 = now(),
    Done = (timer:now_diff(T2, T1) div 1000),
    case Print of 
        true ->
            io:format("~w lookup operation in ~w ms ~n", [length(Keys), Done]),
            io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]),
            Done;
        false-> Done
    end.


check([], _, Failed, Timeout) ->
    {Failed, Timeout};

check([Key|Keys], P, Failed, Timeout) ->
    case lookup(Key, P) of
        {Key, _} -> 
            check(Keys, P, Failed, Timeout);
        {error, _} -> 
            check(Keys, P, Failed, Timeout+1);
        false ->
            check(Keys, P, Failed+1, Timeout)
    end.


    








