-module(test).

-compile(export_all).

-define(Timeout, 1000).


run() -> 
    N = 1000,
    % 4 machines building the ring
    % 4 machines testing the performance

    % add 1 node to the ring
    FirstNode = start(node2),
    % add 3 extra nodes to the ring
    ThreeNodes = start(node2, 3, FirstNode),
    timer:sleep(10000),
    % add N keys, all starting from the first node
    ExistingKeys = keys(N),
    add(ExistingKeys, FirstNode),
    % generate N keys
    NewKeys = keys(N),

    timer:sleep(10000),
    % First Experiment: One machine handles 4000 lookups  
    io:format("Experiment 1:~n~nOne machine with 4000 lookups~n"),
    Exp1Keys = ExistingKeys ++ ExistingKeys ++ NewKeys ++ NewKeys,
    check(Exp1Keys, FirstNode, true),

    timer:sleep(1000),

    % % Second Experiment: 4 machine handles 1000 lookups each
    Total = init_run_check(ExistingKeys, NewKeys, FirstNode, ThreeNodes),
     io:format("~n~nExperiment 2:~n4 machines with 1000 lookups each~n"),
    io:format("Total: ~p~n", [Total])
    .

init_run_check(ExistingKeys, NewKeys, FirstNode, ThreeNodes) -> 
    S = self(),
    % First Node
    spawn(fun() -> run_check(ExistingKeys, FirstNode, S) end),
    % Second Node
    spawn(fun() -> run_check(NewKeys, lists:nth(1, ThreeNodes), S) end),
    % Third Node
    spawn(fun() -> run_check(ExistingKeys, lists:nth(2, ThreeNodes), S) end),
    % Last Node
    spawn(fun() -> run_check(NewKeys, lists:nth(3, ThreeNodes), S) end),
    
    gather_results(3, 0).

gather_results(0, Total) -> Total;

gather_results(N, Total) -> 
    receive 
        {result, X} -> gather_results(N-1, Total+X)
    end.

run_check(Keys, P, Pid) -> 
    Result = check(Keys, P, false),
    Pid ! {result, Result}.


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
            io:format("~w lookups failed, ~w caused a timeout ~n", [Failed, Timeout]);
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


    








