-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2, vector/1, isLeq/2, atLeastOneLess/2]).

% represent a Lamport time

% return an initial Vector value
zero() -> 0.

% create a vector
vector(Nodes) -> lists:map(fun(Node) -> {Node, zero()} end, Nodes).

% return the time T incremented by one
inc(Name, Vector) -> {_, T} = lists:keyfind(Name, 1, Vector),
                    lists:keyreplace(Name, 1, Vector, {Name, T+1}).

% merge the two Vector Clocks
merge(Vectori, Vectorj) -> Combined = Vectori ++ Vectorj,
                           % for each key, get the max between the two values
                           lists:map(fun(Key) -> {Key, lists:max(proplists:get_all_values(Key, Combined))} end, 
                                                 proplists:get_keys(Combined)).

%  true if Ti is less than or equal to Tj
leq(Ti, Tj) -> (Ti =< Tj).

less(Ti, Tj) -> (Ti < Tj).

% return a clock that can keep track of the nodes
clock(Nodes) -> lists:map(fun(Node) -> {Node, vector(Nodes)} end, Nodes).

% return a clock that has been updated given that we have received a log message from a node at a given time
update(Node, Vector, Clock) -> 
    {_, Vector2} = lists:keyfind(Node, 1, Clock),
    V1LeqV2 = leq(Vector, Vector2),
    % update all Clock's entries
    if  V1LeqV2 ->  Clock;
        true -> lists:keyreplace(Node, 1, Clock, {Node, Vector})
    end.

% is it safe to log an event that happened at a given time, true or false
safe(Vector, Clock) ->
    
    List1 = lists:foldl(fun({Node, Vector2}, Result) -> [isLeq(Vector, Vector2) |Result] end,
        [], Clock),
    List2 = lists:foldl(fun({Node, Vector2}, Result) -> [atLeastOneLess(Vector, Vector2) |Result] end,
        [], Clock),
    and_operation(List1) and and_operation(List2).


% return if Vector1 is smaller than Vector2
% smaller means: Vector1[j] <= Vector2[j], for each j=1,..J
% return true or false
isLeq(Vector1, Vector2) ->  
    
    Combined = Vector1 ++ Vector2,
    %io:format("VECTOR1: ~p ~n VECTOR2: ~p ~n COMBINED: ~p ~n", [Vector1, Vector2, Combined]),
    % return a list with true/false
    % true if vector[i] <= clock[i] else false
    List = lists:map(fun(Key) -> TwoValues = proplists:get_all_values(Key, Combined), leq(lists:nth(1, TwoValues), lists:nth(2, TwoValues)) end, 
                                                 proplists:get_keys(Combined)),
    and_operation(List).

atLeastOneLess(Vector1, Vector2) ->
    Combined = Vector1 ++ Vector2,
    List = lists:map(fun(Key) -> TwoValues = proplists:get_all_values(Key, Combined), leq(lists:nth(1, TwoValues), lists:nth(2, TwoValues)) end, 
                                                 proplists:get_keys(Combined)),
    or_operation(List).

and_operation(List) -> lists:foldl(fun(X, Result) -> X and Result end, true, List).

or_operation(List) -> lists:foldl(fun(X, Result) -> X or Result end, true, List).