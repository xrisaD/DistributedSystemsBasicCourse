-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2, vector/1, geq/2, isLeqOrConcurrent/2]).

% represent a Lamport time

% return an initial Vector value
zero() -> 0.

% create a vector
vector(Nodes) -> lists:map(fun(Node) -> {Node, zero()} end, Nodes).

% return a clock that can keep track of the nodes
clock(Nodes) -> lists:map(fun(Node) -> {Node, vector(Nodes)} end, Nodes).


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

geq(Ti, Tj) -> (Ti >= Tj).

% return a clock that has been updated given that we have received a log message from a node at a given time
update(Node, Vector, Clock) -> 
    {_, Vector2} = lists:keyfind(Node, 1, Clock),
    V1LeqV2 = isLeqOrConcurrent(Vector, Vector2),
    % update all Clock's entries
    if  V1LeqV2 ->  Clock;
        true -> lists:keyreplace(Node, 1, Clock, {Node, Vector})
    end. 

% is it safe to log an event that happened at a given time, true or false
% an event is safe to be printed if its vector is leq or concurrent than every other vector in the clock
safe(Vector, Clock) ->
    List = lists:foldl(fun({Node, Vector2}, Result) -> [ isLeqOrConcurrent(Vector, Vector2) |Result] end,
        [], Clock),
    and_operation(List).

% return true if Vector1 is less or qual than Vector2 
isLeq(Vector1, Vector2) ->
    Combined = Vector1 ++ Vector2,

    % return a list with true/false
    % true if vector[i] <= clock[i] else false
    ListLeq = lists:map(fun(Key) -> TwoValues = proplists:get_all_values(Key, Combined), leq(lists:nth(1, TwoValues), lists:nth(2, TwoValues)) end, 
                                                 proplists:get_keys(Combined)),
    and_operation(ListLeq).

% return true if Vector1 is less or qual than Vector2 
% or Vector1 and vector 2 are concurrent
isLeqOrConcurrent(Vector1, Vector2) ->  
    Combined = Vector1 ++ Vector2,

    % return a list with true/false
    % true if vector[i] => clock[i] else false
     ListGeq = lists:map(fun(Key) -> TwoValues = proplists:get_all_values(Key, Combined), geq(lists:nth(1, TwoValues), lists:nth(2, TwoValues)) end, 
                                                 proplists:get_keys(Combined)),
    % is V1 leq than V2? true or false
    IsLeq = isLeq(Vector1, Vector2),
    % is V1 get than V2? true or false
    IsGeq = and_operation(ListGeq),
    % are concurrent? 2 vectors are concurrent when they are not leq nor geq
    AreCon = nor(IsLeq, IsGeq),
    IsLeq or AreCon.

and_operation(List) -> lists:foldl(fun(X, Result) -> X and Result end, true, List).

nor(X, Y) -> (not (X or Y)).