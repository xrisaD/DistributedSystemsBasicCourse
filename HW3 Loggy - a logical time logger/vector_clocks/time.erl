-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/2, update/3, safe/2, vector_update/3]).

% represent a Vector Clock

% return an initial value
zero() -> 0.

% return the time T incremented by one
inc(Vector, Name) ->
    Node = lists:keyfind(Name, 1, Vector),
    {_, T} = Node,
    update(Node, T+1, Vector).

% merge the two Lamport time stamps
merge(Ti, Tj) -> max(Ti, Tj).

%  true if Ti is less than or equal to Tj
leq(Ti,Tj) -> (Ti =< Tj).

% return a clock that can keep track of the nodes
clock(Nodes) -> lists:map(fun(Node) -> {Node, 0} end, Nodes).

% return a clock that has been updated given that we have received a log message from a node at a given time
update(Node, Time, Clock) -> lists:keyreplace(Node, 1, Clock, {Node, Time}).

% return a clock that has been updated given that we have received a log message from a node at a given time
update(PeerVector, Vector) -> vector_update(PeerVector, Vector, '').

% is it safe to log an event that happened at a given time, true or false
safe(Time, Clock) -> 
    % min time
    Times = lists:foldl(fun({X, Y}, Times) -> [Y|Times] end, [], Clock),
    MinTime = lists:min(Times),
    % io:format("Min: ~p ~n",[MinTime]),
    leq(Time, MinTime).

vector_update(PeerVector, Vector, Name) -> 
    % initialize combined list
    CombinedList = [],
    lists:forearch(fun({Name2, _}) ->  
                    {_, V1} = lists:keyfind(Name2, 1, PeerVector),
                    {_, V2} = lists:keyfind(Name2, 1, Vector),
                    CombinedList = [{Name2, V1, V2}] ++ CombinedList
                    end
                    ,Vector),
    % create the new vector
    lists:foldl(fun({Name2, V1, V2}, NewVector) ->  
                    if Name2 == Name -> NewVector ++ [{Name, V2 + 1}]; % increase
                       true -> NewVector ++ [{Name2}, merge(V1, V2)] % get the max
                    end
                    end, [], CombinedList).