-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% represent a Lamport time

% return an initial Lamport value
zero() -> 0.

% return the time T incremented by one
inc(Name, T) -> T + 1.

% merge the two Lamport time stamps
merge(Ti, Tj) -> max(Ti, Tj).

%  true if Ti is less than or equal to Tj
leq(Ti,Tj) -> (Ti =< Tj).

% return a clock that can keep track of the nodes
clock(Nodes) -> lists:map(fun(Node) -> {Node, 0} end, Nodes).

% return a clock that has been updated given that we have received a log message from a node at a given time
update(Node, Time, Clock) -> lists:keyreplace(Node, 1, Clock, {Node, Time}).

% is it safe to log an event that happened at a given time, true or false
safe(Time, Clock) -> 
    % min time
    Times = lists:foldl(fun(X, Times) -> [X|Times] end, [], Clock),
    MinTime = lists:min(Times),
    {_, Time2} = MinTime,
    leq(Time, Time2).
