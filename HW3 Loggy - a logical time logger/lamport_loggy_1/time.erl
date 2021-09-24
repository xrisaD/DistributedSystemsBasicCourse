-module(time).
-export([zero/0, inc/2, merge/2, leq/2]).

% represent a Lamport time

% return an initial Lamport value
zero() -> 0.

% return the time T incremented by one
inc(Name, T) -> T + 1.

% merge the two Lamport time stamps
merge(Ti, Tj) -> max(Ti, Tj).

%  true if Ti is less than or equal to Tj
leq(Ti,Tj) -> (Ti =< Tj).