-module(key).
-export([generate/0, between/3]).

% return a random number from 1 to 1.000.000.000 
generate() -> random:uniform(100000000).

% partly closed interval, (From, To]
between(Key, From, To) -> 
    if From > To-> (Key > From) or (Key=<To);
       To > From -> (Key>From) and (Key=<To);
       true -> true
    end.
    %(From < Key) and ((Key =< To) or (To < From)).
