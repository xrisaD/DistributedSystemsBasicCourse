-module(vec).
-export([zero/0, clock/0, inc/2, merge/2, update/3, safe/2]).

% represent a Vector Clock

% return the initial Vector
zero() -> [].

clock() -> zero().

% return the time T incremented by one
inc(Name, Vector) -> case lists:keyfind(Name, 1, Vector) of
                        % increase the time
                        {_, T} -> lists:keyreplace(Name, 1, Vector, {Name, T+1});
                        % init
                        false -> [{Name, 0} | Vector] end.

% merge two Vectros
merge([], Time) -> Time;

merge([{Name, Ti}|Rest], Time) ->
            case lists:keyfind(Name, 1, Time) of
                {Name, Tj} -> [{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
                false -> [ {Name, Ti} | merge(Rest, Time)]
            end.

%  true if Ti is less than or equal to Tj
% a vector is less than or equal to another vector 
% if each of its entries are less than or equal to the entries of the other one
leq([], Time) -> [true];

leq([{Name, Ti}|Rest], Time) ->
            case lists:keyfind(Name, 1, Time) of
                {Name, Tj} -> [(Ti =< Tj) | leq(Rest, lists:keydelete(Name, 1, Time))];
                false -> [false | leq(Rest, Time)]
            end.

% return a clock that has been updated given that we have received a log message from a node at a given time
update(From, Vector, Clock) ->
        {_, NewTime} = lists:keyfind(From, 1, Vector),
        case lists:keyfind(From, 1, Clock) of
            % replace the old value
            {From, _} -> lists:keyreplace(From, 1, Clock, {From, NewTime});
            % insert the new value
            false -> [{From, NewTime}| Clock] end.

% is it safe to log an event that happened at a given time, true or false
safe(Vector, Clock) ->
    X = leq(Vector, Clock),
    and_operation(X).

and_operation(List) -> lists:foldl(fun(X, Result) -> X and Result end, true, List).

