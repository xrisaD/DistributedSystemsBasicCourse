-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

% create a new store
create() -> [].

% add a key value pair, return the updated store
add(Key, Value, Store) -> [{Key, Value}] ++ Store.

% return a tuple {Key, Value} or the atom false
lookup(Key, Store) -> lists:keyfind(Key, 1, Store).

% return a tuple {Updated, Rest} where the
% updated store only contains the key value pairs requested and the rest
% are found in a list of key-value pairs
split(From, To, Store) -> 
    lists:foldl(fun({Key, Value},  {Updated, Rest}) ->
                    case key:between(Key, From, To) of
                        true ->
                            {[{Key, Value} | Updated], Rest};
                        false ->
                            {Updated, [{Key, Value} | Rest]}
                    end
                end, {[],[]}, Store).

% add a list of key-value pairs to a store
merge(Entries, Store) -> Entries ++ Store.

