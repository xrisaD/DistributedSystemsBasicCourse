-module(closet_keep_state).
-compile(export_all).

my_closet(ClothList) ->
    receive
    {From, {store, Cloth}} -> From ! {self(), ok},my_closet([Cloth|ClothList]);
    {From, {take, Cloth}} ->
        case lists:member(Cloth, ClothList) of
            true ->
                From ! {self(), {ok, Cloth}},
                my_closet(lists:delete(Cloth, ClothList));
            false ->
                From ! {self(), not_found},
                my_closet(ClothList)
        end;
    terminate ->
        ok
end.

store(Pid, Cloth) ->
    Pid ! {self(), {store, Cloth}},
    receive
    {Pid, Msg} -> Msg
    after 3000 ->
timeout
    end.
 
take(Pid, Cloth) ->
    Pid ! {self(), {take, Cloth}},
    receive
    {Pid, Msg} -> Msg
end.

start(ClothList) -> spawn(?MODULE, my_closet, [ClothList]).