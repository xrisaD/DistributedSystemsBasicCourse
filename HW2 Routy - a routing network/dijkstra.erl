-module(dijkstra).
-export([entry/2, replace/4]).

% returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted) -> {NodeName, Length, Gateway} = lists:keyfind(Node, 1, Sorted), Length.

% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. The resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
    % check that Node exists in list
    Found = lists:keyfind(Node, 1, Sorted),
    if 
         Found == false -> 
            Sorted; % return the same list
        true ->
            % delete the old element
            NewSorted = lists:delete(Found, Sorted),
            % add the new element and sort the new list
            lists:keysort(1, NewSorted ++ [{Node, N, Gateway}])
    end.