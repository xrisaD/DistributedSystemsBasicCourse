-module(dijkstra).
-export([entry/2, replace/4, update/4]).

% returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted) -> 
    case lists:keyfind(Node, 1, Sorted) of
        false -> 0;
        {_, Length, _} -> Length
    end.

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
            % add the new element and 
            % sort the new list based on the second element (the distance)
            lists:keysort(2, NewSorted ++ [{Node, N, Gateway}])
    end.

% update the list Sorted given
% the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added. Only if we have a
% better (shorter) path should we replace the existing entry
update(Node, N, Gateway, Sorted) -> 
    % get the length of the existing path
    ExistingN = entry(Node, Sorted),
    % if the entry exists and the new path is shorter than the old path, replace the old one with the new one
    if 
        (ExistingN /= 0) and (N < ExistingN) -> replace(Node, N, Gateway, Sorted);
        true ->  Sorted
    end.