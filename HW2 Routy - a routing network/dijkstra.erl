-module(dijkstra).
-export([iterate/3, table/2, route/2]).

% SortedList:

% returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted) -> 
    case lists:keyfind(Node, 1, Sorted) of
        false -> 0;
        {_, Length, _} -> Length
    end.

% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. The resulting list should of course be sorted.
replace(Node, N, Gateway, Sorted) ->
    NewSorted = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
    lists:keysort(2, NewSorted).

% update the list Sorted given
% the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added. Only if we have a
% better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) -> 
    % get the length of the existing path
    ExistingN = entry(Node, Sorted),
    % if the entry exists and the new path is shorter than the old path, replace the old one with the new one
    if 
        (ExistingN /= 0) and (N < ExistingN) -> replace(Node, N, Gateway, Sorted);
        true ->  Sorted
    end.

% Dijkstra
% construct a table given a sorted list of nodes, a map and a table constructed so far.
iterate(Sorted, Map, Table) ->
    case Sorted of
        % empty means we're done, the table is completed
        [] -> Table; 
        [H|T] -> % not empty
           {Node, Length, Gateway} =  H, % the first element
           if 
              % dummy entry means the table is completed
              (Length == inf) -> Table; 
              % the table is not completed
              true -> 
                % find the nodes in the map reachable from this entry
                ReachableNodes = map:reachable(Node, Map),
                % for each of these nodes update the Sorted list. 
                NewSorted = lists:foldl(fun(Node2, SortedList) -> update(Node2, Length + 1, Node, SortedList) end, T, ReachableNodes),
                % The entry that you took from the sorted list is added to the routing table.
                iterate(NewSorted, Map, Table ++ [{Node,Gateway}])
           end
    end.

%  construct a routing table given the gateways and a map.
table(Gateways, Map) -> 
    % List the nodes of the map 
    Nodes = map:all_nodes(Map),
    % construct a initial sorted list, gateway nodes should be placed first because the list should be sorted (0 < inf)
    % Constuct gateways nodes
    GatewaysSorted = lists:foldl(fun(Node, Result) -> Result ++ [{Node, 0, Node}]  end, [], Gateways),
    % Construct dummy nodes
    AllSorted = lists:foldl(fun(Node, Result) -> Result ++ [{Node, inf, unknown}]  end, GatewaysSorted, Nodes),
    iterate(AllSorted, Map, []).

% search the routing table and return the gateway suitable to route messages to a node
route(Node, Table) -> case lists:keyfind(Node, 1, Table) of
                            {_, Gateway} -> {ok, Gateway};
                            false -> notfound
                      end.