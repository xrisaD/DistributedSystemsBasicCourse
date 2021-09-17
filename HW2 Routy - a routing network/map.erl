-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% returns an empty map (a empty list)
new() -> [].

% updates the Map to reflect that Node has directional links to all nodes in the list Links. The old entry is removed.
update(Node, Links, Map) -> 
    Map + [{Node, Links}].

% returns the list of nodes directly reachable from Node.
reachable(Node, Map) -> lists:keyfind(Node, 1, Map).

% returns a list of all nodes in the map, also the ones without outgoing links.
all_nodes(Map) -> lists:foldl(fun ({K, V}, AllNodes) -> AllNodes ++ [K] ++ V end, [], Map).

