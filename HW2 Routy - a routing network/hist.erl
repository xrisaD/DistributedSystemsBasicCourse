-module(hist).
-export([]).

% Return a new history, where messages from Name will always be seen as old.
new(Name) -> 0.

% Check if message number N from the Node
% is old or new. If it is old then return old but if it new return {new, Updated}
% where Updated is the updated history
update(Node, N, History) -> 0.
