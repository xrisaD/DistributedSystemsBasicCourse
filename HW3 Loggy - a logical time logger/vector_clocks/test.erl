-module(test).
-export([run/2]).

% report on your initial observations
run(Sleep, Jitter) ->
    % start logger
    Nodes = [john, paul, ringo, george],
    Log = loggy:start(),
    % start 4 workers with a unique seed
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),
    % send peers
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    % sleep 
    timer:sleep(5000),
    % stop logger and workers
    loggy:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).