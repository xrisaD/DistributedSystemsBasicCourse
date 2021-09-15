-module(test2).
-export([run_bench/5]).

% Test 2: Get the average time
% Id: the caller id e.g. self()
% Host: the host
% Port: the port
% N: number of machines
% M: the number of requests each machine will send
run_bench(Id, Host, Port, N, M) ->
    %% start receiver which will get the tests' results
    Pid = start_receiver(Id, N),
    %% start machines
    send(Pid, Host, Port, N, M).

% Create N processes
send(Pid, Host, Port, N, M) ->
    if
        N < 0 -> ok;
        true -> spawn(fun() -> run_one_bench(Pid, Host, Port, M) end), % create the N-th process
                send(Pid, Host, Port, N-1, M) % create the next ((N-1)th) process
    end.

start_receiver(Id, N) -> spawn(fun() -> receiver(Id, N, 0, 0) end).

% K: counts the number of received results
% R: the result
receiver(Id, N, K, R) ->
   receive
        Time ->
            if 
                N > K -> receiver(Id, N, K+1, Time+R); % save the time and wait for the next one
                true -> Id ! R/N % send the result back to called process
            end
    end.

run_one_bench(Pid, Host, Port, M) ->
    Pid ! test:bench(Host, Port, M)/math:pow(10,6). 