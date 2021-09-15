-module(test1).
-export([run_bench/4]).

%% Test 1: Get the average time, simulating one machine
% Host: the host
% Port: the 
% N: the number of times the bench function will be called
% M: the number of requests
run_bench(Host, Port, N, M)  ->
    lists:sum([test:bench(Host, Port, M) || _ <- lists:seq(1, N)])/N/math:pow(10,6).