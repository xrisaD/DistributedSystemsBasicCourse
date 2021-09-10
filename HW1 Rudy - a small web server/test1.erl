-module(test1).
-export([run_bench/3]).

%% Test 1: Get the average time, simulating one machine at a time
run_bench(Host, Port, N)  ->
    lists:sum([test:bench(Host, Port) || _ <- lists:seq(1, N)])/N.