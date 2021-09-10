-module(test2).
-compile([export_all]).
-record(result, {res=[]}).


%% Test 2: Get the average time, simulating multiple machines at a time
start_bench(Host, Port, N) ->
    %% start receiver which will get the tests' results
    Pid = start_receiver(N),
    send(Pid, Host, Port, N).


send(Pid, Host, Port, N) ->
    if
        N == 0 -> ok;
        true -> spawn(?MODULE, run_one_bench, [Pid, Host, Port]),
                send(Pid, Host, Port, N-1)
    end.

start_receiver(N) -> spawn(?MODULE, receiver, [N]).

receiver(N) ->
   receive
        {From, Time} ->
            #result { res=[Time|#result.res] }, % save time
            L = lists:flatlength(#result.res),
            io:format("~f~n",[L+0.1]),
            if 
                L == N -> ok
            end
    end,
    receiver(N).

% print(List)->
%  [io:format("Printing ~p ~n",[X])|| X <- List].

run_one_bench(Pid, Host, Port) ->
    %%Time = test:bench(Host, Port),
    Pid ! {self(), test:bench(Host, Port)+0.1}.