-module(loggy).
-export([start/1, stop/1]).

start(Nodes) -> spawn_link(fun() ->init(Nodes) end).

stop(Logger) -> Logger ! stop.

init(Nodes) -> Clock = time:clock(Nodes) ,loop(Clock, []).

loop(Clock, Queue) ->
    receive
        {log, From, Vector, Msg} -> 
            % update the clock 
            UpdatedClock = time:update(Clock, Vector),
            % add the message to the hold-back queue
            UpdatedQueue = queue({From, Vector, Msg}, Queue),
            % then go through the queue to find messages that are now safe to print
            % create the updated queue which won't contain the printed messages
            UpdatedQueue2 = lists:foldl(
                            fun({From, Vector, Msg}, NewQueue) ->
                                % check if it's safe
                                case time:safe(Vector, UpdatedClock) of
                                    true-> log(From, Vector, Msg), NewQueue2 = NewQueue; 
                                    false-> NewQueue2 = queue({From, Vector, Msg}, NewQueue) 
                                end, 
                                NewQueue2 
                            end,
                            [], UpdatedQueue),
            loop(UpdatedClock, UpdatedQueue2);
        stop -> ok
    end.

log(From, Time, Msg) -> io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

queue({From, Time, Msg}, Messages) -> lists:keysort(2, [{From, Time, Msg}] ++ Messages).
