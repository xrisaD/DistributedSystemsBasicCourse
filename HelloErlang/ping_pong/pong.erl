-module(ping).
-export([start_ponger/0]).

start_ponger() -> 
    register(ponger, self()),
    receive
        {ping, P} -> 
        io:format("ping"),
        P ! pong
    end.