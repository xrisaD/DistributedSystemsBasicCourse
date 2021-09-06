-module(ping).
-export([start_pinger/1]).

start_pinger(Address) -> 
    {ponger, Address} ! {ping, self()},
    receive
        pong -> io:format("pong")
    end.