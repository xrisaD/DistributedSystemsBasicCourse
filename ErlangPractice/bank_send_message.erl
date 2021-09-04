-module(bank_send_message).
-export([start/1]).

start(Balance) -> 
    spawn(fun() -> server(Balance) end).
server(Balance) -> receive
                    {deposit, X} -> server(Balance+X);
                    {withdraw, X} -> server(Balance-X);
                    quit -> ok
                   end.