-module(rudy_v1).
-export([start/1, stop/0]).

%% Ruby Version 1
%% Each request is handled concurrently

start(Port) -> register(rudy, spawn(fun() -> init(Port) end)).

stop() -> exit(whereis(rudy), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            gen_tcp:close(Listen),
            ok;
        {error, Error} -> error
end.

%% Create a new process, responsible for handling the requests
handle_request(Client) -> 
    spawn(fun() -> request(Client) end).

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} ->  handle_request(Client),  
                         handler(Listen); % continue handle requests
        {error, Error} -> error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
    {error, Error} ->
        io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    http:ok("test").