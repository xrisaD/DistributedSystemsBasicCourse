-module(rudy_v2).
-export([start/2, stop/0]).

%% N: pool's size
start(Port, N) -> register(rudy, spawn(fun() -> init(Port,N) end)).

stop() -> exit(whereis(rudy), "time to die").

init(Port,N) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            start_handlers(Listen, N),
            gen_tcp:close(Listen),
            ok;
        {error, Error} -> error
end.

start_handlers(Listen, N) ->
    if
        N>0 -> spawn(fun() -> handler(Listen) end),
                start_handlers(N-1, Listen);
        N == 0 -> ok
    end.


%% listen to the socket for an incoming connection.
handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} -> request(Client);
        {error, Error} -> error
    end, 
    handler(Listen).

%% read the request from the client connection and parse it.
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
    %%timer:sleep(40), %% artificial delay
    http:ok("test").