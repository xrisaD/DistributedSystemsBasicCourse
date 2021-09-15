-module(rudy_v2).
-export([start/2, stop/0]).

%% N: pool's size
start(Port, N) -> register(rudy, spawn(fun() -> init(Port, N) end)).

stop() -> rudy ! stop.

init(Port, N) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            start_handlers(Listen, N), % start processes
            super(), % wait for requests
            gen_tcp:close(Listen),
            ok;
        {error, Error} -> io:format("rudy1: error: ~w~n", [Error])
end.

super() ->
    receive
	stop ->
		ok
    end.

% Create a pool of processes
start_handlers(Listen, N) ->
    if
        N > 1 -> spawn(fun() -> handler(Listen, N) end),
                start_handlers(Listen, N-1); % start the next handler
        true -> ok
    end.

handler(Listen, N) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} -> request(Client), handler(Listen, N);
        {error, Error} -> case Error of
                            closed -> ok;
                            true -> io:format("rudy2: error: ~w~n", [Error])
                          end
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