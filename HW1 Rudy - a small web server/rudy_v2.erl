-module(rudy_v2).
-export([start/2, stop/0]).

%% N: pool's size
start(Port, N) -> register(rudy, spawn(fun() -> init(Port, N) end)).

stop() -> exit(whereis(rudy), "time to die").

init(Port, N) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            start_handlers(Listen, N, self()), % start processes
            wait(N), % init will stop only when all processes stop
            gen_tcp:close(Listen),
            ok;
        {error, Error} -> io:format("rudy1: error: ~w~n", [Error])
end.

% wait(N) ->
%     if 
%         N > 0 ->
%             receive
%                 stop -> io:format("closeeee"),wait(N-1) % wait for the next process to stop
%             end;
%         true -> done
%     end.

% Create a pool of processes
start_handlers(Listen, N, Pid) ->
    if
        N > 0 -> spawn(fun() -> handler(Listen, Pid) end),
                start_handlers(Listen, N-1, Pid);
        true -> ok
    end.

handler(Listen, Pid) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} -> request(Client), handler(Listen, Pid);
        {error, Error} -> io:format("rudy2: error: ~w~n", [Error])
    end,
    Pid ! stop.

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