-module(rudy_v3).
-export([start/1, stop/0]).

start(Port) -> register(rudy, spawn(fun() -> init(Port) end)).

stop() -> exit(whereis(rudy), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, Listen} ->
            handler(Listen),
            receive
                handler_done ->
                wait_for_threads(Count-1)
            end,
            ok;
        {error, Error} -> error
end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Client} -> request(Client),  
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
    {ok, CurrentDirectory} = file:get_cwd(), % get current directory
    [FilePath, FileName] = parse_uri(URI),
    File = file:path_open([lists:delete($/, FilePath), CurrentDirectory], FileName, read), % open file
    case File of 
        {ok, _, FullName} ->{ok, {_, Size, _,_,_,_,_,_,_,_,_,_,_,_}} = file:read_file_info(FullName), % read file's info
                            RFile = file:read_file(FullName), % read file
                            case RFile of
                                {ok, Data} -> http:ok(Data, Size);
                                {error, Reason} -> io:format("rudy: error: ~w~n", [Reason]), http:not_found()
                            end;
        {error, Reason} -> io:format("rudy: error: ~w~n", [Reason]), http:not_found()
    end.

parse_uri(URI) ->
    Parsed = uri_string:parse(URI), % parse string
    Path = maps:get(path, Parsed), % parse path
    string:split(Path, "/", trailing). % split path and file name