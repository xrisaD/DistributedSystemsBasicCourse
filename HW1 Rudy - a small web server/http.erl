-module(http).
-export([parse_request/1, ok/1, ok/2, not_found/0, get/1]).

parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

%% -- Request

%% GET case
%% 32: ASCII value for space
request_line([$G, $E, $T, 32 |R0]) ->
{URI, R1} = request_uri(R0),
{Ver, R2} = http_version(R1),
[13,10|R3] = R2,
{{get, URI, Ver}, R3}.

%% URI
request_uri([32|R0])->
{[], R0};
request_uri([C|R0]) ->
{Rest, R1} = request_uri(R0),
{[C|Rest], R1}.

%% version
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
{v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
{v10, R0}.


%% Header
headers([13,10|R0]) -> {[],R0};

headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.
header([13,10|R0]) ->
{[], R0};
header([C|R0]) ->
{Rest, R1} = header(R0),
{[C|Rest], R1}.

%% Message Body
message_body(R) -> {R, []}.

%% -- Reply
ok(Body) -> "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

ok(Body, Size) -> "HTTP/1.1 200 OK\r\n" ++ "Content-Length : " ++ [Size] ++ "\r\n" ++ "Content-Type : text/html\r\n\r\n" ++ Body.

not_found() -> "HTTP/1.1 404 Not Found".

get(URI) -> "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".