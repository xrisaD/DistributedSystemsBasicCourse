-module(routy).
-export([start/2, stop/1, router/6, send_status/1]).

start(Reg, Name) -> register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) -> Node ! stop,unregister(Node).

init(Name) ->
    % initialization
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    % start routy
    router(Name, 0, Hist, Intf, Table, Map).

% router process
router(Name, N, Hist, Intf, Table, Map) ->
    receive
        % add an interface
        {add, Node, Pid} ->
                Ref = erlang:monitor(process, Pid),
                Intf1 = intf:add(Node, Ref, Pid, Intf),
                router(Name, N, Hist, Intf1, Table, Map);
        % remove an interface
        {remove, Node} ->
                {ok, Ref} = intf:ref(Node, Intf),
                erlang:demonitor(Ref),
                Intf1 = intf:remove(Node, Intf),
                router(Name, N, Hist, Intf1, Table, Map);
        % monitor
        {'DOWN', Ref, process, _, _} -> 
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit received from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        % status messages
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);
        % link-state messages
        {links, Node, R, Links} ->
                case hist:update(Node, R, Hist) of
                    {new, Hist1} -> intf:broadcast({links, Node, R, Links}, Intf),
                                    Map1 = map:update(Node, Links, Map),
                                    router(Name, N, Hist1, Intf, Table, Map1);
                    old -> router(Name, N, Hist, Intf, Table, Map)
                end;
        % update Table
        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);
        % broadcast link-state message
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        % the message arrived at the final destination 
        {route, Name, From, Message} ->
            io:format("~w: received message ~w ~n", [Name, Message]),
            router(Name, N, Hist, Intf, Table, Map);
        % f the message is not ours so we should forward it
        {route, To, From, Message} ->
            io:format("~w: routing message (~w)", [Name, Message]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} -> Pid ! {route, To, From, Message};
                        notfound -> ok
                    end;
                notfound -> ok
            end,
            router(Name, N, Hist, Intf, Table, Map);
        % send message
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        stop -> ok
    end.

send_status(Reg) -> 
    % send status message
    Reg ! {status, self()},
    % receive the reply
    receive
        % print the reply
        {status, {Name, N, Hist, Intf, Table, Map}} -> io:format("Name: ~w~nN: ~w~nHist: ~w~nIntf: ~w~nTable: ~w~nMap: ~w~n", [Name, N, Hist, Intf, Table, Map])
    end.

