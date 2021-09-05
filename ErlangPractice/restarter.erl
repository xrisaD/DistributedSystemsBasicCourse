-module(restarter).
-compile([export_all]).

start() -> spawn(?MODULE, restarter, []).
 
restarter() ->
        process_flag(trap_exit, true),
        Pid = spawn_link(?MODULE, critic, []),
        register(critic, Pid),
        receive
            {'EXIT', Pid, normal} -> % not a crash
                ok;
            {'EXIT', Pid, shutdown} -> % manual termination, not a crash
                ok;
            {'EXIT', Pid, _} -> % crash
                restarter()
    end.

critic() ->
    receive
        {From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
            From ! {Ref, "They are great!"};
        {From, Ref, {"System of a Downtime", "Memoize"}} ->
            From ! {Ref, "They're not Johnny Crash but they're good."};
        {From, Ref, {"Johnny Crash", "The Token Ring of Fire"}} ->
            From ! {Ref, "Simply incredible."};
        {From, Ref, {_Band, _Album}} ->
            From ! {Ref, "They are terrible!"}
    end,
    critic().

judge(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    receive
        {Ref, Criticism} -> Criticism
    after 2000 ->
        timeout
    end.