-module(worker).
-export([start/5, stop/1, peers/2]).

% Name: worker's unique name
% Logger: the logger
% Seed: a unique value for the random generator
% Sleep: determine how active the worker is sending messages
% Jitter:  introduce a random delay between the sending of a message and the sending of a log entry
start(Name, Logger, Seed, Sleep, Jitter) -> spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) -> Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        % receive your peers
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
        stop ->
            ok
    end.

% send peers
% Wrk: the worker
% Peers: worker's peers
peers(Wrk, Peers) -> Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Timestamp)->
    Wait = random:uniform(Sleep),
    receive
        % message from one of its peers
        {msg, Time, Msg} -> 
             % update Clock
            NewTimestamp = time:inc(Name, time:merge(Time, Timestamp)),
            % inform logger that you received a message from a peer
            Log ! {log, Name, Time, {received, Msg}}, 
            loop(Name, Log, Peers, Sleep, Jitter, NewTimestamp);
        stop -> ok;
        Error -> Log ! {log, Name, time, {error, Error}}
    % after a random sleep time select a peer process that is sent a message.
    after Wait -> 
            % select a rendom peer
            Selected = select(Peers),
            % increase Clock
            NewTimestamp = time:inc(Name, Timestamp),
            % create a hopefully unique random message, so that we can track the sending and receiving of a message.
            Message = {hello, random:uniform(100)},
            % send message to the selected peer
            Selected ! {msg, NewTimestamp, Message},
            jitter(Jitter),
            % send message to logger
            Log ! {log, Name, NewTimestamp, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, NewTimestamp)
    end.

select(Peers) -> lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).