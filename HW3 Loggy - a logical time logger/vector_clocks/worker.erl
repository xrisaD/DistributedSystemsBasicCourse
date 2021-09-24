-module(worker).
-export([start/5, stop/1, peers/2]).

% A worker has a vector

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
        % initialize the Vector clock
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:clock(Peers ++ [Name]));
        stop ->
            ok
    end.

% send peers
% Wrk: the worker
% Peers: worker's peers
peers(Wrk, Peers) -> Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Vector)->
    Wait = random:uniform(Sleep),
    receive
        % message from one of its peers
        {msg, PeerVector, PeerName, Msg} -> 
            % update Vector 
            % update your entry and all other entries based on peer's vector
            UpdatedVector = time:vector_update(PeerVector, Vector, Name),
            % inform logger that you received a message from a peer
            % send your vector
            Log ! {log, Name, UpdatedVector, {received, Msg}}, 
            loop(Name, Log, Peers, Sleep, Jitter, UpdatedVector);
        stop -> ok;
        Error -> Log ! {log, Name, time, {error, Error}}
    % after a random sleep time select a peer process that is sent a message.
    after Wait -> 
            % select a random peer
            Selected = select(Peers),
            % increase your entry at the vector
            UpdatedVector = time:inc(Vector, Name),
            % create a hopefully unique random message, so that we can track the sending and receiving of a message.
            Message = {hello, random:uniform(100)},
            % send message to the selected peer
            Selected ! {msg, UpdatedVector, Name, Message},
            jitter(Jitter),
            % send message to logger
            Log ! {log, Name, UpdatedVector, {sending, Message}},
            loop(Name, Log, Peers, Sleep, Jitter, UpdatedVector)
    end.

select(Peers) -> lists:nth(random:uniform(length(Peers)), Peers).

% introduce a slight delay between sending the message to the peer and informing the logger.
% If we don’t introduce a delay here we would hardly ever have messages occur out of order when running in the same virtual machine.
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
