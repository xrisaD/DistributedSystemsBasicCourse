-module(intf).
-export([]).

% A set of interfaces
% A interface is described by the symbolic name, a process reference and a process identifier.

% returns an empty set of interfaces.
new() -> [].

% add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) -> 
    % it is a set so we don't want to add the same interface a second time
    case lists:keyfind(Name, 1, Intf) of
        {_, _, _}->
            Intf
        false->
            Intf ++ [{Name, Ref, Pid}]
    end.

% remove an entry given a name of an interface, return a new set of interfaces
remove(Name, Intf) -> lists:keydelete(Name, 1, Intf).

% find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) -> case lists:keyfind(Name, 1, Intf) of
                            {_, _, Pid}->
                                {ok, Pid};
                            false->
                                notfound
                      end.

% find the reference given a name and return {ok, sRef} or notfound.
ref(Name, Intf) -> case lists:keyfind(Name, 1, Intf) of
                            {_, Ref, _}->
                                {ok, Ref};
                            false->
                                notfound
                    end.

% find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) -> case lists:keyfind(Ref, 2, Intf) of
                            {Name, _, _}->
                                {ok, Name};
                            false->
                                notfound
                    end.

% return a list with all names.
list(Intf) -> lists:foldl(fun({Name, Ref, Pid}, Result) -> Result ++ [Name] end, [], Intf).

% send the message to all interface processes.
broadcast(Message, Intf) -> lists:foreach(fun({_, _, Pid}) -> Pid ! Message end ,Intf).
