-module(linkmon).
-author("Manish Gill").
-export([myproc/0, chain/1]).

%% Links are used to link up processes so that if one does, the others linked
%% to it die as well. (Fail Fast!)
%% This is useful because we don't want processes to deal with a disappeared
%% dependency.

%% When one of the linked processes crashes, a special kind of message is sent,
%% with information relative to what happened. No such message is sent if the
%% process dies of natural causes (read: is done running its functions.)

myproc() ->
    timer:sleep(3000),
    exit(reason).

%% Usage: 
%% spawn(fun linkmon:myproc/0).
%% *wait 3 second* nothing happens, as process dies naturally
%% link(spawn(fun linkmon:myproc/0)).
%% wait 3 second, dies with reason.

%% Links are used to establish large groups of processes that should all die 
%% together:

%% the function takes N as argument, starts N processes, all linked to each other.
chain(0) ->
    receive 
        _ -> ok
    after
        2000 -> exit("chain dies here")
    end;
chain(N) ->
    Pid = spawn(fun() -> chain(N - 1) end), % wrapped inside anon function to call with N - 1. Same as spawn(?MODULE, chain, [N-1])
    link(Pid),
    receive
        _ -> ok
    end.

%% > link(spawn(linkmon, chain, [3])).
%% A drawn representation of the spawned processes and links going down:
% [shell] == [3] == [2] == [1] == [0]
% [shell] == [3] == [2] == [1] == *dead*
% [shell] == [3] == [2] == *dead*
% [shell] == [3] == *dead*
% [shell] == *dead*
% *dead, error message shown*
% [shell] <-- restarted
%
%% The crash could have happened in any of the linked processes; because links
%% are bidirectional, you only need one of them to die for the others to follow
%% suit.
%% PSA: spawn_link is a safer alternative because sometimes processes might die before link has been created, which causes UB.
