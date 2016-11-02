-module(linkmon).
-author("Manish Gill").
-export([myproc/0, chain/1, start_critic/0, judge/3, critic/0, start_critic_2/0, restarter/0, judge2/2, critic2/0, judge3/2]).
%% Ref: http://learnyousomeerlang.com/errors-and-processes

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

%% SIGNALS: Signals are basically messages that are secretly communicated b/w processes in order to propagate errors, that result
%% in killing them etc.
%% So we can now link processes and kill all of them together. But for an application to be reliable, we also need to be able to
%% quickly restart processes. That can be done by capturing these signals and acting on them. 
%% How to do that? "System Processes".
%% SYSTEM PROCESSES: Basically normal processes that convert _exit_ signals to regular messages.
%% Usage: process_flag(trap_exit, true) in a running process.
%%
%% > process_flag(trap_exit, true).
%% > spawn_link(fun() -> chain(3) end).
%% > receive X -> X end.

%% Monitoring: Just like links, except you can stack them and they are unidirectional. This is used because links themselves
%% are an architectural/organizational construct. But what happens when we are using multiple libraries and want to check whether
%% a particular process is alive or not? links being bidirectional and not allowing stacking works against us here, hence monitor.
%% erlang:monitor(process, spawn(fun() -> timer:sleep(1000) end)).
%% flush().
%% can also use spawn_monitor, just like spawn_link
%% {Pid, Ref} = spawn_monitor(fun() -> receive _ -> exit(boom) end end).
%% Pid ! die.
%% flush().
%% % We can also just demonitor a process using the Ref
%% erlang:demonitor(Ref).
%% erlang:demonitor(Ref, [flush, info]).

start_critic() ->
    spawn(?MODULE, critic, []).

judge(Pid, Band, Album) ->
    Pid ! {self(), {Band, Album}},
    receive 
        {Pid, Criticism} -> Criticism
    after 
        2000 -> timeout
    end.

critic() ->
    receive 
        {From, {"Rage against the Turing Machine", "Unit Testify"}} -> From ! {self(), "They are great!"};
        {From, {"System of a Downtime", "Memoize"}} -> From ! {self(), "They're not Johny Crash but they're good."};
        {From, {"Johny Crash", "The Token Ring of Fire"}} -> From ! {self(), "Simply Incredible."};
        {From, {_Band, _Album}} -> From ! {self(), "They are terrible!"}
    end,
    critic().

%% > Critic = linkmon:start_critic().
%% > linkmon:judge(Critic, "Genesis", "The lambda lies on broadway").
%% "They are terrible!"
%% problem: lets say we exit the process because of a problem
%% > exit(Critic, solar_storm).
%% > linkmon:judge(Critic, "Genesis", "The trick of the tail recursion")
%% timeout

%% What to do about this annoying this? We don't want to stop judging when the process goes down!
%% Lets write a supervisor process whose role is to restart the process when it goes down.

start_critic_2() ->
    spawn(?MODULE, restarter, []).

restarter() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, critic, []),
    register(critic, Pid),                  % Assign a name critic to the critic process Pid, so it can be referred to from anywhere.
    receive 
        { 'EXIT', Pid, normal } -> % not a crash
            ok;
        { 'EXIT', Pid, shutdown } -> % manual termination, not a crash
            ok;
        { 'EXIT', Pid, _ } -> 
            restarter()
    end.

% We can find the Pid from the name using whereis
judge2(Band, Album) ->
    critic ! {self(), {Band, Album}},
    Pid = whereis(critic),
    receive
        {Pid, Criticism} -> Criticism
    after
        2000 -> timeout
    end.
% Problem with above code: critic value might change b/w sending it the message and retrieving its Pid 
% in the next line: the critic process could die, killing our process, or it could die and restart, giving
% us the wrong Pid. These are race conditions.
% Fix: use unique references.

judge3(Band, Album) ->
    Ref = make_ref(),
    critic ! {self(), Ref, {Band, Album}},
    Pid = whereis(critic),
    receive
        {Ref, Criticism} -> Criticism
    after
        2000 -> timeout
    end.

critic2() ->
    receive 
        {From, Ref, {"Rage against the Turing Machine", "Unit Testify"}} -> From ! {self(), "They are great!"};
        {From, Ref, {"System of a Downtime", "Memoize"}} -> From ! {self(), "They're not Johny Crash but they're good."};
        {From, Ref, {"Johny Crash", "The Token Ring of Fire"}} -> From ! {self(), "Simply Incredible."};
        {From, Ref, {_Band, _Album}} -> From ! {self(), "They are terrible!"}
    end,
    critic2().
