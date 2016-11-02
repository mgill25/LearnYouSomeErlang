-module(multiproc).
-author("Manish Gill").
-export([sleep/1, flush/0, important/0, normal/0]).

%% there are other uses for the timers besides giving up afer too long.
sleep(T) ->
    receive
    after T -> ok
    end.
%% Here is how the sleep function works:
%% basically, keep receiving until the given time passes. No message will match
%% in the receive part.

%% -------------------------------------------------------------------------------

%% Special case: timeout is at 0.
flush() ->
    receive
        _ -> flush()
    after
        0 -> ok
    end.
%% Here, VM tries to find a message that matches, and anything matches. Keep calling
%% recursively until the mailbox is empty and after that, since the delay is 0, 
%% return immediately.

%% -------------------------------------------------------------------------------

%% Selective Receives: Give priority to receiving messages by nesting calls

important() ->
    receive 
        {Priority, Message} when Priority > 10 ->
            [Message | important()]
    after 
        0 -> normal()
    end.

normal() ->
    receive
        {_, Message} ->
            [Message | normal()]
    after
        0 -> []
    end.
%% Usage:
%% > self() ! {15, high}, self() ! {7, low}, self() ! {1, low}, self() ! {17, high}.      
%% > multiproc:important().
%%
%% Every message will be obtained until none is left, but the process will try
%% to grab all those with a priority above 10 before even considering the other
%% messages, which are accumulated in the normal/0 call.
%% Be aware that this practice can sometimes be unsafe due to the way selective receiving
%% works in Erlang.
