-module(what_the_if).

-export([heh_fine/0, oh_god/1, help_me/1]).

heh_fine() ->
        if 1 =:= 1 ->
                   works
        end,
        if 1 =:= 2; 1 =:= 1 ->
                   works
        end,
        %% This final clause will throw up a warning since it will always be false.
        %% when Erlang can't find a way to have a guard succeed, it will crash:
        %% it cannot _not_ return something
        if 1 =:= 2, 1 =:= 1 ->
                   fails
        end.

%% To solve the problem of not returning something, we have a catch-all branch
%% that will always evaluate to true and handle the failure case in there. This branch is called "true".

oh_god(N) ->
        if N =:= 2 -> might_succeed;
           true -> always_does  %% this is Erlang's "else".
        end.

%% Note: Erlang also has no null/None value!

help_me(Animal) ->
        Talk = if Animal == cat -> "Meow";
                  Animal == beef -> "mooo";
                  Animal == dog -> "bark";
                  Animal == tree -> "bark";
                  true -> "wait, I'm not an animal! :o"
               end,
        {Animal, "says " ++ Talk ++ "!"}.

%% Ideally, the true branches should be avoided altogether and all conditions represented in the if statements.
