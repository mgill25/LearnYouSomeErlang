-module(hello).
-export([add/2, hello/0, greet_and_add_two/1, greet/2, valid_time/1]).
-author("Manish Gill").

%% Shows greetings
%% io:format/1 is the standard function used to output text.

hello() ->
        io:format("Hello, World!~n").

add(X, Y) ->
        X + Y.

greet_and_add_two(X) ->
        hello(),
        add(X, 2).

greet(male, Name) ->
        io:format("Hello, Mr. ~s!", [Name]);
greet(female, Name) ->
        io:format("Hello, Mrs. ~s!", [Name]);
greet(_, Name) ->
        io:format("Hello, ~s!", Name).

valid_time({Date = {Y, M, D}, Time = {H, Min, S}}) ->
        io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
        io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
        io:format("Stop feeding me wrong data!~n").
