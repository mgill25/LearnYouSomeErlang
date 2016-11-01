-module(hhfunc).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().


map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.
sq(X) -> X * X.

%% Anonymous functions
%% Support Closures, IIFEs, Named Funs etc.
Fn = fun() -> a end. 

PrepareAlarm = fun(Room) ->
                               io:format("Alarm set in ~s.~n", [Room]),
                               fun() -> io:format("Alarm tripped in ~s! Call Batman!~n", [Room])
                               end.
               end.

AlarmReady = PrepareAlarm("bathroom").
AlarmReady().

Base = 2.
PowerOfTwo = fun(X) -> math:pow(Base, X) end.

%% Named anon functions
PreparedAlarm_2 = fun(Room) ->
                                  io:format("Alarm now set in ~s.~n", [Room]),
                                  % Loop is the named anon function
                                  fun Loop() ->
                                        io:format("Alarm Tripped in ~s! Call Batman!~n", [Room]),
                                        timer:sleep(500),
                                        Loop()
                                  end
                  end.
