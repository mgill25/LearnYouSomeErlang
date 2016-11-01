-module(guards).
-author("Manish Gill").

%% Guards are used to make pattern matching more expressive
%% With normal pattern matching, we cannot represent concepts such
%% as a "range" or certain data types. That's where guards come into play.

-export([old_enough/1, right_age/1, wrong_age/1]).

%% Question: Is this 12 year old player too young to play in the NBA?
%% How do we represent this as a pattern-matching solution? 1 technique is
%% to pattern match individually on every possible age and return false except 
%% for all invalid ages, but that is too cumbresome.

%% Guard: Pattern matching in conjunction with "when" where you can specify conditions
%% on parameters.
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

%% XXX: Note, the guard above does no type-checking so the comparison will return true 
%% for strings, tuples and be weird in general.
%% Solution: type test BIFs are allowed in guard expressions. Stuff like is_atom/1, is_integer/1

%% Here the comma in the guard acts the same way as "andalso"

right_age(X) when X >= 16, X =< 104 ->
        true;
right_age(_) -> 
        false.

%% We can also use semicolon ; to act as "orelse"
wrong_age(X) when X < 16; X > 104 ->
        true;
wrong_age(_) ->
        false.

%% Negative of guards: they will not accept user-defined functions because of side-effects.

%% Note: I've compared , and ; in guards to the operators andalso and orelse.
%% They're not exactly the same, though. The former pair will catch exceptions
%% as they happen while the latter won't. What this means is that if there is
%% an error thrown in the first part of the guard X >= N; N >= 0, the second
%% part can still be evaluated and the guard might succeed; if an error was
%% thrown in the first part of X >= N orelse N >= 0, the second part will also
%% be skipped and the whole guard will fail.

%% 
