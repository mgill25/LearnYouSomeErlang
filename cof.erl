-module(cof).
-author("Manish Gill").
-export([insert/2, beach/1, bad_beach/1]).

insert(X, []) ->
        [X];
insert(X, Set) ->
        case lists:member(X, Set) of
                true -> Set;
                false -> [X|Set]
        end.


beach(Temperature) ->
        case Temperature of 
                {celsius, N} when N >= 20, N =< 45 ->
                        'favourable';
                {kelvin, N} when N >= 293, N =< 318 ->
                        'scientifically favourable';
                {fahrenheit, N} when N >= 68, N =< 113 ->
                        'favourable in the US';
                _ ->
                        'avoid beach pls'
        end.

%% case...of expressions are pretty much the same thing as a bunch
%% of function heads with guards.

%% we could've written the above code the following way:
bad_beach({celsius, N}) when N >= 20, N =< 45 ->
        'favourable';
%% ...other function heads go here...
bad_beach(_) ->
        'avoid beach'.

%% Question: When to use case...of and when to use function heads for conditions?
%% No big diff in terms of perf cost. However when multiple arguments need to be conditioned upon,
%% functions are better than something like "case {Arg1, Arg2} of" which is rarely seen.
