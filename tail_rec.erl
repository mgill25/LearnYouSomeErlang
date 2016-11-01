%% Tail Recursion in Erlang
%% Factorial function
-module(tail_rec).
-author("Manish Gill").
-export([
         tail_fact/1, 
         tail_len/1, 
         tail_dup/2, 
         reverse_tail/1, 
         tail_sublist/2, 
         zip/2,
         lenient_zip/2,
         tail_zip/2
]).

tail_fact(N) -> tail_fact(N, 1).

tail_fact(0, Acc) -> Acc;
tail_fact(N, Acc) when N > 0 -> tail_fact(N - 1, N * Acc).

tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, 1 + Acc).

tail_dup(N, Term) -> tail_dup(N, Term, []).

tail_dup(0, _, List) -> List;
tail_dup(N, Term, List) when N > 0 ->
        tail_dup(N - 1, Term, [Term|List]).

%% slow reverse - has to traverse the entire list on every recursive call &
%% also stacks up the append operations.
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].    %% ++ is the concatenation operator

%% tail recursive version of reverse
reverse_tail(List) -> reverse_tail(List, []).
reverse_tail([], Acc) -> Acc;
reverse_tail([H|T], Acc) -> reverse_tail(T, [H|Acc]).

%% Sublist
sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([H|T], N) when N > 0 -> [H | sublist(T, N - 1)].

%% We have to use reverse here because tail_sublist/3 returns reverse order
tail_sublist(List, N) -> lists:reverse(tail_sublist(List, N, [])).

%% Tail recursive version of sublist - returns reverse order of sublist
tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([], 0, Acc) -> Acc;
tail_sublist([H|T], N, Acc) -> tail_sublist(T, N - 1, [H|Acc]).

%% zip function
zip([], []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X, Y}|zip(Xs, Ys)].

%% Lenient Zip - done when any one of the 2 lists is done
lenient_zip(_, []) -> [];
lenient_zip([], _) -> [];
lenient_zip([X|Xs], [Y|Ys]) -> [{X, Y}|lenient_zip(Xs, Ys)].

%% Tail Recursive Zipping!
tail_zip(L1, L2) -> lists:reverse(tail_zip(L1, L2, [])).

tail_zip([], _, Acc) -> Acc;
tail_zip(_, [], Acc) -> Acc;
tail_zip([X|Xs], [Y|Ys], Acc) -> tail_zip(Xs, Ys, [{X, Y}|Acc]).

%% Note: tail recursion as seen here is not making the memory grow because when
%% the virtual machine sees a function calling itself in a tail position (the
%% last expression to be evaluated in a function), it eliminates the current
%% stack frame. This is called tail-call optimisation (TCO) and it is a special
%% case of a more general optimisation named Last Call Optimisation (LCO).

%% LCO is done whenever the last expression to be evaluated in a function body
%% is another function call. When that happens, as with TCO, the Erlang VM
%% avoids storing the stack frame. As such tail recursion is also possible
%% between multiple functions. As an example, the chain of functions a() ->
%% b(). b() -> c(). c() -> a(). will effectively create an infinite loop that
%% won't go out of memory as LCO avoids overflowing the stack. This principle,
%% combined with our use of accumulators is what makes tail recursion useful.


%% Quicksort
quicksort([]) -> [];
quicksort([Pivot|Rest]) -> 
        {Smaller, Larger} = partition(Pivot, Rest, [], []),
        quicksort(Smaller) ++ Pivot ++ quicksort(Larger).

partition(_, [], Smaller, Larger) -> {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
        if H =< Pivot -> partition(Pivot, T, [H|Smaller], Larger);
           H > Pivot -> partition(Pivot, T, Smaller, [H|Larger])
        end.

%% Quicksort using List Comprehensions
quicksort_lc([]) -> [];
quicksort_lc([Pivot|Rest]) ->
        quicksort_lc([Smaller || Smaller <- Rest, Smaller =< Pivot])
        ++ [Pivot] ++
        quicksort_lc([Larger || Larger <- Rest, Larger > Pivot]).
