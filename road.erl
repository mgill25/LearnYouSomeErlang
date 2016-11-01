-module(road).
-author("Manish Gill").
-compile(export_all).

%% read from binary data and return a list
parse_map(Bin) when is_binary(Bin) ->
        parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
        Vals = [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")],
        group_vals(Vals, []).

%% Transform the map into a readable map of triples.
group_vals([], Acc) ->
        lists:reverse(Acc);
group_vals([A, B, X|Rest], Acc) ->
        group_vals(Rest, [{A, B, X}|Acc]).


%% Actual problem solving
%% change triples of the form {A, B, X}
%% where A, B and X are distances and a, b, x are possible paths
%% to the form {DistanceSum, PathList} - we start from {0, []} for both A and B
shortest_step({A, B, X}, {{DistA, PathA}, {DistB, PathB}}) ->
        OptA1 = {DistA + A, [{a, A}|PathA]},
        OptA2 = {DistB + B + X, [{x, X}, {b, B}|PathB]},
        OptB1 = {DistB + B, [{b, B}|PathB]},
        OptB2 = {DistA + A + X, [{x, X}, {a, A}|PathA]},
        {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

%% Use foldl to pick the best path.
%% At the end of the fold, both paths should end up having the same distance,
%% except one's going through the final {x,0} segment.
optimal_path(Map) ->
        {A, B} = lists:foldl(fun shortest_step/2, {{0, []}, {0, []}}, Map),
        {_Dist, Path} = if hd(element(2, A)) =/= {x, 0} -> A;
                           hd(element(2, B)) =/= {x, 0} -> B
                        end,
        lists:reverse(Path).

%% Usage:
%% Erlang ❯ erlc road.erl
%% Erlang ❯ erl -noshell -run road main road.txt
main([FileName]) ->
        {ok, Bin} = file:read_file(FileName),
        Map = parse_map(Bin),
        io:format("~p~n", [optimal_path(Map)]),
        erlang:halt().

