% Tuples

X = 10, Y = 4.

Point = {X, Y}.

{P, Q} = Point.

% List Comprehensions
[2 * n || n <- [1, 2, 3, 4, 5]].

[X || X <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], X rem 2 =:= 0].

% The general format:
% newlist = [expression || pattern <- list, condition1, condition2, ... condition n]
% the part pattern <- list is a "generator expression". You can have more than 1.

[X + Y || X <- [1, 2, 3], Y <- [4, 5, 6]].

Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}].
Foggy = [X || {X, fog} <- Weather].     % generator expression coupled with pattern matching, acting like filter.

% Erlang binary segment format:
% Value
% Value:Size
% Value/TypeSpecifierList
% Value:Size/TypeSpecifierList

% Examples
<<X1/unsigned> = <<-44>>.
<<X2/signed>> = <<-44>>.
<<X2/integer-signed-little>> = <<-44>>.
<<N:8/unit:1>> = <<72>>.
<<N/integer>> = <<72>>.
<<Y:4/little-unit:8>> = <<72, 0, 0, 0>>.

% Standard binary operations
% bsl - Bit Shift Left
% bsr - Bit Shift Right
% band, bor, bxor, bnot.

2#00100 = 2#00010 bsl 1.
2#00001 = 2#00010 bsr 1.

% Erlang is slow compared to languages like C and C++.
% Bad idea to do stuff like converting videos or images in it,
% Erlang is not great at heavy number crunching.

% Might fast at other things: Reacting to events, message passing,
% (with the help of atoms being extremely light) etc. It candeal with
% events in mattters of milliseconds and as such is a great candidate
% for soft-real-time applications.

% Bit Strings: More like C arrays (contiguous block of memory) instead of
% like lists, which are linked lists.

% Syntax:
<<"This is a bit string!">>.
% avoid using them to tag values, use atoms for that!
% Atoms only ever take 4 or 8 bytes, no matter how long they are,
% so there is no overhead while copying data from function to function or
% sending it to another Erlang node on another server.

% Binary Comprehensions
% Are to bit syntax what list comprehensions are to lists.

[ X || <<X>> <= <<1, 2, 3, 4, 5>>, X rem 2 == 0]
% change 1: Arrow is now <= instead of <-
% change 2: Using binaries <<...>> instead of lists [...]

Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.

RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ].
% [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

% The <= arrow changed binary data to integers inside tuples
% We can do the opposite via another syntax:
<< <<R:8, G:8, B:8>> || {R, G, B} <- RGB >>.
% <<213,45,132,64,76,32,76,0,0,234,32,15>>
