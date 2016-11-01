-module(bintree).
-export([empty/0, insert/3, lookup/2, has_value/2, has_value_better/2]).

%% A Tree is a node containing more nodes.
%% Nodes are represented as Tuples.
%% One node can be { node, {Key, Value, Smaller, Larger}}
%% where Smaller and Larger are either similar nodes or
%% empty node {node, 'nil'}

empty() -> {node, 'nil'}.

%% Insert a new node. Use the binary tree property to navigate
%% through the appropriate tree branch until we hit the empty node
%% where we need to insert the new node.

insert(Key, Val, {node, 'nil'}) -> 
        {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Value, Smaller, Larger}}) when NewKey < Key ->
        {node, {Key, Value, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Value, Smaller, Larger}}) when NewKey > Key ->
        {node, {Key, Value, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) ->
        {node, {Key, Val, Smaller, Larger}}.

lookup(_, {node, 'nil'}) ->
        undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
        {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey ->
        lookup(Key, Smaller);
lookup(Key, {node, {_, _, _, Larger}}) ->
        lookup(Key, Larger).

has_value(_, {node, 'nil'}) ->
        false;
has_value(Val, {node, {_, Val, _, _}}) ->
        true;
has_value(Val, {node, {_, _, Left, Right}}) ->
        case has_value(Val, Left) of
                true -> true;
                false -> has_value(Val, Right)
        end.


%% has_value does some unnecessary comparisons: for each node where we branch, 
%% it has to test for the result of the previous branch. Better implementation using exceptions 
%% to reduce comparisons:
has_value_better(Val, Tree) ->
        try has_value_1(Val, Tree) of
                false -> false
        catch 
                true -> true
        end.

has_value_1(_, {node, 'nil'}) ->
        false;
has_value_1(Val, {node, {_, Val, _, _}}) ->
        throw(true);
has_value_1(Val, {node, {_, _, Left, Right}}) ->
        has_value_1(Val, Left),
        has_value_1(Val, Right).
