%% List Comprehensions
%% This isn't a legit Erlang file right now. I'll fix things 
%% later when I know how to!

%% List comprehensions are based off of the idea of Set Notations.
%% Set notation tells you how to build a set or a list by specifying properties
%% its members must satisfy.
%% Example Set Notation: {x ∈ ℜ : x = x^2}


%% Ex 1
[2 * N || N <- [1, 2, 3, 4]].

%% Ex 2: even numbers
[X || X <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10], X rem 2 =:= 0].

%% NewList = [Expression || Pattern <- List, Condition1, Condition2, ... ConditionN]. 
%% The part Pattern <- List is named a Generator expression. You can have more than one!
