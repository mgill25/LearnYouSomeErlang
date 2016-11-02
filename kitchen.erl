-module(kitchen).
-author("Manish Gill").
-export([fridge/1, store/2, take/2, start/1, store_2/2, take_2/2]).

%% This module will show us processes share state. This is needed because just
%% having every function as process and allowing message passing won't really
%% get us as far as we want to.

%% Analogy: Process is a fridge. 2 operations: Store food in fridge & Take food out
%% that has been stored beforehand.

%% State is held in the parameters with the help of recursion!!

fridge(FoodList) ->
    receive
        { From, { store, Food } } ->
            From ! { self(), ok },
            fridge([Food | FoodList]);
        { From, { take, Food } } ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! { self(), {ok, Food} },
                    fridge(lists:delete(Food, FoodList));
                false ->
                    From ! { self(), not_found },
                    fridge(FoodList)
            end;
        terminate ->
            ok
    end.

%% We invented a protocol to communicate with the fridge above. That's annoying
%% and a hassle we don't want to deal with every time we want to pass messages.
%% Lets abstract away the messages with the help of functions dealing with sending
%% and receiving them!

store(Pid, Food) ->
    Pid ! {self(), {store, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

take(Pid, Food) ->
    Pid ! {self(), {take, Food}},
    receive
        {Pid, Msg} -> Msg
    end.

%% This makes the interaction b/w processes much cleaner!
%% Pid = spawn(kitchen, fridge, [[baking_soda]]).
%% kitchen:store(Pid, water).
%% kitchen:take(Pid, water).
%% kitchen:store(Pid, juice).

%% Last thing left to do - hide the process spawning
start(FoodList) ->
    spawn(?MODULE, fridge, [FoodList]).     % ?MODULE is a macro returning current module name

%% Problem: Lets try to mess with this program by passing it a non-existing process Pid
%% kitchen:take(pid(0, 250, 0), whatever).
%% This will hang because our own process, after sending the message keeps waiting to receive
%% a message. But there is no pid(0, 250, 0) for the take() method to send the message back to,
%% so we are forever stuck
%% Solution: use the "after" construct that is part of the receive statement to set a timeout delay!

store_2(Pid, Food) ->
    Pid ! { self(), {store, Food} },
    receive
        {Pid, Msg} -> Msg
    after
        3000 -> timeout
    end.

take_2(Pid, Food) ->
    Pid ! { self(), {take, Food} },
    receive
        {Pid, Msg} -> Msg
    after
        3000 -> timeout
        % after can also take "infinite" atom. Not often useful
    end.
