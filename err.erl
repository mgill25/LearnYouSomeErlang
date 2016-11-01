-module(err).
-author("Manish Gill").
-compile(export_all).

%% There are 3 types of exceptions in Erlang: 
%% 1. errors, 2. throws, 3. exits

%% erlang:error(Reason).
%% ends execution of current process, returns stack trace of last function call with arguments when you catch it.
%% these are basically run-time errors like badarg, badfun, undef, badarity etc.
%% can be custom: erlang:error(my_custom_error).
%% 
%% Exits: 2 types of exists: Internal exit/1 and External exit/2, the latter has to do with concurrency. Later.
%% internal exits are pretty similar to errors, historically one and the same.
%% Actor model: Processes communicate to each other, listen and talk, via messages. 
%% An exit is basically a process's last breath before dying. They're sent right before a process dies and
%% the code it contains stops executing. Other processes that were listening for that specific kind of message 
%% can then know about the event and do whatever they please with it. This includes logging, restarting 
%% the process that died, etc.
%%
%% So basically, the intent is different. You use exit/1 when you think it's an error worthy of killing the 
%% process. exit/1 does *not* return any stack trace. Copying exit messages with stack traces would blow up very
%% quickly.

%% Throws: Exceptions that we can expect a programmer to be able to handle. More about control flow. Good idea 
%% to document their use within the module using them.
%% throw(permission_denied).
%% Can also be used for non-local returns when in deep recursion.


%% We deal with exceptions using try...catch expressions.

throws(F) ->
        try F() of
                           _ -> ok
        catch 
                Throw -> {throw, caught, Throw}
        end.


errors(F) ->
        try F() of
                           _ -> ok
        catch 
                error:Error -> {error, caught, Error}
        end.

exits(F) ->
        try F() of
                           _ -> ok
        catch 
                exit:Exit-> {exit, caught, Exit}
        end.

%% How to combine these all different types of exceptions?

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

black_knight(Attack) when is_function(Attack, 0) ->
        try Attack() of
                _ -> "None shall pass."
        catch 
                throw:slice -> "It is but a scratch.";
                error:cut_arm -> "I've had worse.";
                exit:cut_leg -> "Come on you pansy!";
                _:_ -> "Just a flesh wound."
        end.

%% A regular function to test the above's success condition with
talk() -> "blah blah".

%% In the repl: err:black_knight(fun err:talk/0).     % remember to use "fun" when passing functions as arguments!
%%
%% Something that should always execute after the exception handling (like finally in python) is "after"
%% try Expr of
%%      Pattern -> Expr1
%% catch
%%      Type:Exception -> Expr2
%% after
%%      Expr3   % This is guarnateed to run. But you won't get a return value. Use for side-effects. File was closed etc.
%% end

%% We can also have more than 1 expression b/w try...of
whoa() ->
        try
                talk(),
                _Knight = "None shall pass!",
                _Doubles = [N * 2 || N <- lists:seq(1, 100)],
                throw(up),
                _WillReturnThis = tequila
        of
                tequila -> "hey this worked!"
        catch
                Exception:Reason -> {caught, Exception, Reason}
        end.

%% the "of" part is a bit useless now, so we can just give it up and just use try...catch...end

im_impressed() ->
        try
                talk(),
                throw(up),
                _WillReturnThis = tequila
        catch 
                throw:foo -> "Got foo!";
                throw:up -> "Got up!";
                Exception:Reason -> {caught, Exception, Reason}
        end.

%% Because the try ... catch construct without the of part has nothing but a
%% protected part, calling a recursive function from there might be dangerous
%% for programs supposed to run for a long time (which is Erlang's niche).
%% After enough iterations, you'll go out of memory or your program will get
%% slower without really knowing why. By putting your recursive calls between
%% the of and catch, you are not in a protected part and you will benefit from
%% Last Call Optimisation.
%%
%% Some people use try ... of ... catch rather than try ... catch by default to
%% avoid unexpected errors of that kind, except for obviously non-recursive
%% code with results that won't be used by anything.


%% Just a regular "catch" that is used to catch all type of exceptions on top of good results

catch_1() ->
        catch 1 + 1.
catch_2() ->
        catch throw(foo).
catch_3() ->
        catch 1/0.
catch_4() ->
        catch doesnt:exist(a, 4).

%% PS: Learn to read the erlang stacktrace
%%{'EXIT',{undef,[{doesnt,exist,[a,4],[]},
%                  {err,catch_4,0,[{file,"err.erl"},{line,136}]},
%                   {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,674}]},
%                   {shell,exprs,7,[{file,"shell.erl"},{line,686}]},
%                   {shell,eval_exprs,7,[{file,"shell.erl"},{line,641}]},
%                   {shell,eval_loop,3,[{file,"shell.erl"},{line,626}]}]}}
%% Here, "EXIT" is the type of exception, 
%% "undef" is the name of exception - undef meaning the function was undefined
%% The tuple on top of the stack trace represents the last function to be called ({Module, Function, Arguments}). That's your undefined function.
%% The tuples after that are the functions called before the error. This time they're of the form {Module, Function, Arity}.
%% starting from the most recently executed function to the previously executed ones going back.


%% Manually get the stacktrace by calling erlang:get_stacktrace/0 in the process that crashed.

% Common way to write catch with case statements, this is compact way to handle exceptions:
catcher(X, Y) ->
        case catch X/Y of 
                {'EXIT', {badarith, _}} -> "uh oh";
                N -> N
        end.

%% However there are a few problems with catch. Operator precedence. 
%% X = catch 4 + 2  % this will error out. A fix for this expression is:

%% Another problem with catch

one_or_two(1) ->
        return;
one_or_two(2) ->
        throw(return).

%% now try the following in repl:
% catch one_or_two(1).
% catch one_or_two(2).
% these will both return the same result, which is really problematic as one is actually throwing an exception!! 
% We will never know if the function threw an exception behind the catch. :o
