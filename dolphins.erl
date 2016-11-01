-module(dolphins).
-compile(export_all).

dolphin1() ->
        receive
                {From, do_a_flip} ->
                        From ! "How about a no?~n",
                        dolphin1();
                {From, fish} ->
                        From ! "So long and thanks for all the fish!~n",
                        dolphin1();
                _ ->
                        io:format("Heh, we're smarter than you humans.~n"),
                        dolphin1()      % recurse to restart the process as it dies after the call
        end.
