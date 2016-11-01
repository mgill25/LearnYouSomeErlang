-module(records).
-author("Manish Gill").
-include("records.hrl").
-compile(export_all).

%% Records are, first of all, a hack.
%% Erlang records are a lot like C structs.
%% They are declared as module attributes, like so:

-record(robot, {name,
                type=industrial,
               hobbies,
               details=[]}).

%% Here's how to create a record "instance"!
first_robot() ->
        #robot{name="Mechatron",
              type=handmade,
              details=["Moved by a small man inside"]}.

%% Notice that it is ok for fields to be left undefined (hobbies in the above).

car_factory(CorpName) ->
        #robot{name=CorpName, hobbies="building cars"}.

%% Note: The function rr() can take more than a module name: it can take a
%% wildcard (like rr("*")) and also a list as a second argument to specify
%% which records to load.

%% There are a few other functions to deal with records in the shell: rd(Name,
%% Definition) lets you define a record in a manner similar to the
%% -record(Name, Definition) used in our module. You can use rf() to 'unload'
%% all records, or rf(Name) or rf([Names]) to get rid of specific definitions.
%% You can use rl() to print all record definitions in a way you could
%% copy-paste into the module or use rl(Name) or rl([Names]) to restrict it to
%% specific records.  Finally, rp(Term) lets you convert a tuple to a record
%% (given the definition exists).

-record(user, {id, name, group, age}).

%% use pattern matching to filter

%% we can pattern match on any field we care about, ignoring others.
admin_panel(#user{name=Name, group=admin}) ->
        Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
        Name ++ " is not allowed!!".

%% can extend user without problem
%% notice how we don't need to know or care about all the fields of records
%% before matching in the guard.
adult_section(U = #user{}) when U#user.age >= 18 ->
        %% adult stuff here lol
        allowed;
adult_section(_) ->
        %% redirect to sesame street site :)
        forbidden.


%% updating a record
repairmap(Rob) ->
        Details = Rob#robot.details,
        NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
        {repaired, NewRob}.

%% Because they're pretty useful and code duplication is annoying, Erlang
%% programmers frequently share records across modules with the help of header
%% files.

%% check if the record from header file is included
included() -> 
        #included{some_field="Some value"}.
