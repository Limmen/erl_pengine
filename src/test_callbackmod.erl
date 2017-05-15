%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2016, Kim Hammar
%% @doc test_callbackmod.
%% @end
%%%-------------------------------------------------------------------
-module(test_callbackmod).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([oncreate/1, onsuccess/3, onfailure/1, onerror/2, onprompt/2,
         onoutput/2, onstop/1, onabort/1, ondestroy/1]).

%%====================================================================
%% API functions
%%====================================================================

%% function to be called when a pengine has been created. 
%% Id points to the id of the pengine.
-spec oncreate(integer()) -> ok.
oncreate(Id) ->
    io:format("Pengine created, id: ~p ~n", [Id]),
    ok.

%% Called when the pengine responds with a successful answer to a query.
%% Data points to a list of objects each representing a solution to the query.
%% More evaluates to a boolean indicating whether more solutions may exist.
%% Id points to the id of the pengine returning the answer.
-spec onsuccess(integer(), list(), boolean()) -> ok.
onsuccess(Id, Data, More) ->
    io:format("Query successfully answered by pengine ~p, data: ~p, more solutions: ~p", [Id, Data, More]),
    ok.

%% Called when the pengine fails to find a solution. 
%% The expression this.id points to the id of the pengine reporting the failure.
-spec onfailure(integer()) -> ok.
onfailure(Id) ->
    io:format("Pengine ~p failed to find a solution ~n", [Id]),
    ok.

%% Called when the pengine throws an error. 
%% The expression Data evaluates to an error message in the form of a string. 
%% The expression Id points to the id of the pengine returning the error.
-spec onerror(integer(), list()) -> ok.
onerror(Id, Data) ->
    io:format("Pengine-error, Data: ~p Id: ~p ~n", [Data, Id]),
    ok.

%% Called when the pengine evaluates the pengine_input/2 predicate. 
%% The expression Data evaluates to a prompt in the form of a string or an erlang map. 
%% The expression Id points to the id of the pengine producing the prompt.
-spec onprompt(integer(), list()) -> ok.
onprompt(Id, Data)->
    io:format("Pengine ~p prompt ~p ~n", [Id, Data]),
    ok.

%% Called when the pengine has evaluated the built in pengine_output/1 predicate.
%% The expression Data evaluates to a string or a erlang-map. 
%% The expression Id points to the id of the pengine generating the output.
-spec onoutput(integer(), list()) -> ok.
onoutput(Id, Data)->
    io:format("Pengine ~p output ~p  ~n", [Id, Data]),
    ok.

%% Called when a running query has been successfully stopped. 
%% The expression Id points to the id of the pengine having been stopped.
-spec onstop(integer()) -> ok.
onstop(Id)->
    io:format("Pengine ~p stopped ~n", [Id]),
    ok.

%% Called when a running query has been successfully aborted. 
%% The expression Id points to the id of the pengine having been aborted.
-spec onabort(integer()) -> ok.
onabort(Id)->
    io:format("Pengine ~p query aborted ~n", [Id]),
    ok.

%% Called when the pengine has been successfully destroyed. 
%% The expression Id points to the id of the pengine having been destroyed.
-spec ondestroy(integer()) -> ok.
ondestroy(Id)->
    io:format("Pengine ~p destroyed ~n", [Id]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
