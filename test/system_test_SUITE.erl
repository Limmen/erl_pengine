%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc test suite for module system_test
%% @end
%%%-------------------------------------------------------------------
-module(system_test_tests).
-author('Kim Hammar <kimham@kth.se>').


%% API
-compile(export_all).

%% Includes
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Common Test Callbacks
%%===================================================================

init_per_suite(_Config) ->
    ok.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, _Config) ->
    ok.

end_per_testcase(_Case, _Config) ->
    ok.

all() ->
    [
    ].


%%===================================================================
%% TESTS
%%===================================================================


%%===================================================================
%% Internal functions
%%===================================================================
