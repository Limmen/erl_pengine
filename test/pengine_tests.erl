%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc Unit test suite for module pengine.erl
%% @end
%%%-------------------------------------------------------------------
-module(pengine_tests).
-author('Kim Hammar <kimham@kth.se>').


%% API
-compile(export_all).

%% Includes
-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% TESTS
%%===================================================================

%% Test options_to_list/1
-spec options_to_list_test_() -> list().
options_to_list_test_() ->
    [
     ?_assertEqual("[template([A,B,C])]", pengine:options_to_list(#{template => "[A,B,C]"})),
     ?_assertEqual("[chunk(10),template([A,B,C])]", pengine:options_to_list(#{template => "[A,B,C]", chunk => "10"})),
     ?_assertEqual("[]", pengine:options_to_list(#{})),
     ?_assertError(badarg, pengine:options_to_list(#{chunk => 10}))
    ].

%%===================================================================
%% Internal functions
%%===================================================================
