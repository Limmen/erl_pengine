%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc Unit test suite for module table_mngr
%% @end
%%%-------------------------------------------------------------------
-module(table_mngr_tests).
-author('Kim Hammar <kimham@kth.se>').


%% API
-compile(export_all).

%% Includes
-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% TESTS
%%===================================================================

%% Test generator for module table_mngr
-spec table_mngr_test_() -> list().
table_mngr_test_() ->
    %% add your asserts in the returned list, e.g.:
    %% [
    %%   ?assert(?MODNAME:double(2) =:= 4),
    %%   ?assertMatch({ok, Pid}, ?MODNAME:spawn_link()),
    %%   ?assertEqual("ba", ?MODNAME:reverse("ab")),
    %%   ?assertError(badarith, ?MODNAME:divide(X, 0)),
    %%   ?assertExit(normal, ?MODNAME:exit(normal)),
    %%   ?assertThrow({not_found, _}, ?MODNAME:func(unknown_object))
    %% ]
    [].


%%===================================================================
%% Internal functions
%%===================================================================
