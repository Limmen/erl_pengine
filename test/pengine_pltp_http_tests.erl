%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc Unit test suite for module pengine_pltp_http
%% @end
%%%-------------------------------------------------------------------
-module(pengine_pltp_http_tests).
-author('Kim Hammar <kimham@kth.se>').


%% API
-compile(export_all).

%% Includes
-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% TESTS
%%===================================================================

%% Test generator for module pengine_pltp_http
-spec pengine_pltp_http_test_() -> list().
pengine_pltp_http_test_() ->
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
