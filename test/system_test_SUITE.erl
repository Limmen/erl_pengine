%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc test suite for module system_test
%% @end
%%%-------------------------------------------------------------------
-module(system_test_SUITE).
-author('Kim Hammar <kimham@kth.se>').


%% API
-compile(export_all).

%% Includes
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Common Test Callbacks
%%===================================================================

init_per_suite(Config) ->
    os:cmd("prolog ~/workspace/erlang/erlang_pengine/prolog/daemon.pl --http=4000 --pidfile=/home/kim/workspace/erlang/erlang_pengine/prolog/pid/http.pid"),
    application:ensure_all_started(erlang_pengine),
    Config.

end_per_suite(Config) ->
    os:cmd("cat ~/workspace/erlang/erlang_pengine/prolog/pid/http.pid | xargs kill -9"),
    Config.

init_per_testcase(Case, Config) ->
    ct:pal("Starting testcase: ~p", [Case]),
    Config.

end_per_testcase(Case, Config) ->
    ct:pal("Testcase ~p ended", [Case]),
    Config.

all() ->
    [
     test_create
    ].


%%===================================================================
%% TESTS
%%===================================================================

%% Test creation of pengines
%% The pengine server used for test uses default max_limit of slaves=3
test_create(_Config)->
    ?assertMatch({ok, {_P1, _Id1}},pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{})),
    ?assertMatch({ok, {_P2, _Id2}},pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{})),
    ?assertMatch({ok, {_P3, _Id3}}, pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{})),
    {error, {max_limit, _Reason}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{}).

%%===================================================================
%% Internal functions
%%===================================================================

