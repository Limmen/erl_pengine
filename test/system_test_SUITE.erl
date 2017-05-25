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
    case os:cmd("prolog ~/workspace/erlang/erlang_pengine/prolog/daemon.pl --http=4000 --pidfile=/home/kim/workspace/erlang/erlang_pengine/prolog/pid/http.pid") of
        [] -> 
            application:ensure_all_started(erlang_pengine),
            Config;
        Error ->
            {skip,{"Could not start pengine server", Error}}

    end.

end_per_suite(Config) ->
    application:stop(erlang_pengine),
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
     test_create,
     test_kill_all_pengines,
     test_destroy,
     test_ping,
     test_id,
     test_ask
    ].


%%===================================================================
%% TESTS
%%===================================================================

%% Test creation of pengines
%% The pengine server used for test uses default max_limit of slaves=3
test_create(_Config)->
    ?assertMatch({{ok, {_P1, _Id1}}, {no_create_query}}, pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{})),
    ?assertMatch({{ok, {_P2, _Id2}}, {no_create_query}}, pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{})),
    ?assertMatch({{ok, {_P3, _Id3}}, {no_create_query}}, pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{})),
    {{error, {max_limit, _Reason1}}, {pengine_destroyed, _Reason2}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}).

%% Test kill all active slave pengines
test_kill_all_pengines(_Config)->
    pengine_master:kill_all_pengines().

%% test destroy of one pengine-slave
test_destroy(_Config)->
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}),
    ?assertMatch({pengine_destroyed, _Reply}, pengine:destroy(P1)),
    ?assertNot(lists:member(Id1, pengine_master:list_pengines())),
    ?assertNot(is_process_alive(P1)), 
    ?assertMatch({ok, 
                  #{<<"event">> := <<"error">>, <<"id">> := Id1, 
                    <<"code">> := <<"existence_error">>, <<"data">> := _}
                 }, 
                 pengine_pltp_http:send(Id1, "http://127.0.0.1:4000/pengine", "destroy", "json")).

%% test ping request to a slave pengine
test_ping(_Config)->
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}),
    ?assertMatch({ping_response, Id1, _Data}, pengine:ping(P1, 10)),
    pengine_master:kill_all_pengines().

%% test id() function
test_id(_Config)->
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}),
    ?assertMatch(Id1, pengine:id(P1)),
    pengine_master:kill_all_pengines().

%% test query to pengine
%% tests next(), ask() as well as create-with-query
test_ask(_Config)->
    Options = #{destroy => true, application => "pengine_sandbox", chunk => 1, format => json},
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options),
    {success, Id1, [[1]], true} = pengine:ask(P1, "member(X, [1,2])", #{template => "[X]", chunk => "1"}),
    ?assert(is_process_alive(P1)),
    {{success, Id1, [[2]], false}, {pengine_destroyed, _}} = pengine:next(P1),
    ?assertNot(lists:member(Id1, pengine_master:list_pengines())),
    ?assertNot(is_process_alive(P1)),
    Options1 = #{destroy => false, application => "pengine_sandbox", chunk => 1, format => json, ask => "member(X, [1,2])", template => "[X]"},
    {{ok, {P2, Id2}}, {create_query, {success, Id2, [[1]], true}}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options1),
    {success, Id2, [[2]], false} = pengine:next(P2),
    ?assert(is_process_alive(P2)),
    {success, Id2, [[1],[2]], false} = pengine:ask(P2, "member(X, [1,2])", #{template => "[X]", chunk => "10"}),
    ?assert(is_process_alive(P2)),
    {{ok, {P3, Id3}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options),
    {{success, Id3, [[1],[2]], false}, {pengine_destroyed, _}} = pengine:ask(P3, "member(X, [1,2])", #{template => "[X]", chunk => "2"}),
    ?assertNot(is_process_alive(P3)),
    pengine_master:kill_all_pengines().

%%===================================================================
%% Internal functions
%%===================================================================

