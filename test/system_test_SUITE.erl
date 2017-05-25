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
     test_ask,
     test_abort,
     test_lookup,
     test_list_pengines,
     test_stop
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
    {{ok, {P4, Id4}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options),
    {{failure, Id4}, {pengine_destroyed, _}} = pengine:ask(P4, "member(3, [1,2])", #{template => "[]", chunk => "10"}),
    pengine_master:kill_all_pengines().

%% test abortion request
test_abort(_Config)->
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}),
    Self = self(),
    spawn(fun() -> 
                  {{aborted, Id1}, {pengine_destroyed, _Reason1}} = pengine:ask(P1, "long_query(X)", #{template => "[X]", chunk => "1"}),
                  Self ! aborted
          end),
    ct:sleep(1000),
    {pengine_died,_Reason2} = pengine_master:abort(Id1, "http://127.0.0.1:4000/pengine"),
    receive
        aborted ->
            ok
    after 5000 ->
            ct:fail("Pengine abort timeout")
    end.

%% test lookup
test_lookup(_Config)->
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}),
    ?assertMatch(P1, pengine_master:lookup_pengine(Id1)),
    pengine_master:kill_all_pengines().

%% test list_all_pengines
test_list_pengines(_Config)->
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}),
    List1 = pengine_master:list_pengines(),
    ?assert(lists:member({P1,Id1}, List1)),
    ?assert(length(List1) =:= 1),
    {{ok, {P2, Id2}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", #{}),
    List2 = pengine_master:list_pengines(),
    ?assert(lists:member({P1,Id1}, List2)),
    ?assert(lists:member({P2,Id2}, List2)),
    ?assert(length(List2) =:= 2),
    pengine:destroy(P1),
    ct:sleep(500),
    List3 = pengine_master:list_pengines(),
    ?assertNot(lists:member({P1,Id1}, List3)),
    ?assert(lists:member({P2,Id2}, List3)),
    ?assert(length(List3) =:= 1),
    pengine:destroy(P2),
    ct:sleep(500),
    List4 = pengine_master:list_pengines(),
    ?assertNot(lists:member({P1,Id1}, List4)),
    ?assertNot(lists:member({P2,Id2}, List4)),
    ?assert(length(List4) =:= 0).

%% test stop request
test_stop(_Config)->
    Options = #{destroy => false, application => "pengine_sandbox", chunk => 1, format => json},
    {{ok, {P1, Id1}}, {no_create_query}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options),
    spawn(fun() -> 
                  pengine:ask(P1, "member(X, [1,2])", #{template => "[X]", chunk => "1"})
          end),
    Res = pengine_master:stop(Id1, "http://127.0.0.1:4000/pengine"),
    case Res of 
        {stopped, Id1} -> ok;
        {error, Id1, _Reason, _Code} -> ok; %% error if nothing to stop
        _ -> ct:fail("Received unexpected result from pengine: ~p", [Res])
    end,
    pengine_master:kill_all_pengines().

%%===================================================================
%% Internal functions
%%===================================================================

