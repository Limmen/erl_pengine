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
-include("../src/records.hrl").

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

%% tests handle_call
-spec handle_call_test_() -> tuple().
handle_call_test_()->
    {"handle call test", {foreach,
                          fun() ->
                                  meck:new(pengine_pltp_http),
                                  meck:new(syn)
                          end,
                          fun(_) ->
                                  meck:unload(pengine_pltp_http),
                                  meck:unload(syn)
                          end,
                          [
                           fun()->
                                   State = #pengine_state{
                                              id = <<"1">>, 
                                              server = <<"server">>, 
                                              callback_module = test
                                             },
                                   meck:expect(pengine_pltp_http, send, mock_send()),
                                   meck:expect(pengine_pltp_http, abort, mock_abort()),
                                   meck:expect(pengine_pltp_http, pull_response, mock_pull_response()),
                                   meck:expect(syn, find_by_key, fun(_) -> undefined end),
                                   ?assertMatch({reply, ok, State}, pengine:handle_call({id}, nil, State)),
                                   ?assertMatch({reply, ok, State}, pengine:handle_call({ask, "test", #{}}, nil, State)),
                                   ?assertMatch({reply, ok, State}, pengine:handle_call({next}, nil, State)),
                                   ?assertMatch({reply, ok, State}, pengine:handle_call({stop}, nil, State)),
                                   ?assertMatch({reply, ok, State}, pengine:handle_call({respond, "[]"}, nil, State)),
                                   ?assertMatch({reply, ok, State}, pengine:handle_call({abort}, nil, State)),
                                   ?assertExit(noproc, pengine:handle_call({destroy}, nil, State))
                           end
                          ]
                         }
    }.


%%===================================================================
%% Internal functions
%%===================================================================

mock_send()->
    fun
        (_, _, "create", _)->
            {ok, #{<<"event">> => <<"create">>, <<"id">> => <<"1">>, <<"slave_limit">> => 1}};
        (_, _, "stop", _)->
            {ok, #{<<"event">> => <<"stop">>, <<"id">> => <<"1">>}};
        (_, _, "failure", _)->
            {ok, #{<<"event">> => <<"failure">>, <<"id">> => <<"1">>}};
        (_, _, "prompt", _)->
            {ok, #{<<"event">> => <<"prompt">>, <<"id">> => <<"1">>, <<"data">> => <<"[]">>}};
        (_, _, "error", _)->
            {ok, #{<<"event">> => <<"error">>, <<"id">> => <<"1">>, <<"code">> => <<"existence_error">>, <<"data">> => <<"[]">>}};
        (_, _, "success", _)->
            {ok, #{<<"event">> => <<"success">>, <<"id">> => <<"1">>, <<"data">> => <<"[]">>, <<"more">> => true}};
        (_, _, "output", _)->
            {ok, #{<<"event">> => <<"output">>, <<"id">> => <<"1">>, <<"data">> => <<"[]">>}};
        (_, _, "debug", _)->
            {ok, #{<<"event">> => <<"debug">>}};
        (_, _, "destroy", _)->
            {ok, #{<<"event">> => <<"destroy">>, <<"id">> => <<"1">>}};
        (_, _, "died", _)->
            {ok, #{<<"event">> => <<"died">>, <<"id">> => <<"1">>, <<"data">> => <<"[]">>}};
        (_, _, _, _)->
            {ok, #{<<"event">> => <<"success">>, <<"id">> => <<"1">>, <<"data">> => <<"[]">>, <<"more">> => true}}
    end.

mock_abort()->
    fun (_, _, _)->
            {ok, #{<<"event">> => <<"abort">>, <<"id">> => <<"1">>}}
    end.

mock_ping()->
    fun (_, _,  _)->
            {ok, #{<<"event">> => <<"ping">>}}
    end.

mock_pull_response()->
    fun (_, _, _)->
            {ok, #{<<"event">> => <<"success">>, <<"id">> => <<"1">>, <<"data">> => <<"[]">>, <<"more">> => true}}
    end.

