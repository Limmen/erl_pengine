%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc Unit test suite for module pengine_master
%% @end
%%%-------------------------------------------------------------------
-module(pengine_master_tests).
-author('Kim Hammar <kimham@kth.se>').


%% API
-compile(export_all).

%% Includes
-include_lib("eunit/include/eunit.hrl").
-include("../src/records.hrl").

%%===================================================================
%% TESTS
%%===================================================================


%% tests cleanup_pengines_test()
-spec cleanup_pengines_test_() -> tuple().
cleanup_pengines_test_() ->
    {"cleanup_pengines tests", {foreach,
                                fun()->
                                        meck:new(syn)
                                end,
                                fun(_)->
                                        meck:unload(syn)
                                end,
                                [
                                 fun()->
                                         TableId = ets:new(pengines, [named_table, set, private]),
                                         ets:insert(TableId, [{<<"1">>}, {<<"2">>}]),
                                         meck:expect(syn, find_by_key, fun (<<"1">>) -> undefined;
                                                                           (<<"2">>) -> self() end),
                                         pengine_master:cleanup_pengines(TableId),
                                         ?assertMatch([{<<"2">>}], ets:tab2list(TableId))
                                 end
                                ]
                               }
    }.

%% tests handle_call
-spec handle_call_test_() -> tuple().
handle_call_test_()->
    {"handle call test", {foreach,
                          fun() ->
                                  meck:new(pengine_pltp_http),
                                  meck:new(pengine),
                                  meck:new(syn)
                          end,
                          fun(_) ->
                                  meck:unload(pengine_pltp_http),
                                  meck:unload(pengine),
                                  meck:unload(syn)
                          end,
                          [
                           fun()->
                                   meck:expect(pengine_pltp_http, create, fun(_,_) -> {ok, res} end),
                                   meck:expect(pengine, process_response, fun(_,_) -> {ok, res2} end),
                                   meck:expect(syn, find_by_key, fun (<<"1">>) -> pid1;
                                                                     (<<"2">>) -> pid2 end),
                                   TableId = ets:new(pengines, [named_table, set, private]),
                                   State = #master_state{
                                              table_id = TableId
                                             },
                                   ets:insert(TableId, [{<<"1">>}, {<<"2">>}]),
                                   ?assertMatch({reply, {ok, res2}, State}, pengine_master:handle_call({create, "server", test, #{}}, nil, State)),
                                   ?assertMatch({reply, [{pid2, <<"2">>}, {pid1, <<"1">>}], State}, pengine_master:handle_call({list_pengines}, nil, State))
                           end
                          ]
                         }
    }.

%%===================================================================
%% Internal functions
%%===================================================================
