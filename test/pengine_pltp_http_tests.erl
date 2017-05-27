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

%% tests create
-spec create_test_() -> tuple().
create_test_()->
    {"create test", {foreach,
                     fun() ->
                             meck:new(hackney)
                     end,
                     fun(_) ->
                             meck:unload(hackney)
                     end,
                     [
                      fun()->
                              meck:expect(hackney, post, fun(<<"127.0.0.1:4000/pengine/create">>, _, _, _) -> 
                                                                 {ok, 200, [], ref1} 
                                                         end),
                              Res = <<"{\n  \"event\":\"create\",\n  \"id\":\"e8d94208-168e-42c2-8666-f938c9a5f388\",\n  \"slave_limit\":3\n}">>,
                              meck:expect(hackney, body, fun(ref1) -> 
                                                                 {ok,
                                                                  Res
                                                                 } end),
                              ?assertMatch({ok, #{
                                              <<"event">> := <<"create">>,
                                              <<"id">> := <<"e8d94208-168e-42c2-8666-f938c9a5f388">>,
                                              <<"slave_limit">> := 3
                                             }},
                                           pengine_pltp_http:create("127.0.0.1:4000/pengine", #{
                                                                      application => "pengine_sandbox",
                                                                      chunk => 1, 
                                                                      destroy => true, 
                                                                      format => json
                                                                     }))
                      end
                     ]
                  }
    }.

%% tests ping
-spec ping_test_() -> tuple().
ping_test_()->
    {"ping test", {foreach,
                   fun() ->
                           meck:new(hackney)
                   end,
                   fun(_) ->
                           meck:unload(hackney)
                   end,
                   [
                    fun()->
                            meck:expect(hackney, get, fun(<<"127.0.0.1:4000/pengine/ping?id=38612376351347808823784683757103067622&format=json">>, _, _, _) -> 
                                                              {ok, 200, [], ref1} 
                                                      end),
                            meck:expect(hackney, body, fun(ref1) -> 
                                                               {ok,
                                                                mock_ping_response()
                                                               } end),
                            Id = <<"38612376351347808823784683757103067622">>,
                            ?assertMatch({ok, #{
                                            <<"event">> := <<"ping">>,
                                            <<"id">> := Id,
                                            <<"data">> := _
                                           }},
                                         pengine_pltp_http:ping(Id, "127.0.0.1:4000/pengine", "json"))
                    end
                   ]
                         }
    }.

%% tests abort
-spec abort_test_() -> tuple().
abort_test_()->
    {"abort test", {foreach,
                    fun() ->
                            meck:new(hackney)
                    end,
                    fun(_) ->
                            meck:unload(hackney)
                    end,
                    [
                     fun()->
                             meck:expect(hackney, get, fun(<<"127.0.0.1:4000/pengine/abort?id=38612376351347808823784683757103067622&format=json">>, _, _, _) -> 
                                                               {ok, 200, [], ref1} 
                                                       end),
                             meck:expect(hackney, body, fun(ref1) -> 
                                                                {ok,
                                                                 mock_abort_response()
                                                                } end),
                             Id = <<"38612376351347808823784683757103067622">>,
                             ?assertMatch({ok, #{
                                             <<"event">> := <<"destroy">>,
                                             <<"id">> := Id,
                                             <<"data">> := _
                                            }},
                                          pengine_pltp_http:abort(Id, "127.0.0.1:4000/pengine", "json"))
                     end
                    ]
                  }
    }.

%% tests pull_response
-spec pull_response_test_() -> tuple().
pull_response_test_()->
    {"pull_response test", {foreach,
                            fun() ->
                                    meck:new(hackney)
                            end,
                            fun(_) ->
                                    meck:unload(hackney)
                            end,
                            [
                             fun()->
                                     ok
                             end
                            ]
                  }
    }.

%% tests ping
-spec send_test_() -> tuple().
send_test_()->
    {"send test", {foreach,
                   fun() ->
                           meck:new(hackney)
                   end,
                   fun(_) ->
                           meck:unload(hackney)
                   end,
                   [
                    fun()->
                            meck:expect(hackney, post, fun(<<"127.0.0.1:4000/pengine/send?format=json&id=38612376351347808823784683757103067622">>, _, _, _) -> 
                                                               {ok, 200, [], ref1} 
                                                       end),                            
                            meck:expect(hackney, body, fun(ref1) -> 
                                                               {ok,
                                                                mock_destroy_response()
                                                               } end),
                            Id = <<"38612376351347808823784683757103067622">>,
                            ?assertMatch({ok, 
                                          #{
                                            <<"event">> := <<"destroy">>,
                                            <<"id">> := Id
                                           }},
                                         pengine_pltp_http:send(Id, "127.0.0.1:4000/pengine", "destroy", "json"))
                    end,
                    fun()->
                            meck:expect(hackney, post, fun(<<"127.0.0.1:4000/pengine/send?format=json&id=38612376351347808823784683757103067622">>, _, _, _) -> 
                                                               {ok, 200, [], ref1} 
                                                       end),                            
                            meck:expect(hackney, body, fun(ref1) -> 
                                                               {ok,
                                                                mock_query_response()
                                                               } end),
                            Id = <<"38612376351347808823784683757103067622">>,
                            ?assertMatch({ok, 
                                          #{
                                            <<"event">> := <<"success">>,
                                            <<"id">> := Id,
                                            <<"more">> := true,
                                            <<"data">> := [[1]],
                                            <<"time">> := _
                                           }},
                                         pengine_pltp_http:send(Id, "127.0.0.1:4000/pengine", "ask((member(X, [1,2])), [template([X])])", "json"))
                    end
                   ]
                  }
    }.




%%===================================================================
%% Internal functions
%%===================================================================

mock_ping_response()->
    <<"{\n  \"data\": {\n    \"id\":11,\n    \"stacks\": {\n      \"global\": {\n\t\"allocated\":61424,\n\t\"limit\":268435456,\n\t\"name\":\"global\",\n\t\"usage\":2224\n      },\n      \"local\": {\n\t\"allocated\":28672,\n\t\"limit\":268435456,\n\t\"name\":\"local\",\n\t\"usage\":1408\n      },\n      \"total\": {\n\t\"allocated\":120808,\n\t\"limit\":805306368,\n\t\"name\":\"stacks\",\n\t\"usage\":4296\n      },\n      \"trail\": {\n\t\"allocated\":30712,\n\t\"limit\":268435456,\n\t\"name\":\"trail\",\n\t\"usage\":664\n      }\n    },\n    \"status\":\"running\",\n    \"time\": {\"cpu\":0.004206131, \"epoch\":1495369731.2359762, \"inferences\":199}\n  },\n  \"event\":\"ping\",\n  \"id\":\"38612376351347808823784683757103067622\"\n}">>.

mock_abort_response()->
    <<"{\n  \"data\": {\"event\":\"abort\", \"id\":\"38612376351347808823784683757103067622\"},\n  \"event\":\"destroy\",\n  \"id\":\"38612376351347808823784683757103067622\"\n}">>.

mock_query_response()->
    <<"{\n  \"data\": [ [1 ] ],\n  \"event\":\"success\",\n  \"id\":\"38612376351347808823784683757103067622\",\n  \"more\":true,\n  \"time\":1.7486000000000584e-5\n}">>.

mock_destroy_response()->
    <<"{\"event\":\"destroy\", \"id\":\"38612376351347808823784683757103067622\"}">>.
