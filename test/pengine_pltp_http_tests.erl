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
                              meck:expect(hackney, post, fun(_, _, _, _) -> 
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
                                           pengine_pltp_http:create("127.0.0.1/pengine", #{
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
                            ok
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
                             ok
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
                            meck:expect(hackney, post, fun(_, _, _, _) -> 
                                                               {ok, 200, [], ref1} 
                                                       end),
                            Res = <<"{\"event\":\"destroy\", \"id\":\"38612376351347808823784683757103067622\"}">>,
                            meck:expect(hackney, body, fun(ref1) -> 
                                                               {ok,
                                                                Res
                                                               } end),
                            Id = <<"38612376351347808823784683757103067622">>,
                            ?assertMatch({ok, 
                                          #{
                                            <<"event">> := <<"destroy">>,
                                            <<"id">> := Id
                                           }},
                                         pengine_pltp_http:send(Id, "127.0.0.1/pengine", "destroy", "json"))
                    end
                   ]
                  }
    }.




%%===================================================================
%% Internal functions
%%===================================================================
