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

%%===================================================================
%% TESTS
%%===================================================================

%% Test generator for module pengine_master
-spec pengine_master_test_() -> list().
pengine_master_test_() ->
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

%%-spec cleanup_pengines_test_() -> list().
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


%%===================================================================
%% Internal functions
%%===================================================================
