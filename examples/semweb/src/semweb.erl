%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc example sudoku_solver using erl_pengine
%% @end
%%%-------------------------------------------------------------------

-module(semweb).

%% API exports
-export([supervises/0]).

%%====================================================================
%% API functions
%%====================================================================
-spec supervises() -> any().
supervises()->
    Options = #{
      destroy => true, 
      application => "pengine_sandbox", 
      chunk => 10, 
      format => json, 
      ask => "supervises(X, Y)"
     },
    {{ok, {pengine_destroyed, Id}}, 
     {create_query, {
        {success, Id, Result, false},
        {pengine_destroyed, _}}}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options),
    lists:foldl(fun(#{<<"X">> := X, <<"Y">> := Y}, A) -> [{X, supervises, Y}|A] end, [], Result).

%%====================================================================
%% Internal functions
%%====================================================================
