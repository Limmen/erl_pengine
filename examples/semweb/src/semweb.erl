%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc example querying a prolog triple-store using erl_pengine
%% @end
%%%-------------------------------------------------------------------

-module(semweb).

%% API exports
-export([supervises/2]).

%%====================================================================
%% API functions
%%====================================================================

%% Create pengine and issue single query and parse and return result as a list.
-spec supervises(string(), string()) -> list().
supervises(X,Y)->
    Query = "supervises(" ++ X ++ "," ++ Y ++ ")",
    Options = #{
      destroy => true, 
      application => "pengine_sandbox", 
      chunk => 10, 
      format => json, 
      ask => Query
     },
    {{ok, {pengine_destroyed, Id}}, 
     {create_query, {
        {success, Id, Result, false},
        {pengine_destroyed, _}}}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options),
    lists:foldl(fun
                    (#{<<"X">> := Xx, <<"Y">> := Yy}, A) -> [{Xx, supervises, Yy}|A];
                    (#{<<"X">> := Xx}, A) -> [{Xx, supervises,Y}|A];
                    (#{<<"Y">> := Yy}, A) -> [{X, supervises, Yy}|A] end, [], Result).
