%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc example sudoku_solver using erl_pengine
%% @end
%%%-------------------------------------------------------------------
-module(sudoku_solver).

%% API exports
-export([solve_sudoku/1]).

%%====================================================================
%% API functions
%%====================================================================

%% Query a pengine for a solution to a sudoku instance specified in 
%% Src input (prolog source).
-spec solve_sudoku(binary()) -> ok.
solve_sudoku(Src)->
    Options = #{
      destroy => true, 
      application => "pengine_sandbox", 
      chunk => 1, 
      format => json, 
      src_text => Src,
      ask => "problem(1, Rows), sudoku(Rows)"
     },
    {{ok, {_, Id}}, {create_query, {success, Id, [#{<<"Rows">> := Rows}], true}}} = 
        pengine_master:create_pengine("http://127.0.0.1:4000/pengine", Options),
    print_solution(Rows).

%%====================================================================
%% Internal functions
%%====================================================================

%% print sudoku solution
-spec print_solution(list()) -> ok.
print_solution(Rows)->
    io:format("Sudoku solution: ~n"),
    lists:map(fun(Row) -> io:format("~p~n", [Row]) end, Rows),
    ok.
