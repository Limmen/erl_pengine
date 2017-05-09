%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erlang_pengine public API
%% @end
%%%-------------------------------------------------------------------
-module(erlang_pengine_app).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%% @private
%% @doc
%% Startup function
-spec start(normal | {takeover , node()} | {failover, node()}, term()) ->
                   {ok, pid()}.
start(_StartType, _StartArgs) ->
    erlang_pengine_sup:start_link().


%% @private
%% @doc
%% Cleanup function
-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
