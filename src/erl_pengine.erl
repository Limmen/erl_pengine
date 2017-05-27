%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_pengine top-level API to start/stop the application
%% @end
%%%-------------------------------------------------------------------
-module(erl_pengine).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%%====================================================================
%% Application callbacks
%%====================================================================

%% @private
%% @doc
%% Startup function
-spec start(normal | {takeover , node()} | {failover, node()}, term()) ->
                   {ok, pid()}.
start(_StartType, _StartArgs) ->
    lager:info("starting erl_pengine application"),
    syn:init(),
    erl_pengine_sup:start_link().

%% @private
%% @doc
%% Cleanup function
-spec stop(any()) -> ok.
stop(_State) ->
    lager:info("erl_pengine application stopping"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc
%% Auxilliary function that starts necessary dependency-applications
-spec start()-> {atom(), atom()}.
start()->
    application:ensure_all_started(erl_pengine),
    {ok, started}.
