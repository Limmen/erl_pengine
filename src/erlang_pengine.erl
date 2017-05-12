%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erlang_pengine top-level API to start/stop the application
%% @end
%%%-------------------------------------------------------------------
-module(erlang_pengine).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(application).

%% API
-export([create_pengine/3, start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc
%% Creates pengine with given options
-spec create_pengine(string(), atom(), pengine:pengine_create_options()) -> atom().
create_pengine(Server, CallBackModule, CreateOptions) ->
    %%start(),
    lager:info("creating pengine, server: ~p, callbackmod: ~p, createOpts: ~p", [Server, CallBackModule, CreateOptions]),
    supervisor:start_child(erlang_pengine_sup, [[Server, CallBackModule, CreateOptions]]).    

%%====================================================================
%% Application callbacks
%%====================================================================

%% @private
%% @doc
%% Startup function
-spec start(normal | {takeover , node()} | {failover, node()}, term()) ->
                   {ok, pid()}.
start(_StartType, _StartArgs) ->
    lager:info("starting erlang_pengine application"),
    erlang_pengine_sup:start_link().

%% @private
%% @doc
%% Cleanup function
-spec stop(any()) -> ok.
stop(_State) ->
    lager:info("erlang_pengine application stopping"),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc
%% Auxilliary function that starts necessary dependency-applications
-spec start()-> {atom(), atom()}.
start()->
    application:ensure_all_started(erlang_pengine),
    {ok, started}.
