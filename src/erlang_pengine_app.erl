%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erlang_pengine top-level API to start/stop the application
%% @end
%%%-------------------------------------------------------------------
-module(erlang_pengine_app).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(application).

%% API
-export([create_pengine/1]).
-export_type([pengine_options/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% types

%% Client options for creating the pengine.
%% server and callback_module are mandatory arguments, rest are optional.
-type pengine_options():: #{
                       server := string(),
                       application => string(),
                       ask => string(),
                       template => string(),
                       chunk => integer(),
                       destroy => boolean(),
                       srctext => string(),
                       srcurl => string(),
                       format => string(),
                       callback_module := atom()
                      }.

%%====================================================================
%% API
%%====================================================================

%% @doc
%% Creates pengine with given options
-spec create_pengine(pengine_options()) -> atom().
create_pengine(Options) ->
    start(),
    lager:info("creating pengine with arguments: ~p", [Options]),
    Child = #{id => 'pengine',
              start => {'pengine', start_link, [Options]},
              restart => permanent,
              shutdown => 5000,
              type => worker},
    supervisor:start_child(erlang_pengine_sup, Child).

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
    lager:start(),
    application:start(erlang_pengine),
    {ok, started}.
