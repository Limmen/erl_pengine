%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erlang_pengine top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(erlang_pengine_sup).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% macros
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Starts top-level supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @private
%% @doc
%% Initialize the supervisor with supervisor-flags and child specifications.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([]) -> {ok, {supervisor:sup_flags(),
                        [supervisor:child_spec()]}}.
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
