%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc pengine_sup, supervises pengine-processes that are created
%% dynamically.
%% @end
%%%-------------------------------------------------------------------
-module(pengine_sup).
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
    lager:debug("starting pengine supervisor"),
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
    lager:debug("initializing pengine supervisor"),
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    Pengine = #{id => 'pengine',
                start => {'pengine', start_link, []},
                restart => transient,
                shutdown => 5000,
                type => worker,
                modules => ['pengine']},

    {ok, {SupFlags, [Pengine]} }.
