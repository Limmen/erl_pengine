%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erl_pengine_sup top level supervisor.
%% Supervises pengine_master and pengine_sup.
%% @end
%%%-------------------------------------------------------------------
-module(erl_pengine_sup).
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
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    Master = #{id => 'pengine_master',
               start => {'pengine_master', start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => ['pengine_master']},

    PengineSup = #{id => 'pengine_sup',
                   start => {'pengine_sup', start_link, []},
                   restart => permanent,
                   shutdown => infinity,
                   type => supervisor,
                   modules => ['pengine_sup']},

    TableMngr = #{id => 'table_mngr',
                  start => {'table_mngr', start_link, []},
                  restart => permanent,
                   shutdown => 5000,
                   type => worker,
                  modules => ['table_mngr']},

    {ok, {SupFlags, [Master, PengineSup, TableMngr]} }.
