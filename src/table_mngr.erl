%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc table_mngr server. Only purpose to maintain the ETS-table with
%% all the active slave-pengines and add fault-tolerance in case
%% the pengine_master dies. This process have very low-chance of crashing
%% due to its limited purpose.
%% @end
%%%-------------------------------------------------------------------
-module(table_mngr).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(gen_server).

%% includes
-include("records.hrl").

%% API
-export([start_link/0, handover/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% macros
-define(SERVER, ?MODULE).

%% types

-type table_mngr_state() :: #table_mngr_state{}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Starts the server
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% handover store to pengine_master
-spec handover() -> ok.
handover() ->
    gen_server:cast(?MODULE, {handover}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server
-spec init(list()) -> {ok, table_mngr_state()}.
init([]) ->
    process_flag(trap_exit, true),
    handover(),
    {ok, #table_mngr_state{}}.

%% @private
%% @doc
%% Handling call messages
-spec handle_call(term(), term(), table_mngr_state()) -> {reply, ok, table_mngr_state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
-spec handle_cast(term(), table_mngr_state()) -> {noreply, table_mngr_state()}.
handle_cast({handover}, State) ->
    MasterPengine = whereis(pengine_master),
    link(MasterPengine),
    TableId = ets:new(pengines, [named_table, set, private]),
    lager:info("pengines state-table created, handing it over to master-pengine ~p MasterPengine"),
    lager:info("table: ~p", [ets:tab2list(TableId)]),
    ets:setopts(TableId, {heir, self(), {table_handover}}),
    ets:give_away(TableId, MasterPengine, {table_handover}),
    {noreply, State#table_mngr_state{table_id=TableId}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
-spec handle_info(timeout | term(), table_mngr_state()) -> {noreply, table_mngr_state()}.
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:info("MasterPengine ~p crashed, reason: ~p", [Pid, Reason]),
    {noreply, State};

handle_info({'ETS-TRANSFER', TableId, Pid, Data}, State) ->
    lager:info("Received backup tableId ~p of master-pengine ~p state who have crashed, waiting for it to be restored",
               [TableId, Pid]),
    MasterPengine = wait_for_master(),
    lager:info("Master-pengine restored ~p, handing over the state, tableId: ~p", [MasterPengine, TableId]),
    link(MasterPengine),
    ets:give_away(TableId, MasterPengine, Data),
    {noreply, State#table_mngr_state{table_id=TableId}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Cleanup function
-spec terminate(normal | shutdown | {shutdown, term()}, table_mngr_state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
-spec code_change(term | {down, term()}, table_mngr_state(), term()) -> {ok, table_mngr_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc
%% Wait for master pengine to be restored
-spec wait_for_master() -> pid().
wait_for_master() ->
    case whereis(pengine_master) of
        undefined ->
            timer:sleep(100),
            wait_for_master();
        Pid -> Pid
    end.