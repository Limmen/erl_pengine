%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc table_mngr server
%% @end
%%%-------------------------------------------------------------------
-module(table_mngr).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(gen_server).

%% API
-export([start_link/0, handover/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% macros
-define(SERVER, ?MODULE).

%% records
-record(state, {table_id :: ets:tid()}).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Starts the server
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handover() ->
    gen_server:cast(?MODULE, {handover}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server
-spec init(list()) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    handover(),
    {ok, #state{}}.

%% @private
%% @doc
%% Handling call messages
-spec handle_call(term(), term(), #state{}) -> {reply, ok, #state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({handover}, State) ->
    MasterPengine = whereis(pengine_master),
    link(MasterPengine),
    TableId = ets:new(pengines, [named_table, set, private]),
    lager:info("pengines state-table created, handing it over to master-pengine ~p MasterPengine"),
    lager:info("table: ~p", [ets:tab2list(TableId)]),
    ets:setopts(TableId, {heir, self(), {table_handover}}),
    ets:give_away(TableId, MasterPengine, {table_handover}),
    {noreply, State#state{table_id=TableId}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
-spec handle_info(timeout | term(), #state{}) -> {noreply, #state{}}.
handle_info({'EXIT', Pid, Reason}, State) ->
    lager:info("MasterPengine ~p crashed, reason: ~p", [Pid, Reason]),
    {noreply, State};

handle_info({'ETS-TRANSFER', TableId, Pid, Data}, State) ->
    lager:info("Received backup tableId ~p of master-pengine ~p state who have crashed, waiting for it to be restored", [TableId, Pid]),
    MasterPengine = wait_for_master(),
    lager:info("Master-pengine restored ~p, handing over the state, tableId: ~p",[MasterPengine, TableId]),
    link(MasterPengine),
    ets:give_away(TableId, MasterPengine, Data),
    {noreply, State#state{table_id=TableId}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Cleanup function
-spec terminate(normal | shutdown | {shutdown,term()}, #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
-spec code_change(term | {down, term()}, #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc
%% Wait for master pengine to be restored
wait_for_master() -> 
    case whereis(pengine_master) of
        undefined -> 
            timer:sleep(100),
            wait_for_master();
        Pid -> Pid
    end.
