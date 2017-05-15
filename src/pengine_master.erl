%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc pengine_master gen_server. Master process that controls and
%% manages the erlang-processes that are connected to pengine-slaves
%% @end
%%%-------------------------------------------------------------------
-module(pengine_master).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(gen_server).

%% API
-export([start_link/0, create_pengine/3, list_pengines/0]).
-export_type([pengine_create_options/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% macros
-define(SERVER, ?MODULE).

%% types

%% pengine create options
-type pengine_create_options():: #{
                              application => string(),
                              ask => string(),
                              template => string(),
                              chunk => integer(),
                              destroy => boolean(),
                              format => string()
                             }.

%% records
-record(state, {
          pengines = sets:new()
         }).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Starts the server
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Creates pengine with given options
-spec create_pengine(string(), atom(), pengine:pengine_create_options()) -> 
                            {ok, pid()} | already_present | 
                            {already_started, pid()} | term().
create_pengine(Server, CallBackModule, CreateOptions) ->
    lager:info("creating pengine, server: ~p, callbackmod: ~p, createOpts: ~p", [Server, CallBackModule, CreateOptions]),
    gen_server:call(?SERVER, {create, Server, CallBackModule, CreateOptions}).

%% @doc
%% Returns list of active pengines
list_pengines()->
    gen_server:call(?SERVER, {list_pengines}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server
-spec init(list()) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

%% @private
%% @doc
%% Handling call messages
-spec handle_call(term(), term(), #state{}) -> {reply, ok, #state{}}.
handle_call({create, Server, CallBackModule, CreateOptions}, _From, State) ->
    State1 = State#state{pengines = cleanup_pengines(State#state.pengines)},
    Opts = #{application => "pengine_sandbox", chunk => 1, destroy => true, format => json}, %%Default Options
    Opts1 = maps:fold(fun(K,V,S) -> S#{K => V}  end, Opts, CreateOptions),
    {Id, MaxSlaves} = pengine_pltp_http:create(Server, Opts1),
    Size = sets:size(State1#state.pengines),
    lager:info("Attempting to create pengine, max_slaves: ~p , active pengines: ~p", [MaxSlaves, Size]),
    if 
        MaxSlaves > Size ->
            {ok, Pid} = supervisor:start_child(pengine_sup, [[Id, Server, CallBackModule]]),
            pengine:call_callback(CallBackModule, oncreate, [Id]),
            Pengines = sets:add_element(Id, State1#state.pengines),
            {reply, {ok, Pid, Id}, State1#state{pengines = Pengines}};
        true -> 
            lager:info("Attemt to create too many pengines. The limit is: ~p ~n", [MaxSlaves]),
            Reason = "Attemt to create too many pengines. The limit is: " ++ [MaxSlaves] ++ "\n",
            pengine:call_callback(CallBackModule, onerror, [Id, Reason]),
            pengine_pltp_http:send(Id, Server, "destroy"),
            {reply, {error, Reason}, State1}
    end;

handle_call({list_pengines}, _From, State) ->
    Pengines = cleanup_pengines(State#state.pengines),
    ResultList = lists:foldl(fun(Id, A) -> [{syn:find_by_key(Id), Id}|A] end, [], sets:to_list(Pengines)),
    {reply, ResultList, State#state{pengines = Pengines}}.

%% @private
%% @doc
%% Handling cast messages
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
-spec handle_info(timeout | term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Cleanup function, kill all pengines before termination.
-spec terminate(normal | shutdown | {shutdown,term()}, #state{}) -> ok.
terminate(Reason, State) ->
    cleanup_pengines(State#state.pengines),
    lists:map(fun(Id) -> pengine:destroy(syn:find_by_key(Id), Reason) end, sets:to_list(State#state.pengines)),
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
%% update the state of active pengines, check if any pengines are destroyed/killed and if so remove them.
cleanup_pengines(Pengines)->
    sets:filter(fun(Id) -> undefined =/= syn:find_by_key(Id) end, Pengines).

