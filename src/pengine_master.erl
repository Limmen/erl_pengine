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

%% includes
-include("records.hrl").

%% API
-export([start_link/0, create_pengine/2, list_pengines/0, kill_all_pengines/0,
         stop/2, lookup_pengine/1, abort/2]).
-export_type([pengine_create_options/0, master_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% exports just for tests
-ifdef(TEST).
-export([cleanup_pengines/1]).
-endif.

%% macros
-define(SERVER, ?MODULE).

%% types

%% state
-type master_state()::#master_state{}.

%% pengine create options
-type pengine_create_options():: #{
                              application => binary() | string(),
                              ask => binary() | string(),
                              template => binary() | string(),
                              chunk => integer(),
                              destroy => boolean(),
                              format => binary() | string(),
                              src_text => binary() | string(),
                              src_url => binary() | string()
                             }.

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
-spec create_pengine(string(), pengine:pengine_create_options()) ->
                            pengine:create_response() | pengine:error_response().
create_pengine(Server, CreateOptions) ->
    gen_server:call(?SERVER, {create, Server, CreateOptions}).

%% @doc
%% Returns list of active pengines
-spec list_pengines() -> list().
list_pengines()->
    gen_server:call(?SERVER, {list_pengines}).

%% @doc
%% Lookup pengin pid based on id
-spec lookup_pengine(binary()) -> pid().
lookup_pengine(Id)->
    syn:find_by_key(Id).

%% @doc
%% Kill all active pengines
-spec kill_all_pengines() -> ok.
kill_all_pengines()->
    gen_server:call(?SERVER, {kill_all_pengines}).

%% @doc
%% Tells a busy pengine to stop searching for solutions. Terminates the running query gracefully.
-spec stop(binary(), string() | binary()) -> pengine:stop_response() | pengine:error_response().
stop(Id, Server) ->
    gen_server:call(?SERVER, {stop, Id, Server}).


%% @doc
%% Terminates the running query of a busy pengine by force
-spec abort(binary(), string() | binary()) -> pengine:abort_response() | pengine:error_response().
abort(Id, Server) ->
    gen_server:call(?SERVER, {abort, Id, Server}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server
-spec init(list()) -> {ok, master_state()}.
init([]) ->
    {ok, #master_state{}}.

%% @private
%% @doc
%% Handling call messages
-spec handle_call(term(), term(), master_state()) ->
                         {reply, any(), master_state()}.
handle_call({create, Server, CreateOptions}, _From, State) ->
    cleanup_pengines(State#master_state.table_id),
    Opts = maps:fold(fun(K, V, S) -> S#{K => V}  end, default_create_options(), CreateOptions),
    {ok, Res} = pengine_pltp_http:create(Server, Opts),
    pengine:process_response(Res, State, {create, State#master_state.table_id, Server});

handle_call({list_pengines}, _From, State) ->
    cleanup_pengines(State#master_state.table_id),
    ResultList = lists:foldl(fun({Id}, A) -> [{syn:find_by_key(Id), Id}|A] end, [], ets:tab2list(State#master_state.table_id)),
    {reply, ResultList, State};

handle_call({kill_all_pengines}, _From, State) ->
    cleanup_pengines(State#master_state.table_id),
    kill_all_pengines(State#master_state.table_id),
    {reply, ok, State};

handle_call({stop, Id, Server}, _From, State) ->
    {ok, Res} = pengine_pltp_http:send(Id, Server, "stop", "json"),
    pengine:process_response(Res, State, {});

handle_call({abort, Id, Server}, _From, State) ->
    {ok, Res} = pengine_pltp_http:abort(Id, Server, "json"),
    pengine:process_response(Res, State, {});

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
-spec handle_cast(term(), master_state()) -> {noreply, master_state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
-spec handle_info(timeout | term(), master_state()) -> {noreply, master_state()}.
handle_info({'ETS-TRANSFER', TableId, _Pid, _Data}, State) ->
    {noreply, State#master_state{table_id=TableId}};

handle_info(_Info, State) ->
    {noreply, State}.


%% @private
%% @doc
%% Cleanup function, kill all pengines before termination.
-spec terminate(normal | shutdown | {shutdown, term()}, master_state()) -> ok.
terminate(_Reason, State) ->
    cleanup_pengines(State#master_state.table_id),
    kill_all_pengines(State#master_state.table_id),
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
-spec code_change(term | {down, term()}, master_state(), term()) -> {ok, master_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc
%% update the state of active pengines, check if any pengines are destroyed/killed and if so remove them.
-spec cleanup_pengines(ets:tid()) -> true.
cleanup_pengines(TableId)->
    Pengines = lists:filter(fun({Id}) -> undefined =/= syn:find_by_key(Id) end, ets:tab2list(TableId)),
    ets:delete_all_objects(TableId),
    ets:insert(TableId, Pengines).

%% @private
%% @doc
%% returns default options for creating a new pengine slave
default_create_options()->
    #{application => "pengine_sandbox", chunk => 1, destroy => true, format => json}.

%% @private
%% @doc
%% kill all active pengines
-spec kill_all_pengines(ets:tid()) -> ok.
kill_all_pengines(TableId)->
    lists:map(fun({Id}) ->
                      case syn:find_by_key(Id) =:= undefined of
                          true ->
                              {ok, undefined};
                          false ->
                              gen_server:stop(syn:find_by_key(Id)),
                              {ok, stopped}
                      end
              end, ets:tab2list(TableId)),
    ets:delete_all_objects(TableId),
    ok.
