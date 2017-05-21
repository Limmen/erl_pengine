%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc pengine gen_server. Controls a pengine slave
%% @end
%%%-------------------------------------------------------------------
-module(pengine).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(gen_server).

%% includes
-include("records.hrl").

%% API
-export([start_link/1, id/1, ask/3, next/1, stop/1, respond/2, abort/1, 
         destroy/1, call_callback/3, process_response/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% exports just for tests
-ifdef(TEST).
-export([options_to_list/1]).
-endif.

%% macros
-define(SERVER, ?MODULE).

%% types

%% query_options to the ask() function.
-type query_options():: #{
                     template := string(),
                     chunk := integer()
                    }.

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Starts the server
-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
    lager:info("starting pengine"),
    gen_server:start_link(?MODULE, Args, []).

%% @doc
%% call function of callback-module
call_callback(CallBackMod, CallBackFunc, Args)->
    try apply(CallBackMod, CallBackFunc, Args) of
        Result ->
            Result
    catch
        _:_ ->
            no_match
    end.

%% @doc
%% Returns the id of the pengine (a string). 
%% Note that the pengine must have been created before this field will have a 
%% non-null value, i.e. the oncreate handler must have been called.
-spec id(pid()) -> binary().
id(Pengine)->
    lager:info("querying the pengine for its id"),
    gen_server:call(Pengine, {id}).

%% @doc
%% Runs query in search for the first solution. 
%% Throws an error if the query is syntactically or semantically malformed 
%% or if running it could compromise the safety of the server. 
%% options is a Erlang map contaning zero or more of:
%% template :: string() : 
%% A prolog variable (or a term containing problog variables) shared with the query.
%% chunk :: integer() :
%% The maximum number of solutions to retrieve in one chunk. 1 means no chunking (default).
-spec ask(pid(), string(), query_options()) -> atom().
ask(Pengine,Query, Options) ->
    lager:info("sending a query ~p to the pengine", [Query]),
    gen_server:call(Pengine, {ask, Query, Options}).

%% @doc
%% Triggers a search for the next solution.
next(Pengine) ->
    lager:info("asking the pengine for next solution"),
    gen_server:call(Pengine, {next}).

%% @doc
%% Stops searching for solutions. Terminates the running query gracefully.
stop(Pengine) ->
    lager:info("stopping the running query"),
    gen_server:call(Pengine, {stop}).

%% @doc
%% Inputs a term in response to a prompt from an invocation of pengine_input/2
%% that is now waiting to receive data from the outside. 
%% Throws an error if string cannot be parsed as a Prolog term or if object cannot be serialised into JSON.
respond(Pengine, PrologTerm) ->
    lager:info("responds to pengine_input with ~p", [PrologTerm]),
    gen_server:call(Pengine, {respond, PrologTerm}).

%% @doc
%% Terminates the running query by force.
abort(Pengine) ->
    lager:info("aborts the running query abruptely"),
    gen_server:call(Pengine, {abort}).

%% @doc
%% Destroys the pengine.
destroy(Pengine) ->
    lager:info("destroying the pengine"),
    gen_server:call(Pengine, {destroy}),
    gen_server:stop(Pengine).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server, creates the pengine.
%%-spec init([string(), atom(), pengine_create_options()]) -> {ok, #pengine_state{}}.
-spec init(list()) -> {ok, #pengine_state{}}.
init([Id, Server, CallBackModule]) ->
    lager:info("Initializing pengine"),
    syn:register(Id, self()),
    {ok, #pengine_state{id = Id, server = Server, callback_module = CallBackModule}}.

%% @private
%% @doc
%% Handling call messages
-spec handle_call(term(), term(), #pengine_state{}) -> {reply, ok, #pengine_state{}}.
handle_call({id}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call({ask, Query, Options}, _From, State) ->
    Send = "ask((" ++ Query ++ "), " ++ options_to_list(Options) ++ ")",
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, Send, "json"),
    process_response(Res, {State#pengine_state.callback_module}),
    Reply = ok,
    {reply, Reply, State};

handle_call({next}, _From, State) ->
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, "next", "json"),
    process_response(Res, {State#pengine_state.callback_module}),
    Reply = ok,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, "stop", "json"),
    process_response(Res, {State#pengine_state.callback_module}),
    Reply = ok,
    {reply, Reply, State};

handle_call({respond, PrologTerm}, _From, State) ->
    Send = "input((" ++ PrologTerm ++ "))",
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, Send, "json"),
    process_response(Res, {State#pengine_state.callback_module}),
    Reply = ok,
    {reply, Reply, State};

handle_call({abort}, _From, State) ->
    {ok, Res} = pengine_pltp_http:abort(State#pengine_state.id, State#pengine_state.server, "json"),
    process_response(Res, {State#pengine_state.callback_module}),
    Reply = ok,
    {reply, Reply, State};

handle_call({destroy}, _From, State) ->
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, "destroy", "json"),
    process_response(Res, {State#pengine_state.callback_module}),
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
-spec handle_cast(term(), #pengine_state{}) -> {noreply, #pengine_state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
-spec handle_info(timeout | term(), #pengine_state{}) -> {noreply, #pengine_state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Cleanup function
-spec terminate(normal | shutdown | {shutdown,term()}, #pengine_state{}) -> ok.
terminate(Reason, State) ->
    lager:info("pengine terminating, reason: ~p, state: ~p", [Reason, State]),
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
-spec code_change(term | {down, term()}, #pengine_state{}, term()) -> {ok, #pengine_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc
%% @private
%% process response to sent request
-spec process_response(map(), tuple()) -> {ok, any()}|
                                          {error, any()}.
process_response(#{<<"event">> := <<"create">>, <<"id">> := Id, <<"slave_limit">> := SlaveLimit}, {TableId, CallBackModule, Server})->
    {size, Size} = lists:keyfind(size, 1, ets:info(TableId)),
    lager:info("Attempting to create pengine, max_slaves: ~p , active pengines: ~p", [SlaveLimit, Size]),
    if 
        SlaveLimit > Size ->
            {ok, Pid} = supervisor:start_child(pengine_sup, [[Id, Server, CallBackModule]]),
            call_callback(CallBackModule, oncreate, [Id]),
            ets:insert(TableId, {Id}),
            {ok, {Pid, Id}};
        true -> 
            lager:info("Attempt to create too many pengines. The limit is: ~p ~n", [SlaveLimit]),
            Reason = "Attemt to create too many pengines. The limit is: " ++ [SlaveLimit] ++ "\n",
            call_callback(CallBackModule, onerror, [Id, Reason]),
            {ok, Res} = pengine_pltp_http:send(Id, Server, "destroy", "json"),
            process_response(Res, {CallBackModule}),
            {error, Reason}
    end;

process_response(#{<<"event">> := <<"stop">>, <<"id">> := Id}, {CallBackModule})->
    call_callback(CallBackModule, onstop, [Id]),
    {ok, {}};


process_response(#{<<"event">> := <<"failure">>, <<"id">> := Id}, {CallBackModule})->
    call_callback(CallBackModule, onfailure, [Id]),
    {ok, {}};

process_response(#{<<"event">> := <<"prompt">>, <<"id">> := Id, <<"data">> := Data}, {CallBackModule})->
    call_callback(CallBackModule, onprompt, [Id, Data]),
    {ok, {}};

process_response(#{<<"event">> := <<"error">>, <<"id">> := Id, <<"code">> := <<"existence_error">>, <<"data">> := Data}, {CallBackModule})->
    call_callback(CallBackModule, onerror, [Id, Data]),
    gen_server:stop(syn:find_by_key(Id)),
    {ok, {}};

process_response(#{<<"event">> := <<"error">>, <<"id">> := Id, <<"data">> := Data}, {CallBackModule})->
    call_callback(CallBackModule, onerror, [Id, Data]),

    {ok, {}};

process_response(#{<<"event">> := <<"success">>, <<"id">> := Id, <<"data">> := Data, <<"more">> := More}, {CallBackModule})->
    call_callback(CallBackModule, onsuccess, [Id, Data, More]),
    {ok, {}};

process_response(#{<<"event">> := <<"output">>, <<"id">> := Id, <<"data">> := Data}, {CallBackModule, Server, Format})->
    call_callback(CallBackModule, onoutput, [Id, Data]),
    Res = pengine_pltp_http:pull_response(Id, Server, Format),
    {ok, {Res}};

process_response(#{<<"event">> := <<"debug">>, <<"id">> := Id, <<"data">> := Data}, {CallBackModule})->
    call_callback(CallBackModule, ondebug, [Id, Data]),
    {ok, {}};

process_response(#{<<"event">> := <<"ping">>, <<"id">> := Id, <<"data">> := Data}, {CallBackModule})->
    call_callback(CallBackModule, onping, [Id, Data]),
    {ok, {}};

process_response(#{<<"event">> := <<"abort">>, <<"id">> := Id}, {CallBackModule})->
    call_callback(CallBackModule, onabort, [Id]),
    {ok, {}};

process_response(#{<<"event">> := <<"destroy">>, <<"id">> := Id}, {CallBackModule})->
    call_callback(CallBackModule, ondestroy, [Id]),
    gen_server:stop(syn:find_by_key(Id)),
    {ok, {}};

process_response(#{<<"event">> := <<"died">>, <<"id">> := Id, <<"data">> := Data}, {CallBackModule})->
    call_callback(CallBackModule, onerror, [Id, Data]),
    gen_server:stop(syn:find_by_key(Id)),
    {ok, {}}.


%% @doc
%% @private
%% Turn a erlang map into a Prolog option list. 
%% The option values must be valid Prolog syntax.
-spec options_to_list(map()) -> string().
options_to_list(Options)->
    Opts = maps:fold(
             fun(K,V,Opts = [$[]) -> Opts ++ atom_to_list(K) ++ "(" ++ V ++ ")";
                (K,V,Opts) -> Opts ++ "," ++ atom_to_list(K) ++ "(" ++ V ++ ")"
             end, "[", Options),
    Opts ++ "]".

