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
-export([start_link/1, id/1, ask/3, next/1, respond/2,
         destroy/1, process_response/3, ping/2]).

-export_type([response/0, abort_response/0, stop_response/0, create_response/0]).

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

%% state
-type pengine_state()::#pengine_state{}.

%% query_options to the ask() function.
-type query_options():: #{
                     template := string(),
                     chunk := integer()
                    }.

%% response from some request to pengine
-type response():: create_response() |
                   ask_response() |
                   destroy_response() |
                   ping_response() |
                   died_response() |
                   abort_response() |
                   stop_response() |
                   error_response() |
                   prompt_response().

%% response to a create request
-type create_response()::{{ok, {PengineProcess :: pid(), Id :: binary()}}, {no_create_query}} |
                         {{ok, {PengineProcess :: pengine_destroyed, Id :: binary()}}, {no_create_query}} |
                         {{ok, {PengineProcess :: pid(), Id :: binary()}}, {create_query, query_response()}} |
                         {{ok, {PengineProcess :: pengine_destroyed, Id :: binary()}}, {create_query, query_response()}} |
                         {{error, {max_limit, Reason :: any()}}, destroy_response()}.

%% response to a ask-request
-type ask_response():: {query_response(), destroy_response()} |
                       query_response() |
                       {output_response(), destroy_response()} |
                       output_response() |
                       died_response().

%% response to a query
-type query_response()::{failure, Id :: binary()} |
                        {success, Id :: binary(), Data :: list(), More :: boolean()}.

%% response if pengine to send request to was destroyed after the request
-type destroy_response()::{pengine_destroyed, Reason :: any()}.

%% response for ping request
-type ping_response():: {ping_response, Id :: binary(), Data :: map()} |
                        {ping_interval_set, Interval :: integer()} |
                        died_response().

%% response if pengine to send request to was dead before request could be handled
-type died_response()::{pengine_died, Reason :: any()}.

%% response to abort-request
-type abort_response()::{aborted, Id :: binary()} |
                        {{aborted, Id :: binary()}, destroy_response()} |
                        died_response().

%% response to stop-request
-type stop_response()::{stopped, Id :: binary()} |
                       died_response().

%% response when pengine signaling that some error occurred
-type error_response()::{error, Id :: binary(), Reason :: binary(), Code :: binary()}.

%% response to a pengine-prompt
-type prompt_response()::{prompt, Id :: binary(), Data :: binary()}.

%% response to pengine-output
-type output_response()::{{output, PrologOutput :: any()}, {pull_response, ask_response()}}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Starts the server
-spec start_link(list()) -> {ok, pid()}.
start_link(Args) ->
    lager:debug("starting pengine"),
    gen_server:start_link(?MODULE, Args, []).

%% @doc
%% Returns the id of the pengine (a string).
%% Note that the pengine must have been created before this field will have a
%% non-null value, i.e. the oncreate handler must have been called.
-spec id(pid()) -> Id :: binary().
id(Pengine)->
    lager:debug("querying the pengine for its id"),
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
-spec ask(pid(), string(), query_options()) -> ask_response() | error_response().
ask(Pengine, Query, Options) ->
    lager:debug("sending a query ~p to the pengine", [Query]),
    gen_server:call(Pengine, {ask, Query, Options}, infinity).

%% @doc
%% Triggers a search for the next solution.
-spec next(pid()) -> ask_response() | error_response().
next(Pengine) ->
    lager:debug("asking the pengine for next solution"),
    gen_server:call(Pengine, {next}, infinity).

%% @doc
%% Inputs a term in response to a prompt from an invocation of pengine_input/2
%% that is now waiting to receive data from the outside.
%% Throws an error if string cannot be parsed as a Prolog term or if object cannot be serialised into JSON.
-spec respond(pid(), list()) -> ask_response() | error_response().
respond(Pengine, PrologTerm) ->
    lager:debug("responds to pengine_input with ~p", [PrologTerm]),
    gen_server:call(Pengine, {respond, PrologTerm}, infinity).

%% @doc
%% Destroys the pengine.
-spec destroy(pid()) -> destroy_response() | error_response().
destroy(Pengine) ->
    lager:debug("destroying the pengine"),
    gen_server:call(Pengine, {destroy}, infinity).

%% @doc
%% Sends a ping request to the pengine
%% If Interval = 0, send a single ping.
%% If Interval > 0, set/change periodic ping event, if 0, clear periodic interval
-spec ping(pid(), integer()) -> ping_response() | error_response().
ping(Pengine, Interval) ->
    lager:debug("pinging the pengine"),
    gen_server:call(Pengine, {ping, Interval}, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server, creates the pengine.
-spec init(list()) -> {ok, pengine_state()}.
init([Id, Server]) ->
    lager:debug("Initializing pengine"),
    syn:register(Id, self()),
    {ok, #pengine_state{id = Id, server = Server}}.

%% @private
%% @doc
%% Handling call messages
-spec handle_call(term(), term(), pengine_state()) -> {reply, any(), pengine_state()}.
handle_call({id}, _From, State) ->
    Reply = State#pengine_state.id,
    {reply, Reply, State};

handle_call({ask, Query, Options}, _From, State) ->
    Send = "ask((" ++ Query ++ "), " ++ options_to_list(Options) ++ ")",
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, Send, "json"),
    lager:debug("Ask response: ~p", [Res]),
    process_response(Res, State, {});

handle_call({next}, _From, State) ->
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, "next", "json"),
    lager:debug("next response: ~p", [Res]),
    process_response(Res, State, {});

handle_call({respond, PrologTerm}, _From, State) ->
    Send = "input((" ++ PrologTerm ++ "))",
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, Send, "json"),
    process_response(Res, State, {});

handle_call({destroy}, _From, State) ->
    {ok, Res} = pengine_pltp_http:send(State#pengine_state.id, State#pengine_state.server, "destroy", "json"),
    process_response(Res, State, {});

handle_call({ping, 0}, _From, State) ->
    erlang:cancel_timer(State#pengine_state.ping_timer),
    {ok, Res} = pengine_pltp_http:ping(State#pengine_state.id, State#pengine_state.server, "json"),
    process_response(Res, State, {});

handle_call({ping, Interval}, _From, State) when Interval > 0 ->
    erlang:cancel_timer(State#pengine_state.ping_timer),
    Timer = erlang:send_after(Interval*1000, self(), {ping_timeout, Interval}),
    {reply, {ping_interval_set, Interval}, State#pengine_state{ping_timer = Timer}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% @doc
%% Handling cast messages
-spec handle_cast(term(), pengine_state()) -> {noreply, pengine_state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Handling all non call/cast messages
-spec handle_info(timeout | term(), pengine_state()) -> {noreply, pengine_state()}.
handle_info({ping_timeout, Interval}, State) ->
    {ok, Res} = pengine_pltp_http:ping(State#pengine_state.id, State#pengine_state.server, "json"),
    case ping_verdict(Res) of
        true ->
            Status = maps:get(<<"status">>, maps:get(<<"data">>, Res)),
            lager:debug("ping timeout, pengine answered ping, status: ~p", [Status]),
            erlang:cancel_timer(State#pengine_state.ping_timer),
            Timer = erlang:send_after(Interval*1000, self(), {ping_timeout, Interval}),
            {noreply, State#pengine_state{ping_timer = Timer}};
        false ->
            erlang:cancel_timer(State#pengine_state.ping_timer),
            lager:debug("Bad ping response from pengine: ~p", [Res]),
            {stop, normal, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc
%% Cleanup function
-spec terminate(normal | shutdown | {shutdown, term()}, pengine_state()) -> ok.
terminate(Reason, State) ->
    lager:debug("pengine terminating, reason: ~p, state: ~p", [Reason, State]),
    ok.

%% @private
%% @doc
%% Convert process state when code is changed
-spec code_change(term | {down, term()}, pengine_state(), term()) -> {ok, pengine_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc
%% @private
%% process response to sent request
-spec process_response(map(), pengine_state() | pengine_master:master_state(), tuple()) ->
                              {reply, response(), pengine_state()} |
                              {stop, Reason::any(), response(), pengine_state()} |
                              {reply, response(), pengine_master:master_state()} |
                              {stop, Reason::any(), response(), pengine_master:master_state()}.

process_response(Res = #{<<"event">> := <<"create">>, <<"id">> := Id, <<"slave_limit">> := SlaveLimit},
                 State, {create, TableId, Server})->
    {size, Size} = lists:keyfind(size, 1, ets:info(TableId)),
    lager:debug("Attempting to create pengine, max_slaves: ~p , active pengines: ~p", [SlaveLimit, Size]),
    case SlaveLimit > Size of
        true ->
            case get_create_query(Res, State) of
                {nostop, Reply2, State1} ->
                    {ok, Pid} = supervisor:start_child(pengine_sup, [[Id, Server]]),
                    ets:insert(TableId, {Id}),
                    Reply1 = {ok, {Pid, Id}},
                    {reply, {Reply1, Reply2}, State1};
                {stop, Reply2, State1} ->
                    Reply1 = {ok, {pengine_destroyed, Id}},
                    {reply, {Reply1, Reply2}, State1}
            end;
        false ->
            lager:debug("Attempt to create too many pengines. The limit is: ~p ~n", [SlaveLimit]),
            Reason = "Attempt to create too many pengines. The limit is: " ++ [SlaveLimit] ++ "\n",
            {ok, DestroyRes} = pengine_pltp_http:send(Id, Server, "destroy", "json"),
            Reply1 = {error, {max_limit, Reason}},
            {stop, _, Reply2, State1} = process_response(DestroyRes, State, {}),
            {reply, {Reply1, Reply2}, State1}
    end;


process_response(#{<<"event">> := <<"stop">>, <<"id">> := Id}, State, _)->
    lager:debug("process response: stop"),
    Reply = {stopped, Id},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"failure">>, <<"id">> := Id}, State, _)->
    lager:debug("process response: failure"),
    Reply = {failure, Id},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"prompt">>, <<"id">> := Id, <<"data">> := Data}, State, _)->
    lager:debug("process response: prompt"),
    Reply = {prompt, Id, Data},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"error">>, <<"id">> := Id, <<"code">> := <<"existence_error">>,
                   <<"data">> := Data}, State, _)->
    Reply = {error, Id, Data, <<"existence_error">>},
    Reason = normal,
    {stop, Reason, Reply, State};

process_response(#{<<"event">> := <<"error">>, <<"id">> := Id, <<"code">> := Code, <<"data">> := Data}, State, _)->
    lager:debug("process response: error"),
    Reply = {error, Id, Data, Code},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"success">>, <<"id">> := Id, <<"data">> := Data, <<"more">> := More}, State, _)->
    lager:debug("process response: success"),
    Reply = {success, Id, Data, More},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"output">>, <<"id">> := Id, <<"data">> := Output}, State, _)->
    lager:debug("process response: output"),
    {ok, Res} = pengine_pltp_http:pull_response(Id, State#pengine_state.server, "json"),
    lager:debug("Pull response returned: ~p", [Res]),
    PullResponse = process_response(Res, State, {}),
    case PullResponse of
        {stop, Reason, Reply, State1} ->
            Reply1 = {{output, Output}, {pull_response, Reply}},
            {stop, Reason, Reply1, State1};
        {reply, Reply, State1} ->
            Reply1 = {{output, Output}, {pull_response, Reply}},
            {reply, Reply1, State1}
    end;

process_response(#{<<"event">> := <<"ping">>, <<"id">> := Id, <<"data">> := Data}, State, _)->
    lager:debug("process response: ping"),
    Reply = {ping_response, Id, Data},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"abort">>, <<"id">> := Id}, State, _)->
    lager:debug("process response: abort"),
    Reply = {aborted, Id},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"destroy">>, <<"id">> := Id, <<"data">> := Data}, State, _)->
    lager:debug("process response: destroy"),
    {reply, Reply1, State1} = process_response(Data, State, {}),
    Reply2 = {pengine_destroyed, "Pengine slave: " ++ [Id] ++ " was destroyed"},
    lager:debug("Returning reply: ~p", [{Reply1, Reply2}]),
    Reason = normal,
    {stop, Reason, {Reply1, Reply2}, State1};

process_response(#{<<"event">> := <<"destroy">>, <<"id">> := Id}, State, _)->
    lager:debug("process response: destroy"),
    Reason = normal,
    Reply = {pengine_destroyed, "Pengine slave: " ++ [Id] ++ " was destroyed"},
    {stop, Reason, Reply, State};

process_response(#{<<"event">> := <<"died">>, <<"id">> := Id, <<"data">> := Data}, State, _)->
    lager:debug("process response: died"),
    Reply = {pengine_died, {"Pengine slave: " ++ [Id] ++ " is dead", Data}},
    {reply, Reply, State};

process_response(#{<<"event">> := <<"died">>, <<"id">> := Id}, State, _)->
    lager:debug("process response: died"),
    Reply = {pengine_died, {"Pengine slave: " ++ [Id] ++ " is dead"}},
    {reply, Reply, State}.


%% @doc
%% @private
%% Turn a erlang map into a Prolog option list.
%% The option values must be valid Prolog syntax.
-spec options_to_list(map()) -> string().
options_to_list(Options)->
    Opts = maps:fold(
             fun(K, V, Opts = [$[]) -> Opts ++ atom_to_list(K) ++ "(" ++ V ++ ")";
                (K, V, Opts) -> Opts ++ "," ++ atom_to_list(K) ++ "(" ++ V ++ ")"
             end, "[", Options),
    Opts ++ "]".

%% @doc
%% @private
%% extract create-query response
-spec get_create_query(CreateResponse :: map(), pengine_state())->
                              {{create_query, response()}, pengine_state()} |
                              {{no_create_query}, pengine_state()}.
get_create_query(#{<<"answer">> := QueryRes}, State) ->
    case process_response(QueryRes, State, {}) of
        {reply, Reply, State1} ->
            {nostop, {create_query, Reply}, State1};
        {stop, _Reason, Reply, State1} -> 
            {stop, {create_query, Reply}, State1}
    end;

get_create_query(_, State) ->
    {nostop, {no_create_query}, State}.

%% @doc
%% @private
%% utility function that checks based on ping if pengine should be considered dead.
-spec ping_verdict(map()) -> boolean().
ping_verdict(#{<<"event">> := <<"ping">>})->
    true;
ping_verdict(#{<<"event">> := D}) when D =/= <<"ping">> ->
    false.
