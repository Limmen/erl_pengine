%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc erlang_pengine_server server
%% @end
%%%-------------------------------------------------------------------
-module(pengine).
-author('Kim Hammar <kimham@kth.se>').

-behaviour(gen_server).

%% API
-export([start_link/0, id/0, ask/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% macros
-define(SERVER, ?MODULE).

%% types

%% supported formats
-type format():: json.

%% query_options to the ask() function.
-type query_options():: #{
                     template := string(),
                     chunk := integer()
                    }.

%% records

%% state of the pengine, see http://pengines.swi-prolog.org/docs/documentation.html for documentation.
-record(state, {
          server :: string(),
          application = "pengine_sandbox" :: string(),
          ask :: string(),
          template :: string(),
          chunk = 1 :: integer(),
          destroy = true :: boolean(),
          srctext :: string(),
          srcurl :: string(),
          format = json :: format(),
          callback_module :: atom(),
          id :: string()
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
%% Returns the id of the pengine (a string). 
%% Note that the pengine must have been created before this field will have a 
%% non-null value, i.e. the oncreate handler must have been called.
-spec id() -> integer().
id()->
    ok.

%% @doc
%% Runs query in search for the first solution. 
%% Throws an error if the query is syntactically or semantically malformed 
%% or if running it could compromise the safety of the server. 
%% options is a Erlang map contaning zero or more of:
%% template :: string() : 
%% A prolog variable (or a term containing problog variables) shared with the query.
%% chunk :: integer() :
%% The maximum number of solutions to retrieve in one chunk. 1 means no chunking (default).
-spec ask(string(), query_options()) -> atom().
ask(Query, Options) ->
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server
-spec init(list(erlang_pengine_app:pengine_options())) -> {ok, #state{}}.
init([Options]) ->
    State = maps:fold(fun(K, V, Acc) -> set_state(K, V, Acc) end, Options),
    {ok, State}.

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
%% Cleanup function
-spec terminate(normal | shutdown | {shutdown,term()}, #state{}) -> ok.
terminate(Reason, State) ->
    lager:info("pengine terminating, reason: ~p, state: ~p", [Reason, State]),
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

set_state(server, V, State) ->
    State#state{server = V};

set_state(application, V, State) ->
    State#state{server = V};

set_state(ask, V, State) ->
    State#state{ask = V};

set_state(template, V, State) ->
    State#state{template = V};

set_state(chunk, V, State) ->
    State#state{chunk = V};

set_state(destroy, V, State) ->
    State#state{destroy = V};

set_state(srctext, V, State) ->
    State#state{srctext = V};

set_state(srcurl, V, State) ->
    State#state{srcurl = V};

set_state(format, V, State) ->
    State#state{format = V};

set_state(callback_module, V, State) ->
    State#state{callback_module = V}.
