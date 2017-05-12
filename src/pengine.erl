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
-export_type([pengine_create_options/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% macros
-define(SERVER, ?MODULE).

%% types

%% query_options to the ask() function.
-type query_options():: #{
                     template := string(),
                     chunk := integer()
                    }.

%% server and callback_module are mandatory arguments, rest are optional.
-type pengine_create_options():: #{
                              application => string(),
                              ask => string(),
                              template => string(),
                              chunk => integer(),
                              destroy => boolean(),
                              format => string()
                             }.

%% records

%% state of the pengine, see http://pengines.swi-prolog.org/docs/documentation.html for documentation.
-record(state, {
          server :: string(),
          srctext :: string(),
          srcurl :: string(),
          callback_module :: atom(),
          pengine_create_options = #{application => "pengine_sandbox", chunk => 1, destroy => true, format => json} :: pengine_create_options(),
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

%% @doc
%% Triggers a search for the next solution.
next() ->
    ok.

%% @doc
%% Stops searching for solutions. Terminates the running query gracefully.
stop() ->
    ok.

%% @doc
%% Inputs a term in response to a prompt from an invocation of pengine_input/2
%% that is now waiting to receive data from the outside. 
%% Throws an error if string cannot be parsed as a Prolog term or if object cannot be serialised into JSON.
respond(PrologTerm) ->
    ok.

%% @doc
%% Terminates the running query by force.
abort() ->
    ok.

%% @doc
%% Destroys the pengine.
destroy() ->
    ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% @doc
%% Initializes the server
init([Server, CallBackModule, PengineOptions]) ->
    State = #state{server = Server, callback_module = CallBackModule},
    State1 = maps:fold(fun(K,V, S) -> OldMap = S#state.pengine_create_options, S#state{pengine_create_options = OldMap#{K => V}} end, State, PengineOptions),
    {ok, State1}.

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

