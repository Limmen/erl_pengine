%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc pengine_plptp_http library.
%% Erlang implementation of Prolog Transport Protocol (PLTP) over HTTP.
%% @end
%%%-------------------------------------------------------------------
-module(pengine_pltp_http).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([ping/1, pull_response/0, send/1, create/3]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Ping the status of the pengine.
ping(Interval) ->
    ok.

%% @doc
%% Process the reply to a pull_response.  If the last answer was
%% final, this question will be asked to a death pengine.  We do not
%% consider this an error.
pull_response() ->
    ok.

%% @doc
%% Invoke /pengine/send.  This method takes three parameters: the
%% reply format (one of prolog or json), the pengine id (a UUID)
%% and the event (a serialized Prolog term).  In old versions, this
%% was sent as a GET request.  This is undesirable, both because GET
%% assumes a side-effect-free operation and because of the size limit
%% to URLs imposed by some infrastructure.  The current version passes
%% the format and id as URL parameters and the content as a POST body.
%% Future versions might use the HTTP Accept header intead of format
%% and add the id to the URL, i.e., using /pengine/send/ID
send(Event) ->
    ok.

%% @doc
%% Creates the pengine with the given options
%% returns id (string) of the created pengine
-spec create(string(), atom(), pengine:pengine_create_options()) -> string().
create(Server, _CallBackModule, Options) ->
    URL = list_to_binary(Server ++ "/create"),
    lager:debug("sending create pengine request to: ~p, options: ~p", [URL, options_to_json(Options)]),
    case hackney:post(URL, [prolog_content_type(), json_accept_header()], options_to_json(Options), []) of
        Res = {ok, StatusCode, Headers, ClientRef} ->
            lager:debug("received hackney response: ~p", [Res]),
            {ok, Body} = hackney:body(ClientRef),
            lager:debug("received hackney response-body:: ~p", [Body]),
            ok;
        {error, Reason} ->
            lager:error("create request failed, reason: ~p", [Reason])
    end.



%%====================================================================
%% Internal functions
%%====================================================================

-spec prolog_content_type() -> {binary(), binary()}.
prolog_content_type() ->
    {<<"Content-Type">>, <<"application/json; charset=utf-8">>}.

json_accept_header()->
    {<<"Accept">>, <<"application/json">>}.

-spec options_to_json(map()) -> binary().
options_to_json(Options)->
    jsx:encode(Options).

call_callback(CallBackMod, CallBackFunc, Args)->
    try apply(CallBackMod, CallBackFunc, Args) of
        Result ->
            Result
    catch
        _:_ ->
            no_match
    end.



%%% {ok, StatusCode, Headers, ClientRef} = hackney:post("http://127.0.0.1:4000/pengine/create", [], <<>>, []).
