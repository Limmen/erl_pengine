%%%-------------------------------------------------------------------
%% @author Kim Hammar <kimham@kth.se>
%% @copyright (C) 2017, Kim Hammar
%% @doc pengine_plptp_http library.
%% Erlang implementation of Prolog Transport Protocol (PLTP) 
%% over HTTP.
%% @end
%%%-------------------------------------------------------------------
-module(pengine_pltp_http).
-author('Kim Hammar <kimham@kth.se>').

%% API
-export([ping/1, pull_response/0, create/2, send/3, send/4, abort/3]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Ping the status of the pengine.
ping(_Interval) ->
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
-spec send(string(), string(), string()) -> ok.
send(Id, Server, Event) ->
    send(Id, Server, Event, "json").

-spec send(string(), string(), string(), string()) -> {ok, map()} |
                                                      {error, any()}.
send(Id, Server, Event, Format) ->
    URL = list_to_binary(Server ++ "/send?format=" ++ Format ++ "&id=" ++ Id),
    Data = list_to_binary(Event ++ ".\n"),
    lager:info("sending event ~p to pengine: ~p", [Event, Id]),
    case hackney:post(URL, [prolog_content_type()], Data, []) of
        {ok, _StatusCode, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, jsx:decode(Body, [return_maps])};
        {error, Reason} ->
            lager:error("send request failed, reason: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc
%% Creates the pengine with the given options
%% returns id (string) of the created pengine
-spec create(string(), pengine:pengine_create_options()) -> {ok, map()} |
                                                            {error, any()}.
create(Server, Options) ->
    URL = list_to_binary(Server ++ "/create"),
    lager:info("sending create pengine request to: ~p, options: ~p", [URL, options_to_json(Options)]),
    case hackney:post(URL, [json_content_type(), json_accept_header()], options_to_json(Options), []) of
        {ok, _StatusCode, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, jsx:decode(Body, [return_maps])};
        {error, Reason} ->
            lager:error("create request failed, reason: ~p", [Reason]),
            {error, Reason}
    end.

%% @doc
%% Aborts pengine with the given id
-spec abort(binary(), string(), string()) -> {ok, map()} |
                                             {error, any()}.
abort(Id, Server, Format) ->
    URL = list_to_binary(Server ++ "/abort"),
    Options = #{id => Id,  format => Format},
    lager:info("sending abort pengine request to: ~p, options: ~p", [URL, options_to_json(Options)]),
    case hackney:post(URL, [json_content_type(), json_accept_header()], options_to_json(Options), []) of
        {ok, _StatusCode, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, jsx:decode(Body, [return_maps])};
        {error, Reason} ->
            lager:error("abort request failed, reason: ~p", [Reason]),
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc
%% @private
%% get content type for prolog over pltp_http
-spec prolog_content_type() -> {binary(), binary()}.
prolog_content_type() ->
    {<<"Content-Type">>, <<"application/x-prolog; charset=utf-8">>}.

%% @doc
%% @private
%% get content type for json over pltp_http
-spec json_content_type() -> {binary(), binary()}.
json_content_type() ->
    {<<"Content-Type">>, <<"application/json; charset=utf-8">>}.

%% @doc
%% @private
%% get json accept header for http
-spec json_accept_header() -> {binary(), binary()}.
json_accept_header()->
    {<<"Accept">>, <<"application/json">>}.

%% @doc
%% @private
%% convert pengine-options to json
-spec options_to_json(map()) -> binary().
options_to_json(Options)->
    jsx:encode(Options).
