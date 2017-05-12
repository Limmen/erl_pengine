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
-export([ping/1, pull_response/0, send/1, create/1]).

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

create(Options) ->
    ok.



%%====================================================================
%% Internal functions
%%====================================================================
