%% server.pl
%% swi-prolog
%% compile: ['server.pl']
%%
%% Http+Pengine server hosting static files and pengine safe-predicates
%%
%% Author Kim Hammar limmen@github.com <kimham@kth.se>

%%%===================================================================
%%% Facts
%%%===================================================================

:- module(pengine_server,
          [
           server/1,
           stop/1
          ]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pengines)).
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(pengine_sandbox:library(lists)).
:- use_module(library(sandbox)).
:- multifile sandbox:safe_primitive/1.

%% Safe predicates accessible from remote client
sandbox:safe_primitive(lists:member(_,_)).

%%%===================================================================
%%% API
%%%===================================================================

%% Start http/pengine server on Port
%% server(+).
%% server(Port).
server(Port):-
    http_server(http_dispatch, [port(Port)]).

%% Stop http/pengine server on Port
%% stop(+).
%% stop(Port).
stop(Port):-
    http_stop_server(Port, []).