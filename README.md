# erlang_pengine

## Description

**ErlangPengine**

Erlang client to prolog pengine server.
For more information about the pengine project see the following links.

* [http://pengines.swi-prolog.org/docs/documentation.html](http://pengines.swi-prolog.org/docs/documentation.html)
* [http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27)](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27))

## Installation

```bash
# build
$ ./rebar3 compile

```
## Usage

```erlang
{ok, {P1, Id1}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{}).

{ok, {P2, Id2}} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{}).

Pengines = pengine_master:list_pengines().

pengine:next(P1).

5> {ok, _StatusCode, _Headers, ClientRef} = hackney:post(<<"http://127.0.0.1:4000/pengine/send?format=json&id=38612376351347808823784683757103067622">>, [{<<"Content-Type">>, <<"application/x-prolog; charset=utf-8">>}], <<"destroy.\n">>, []).
{ok,200,
    [{<<"Date">>,<<"Thu, 18 May 2017 09:43:45 GMT">>},
     {<<"Connection">>,<<"Keep-Alive">>},
     {<<"Cache-Control">>,
      <<"no-cache, no-store, must-revalidate">>},
     {<<"Pragma">>,<<"no-cache">>},
     {<<"Expires">>,<<"0">>},
     {<<"Content-Type">>,<<"application/json; charset=UTF-8">>},
     {<<"Content-Length">>,<<"66">>}],
    #Ref<0.0.1.7561>}
6> hackney:body(ClientRef).
{ok,<<"{\"event\":\"destroy\", \"id\":\"38612376351347808823784683757103067622\"}">>}
7>

{ok, StatusCode, Headers, ClientRef} = hackney:get(<<"http://127.0.0.1:4000/pengine/ping">>, [{<<"Content-Type">>, <<"application/json; charset=utf-8">>}, {<<"Accept">>, <<"application/json">>}], jsx:encode(#{id => Id1, format => "json"}), []).

{ok, StatusCode, Headers, ClientRef} = hackney:post(<<"http://127.0.0.1:4000/pengine/create">>, [{<<"Content-Type">>, <<"application/json; charset=utf-8">>}, {<<"Accept">>, <<"application/json">>}], jsx:encode(#{application => "pengine_sandbox", chunk => 1, destroy => true, format => json}), []).


7> {ok, StatusCode1, Headers1, ClientRef1} = hackney:get(<<"http://127.0.0.1:4000/pengine/ping?id=27aacaba-42fd-4eb6-9157-ae5229cc7ae4">>, [{<<"Content-Type">>, <<"application/json; charset=utf-8">>}, {<<"Accept">>, <<"application/json">>}], <<>>, []).
{ok,200,
    [{<<"Date">>,<<"Sun, 21 May 2017 09:50:04 GMT">>},
     {<<"Content-Type">>,<<"text/x-prolog; charset=UTF-8">>},
     {<<"Connection">>,<<"Keep-Alive">>},
     {<<"Content-Length">>,<<"428">>}],
    #Ref<0.0.1.643>}
8> {ok, Body} = hackney:body(ClientRef1).
{ok,<<"ping('27aacaba-42fd-4eb6-9157-ae5229cc7ae4',thread{id:12,stacks:stacks{global:stack{allocated:61424,limit:26"...>>}
9> Body.
<<"ping('27aacaba-42fd-4eb6-9157-ae5229cc7ae4',thread{id:12,stacks:stacks{global:stack{allocated:61424,limit:268435456,"...>>
10> io:format("~p", [Body]).
<<"ping('27aacaba-42fd-4eb6-9157-ae5229cc7ae4',thread{id:12,stacks:stacks{global:stack{allocated:61424,limit:268435456,name:global,usage:2224},local:stack{allocated:28672,limit:268435456,name:local,usage:1408},total:stack{allocated:120808,limit:805306368,name:stacks,usage:4296},trail:stack{allocated:30712,limit:268435456,name:trail,usage:664}},status:running,time:time{cpu:0.023156854,epoch:1495360028.0125349,inferences:199}}).\n">>ok

```

```bash

curl --data "event=destroy.\n" -H "Content-Type: application/x-prolog" "http://localhost:4000/pengine/send?format=json&id='159039462391028399231340666254676513087'"

curl --data "event=destroy.\n" -H "Content-Type: application/x-prolog" "http://localhost:4000/pengine/send?format=json&id=26520944056946901811031529588524459285"

curl --data "id=&format=json" -H "Content-Type: application/json" "http://localhost:4000/pengine/ping"

curl http://localhost:4000/pengine/ping -d '{"id" : , "format": "json"}'

curl http://localhost:4000/pengine/create


curl http://localhost:4000/pengine/create


kim@limmen ~> curl "http://localhost:4000/pengine/ping?id=2ab11b38-21fb-4a8b-bc2d-ca78bcc78f4f"
ping('2ab11b38-21fb-4a8b-bc2d-ca78bcc78f4f',thread{id:12,stacks:stacks{global:stack{allocated:61424,limit:268435456,name:global,usage:2184},local:stack{allocated:28672,limit:268435456,name:local,usage:1408},total:stack{allocated:120808,limit:805306368,name:stacks,usage:4256},trail:stack{allocated:30712,limit:268435456,name:trail,usage:664}},status:running,time:time{cpu:0.037169991,epoch:1495358455.875498,inferences:197}}).
kim@limmen ~> curl "http://localhost:4000/pengine/ping?id=2ab11b38-21fb-4a8b-bc2d-ca78bcc78f"
died('2ab11b38-21fb-4a8b-bc2d-ca78bcc78f').
kim@limmen ~> curl "http://localhost:4000/pengine/ping?id=2ab11b38-21fb-4a8b-bc2d-ca78bcc78f4f"
ping('2ab11b38-21fb-4a8b-bc2d-ca78bcc78f4f',thread{id:12,stacks:stacks{global:stack{allocated:61424,limit:268435456,name:global,usage:2184},local:stack{allocated:28672,limit:268435456,name:local,usage:1408},total:stack{allocated:120808,limit:805306368,name:stacks,usage:4256},trail:stack{allocated:30712,limit:268435456,name:trail,usage:664}},status:running,time:time{cpu:0.038939034,epoch:1495358455.875498,inferences:197}}).

curl -H "Accept: application/json" -H "Content-Type: application/json" "http://localhost:4000/pengine/ping?id=f04494dc-2ef7-4dd5-96e2-dfcb33dcdfcc"

```

## Project commands
```bash
# build
$ ./rebar3 compile

# remove temporary files
$ ./rebar3 clean

# run tests
$ ./rebar3 alias testall

# validate codebase, runs: tests, linters, static code analysis
$ ./rebar3 alias validate

# Generate documentation with edoc
$ ./rebar3 edoc

# Start shell with application loaded
$ ./rebar3 shell

# Start shell with configuration
$ rebar3 shell --config erlang_pengine.config

# Start shell with application loaded and listen for code changes
$ ./rebar3 auto

# Run release
$ ./rebar3 run

```

## Author & Maintainer

Kim Hammar <kimham@kth.se>

## Copyright and license

[LICENSE](LICENSE.md)

MIT

(C) 2017, Kim Hammar