# semweb

An example of using `erl_pengine` to query prolog pengine for rdf triples. 

Build and compile

```bash
$ rebar3 compile
```

Open up an erlang shell with the project loaded using `rebar3 shell`

```erlang
$ rebar3 shell
===> Verifying dependencies...
===> Compiling semweb
Eshell V8.3  (abort with ^G)
1> 
```

Start the prolog server somewhere if it isn't already started. 

Use a command similar to this to start it from the erlang shell:
 
```
os:cmd("current_dir=$PWD; cd /home/kim/workspace/erlang/erl_pengine/examples/semweb/prolog; prolog daemon.pl --http=4000 --pidfile=/home/kim/workspace/erlang/erl_pengine/examples/semweb/prolog/pid/http.pid; cd $current_dir").
```

Start the `erl_pengine` application, since this example project is an OTP library we can do it manually 
(in case of an OTP application you would have it started automatically as a dependency):
 
 ```erlang
 application:ensure_all_started(erl_pengine). 
 ```
 Call the single function exported by `semweb` API `supervises/2`:
 
 ```erlang
 %% supervises will create pengine and query it for supervises(X,Y).
2> semweb:supervises("X", "Y").
[{<<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine_sup">>,
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine">>},
 {<<"http://www.limmen.kth.se/ontologies/erl_pengine#erl_pengine_sup">>,
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#table_mngr">>},
 {<<"http://www.limmen.kth.se/ontologies/erl_pengine#erl_pengine_sup">>,
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine_sup">>},
 {<<"http://www.limmen.kth.se/ontologies/erl_pengine#erl_pengine_sup">>,
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine_master">>}]
  
3> semweb:supervises("'http://www.limmen.kth.se/ontologies/erl_pengine#pengine_sup'", "Y").
[{"'http://www.limmen.kth.se/ontologies/erl_pengine#pengine_sup'",
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine">>}]
  
4> semweb:supervises("'http://www.limmen.kth.se/ontologies/erl_pengine#erl_pengine_sup'", "Y").
[{"'http://www.limmen.kth.se/ontologies/erl_pengine#erl_pengine_sup'",
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#table_mngr">>},
 {"'http://www.limmen.kth.se/ontologies/erl_pengine#erl_pengine_sup'",
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine_sup">>},
 {"'http://www.limmen.kth.se/ontologies/erl_pengine#erl_pengine_sup'",
  supervises,
  <<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine_master">>}]
  
5> semweb:supervises("X", "'http://www.limmen.kth.se/ontologies/erl_pengine#pengine'").
[{<<"http://www.limmen.kth.se/ontologies/erl_pengine#pengine_sup">>,
  supervises,
  "'http://www.limmen.kth.se/ontologies/erl_pengine#pengine'"}]
6> 
 ```
 
 Stopping:
 
 ```erlang
application:stop(erl_pengine),
os:cmd("cat /home/kim/workspace/erlang/erl_pengine/examples/semweb/prolog/pid/http.pid | xargs kill -9").
 ```