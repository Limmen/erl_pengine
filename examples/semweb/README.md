# semweb

An example of using `erl_pengine` to query prolog pengine for rdf triples. 

Build and load into erlang shell:

```bash
$ rebar3 compile
$ rebar3 shell
```

Start the prolog server somewhere if it isn't already started. 

Use a command similar to this to start it from the erlang shell:
 
```
os:cmd("current_dir=$PWD; cd /home/kim/workspace/erlang/erl_pengine/examples/sudoku_solver/prolog; prolog daemon.pl --http=4000 --pidfile=/home/kim/workspace/erlang/erl_pengine/examples/sudoku_solver/prolog/pid/http.pid; cd $current_dir").
```

Start the `erl_pengine` application, since this is a erlang library we can do it manually:
 
 ```erlang
 application:ensure_all_started(erl_pengine). 
 ```
 
 Read prolog source with a prolog instance to be injected into the pengine upon creation:
 
 ```erlang
 {ok, Src} = file:read_file("/home/kim/workspace/erlang/erl_pengine/examples/sudoku_solver/prolog/src_text.pl").
 ```
 
 Call the single function exported by `sudoku_solver` API `solve_sudoku/1`:
 
 ```erlang
 solve_sudoku(Src).
 ```
 
 Stopping:
 
 ```erlang
application:stop(erl_pengine),
os:cmd("cat ~/workspace/erlang/erl_pengine/examples/sudoku_solver/prolog/pid/http.pid | xargs kill -9").
 ```