# sudoku_solver

An example of using `erl_pengine` to query a prolog pengine for sudoku solutions. 

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
 3> sudoku_solver:solve_sudoku(Src).
 [[9,8,7,6,5,4,3,2,1],
  [2,4,6,1,7,3,9,8,5],
  [3,5,1,9,2,8,7,4,6],
  [1,2,8,5,3,7,6,9,4],
  [6,3,4,8,9,2,1,5,7],
  [7,9,5,4,6,1,8,3,2],
  [5,1,9,2,8,6,4,7,3],
  [4,7,2,3,1,9,5,6,8],
  [8,6,3,7,4,5,2,1,9]]
 4> 
 ```
 
 Stopping:
 
 ```erlang
application:stop(erl_pengine),
os:cmd("cat /home/kim/workspace/erlang/erl_pengine/examples/sudoku_solver/prolog/pid/http.pid | xargs kill -9").
 ```