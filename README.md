# erl_pengine (v0.1.0)

## Overview

**ErlangPengine**

Erlang client to prolog pengine server.
For more information about the pengine project see the following links.

* [http://pengines.swi-prolog.org/docs/documentation.html](http://pengines.swi-prolog.org/docs/documentation.html)
* [http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27)](http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pengines.html%27))
* [paper](paper/pengines.pdf)

EDoc link:

* [Edoc](doc/index.html) 

### Features

Pengines is short for Prolog Engines, it's a package which allows you to talk to remote prolog servers in a simple
and effective way. 

With a pengines client you can access pretty much the full prolog power remotely. You write your pengine-server and
make it export the predicates you desire as accessible from remote and then you can access it.

If you are looking for just basic prolog access, you should look at [erlog](https://github.com/rvirding/erlog) which is 
a prolog in interpreter for a subset of the prolog standard in erlang.

If you need access to full prolog-power like access to a CLP-solver, a semantic-web server or similar, a pengine-client is a good
way to do it.

Other pengine clients:

* [JavaScriptPengine](http://pengines.swi-prolog.org/docs/index.html)
* [RubyPengine](https://github.com/simularity/RubyPengine)
* [JavaPengine](https://github.com/simularity/JavaPengine)

### Useful Modules

* `erl_pengine.erl`: entrypoint module, application callback module, start with `application:start(erl_pengine)`
* `pengine.erl`: main API for a created slave-pengine
* `pengine_master`: API for administering active pengines and also creating new ones

## QuickStart Usage

 **Add to rebar3 project**

**TODO**: The package is published at [hex.pm](hex.pm)
You can add  it to your project by putting the following to your list of dependencies in `rebar.config`:
 ```erlang
   {deps, [
          {erl_pengine, "0.1.0"}
   ]}.

 ```
 
 Or add it as a github dependency:
 
 ```erlang
  {deps, [
         {erl_pengine, {git, "https://github.com/Limmen/erl_pengine"}}
  ]}.
  ```
 
 **Start application**
 
  ```erlang
 
  ```
 
 **Using the API**
 
   ```erlang
  
   ```
   
## API

## Examples

```erlang

```

## Contribute

Contributions are welcome, for bugreports please use github issues.

### Most useful project commands

It's a rebar3 project so all commands listed here: [rebar3 commands](https://www.rebar3.org/docs/commands) are available.

```bash
# build
$ ./rebar3 compile

# remove temporary files
$ ./rebar3 clean

# run unit tests
$ ./rebar3 eunit

# run system tests, note that the test-suite uses absolute path to a prolog-pengine server.
$ ./rebar3 ct 

# run static code analysis
$ ./rebar3 dialyzer 

# alias to run all tests
$ ./rebar3 alias testall

# alias to run the ci-check which travis will run upon git push
$ ./rebar3 alias ci

# validate codebase, runs: tests, linters, static code analysis
$ ./rebar3 alias validate

# Generate documentation with edoc
$ ./rebar3 edoc

# Start shell with application loaded
$ ./rebar3 shell

# Start shell with configuration
$ rebar3 shell --config erl_pengine.config

# Run release
$ ./rebar3 run

```

Make sure that any PR first passes dialyzer, linter and tests.
 
## Author & Maintainer

Kim Hammar <kimham@kth.se>

## Copyright and license

[LICENSE](LICENSE.md)

MIT

(C) 2017, Kim Hammar