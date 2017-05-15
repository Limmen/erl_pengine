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
{ok, P1, Id1} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{}).

{ok, P2, Id2} = pengine_master:create_pengine("http://127.0.0.1:4000/pengine", test_callbackmod, #{}).

Pengines = pengine_master:list_pengines().

pengine:next(P1).
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