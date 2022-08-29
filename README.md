# Erlang Utility Modules

[![build](https://github.com/saleyn/util/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/util/actions/workflows/erlang.yml)

**Author** Serge Aleynikov <serge(at)gmail.com>

## Installation

- Add dependency in `rebar.config`:
```erlang
{deps,
 [% ...
  {util, "~> 1.0"}
 ]}.
```

## Content

| Module                    | Description                                                                          |
| ------------------------- | ------------------------------------------------------------------------------------ |
| decompiler                | decompiles modules and functions (useful for verifying accuracy of code generation)  |
| csv                       | CSV parsing and loading data to MySQL                                                |
| env                       | environment variables substitution, path normalization                               |
| file_log_reader           | Periodically read an append-only log file and parse newly added data                 |
| iif                       | Ternery if function including `iif/3`, `iif/4`, `ife/3`, `ife/4` parse transforms    |
| io_lib_pretty_limited     | Print term to binary by constraining the output size                                 |
| gin                       | Convenient parse transform for `in(Value, [A,B,C])` type of guards                   |
| hex                       | Hex to bin conversion                                                                |
| listx                     | Miscelaneous list handling functions                                                 |
| osx                       | Execution of os commands with returned stdout and exit status                        |
| pcap                      | reader/writer of packet capture files (tcpdump, wireshark)                           |
| restrict_remsh_mod        | to be used for remote shells to restrict `q()`, `init:stop()`, `erlang:halt()`, etc. |
| smtp                      | SMTP client supporting tcp and ssl protocols                                         |
| sntp                      | simple SNTP client                                                                   |
| str                       | stringification functions including `str/1` and `str/2` parse transforms             |
| stringx                   | miscelaneous string functions                                                        |
| throttle                  | implements a rate limitting algorithm                                                |
| user_default              | extending shell with useful debugging and profiling commands                         |
| build-aux/md-to-edoc.awk  | AWK script for converting `README.md` files to `overview.edoc`                       |

Additionally, the following Elixir modules are included:

| Module       | File             | Description                                                                    |
|--------------|------------------| -------------------------------------------------------------------------------|
| CompileTime  | compile_time.ex  | Evaluate lambdas at compile time                                               |

## Documentation

* See [project documentation](https://saleyn.github.io/util)

This project implements an extension of `EDoc` documentation by using the color scheme similar
to `GitHub`, and generate the `overview.edoc` from the `README.md`.

In order to use this feature, modify your `Makefile` to include:

```
-include build-aux/docs-addon.mk

build-aux/docs-addon.mk:
	@echo "Fetching build-aux/docs-addon.mk" && \
		mkdir -p build-aux && \
		curl -s -o build-aux/docs-addon.mk https://raw.githubusercontent.com/saleyn/util/master/build-aux/docs-addon.mk
```
Also in your `rebar.config` add:
```
{edoc_opts, [{overview,        "build-aux/overview.edoc"},
             {stylesheet_file, "build-aux/edoc.css"},
             {title,           "Project title used by rebar and also inserted as title to the index.html"},
             {keywords,        "HTML meta keywords (comma-delimited) for search engine crawlers"}, 
             ...]}.
```
NOTE: the `keywords` option is not specific to `EDoc` but used by the HTML reformatting make
file `docs-addon.mk`.

This will add the following targets to your `Makefile`:

- `docs` - Make documentation from source code
- `gh-pages` - Create GitHub pages for the current project
- `get-version` - Show application release version from the `*.app.src` and `rebar.config`
- `set-version` - Set the version number for the above `(make set-version version=X.Y.Z)`
- `clean-docs`  - Remove the generated files in the `doc` directory

## Elixir

To add functions from `user_default.erl` to Elixir's `iex` shell, add `~/.iex.exs` file
containing:
```
import :user_default
```

## Download

* [GitHub](http://saleyn.github.io/util)
