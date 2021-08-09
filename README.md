util
====

[![build](https://github.com/saleyn/util/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/util/actions/workflows/erlang.yml)

Erlang utility modules:

| Module                | Description                                                                          |
| --------------------- | ------------------------------------------------------------------------------------ |
| decompiler            | decompiles modules and functions (useful for verifying accuracy of code generation)  |
| csv                   | CSV parsing and loading data to MySQL                                                |
| env                   | environment variables substitution, path normalization                               |
| file_log_reader       | Periodically read an append-only log file and parse newly added data                 |
| iif                   | Ternery if function including `iif/3`, `iif/4`, `ife/3`, `ife/4` parse transforms    |
| io_lib_pretty_limited | Print term to binary by constraining the output size                                 |
| gin                   | Convenient parse transform for `in(Value, [A,B,C])` type of guards                   |
| hex                   | Hex to bin conversion                                                                |
| listx                 | Miscelaneous list handling functions                                                 |
| osx                   | Execution of os commands with returned stdout and exit status                        |
| pcap                  | reader/writer of packet capture files (tcpdump, wireshark)                           |
| restrict_remsh_mod    | to be used for remote shells to restrict `q()`, `init:stop()`, `erlang:halt()`, etc. |
| smtp                  | SMTP client supporting tcp and ssl protocols                                         |
| sntp                  | simple SNTP client                                                                   |
| stringx               | miscelaneous string functions                                                        |
| user_default          | extending shell with useful debugging and profiling commands                         |

Documentation
-------------

See project documentation: http://saleyn.github.io/util
