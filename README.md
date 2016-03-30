util
====

Erlang utility modules:

| Module                | Description                                                                          |
| --------------------- | ------------------------------------------------------------------------------------ |
| decompiler            | decompiles modules and functions (useful for verifying accuracy of code generation)  |
| file_log_reader       | Periodically read an append-only log file and parse newly added data                 |
| iif                   | Ternery if function                                                                  |
| io_lib_pretty_limited | Print term to binary by constraining the output size                                 |
| gin                   | Convenient parse transform for `in(Value, [A,B,C])` type of guards                   |
| hex                   | Hex to bin conversion                                                                |
| listx                 | Miscelaneous list handling functions                                                 |
| osx                   | Execution of os commands with returned stdout and exit status                        |
| pcap                  | reader/writer of packet capture files (tcpdump, wireshark)                           |
| restrict_remsh_mod    | to be used for remote shells to restrict `q()`, `init:stop()`, `erlang:halt()`, etc. |
| smtp                  | SMTP client supporting tcp and ssl protocols                                         |
| sntp                  | simple SNMP client                                                                   |
| stringx               | miscelaneous string functions                                                        |
| user_default          | extending shell with useful debugging and profiling commands                         |

Documentation
-------------

See project documentation: http://saleyn.github.com/util
