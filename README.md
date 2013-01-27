util
====

Erlang utility modules:

* decompiler          - decompiles modules and functions (useful for
                        verifying accuracy of code generation,
                        e.g. erlydtl project)
* pcap                - reader/writer of packet capture files (tcpdump, wireshark)
* restrict_remsh_mod  - to be used for remote shells to prevent a node from
                        being exited with `q()`, `init:stop()`, `erlang:halt()`.
* smtp                - SMTP client supporting tcp and ssl protocols
* sntp                - simple SNMP client
* user_default        - extending shell with useful debugging and profiling commands

Documentation
-------------

See project documentation: http://saleyn.github.com/util
