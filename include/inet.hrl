%% On Windows REUSEADDR socket option is implemented.
%% Instead, when opening UDP sockets, use: gen_udp:open(12345, [?REUSEADDR_OPT])
-define(REUSEADDR_OPT, {raw, 16#ffff, 16#0200, <<1:32/native>>}).
