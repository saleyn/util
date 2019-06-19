-module(ssh_tunnel).
%% @doc Module for creating SSH tunnels using `ssh`.
%% @author Serge Aleynikov
%% @see https://github.com/drowzy/ssh_tunnel
%%
%% It provides functions to create forwarded ssh channels, similair
%% to how other channels can be created using `ssh_connection`.
%% There are two type of channels supported
%% * `directtcp-ip` - Forwards a port from the client machine to the remote machine.
%%   This is the same as `ssh -nNT -L 8080:forward.example.com:9000 user@sshserver.example.com`
%% * `direct-streamlocal` - Forwards to a unix domain socket.
%%   This is the same as `ssh -nNT -L 8080:/var/lib/mysql/mysql.sock user@sshserver.example.com`
%% When using `direct_tcpip/3` or `direct_stream_local/2` directly there
%% will not be any local port or socket bound, this can either be done
%% using `ssh_tunnel` or by manually sending data with `ssh_connection.send/3`.
%% Although `connect/1` can be used to connect to the remote host, other
%% methods are supported.
%% One can use [SSHex](https://github.com/rubencaro/sshex), `ssh:connect/3`
%% for instance.
%%
%% ## Tunnels
%% Tunnels are on-demand TCP servers and are bound listeners to either a port
%% or a path. The tunnel will handle relaying TCP messages to the ssh
%% connection and back.
%%
%% ## Examples
%% ```
%%   {ok, SshRef} = ssh_tunnel:connect("sshserver.example.com", 22, []),
%%   {ok, Pid}    = ssh_tunnel:start_tunnel(Pid, {tcpip, {8080, {"192.168.90.15", 80}}}),
%%   % Send a TCP message for instance HTTP
%%   Resp = HTTPoison.get!("127.0.0.1:8080"),
%%   io:format("Received body: ~p\n", [Resp])
%% ```

-export([start_tunnel/4]).
-export([connect/0, connect/3, direct_tcpip/3, direct_stream_local/2, open_channel/6]).

-define(DIRECT_TCPIP, "direct-tcpip").
-define(STREAM_LOCAL, "direct-streamlocal@openssh.com").

-define(INI_WINDOW_SIZE, 1024 * 1024).
-define(MAX_PACKET_SIZE, 32 * 1024).

-type location() :: {string(), integer()}.

connect() -> connect({127,0,0,1}, 22, []).

%% @doc Create a connetion to a remote host with the provided options.
%% This function is mostly used as convenience wrapper around `:ssh_connect/3`
%% and does not support all options.
%% returns: `{ok, Connection}` or `{error, Reason}`.
%% @see https://manpages.debian.org/stretch/erlang-manpages/ssh.3erl.en.html
-spec connect(list()|tuple(), integer(), list()) -> {ok, pid()} | {error, term()}.
connect(Host, Port, Opts) when (is_list(Host) orelse is_tuple(Host)), is_integer(Port), is_list(Opts) ->
  Config = defaults(Opts),
  ssh:connect(Host, Port, Config).

%% @doc Starts a SSHTunnel.Tunnel process.
%% The tunnel will listen to either a local port or local path and handle
%% passing messages between the TCP client and ssh connection.
%% ## Examples
%%     {ok, SSH} = ssh_tunnel:connect("sshserver.example.com", 22),
%%     {ok, Pid} = ssh_tunnel:start_tunnel(Pid, tcpip, {8080, {"192.168.90.15", 80}})
%%     # Send a TCP message
%%     %HTTPoison.Response{body: body} = HTTPoison.get!("127.0.0.1:8080")
%%     IO.puts("Received body: #{body})
-spec start_tunnel(pid(), tcp|local, tuple(), list()) -> {ok, pid()} | {error, term()}.
start_tunnel(Pid, Transport, To, Opts) when is_tuple(To), is_list(Opts)
                                          , (Transport==tcp orelse Transport==local) ->
  case {Transport, To} of
    {tcp, {From, To}} when is_integer(From), is_integer(To) ->
      direct_tcpip(Pid, {"localhost", From}, {"localhost", To});
    {tcp, {From, {ToHost, ToPort}=To}} when is_integer(From), is_list(ToHost), is_integer(ToPort) ->
      direct_tcpip(Pid, {"localhost", From}, To);
    {tcp, {From, {ToHost, ToPort}=To}} when is_integer(From), is_list(ToHost), is_integer(ToPort) ->
      direct_tcpip(Pid, {"localhost", From}, To);
    {tcp, {{FromHost, FromPort}=From, {ToHost, ToPort}=To}} when is_list(FromHost), is_integer(FromPort), is_list(ToHost), is_integer(ToPort) ->
      direct_tcpip(Pid, From, To);
    {local, To} when is_list(To) ->
      direct_stream_local(Pid, To)
  end.

%% @doc Creates a ssh directtcp-ip forwarded channel to a remote port.
%% The returned channel together with a ssh connection reference (returned from `:ssh.connect/4`) can be used
%% to send messages with `:ssh_connection.send/3`
%% returns: `{:ok, channel}` or `{:error, reason}`.
%% ## Examples:
%%     msg = "GET / HTTP/1.1\r\nHost: localhost:8080\r\nUser-Agent: curl/7.47.0\r\nAccept: */*\r\n\r\n"
%%     {ok, Pid} = ssh_tunnel:connect("192.168.1.10", 22),
%%     {ok, Ch} = ssh_tunnel:direct_tcpip(Pid, {"127.0.0.1", 8080}, {"192.168.1.10", 80}),
%%     ok = ssh_connection:send(Pid, Ch, Msg),
%%     recieve do
%%       {ssh_cm, _, {data, channel, _, Data}} -> io:format("Data: ~p\n", [Data])
%%     end
-spec direct_tcpip(pid(), From::location(), To::location()) -> {ok, integer()} | {error, term()}.
direct_tcpip(Pid, {OrigHost, OrigPort} = _From, {RemHost, RemPort} = _To) when is_pid(Pid) ->
  LocH   = list_to_binary(OrigHost),
  RemH   = list_to_binary(RemHost),
  RemLen = byte_size(RemH),
  LocLen = byte_size(LocH),
  Msg    = <<RemLen:32,RemH/binary,RemPort:32, LocLen:32,LocH/binary,OrigPort:32>>,
  open_channel(Pid, ?DIRECT_TCPIP, Msg, ?INI_WINDOW_SIZE, ?MAX_PACKET_SIZE, infinity).

%% @doc Creates a ssh stream local-forward channel to a remote unix domain socket.
%% It sends the request that the server make a connection to its local Unix domain socket.
%% The returned channel together with a ssh connection reference (returned from `ssh:connect/4`)
%% can be used to send messages with `ssh_connection:send/3`.
%% returns: `{ok, Channel}` or `{error, Reason}`.
%% Ex:
%% ```
%% msg = "GET /images/json HTTP/1.1\r\nHost: /var/run/docker.sock\r\nAccept: */*\r\n\r\n"
%% {ok, Pid} = ssh_tunnel:connect("192.168.90.15", 22),
%% {ok, Ch}  = ssh_tunnel:direct_stream_local(Pid, "/var/run/docker.sock"),
%% ok = ssh_connection.send(Pid, Ch, Msg)
%% ```
-spec direct_stream_local(pid(), string()) -> {ok, integer()} | {error, term()}.
direct_stream_local(Pid, SocketPath) when is_pid(Pid), is_list(SocketPath) ->
  SPath = list_to_binary(SocketPath),
  Msg   = <<(byte_size(SPath)):32/integer, SPath/binary, 0:32, 0:32>>,
  open_channel(Pid, ?STREAM_LOCAL, Msg, ?INI_WINDOW_SIZE, ?MAX_PACKET_SIZE, infinity).

open_channel(Pid, Type, Msg, WindowSize, MaxPktSz, Timeout) ->
  case ssh_connection_handler:open_channel(Pid,Type,Msg,WindowSize,MaxPktSz,Timeout) of
    {open, Ch}                  -> {ok, Ch};
    {open_error, _, Reason, _}  -> {error, to_string(Reason)}
  end.

defaults(_Opts) ->
  [
    {user_interaction, false},
    {silently_accept_hosts, true}
  ].

to_string(A) when is_atom(A)   -> atom_to_list(A);
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(B) when is_list(B)   -> B.
