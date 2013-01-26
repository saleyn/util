%%%------------------------------------------------------------------------
%%% @doc PCAP file reader/writer.
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @end
%%%------------------------------------------------------------------------
%%% Copyright (c) 2010 Serge Aleynikov
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without restriction,
%%% including without limitation the rights to use, copy, modify, merge,
%%% publish, distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do
%%% so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%------------------------------------------------------------------------
-module(pcap).

-export([
      write/2, replay/1, replay/2, replay/5
    , replay_range/3, replay_range/4
]).

write_header(File) ->
    file:write(File,
        <<16#a1b2c3d4:32/native, 2:16/native, 4:16/native, 0:32/native,
            0:32/native, 65535:32/native, 1:32/native>>).

write_packet_header(File, Len) ->
    LenEx = Len + 42,
    file:write(File, <<0:32, 0:32, LenEx:32/native, LenEx:32/native>>).

write_udp_frame(File) ->
    % Eth frame
    file:write(File, list_to_binary(lists:duplicate(14,0))),
    % IP frame (IPPROTO_UDP = 17)
    IP = <<0,0,0,0,0,0,0,0,0,17,0,0,0,0,0,0,0,0,0,0>>,
    file:write(File, IP),
    % UDP frame
    file:write(File, list_to_binary(lists:duplicate(8,0))).

write(Filename, Packet) when is_list(Filename), is_binary(Packet) ->
    {ok, File} = file:open(Filename, [write, raw, binary]),
    ok = write_header(File),
    ok = write_packet_header(File, size(Packet)),
    ok = write_udp_frame(File),
    ok = file:write(File, Packet),
    file:close(File).

replay_range(Filename, FromN, ToN)
  when is_list(Filename), is_integer(FromN) ->
    replay_range(Filename, undefined, FromN, ToN).

replay_range(Filename, Address, FromN, ToN)
  when is_list(Filename), is_integer(FromN) ->
    replay(Filename, Address, undefined, FromN, ToN).

replay(Filename) when is_list(Filename) ->
    replay(Filename, undefined).
replay(Filename, Address) when is_list(Filename), is_list(Address) ->
    replay(Filename, Address, undefined, 0, undefined).
replay(Filename, Address, Port, FromN, ToN)
  when is_list(Filename), is_list(Address)
     , is_integer(FromN), FromN < ToN
 ->
    {ok, File} = file:open(Filename, [read, raw, binary]),
    Endian = 
        case file:read(File, 24) of
        {ok, <<I:32, _/binary>>} when I =:= 16#a1b2c3d4 ->
            big;
        {ok, <<I:32, _/binary>>} when I =:= 16#d4c3b2a1 ->
            little;
        {error, Why} ->
            throw(file:format_error(Why))
        end,
    {ok, S} = gen_udp:open(0, [binary, {sndbuf, 1024*1024*16}]),
    replay_loop(Endian, File, S, parse_address(Address), Port, FromN, ToN).

parse_address(undefined) ->
    undefined;
parse_address(A = {_,_,_,_}) ->
    A;
parse_address(A) when is_list(A) ->
    {ok, T} = inet_parse:address(A),
    T.

replay_loop(_Endian, _File, _Sock, _Addr, _Port, N, N) ->
    ok;
replay_loop(Endian, File, Sock, Addr, Port, I, N) ->
    case {read_packet(File, Endian), Addr, Port} of
    {eof, _, _} ->
        ok;
    {{Packet, DAddr, DPort}, undefined, undefined} when is_binary(Packet) ->
        gen_udp:send(Sock, DAddr, DPort, Packet),
        replay_loop(Endian, File, Sock, DAddr, DPort, I+1, N);
    {{Packet, _DAddr, DPort}, _, undefined} when is_binary(Packet) ->
        gen_udp:send(Sock, Addr, DPort, Packet),
        replay_loop(Endian, File, Sock, Addr, Port, I+1, N);
    {{Packet, _DAddr, _DPort}, _, _} when is_binary(Packet) ->
        gen_udp:send(Sock, Addr, Port, Packet),
        replay_loop(Endian, File, Sock, Addr, Port, I+1, N)
    end.

read_packet(File, Endian) ->
    case file:read(File, 16+42) of
    {ok, <<_:32, _:32, Len:32/little, _:32, _Eth:14/binary, _Ip:12/binary,
        _SrcIp:32, DstIp:4/binary, _SrcPort:16, DstPort:16, _:4/binary>>} when Endian =:= little ->
        {ok, Packet} = file:read(File, Len-42),
        {Packet, decode_ip(DstIp), DstPort};
    {ok, <<_:32, _:32, Len:32/little, _:32, _Eth:14/binary, _Ip:12/binary,
        _SrcIp:32, DstIp:4/binary, _SrcPort:16, DstPort:16, _:4/binary>>} when Endian =:= big ->
        {ok, Packet} = file:read(File, Len-42),
        {Packet, decode_ip(DstIp), DstPort};
    _Other ->
        eof
    end.

decode_ip(<<I1,I2,I3,I4>>) -> {I1, I2, I3, I4}.
