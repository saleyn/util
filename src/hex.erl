%%% vim:ts=4:sw=4:et
%%%-----------------------------------------------------------------------------
%%% @doc    Hexadecimal conversion functions
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Date:   2015-12-10
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 Serge Aleynikov
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
%%%-----------------------------------------------------------------------------
-module(hex).
-author('saleyn@gmail.com').

-export([to_hex/1, to_bin/1, to_int/1]).
-export([hex/1, dehex/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%% @doc Convert an iolist to a hex string.
-spec to_hex(integer()|iolist())  -> binary().
to_hex(0)                           -> <<"0">>;
to_hex(I) when is_integer(I), I > 0 -> to_hex_int(I, []);
to_hex(L) when is_list(L)           -> to_hex_bin(iolist_to_binary(L));
to_hex(B) when is_binary(B)         -> to_hex_bin(B).

%% @doc Convert a hex string to binary.
-spec to_bin(string()) -> binary().
to_bin(Bin) when is_binary(Bin) ->
    << <<((dehex(A) bsl 4) bor dehex(B))>> || <<A,B>> <= Bin >>;
to_bin(L) when is_list(L) ->
    iolist_to_binary(to_bin2(L)).

%% @doc Convert a hex string/binary to integer.
-spec to_int(string()|binary()) -> integer().
to_int(S) when is_list(S)       -> erlang:list_to_integer  (S, 16);
to_int(B) when is_binary(B)     -> erlang:binary_to_integer(B, 16).

%% @doc Convert a hex digit in range [$0..$9,$a..$f,$A..$F] to integer.
-spec dehex(char()) -> integer().
dehex(C) when C >= $0, C =< $9 -> C - $0;
dehex(C) when C >= $a, C =< $f -> C - ($a - 10);
dehex(C) when C >= $A, C =< $F -> C - ($A - 10).

%% @doc Convert an integer to a hex digit in range [0..15].
-spec hex(integer()) -> char().
hex(C) when C >= 0, C =< 9 -> C + $0;
hex(C) when C =< 15        -> C + ($a - 10).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

to_bin2([A,B | T])  -> [((dehex(A) bsl 4) bor dehex(B)) | to_bin2(T)];
to_bin2([])         -> [].

to_hex_bin(Bin)     -> << <<(hex(A)), (hex(B))>> || <<A:4, B:4>> <= Bin >>.

to_hex_int(0, Acc)  -> list_to_binary(Acc);
to_hex_int(I, Acc)  -> to_hex_int(I bsr 4, [hex(I band 15) | Acc]).

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

all_test() ->
    <<"0">> = to_hex(0),
    <<"912ec803b2ce49e4a541068d495ab570">> =
        to_hex([145,46,200,3,178,206,73,228,165,65,6,141,73,90,181,112]),
    <<"fffefdfcfbfa1009080701">> = to_hex(16#fffefdfcfbfa1009080701),

    <<>>                                   = to_bin(<<"0">>),
    <<0>>                                  = to_bin("00"),
    <<0>>                                  = to_bin(<<"00">>),
    <<255,254,253,252,251,250,16,9,8,7,1>> = to_bin(<<"fffefdfcfbfa1009080701">>),
    <<255,254,253,252,251,250,16,9,8,7,1>> = to_bin(<<"FFFEFDFCFBFA1009080701">>),

    0                                      = to_int("0"),
    0                                      = to_int(<<"0">>),
    16#fffefdfcfbfa1009080701              = to_int("fffefdfcfbfa1009080701"),
    16#fffefdfcfbfa1009080701              = to_int(<<"fffefdfcfbfa1009080701">>),
    ok.

-endif.
