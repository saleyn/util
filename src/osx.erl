%%% vim:ts=4:sw=4:et
%%%-----------------------------------------------------------------------------
%%% @doc    OS supporting commands
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
-module(osx).
-author('saleyn@gmail.com').

-export([command/1, command/2, command/3, status/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

-spec command(string()) -> {integer(), list()}.
command(Cmd) ->
	command(Cmd, [], undefined).

-spec command(string(), list()|undefined|fun((list(),any()) -> any())) ->
    {integer(), any()}.
command(Cmd, Fun) when is_function(Fun, 2) ->
    command(Cmd, [], Fun);
command(Cmd, Opt) when is_list(Opt) ->
    command(Cmd, Opt, undefined).

-spec command(string(), list(), undefined|fun((list(),any()) -> any())) ->
    {integer(), any()}.
command(Cmd, Opt, Fun) when is_list(Opt), Fun=:=undefined orelse is_function(Fun, 2) ->
    Opts = Opt ++ [stream, exit_status, use_stdio, in, hide, eof],
    P    = open_port({spawn, Cmd}, Opts),
    get_data(P, Fun, []).

-spec status(integer()) ->
        {status, ExitStatus :: integer()} |
        {signal, Singnal :: integer(), Core :: boolean()}.
status(Status) when is_integer(Status) ->
    TermSignal = Status band 16#7F,
    IfSignaled = ((TermSignal + 1) bsr 1) > 0,
    ExitStatus = (Status band 16#FF00) bsr 8,
    case IfSignaled of
        true ->
            CoreDump = (Status band 16#80) =:= 16#80,
            {signal, TermSignal, CoreDump};
        false ->
            {status, ExitStatus}
    end.

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------
get_data(P, Fun, D) ->
	receive
		{P, {data, {eol, Line}}} when Fun =:= undefined ->
            get_data(P, Fun, [Line|D]);
		{P, {data, {eol, Line}}} when is_function(Fun, 2) ->
            get_data(P, Fun, Fun(eol, {Line, D}));
		{P, {data, {noeol, Line}}} when Fun =:= undefined ->
            get_data(P, Fun, [Line|D]);
		{P, {data, {noeol, Line}}} when is_function(Fun, 2) ->
            get_data(P, Fun, Fun(noeol, {Line, D}));
		{P, {data, D1}} when Fun =:= undefined ->
            get_data(P, Fun, [D1|D]);
		{P, {data, D1}} when is_function(Fun, 2) ->
			get_data(P, Fun, Fun(data, {D1, D}));
		{P, eof} ->
			port_close(P),
            receive
                {P, {exit_status, 0}} when is_function(Fun, 2) ->
                    {ok, Fun(eof, D)};
                {P, {exit_status, N}} when is_function(Fun, 2) ->
                    {error, {N, Fun(eof, D)}};
                {P, {exit_status, 0}} ->
                    {ok, lists:reverse(D)};
                {P, {exit_status, N}} ->
                    {error, {N, lists:reverse(D)}}
            after 5000 ->
                if is_function(Fun, 2) ->
                    throw({no_exit_status, Fun(eof, D)});
                true ->
                    throw({no_exit_status, timeout_waiting_for_output})
                end
            end
	end.

%%%-----------------------------------------------------------------------------
%%% Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

command_test() ->
	{ok,    ["ok\n"]}  = osx:command("echo ok"),
	{error, {1, ""}}   = osx:command("false"),
    {ok, ok}           = osx:command("echo ok", fun(eol, {"ok\n", []}) -> ok; (eof, A) -> A end),
    {ok,["a","b","c"]} = osx:command("echo -en 'a\nb\nc\n'", [{line, 80}]),
    %{error, {143,[]}}  = osx:command("kill $$"),
    {signal,15,true}   = status(143),
    {status,0}         = status(0),
    ok.

-endif.
