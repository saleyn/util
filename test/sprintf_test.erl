-module(sprintf_test).

-compile({parse_transform, sprintf}).

-export([t/0]).

t() ->
  "Test: 1, ok" = sprintf("Test: ~w, ~s", [1, "ok"]),
  "abc"         = sprintf("abc").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

sprintf_test() ->
  sprintf:set_float_fmt([{decimals, 2}]),
  ?assertEqual("Test: 1, ok",   sprintf("Test: ~w, ~s", [1, "ok"])),
  ?assertEqual("1",             sprintf(1)),
  ?assertEqual("1.00",          sprintf(1.0)),
  ?assertEqual("abc",           sprintf("abc")),
  ?assertEqual("abc",           sprintf(<<"abc">>)),
  ?assertEqual("abc",           sprintf(abc)),
  ?assertEqual("[abc,1,\"e\"]", sprintf([abc, 1, "e"])),
  sprintf:reset_float_fmt(),
  ok.

str_test() ->
  ?assertEqual("123",           i2l(123)),
  ?assertEqual("1",             i2l(1)),
  ?assertEqual("1",             b2l(<<"1">>)),
  ?assertEqual("abc",           str(<<"abc">>)),
  ?assertEqual("1",             str(1)),
  sprintf:set_float_fmt([{decimals, 2}]),
  ?assertEqual("1.00",          str(1.0)),
  sprintf:reset_float_fmt(),
  ?assertEqual("{a,1}",         str({a, 1})).

-endif. 
