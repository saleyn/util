-module(str_test).

-compile({parse_transform, str}).

-export([t/0]).

t() ->
  "Test: 1, ok" = str("Test: ~w, ~s", [1, "ok"]),
  "abc"         = str("abc").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

str_test() ->
  str:set_float_fmt([{decimals, 2}]),
  ?assertEqual("Test: 1, ok",   str("Test: ~w, ~s", [1, "ok"])),
  ?assertEqual("1",             str(1)),
  ?assertEqual("1.00",          str(1.0)),
  ?assertEqual("abc",           str("abc")),
  ?assertEqual("abc",           str(<<"abc">>)),
  ?assertEqual("abc",           str(abc)),
  ?assertEqual("[abc,1,\"e\"]", str([abc, 1, "e"])),
  str:reset_float_fmt(),

  ?assertEqual("123",           i2l(123)),
  ?assertEqual("1",             i2l(1)),
  ?assertEqual("1",             b2l(<<"1">>)),
  ?assertEqual("abc",           str(<<"abc">>)),
  ?assertEqual("1",             str(1)),
  str:set_float_fmt([{decimals, 2}]),
  ?assertEqual("1.00",          str(1.0)),
  str:reset_float_fmt(),
  ?assertEqual("{a,1}",         str({a, 1})).

throw_test() ->
  ?assertEqual("Test: 1", try throw("Test: ~w", [1]) catch _:E -> E end),
  ?assertEqual(ok,        try throw(ok)              catch _:E -> E end).

-endif. 
