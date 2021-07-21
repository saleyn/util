-module(iif_test).

-compile({parse_transform, iif}).

-export([t/0]).

t() ->
  A     = is_tuple(erlang:timestamp()),
  ok    = iif(A, ok, error),
  ok    = ife(false, ok),
  error = ife(true, ok, error).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

iif_test() ->
  A = is_tuple(erlang:timestamp()), B = not A,
  ?assertEqual(ok,    iif(A, ok, error)),
  ?assertEqual(error, iif(B, ok, error)),
  ?assertEqual(ok,    iif(is_tuple(erlang:timestamp()), ok, error)),
  ?assertEqual(ok,    iif(1, 1, ok, error)),
  ?assertEqual(error, iif(1, 2, ok, error)).

ife_test() ->
  ?assertEqual(ok,    ife(false, ok)),
  ?assertEqual(true,  ife(true,  ok)),
  ?assertEqual(1,     ife(1,     ok)),
  ?assertEqual(error, ife(true,  ok, error)),
  ?assertEqual(ok,    ife(false, ok, error)),
  ?assertEqual(error, ife(1,     ok, error)).

-endif. 
