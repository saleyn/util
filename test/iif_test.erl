-module(iif_test).

-compile({parse_transform, iif}).

-export([t/0]).

t() ->
  ok    = iif(1 == 1, ok, error),
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
  ?assertEqual(ok,    iif(3 == 3, ok, error)),
  ?assertEqual(error, iif(1 == 2, ok, error)),
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
