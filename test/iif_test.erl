-module(iif_test).

-compile({parse_transform, iif}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

iif_test() ->
  ?assertEqual(ok,    iif(1+2 == 3, ok, error)),
  ?assertEqual(error, iif(1 == 2,   ok, error)),
  ?assertEqual(ok,    iif(is_tuple(erlang:timestamp()), ok, error)),
  ?assertEqual(ok,    iif(1, 1, ok, error)),
  ?assertEqual(error, iif(1, 2, ok, error)).

-endif. 
