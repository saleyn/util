%%------------------------------------------------------------------------------
%% @doc Map with TTL key/value eviction.
%%
%% An insert of a Key/Value pair in the map will store the timestamp of the
%% maybe_add.  Additionally a queue of maybe_adds is maintained by this container,
%% which is checked on each insert and the expired Key/Value pairs are
%% evicted from the map.
%%
%% @author Serge Aleynikov <saleyn at gmail dot com>
%% @end
%%------------------------------------------------------------------------------
%% Copyright (c) 2011 Serge Aleynikov
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without restriction,
%% including without limitation the rights to use, copy, modify, merge,
%% publish, distribute, sublicense, and/or sell copies of the Software,
%% and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%------------------------------------------------------------------------------
-module(ttl_map).
-export([new/1, new/2, try_add/4, size/1, refresh/2, now/0]).

-compile({no_auto_import,[now/0, size/1]}).

-record(ttl_map, {ets, q, ttl}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new(TTL) ->
  new(TTL, #{}).
new(TTL, Opts) when is_integer(TTL), is_map(Opts) ->
  Name = maps:get(name,   Opts, undefined),
  ACL  = maps:get(access, Opts, private),
  EtsOpts =
    case Name of
      undefined -> [ACL];
      _         -> [named_table, ACL]
    end,
  #ttl_map{ets = ets:new(Name, EtsOpts), q = queue:new(), ttl = TTL}.

try_add(TTLMap = #ttl_map{ets = ETS, q = Q}, Key, Value, Now) when is_integer(Now) ->
  TTLMap1 = refresh(TTLMap, Now),  %% Evict stale entries from the ETS
  case ets:lookup(ETS, Key) of
    [] ->
      ets:insert(ETS, {Key, {Value, Now}}),
      {TTLMap1#ttl_map{q = queue:in({Now, Key}, Q)}, true};
    [_] ->
      {TTLMap1, false}
  end.

refresh(TTLMap = #ttl_map{ets = ETS, q = Q, ttl = TTL}, Now) ->
  Threshold = Now - TTL,
  case dropwhile(Q, queue:peek(Q), ETS, 0, Threshold) of
    {_, 0} ->
      TTLMap;
    {V, _} ->
      TTLMap#ttl_map{q = V}
  end.

size(#ttl_map{ets = ETS}) ->
  ets:info(ETS, size).

now() ->
  erlang:system_time(microsecond).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

dropwhile(Q, {value, {Time, Key}}, ETS, N, Threshold) when Time =< Threshold ->
  ets:delete(ETS, Key),
  Q1 = queue:drop(Q),
  dropwhile(Q1, queue:peek(Q1), ETS, N+1, Threshold);
dropwhile(Q, _, _, N, _) ->
  {Q, N}.

%%------------------------------------------------------------------------------
%% Unit testing
%%------------------------------------------------------------------------------

-ifdef(EUNIT).

add_test() ->
  %fun() ->
    Map0 = ttl_map:new(1000),
    ?assertEqual(0, size(Map0)),
    {Map1, R1} = ttl_map:try_add(Map0, 1, 123, 5000),
    ?assertEqual(1, ttl_map:size(Map1)),
    ?assert(R1),
    {Map2, R2} = ttl_map:try_add(Map1, 2, 234, 5001),
    ?assertEqual(2, ttl_map:size(Map2)),
    ?assert(R2),
    {Map3, R3} = ttl_map:try_add(Map2, 1, 124, 5999),
    ?assertEqual(2, ttl_map:size(Map3)),
    ?assertNot(R3),
    Map4 = ttl_map:refresh(Map3, 6000),
    ?assertEqual(1, ttl_map:size(Map4)),
    {Map5, R5} = ttl_map:try_add(Map4, 1, 124, 6000),
    ?assertEqual(2, ttl_map:size(Map5)),
    ?assert(R5),
    Map6 = ttl_map:refresh(Map5, 6500),
    ?assertEqual(1, ttl_map:size(Map6)),
    Map7 = ttl_map:refresh(Map6, 7001),
    ?assertEqual(0, ttl_map:size(Map7)),
    ?assertEqual(queue:new(), Map7#ttl_map.q),
    ok.
  %end.

-endif.

