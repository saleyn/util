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
-author('saleyn@gmail.com').

-export([new/1, new/2, try_add/4, size/1, evict/2, evict/3, now/0]).

-compile({no_auto_import,[now/0, size/1]}).

-record(ttl_map, {ets, q, ttl}).

-type ttl_map() :: #ttl_map{}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Create a new map with a given TTL time for inserted items
new(TTL) ->
  new(TTL, #{}).

%% @doc Create a new map with a given TTL time for inserted items.
%% `Opts' is a list of options:
%% <dl>
%% <dt>name</dt><dd>The name of the ETS table (defaults to `undefined')</dd>
%% <dt>access</dt><dd>The access level of the ETS table (defaults to `private')</dd>
%% </dl>
new(TTL, Opts) when is_integer(TTL), is_map(Opts) ->
  Name = maps:get(name,   Opts, undefined),
  ACL  = maps:get(access, Opts, private),
  EtsOpts =
    case Name of
      undefined -> [ACL];
      _         -> [named_table, ACL]
    end,
  #ttl_map{ets = ets:new(Name, EtsOpts), q = queue:new(), ttl = TTL}.

%% @doc Try to add a `Key/Value' pair to the map.
%% If more than TTL time elapsed since the last insert of the `Key' or the
%% `Key' is not found in the map, the value is inserted, otherwise no modifications
%% are made.
-spec try_add(ttl_map(), any(), any(), non_neg_integer()) -> {ttl_map(), Inserted::boolean()}.
try_add(TTLMap = #ttl_map{ets = ETS, q = Q}, Key, Value, Now) when is_integer(Now) ->
  TTLMap1 = evict(TTLMap, Now),  %% Evict stale entries from the ETS
  case ets:lookup(ETS, Key) of
    [] ->
      ets:insert(ETS, {Key, {Value, Now}}),
      {TTLMap1#ttl_map{q = queue:in({Now, Key}, Q)}, true};
    [_] ->
      {TTLMap1, false}
  end.

%% @doc Evict stale items from the map given the current timestamp `Now'.
-spec evict(ttl_map(), non_neg_integer()) -> ttl_map().
evict(TTLMap = #ttl_map{ets = ETS, q = Q, ttl = TTL}, Now) ->
  Threshold = Now - TTL,
  Size      = size(TTLMap),
  {R, Q1}   = peek(Q),
  case dropwhile(Q1, R, ETS, 0, Threshold, Size) of
    {_,  0} -> TTLMap#ttl_map{q = Q1};
    {Q2, _} -> TTLMap#ttl_map{q = Q2}
  end.

%% @doc Evict stale items (up to the `Limit') from the map given the current timestamp `Now'.
-spec evict(ttl_map(), non_neg_integer(), non_neg_integer()) -> ttl_map().
evict(TTLMap = #ttl_map{ets = ETS, q = Q, ttl = TTL}, Now, Limit) ->
  Threshold = Now - TTL,
  {R, Q1}   = peek(Q),
  case dropwhile(Q1, R, ETS, 0, Threshold, Limit) of
    {_,  0} -> TTLMap#ttl_map{q = Q1};
    {Q2, _} -> TTLMap#ttl_map{q = Q2}
  end.

%% @doc Get the number of items in the map.
-spec size(ttl_map()) -> non_neg_integer().
size(#ttl_map{ets = ETS}) ->
  ets:info(ETS, size).

%% @doc Get the current timestamp in microseconds since Unix epoch.
-spec now() -> non_neg_integer().
now() ->
  erlang:system_time(microsecond).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

dropwhile(Q, {value, {Time, Key}}, ETS, N, Threshold, I) when Time =< Threshold, I > 0 ->
  ets:delete(ETS, Key),
  Q1        = queue:drop(Q),  %% Evict first element from the queue
  {Res, Q2} = peek(Q1),       %% Pick the next element in the queue
  dropwhile(Q2, Res, ETS, N+1, Threshold, I-1);
dropwhile(Q, _, _, N, _, _) ->
  {Q, N}.

%% We use this implementation of `peek' instead of the `queue:peek/1' because
%% the later will occasionally call `lists:reverse/1' when the second list in
%% the queue is empty, and wouldn't update the queue, whereas we want to memorize
%% the result of reversal if one has been made.
peek({_, [V|_]}=Q) -> {{value, V}, Q};
peek({[], []}=Q)   -> {undefined,  Q};
peek({[V],[]}=Q)   -> {{value, V}, Q};
peek({[Y|In],[]}) ->
    [V|_] = L = lists:reverse(In, []),
    {{value,V}, {[Y], L}}.

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
    Map4 = ttl_map:evict(Map3, 6000),
    ?assertEqual(1, ttl_map:size(Map4)),
    {Map5, R5} = ttl_map:try_add(Map4, 1, 124, 6000),
    ?assertEqual(2, ttl_map:size(Map5)),
    ?assert(R5),
    Map6 = ttl_map:evict(Map5, 6500),
    ?assertEqual(1, ttl_map:size(Map6)),
    Map7 = ttl_map:evict(Map6, 7001),
    ?assertEqual(0, ttl_map:size(Map7)),
    ?assertEqual(queue:new(), Map7#ttl_map.q),
    ok.
  %end.

-endif.