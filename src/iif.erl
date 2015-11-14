%%%-----------------------------------------------------------------------------
%%% @doc Conditional expression functions
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @end
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
-module(iif).

-export([
      ife/2, ife/3, ifne/2, ifne/3, iif/3, iif/4
    , group/2, copy_tuple_except/5
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% If empty
ife([],         Value) -> execute([], Value);
ife(false,      Value) -> execute([], Value);
ife(undefined,  Value) -> execute([], Value);
ife(Test,      _Value) -> Test.

ife([],         Empty,_NotEmpty) -> execute([], Empty);
ife(false,      Empty,_NotEmpty) -> execute([], Empty);
ife(undefined,  Empty,_NotEmpty) -> execute([], Empty);
ife(Value,     _Empty, NotEmpty) -> execute(Value, NotEmpty).

% If not empty
ifne([],       _Value) -> [];
ifne(false,    _Value) -> [];
ifne(undefined,_Value) -> [];
ifne(Test,      Value) -> execute(Test, Value).

ifne([],       _NotEmpty, Empty) -> execute([], Empty);
ifne(false,    _NotEmpty, Empty) -> execute([], Empty);
ifne(undefined,_NotEmpty, Empty) -> execute([], Empty);
ifne(Value,     NotEmpty,_Empty) -> execute(Value, NotEmpty).

% If then else
iif([],            _True, False) -> execute([], False);
iif(false,         _True, False) -> execute([], False);
iif(undefined,     _True, False) -> execute([], False);
iif(true,           True,_False) -> execute([], True).

iif(Value, Value,   True,_False) -> execute(Value, True);
iif(Value,_Other,  _True, False) -> execute(Value, False).

execute(_, F) when is_function(F,0) -> F();
execute(V, F) when is_function(F,1) -> F(V);
execute(_, V)                       -> V.

group(Pos, List) when is_integer(Pos), is_list(List) ->
    lists:foldl(fun(T, A) when is_tuple(T), tuple_size(T) >= Pos ->
        TT0 = erlang:make_tuple(tuple_size(T)-1, undefined),
        TT  = copy_tuple_except(Pos, 1, tuple_size(T), T, TT0),
        Key = element(Pos, T),
        Old = maps:get(Key, A, []),
        A#{Key => [TT | Old]}
    end, #{}, List).

%% @doc Copy every element of tuple TS to tuple TT ignoring the item at
%%      Ignore position
copy_tuple_except(_Ignore, I, N,_TS, TT) when I > N -> TT;
copy_tuple_except(I, I, N, TS, TT) -> copy_tuple_except(I, I+1, N, TS, TT);
copy_tuple_except(Ignore, I, N, TS, TT) when Ignore > I ->
    copy_tuple_except(Ignore, I+1, N, TS, setelement(I, TT, element(I, TS)));
copy_tuple_except(Ignore, I, N, TS, TT) -> % Ignore < I
    copy_tuple_except(Ignore, I+1, N, TS, setelement(I-1, TT, element(I, TS))).

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

ife_test() ->
    ?assertEqual(abc, ife ([],        abc)),
    ?assertEqual(abc, ife (false,     abc)),
    ?assertEqual(abc, ife (undefined, abc)),
    ?assertEqual(xxx, ife (xxx,       abc)),
    ?assertEqual(ok,  ife (false,     fun()  -> ok end)),
    ?assertEqual([],  ife (false,     fun(V) -> V  end)),

    ?assertEqual(abc, ife ([],        abc, efg)),
    ?assertEqual(abc, ife (false,     abc, efg)),
    ?assertEqual(abc, ife (undefined, abc, efg)),
    ?assertEqual(efg, ife (xxx,       abc, efg)),
    ?assertEqual(xxx, ife (xxx,       abc, fun(V) -> V  end)).

ifne_test() ->
    ?assertEqual([],  ifne([],        abc)),
    ?assertEqual([],  ifne(false,     abc)),
    ?assertEqual([],  ifne(undefined, abc)),
    ?assertEqual(abc, ifne(xxx,       abc)),
    ?assertEqual(ok,  ifne(false,     abc, fun()  -> ok end)),
    ?assertEqual(x,   ifne(xxx,       fun()  -> x  end, efg)),
    ?assertEqual(xxx, ifne(xxx,       fun(V) -> V  end, efg)),

    ?assertEqual(efg, ifne([],        abc, efg)),
    ?assertEqual(efg, ifne(false,     abc, efg)),
    ?assertEqual(efg, ifne(undefined, abc, efg)),
    ?assertEqual(abc, ifne(xxx,       abc, efg)),
    ?assertEqual(xxx, ifne(xxx,       fun(V) -> V  end, efg)).

iif_test() ->
    ?assertEqual(abc, iif(x, x, abc, efg)),
    ?assertEqual(ok,  iif(x, x, fun() -> ok end, efg)),
    ?assertEqual(x,   iif(x, x, fun(X) -> X end, efg)),
    ?assertEqual(efg, iif(x, y, abc, efg)),
    ?assertEqual(ok,  iif(x, y, abc, fun() -> ok end)),
    ?assertEqual(x,   iif(x, y, abc, fun(X) -> X end)).

group_test() ->
    ?assertEqual(
        #{a => [{11,13},{10,12}],b => [{15,16},{30,60}],c => [{10,15}]},
        iif:group(1, [{a, 10, 12}, {a, 11, 13}, {b, 30, 60}, {b, 15, 16}, {c, 10, 15}])),

    ?assertEqual(
        #{a => [{11,13},{10,12}],b => [{15,16},{30,60}],c => [{15}]},
        iif:group(1, [{a, 10, 12}, {a, 11, 13}, {b, 30, 60}, {b, 15, 16}, {c, 15}])),

    ?assertEqual(
        #{a => [{11,13},{10,12}],b => [{15,16},{30,60}],c => [{}]},
        iif:group(1, [{a, 10, 12}, {a, 11, 13}, {b, 30, 60}, {b, 15, 16}, {c}])).

-endif.
