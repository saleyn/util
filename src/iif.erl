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

-export([ife/2, ife/3, ifne/2, ifne/3, iif/3, iif/4]).

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

-endif.
