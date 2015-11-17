%%%-----------------------------------------------------------------------------
%%% @doc Miscelaneous list functions
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
-module(listx).

-export([group/2, copy_tuple_except/5]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Group elements in the `List' by element at position `Pos'.
-spec group(Pos::integer(), List::[tuple()]) -> [{any(), tuple()}].
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

group_test() ->
    ?assertEqual(
        #{a => [{11,13},{10,12}],b => [{15,16},{30,60}],c => [{10,15}]},
        group(1, [{a, 10, 12}, {a, 11, 13}, {b, 30, 60}, {b, 15, 16}, {c, 10, 15}])),

    ?assertEqual(
        #{a => [{11,13},{10,12}],b => [{15,16},{30,60}],c => [{15}]},
        group(1, [{a, 10, 12}, {a, 11, 13}, {b, 30, 60}, {b, 15, 16}, {c, 15}])),

    ?assertEqual(
        #{a => [{11,13},{10,12}],b => [{15,16},{30,60}],c => [{}]},
        group(1, [{a, 10, 12}, {a, 11, 13}, {b, 30, 60}, {b, 15, 16}, {c}])).

-endif.
