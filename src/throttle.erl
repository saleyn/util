%%------------------------------------------------------------------------------
%% @doc Throttle given rate over a number of seconds.
%%
%% Implementation uses time spacing reservation algorithm where each
%% allocation of samples reserves a fraction of space in the throttling
%% window. The reservation gets freed as the time goes by.  No more than
%% the "rate()" number of samples are allowed to fit in the "window_msec()"
%% window.
%%
%% This is an Erlang implementation of the throttling algorithm from the
%% [utxx library](https://github.com/saleyn/utxx/blob/master/include/utxx/rate_throttler.hpp)
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
-module(throttle).
-export([new/1, new/2, new/3, available/1, available/2, used/1, used/2]).
-export([now/0, reset/1, reset/2, add/1, add/2, add/3, curr_rps/1, curr_rps/2]).

-compile({no_auto_import,[now/0]}).

-record(throttle, {
    rate
  , window  :: integer()    %% In microseconds
  , step    :: integer()    %% In microseconds
  , next_ts :: integer()
}).

-type throttle() :: #throttle{}.
-type time()     :: non_neg_integer().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new(Rate) ->
  new(Rate, 1).

new(Rate, Window) ->
  new(Rate, Window, now()).

%% @doc Create a new throttle given the `Rate' per `Window' milliseconds.
-spec new(non_neg_integer(), non_neg_integer(), time()) -> throttle().
new(Rate, Window, Now) when is_integer(Rate), is_integer(Window), is_integer(Now) ->
  Win  = Window * 1000,
  Step = if Rate == 0 -> 0; true -> Win div Rate end,
  #throttle{rate = Rate, window = Win, step = Step, next_ts = Now}.

reset(#throttle{} = T) ->
  reset(T, now()).

%% @doc Reset the throttle request counter
reset(#throttle{} = T, Now) ->
  T#throttle{next_ts = Now}.

add(T)          -> add(T, 1).
add(T, Samples) -> add(T, Samples, now()).

%% @doc Add `Samples' to the throtlle's counter.
%% Return `{FitSamples, State}', where `FitSamples' are the number of samples
%% that fit in the throttling window. 0 means that the throttler is fully
%% congested, and more time needs to elapse before the throttles gets reset
%% to accept more samples.
-spec add(throttle(), integer(), time()) -> {integer(), throttle()}.
add(#throttle{rate = 0}, Samples, _Now) ->
  Samples;
add(#throttle{next_ts = TS, step = Step, window = Win} = T, Samples, Now) ->
  NextTS    = TS     + Samples * Step,
  NowNextTS = Now    + Win,
  Diff      = NextTS - NowNextTS,
  if
    Diff < -Win ->
      {Samples, T#throttle{next_ts = Now + Step}};
    Diff < 0 ->
      {Samples, T#throttle{next_ts = NextTS}};
    true ->
      N = max(0, Samples - (Diff div Step)),
      {N, T#throttle{next_ts = TS + N * Step}}
  end.

%% @see available/2
available(T) -> available(T, now()).

%% @doc Return the number of available samples given `a_now' current time.
available(#throttle{rate=0}=T,_Now) -> T#throttle.window;
available(#throttle{}      =T, Now) -> calc_available(T, Now).

%% @see used/2
used(T) -> used(T, now()).

%% @doc Return the number of used samples given `a_now' current time.
used(#throttle{rate = 0},     _Now) -> 0;
used(#throttle{rate = R} = T,  Now) -> R-calc_available(T, Now).

%% @see curr_rps/2
curr_rps(T) -> curr_rps(T, now()).

%% @doc Return currently used rate per second.
curr_rps(#throttle{rate=0},   _Now) -> 0;
curr_rps(#throttle{rate=R}=T,  Now) ->
  (R-calc_available(T, Now))*1000000/T#throttle.window.

now() ->
  erlang:system_time(microsecond).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% Return the number of available samples given `Now' current time.
calc_available(#throttle{rate=R, window=W, step=S} = T, Now) ->
  Diff = Now - T#throttle.next_ts,
  if Diff >= 0 -> R;
     true      -> min(R, max(0, (W+Diff) div S))
  end.

%%------------------------------------------------------------------------------
%% Unit testing
%%------------------------------------------------------------------------------

-ifdef(EUNIT).

all_test() ->
  Time = fun(TS, US) -> erlang:universaltime_to_posixtime(TS) * 1000000 + US end,

  Now = Time({{2015, 6, 1}, {11,59,58}}, 900000),
  Thr = throttle:new(10, 1000, Now),  %% Throttle 10 samples / sec
  ?assertEqual(100000, Thr#throttle.step),
  ?assertEqual(10,     throttle:available(Thr, Now)),
  {N, T1} = throttle:add(Thr, 1, Now),
  ?assertEqual(1, N),
  ?assertEqual(9, throttle:available(T1, Now)),
  ?assertEqual(1, throttle:used(T1, Now)),

  Now1 = Time({{2015, 6, 1}, {11,59,58}}, 999999),
  ?assertEqual(9, throttle:available(T1, Now1)),
  ?assertEqual(1, throttle:used(T1, Now1)),

  Now2 = Time({{2015, 6, 1}, {11,59,59}}, 0),
  ?assertEqual(10,throttle:available(T1, Now2)),
  ?assertEqual(0, throttle:used(T1, Now2)),

  Now3 = Time({{2015, 6, 1}, {12,0,0}}, 0),
  % 1 second elapsed, the throttler's interval is reset, and 10 samples are available
  ?assertEqual(10,throttle:available(T1, Now3)),
  ?assertEqual(0, throttle:used(T1, Now2)),

  {N1, T2} = throttle:add(T1, 1, Now3),
  ?assertEqual(1, N1),
  ?assertEqual(9, throttle:available(T2, Now3)),
  ?assertEqual(1, throttle:used(T2, Now3)),

  ok.

-endif.

