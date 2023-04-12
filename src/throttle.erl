%%------------------------------------------------------------------------------
%% @doc Throttle given rate over a number of seconds.
%%
%% Implementation uses time spacing reservation algorithm where each
%% allocation of samples reserves a fraction of space in the throttling
%% window. The reservation gets freed as the time goes by.  No more than
%% the `Rate' number of samples are allowed to fit in the milliseconds `Window'.
%%
%% This is an Erlang implementation of the throttling algorithm from the utxx
%% library found at this URL:
%% [https://github.com/saleyn/utxx/blob/master/include/utxx/rate_throttler.hpp]
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
-export([next_timeout/1, call/2, call/3, call/4, call/5]).
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

-type throttle_opts() :: #{
  retries     => integer(),
  retry_delay => integer(),
  blocking    => boolean()
}.
%% `retries'     - number of retries.
%% `retry_delay' - delay in milliseconds between successive retries.
%% `blocking'    - instructs to block the call if throttled.

-type throttle_result() ::
  {ok, any()} | {error, throttled | {Reason::any(), StackTrace::list()}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Create a new throttle given the `Rate' per second.
-spec new(non_neg_integer()) -> throttle().
new(Rate) ->
  new(Rate, 1000).

%% @see new/3
-spec new(non_neg_integer(), non_neg_integer()) -> throttle().
new(Rate, Window) ->
  new(Rate, Window, now()).

%% @doc Create a new throttle given the `Rate' per `Window' milliseconds.
%%      `Now' is expressesed in microseconds since epoch using `now()'.
-spec new(non_neg_integer(), non_neg_integer(), time()) -> throttle().
new(Rate, Window, Now) when is_integer(Rate), is_integer(Window), is_integer(Now) ->
  Win  = Window * 1000,
  Step = if Rate == 0 -> 0; true -> Win div Rate end,
  #throttle{rate = Rate, window = Win, step = Step, next_ts = Now}.

%% @see reset/2
reset(#throttle{} = T) ->
  reset(T, now()).

%% @doc Reset the throttle request counter
reset(#throttle{} = T, Now) when is_integer(Now) ->
  T#throttle{next_ts = Now}.

%% @doc Call the lambda <tt>F</tt>, ensuring that it's not called more
%% frequently than the throttle would allow.
%%
%% Example:
%% <code>
%% 1> T = throttle:new(10, 1000).
%% 2> lists:foldl(fun(_,{T1,A}) ->
%%      {T2,R} = throttle:call(T1, fun() -> http:get("google.com") end),
%%      {T2, [R|A]}
%%    end, {T,[]}, lists:seq(1, 100)).
%% </code>
call(T, F) ->
  call2(T, F, #{}, now()).

%% @doc Call the lambda `F', ensuring that it's not called more
%% often then the throttle would allow. `Opts' are a map of options.
%% When `{retries, R}' option is given and `R' is greater than 0, the
%% throttler will call the function `F()' up to `R' times if the `F()'
%% raises an exception.  The delay between retries is controlled by
%% the `{retry_delay, D}' options, expressed in milliseconds (default: `1')
%% between successive executions of `F()'.
%% If `F()' still raises an exception after the R's retry, that exception
%% would be reraised and it would be the responsibility of the caller
%% to handle it.
-spec call(#throttle{}, fun(() -> any()), throttle_opts()) ->
  {#throttle{}, throttle_result()}.
call(#throttle{} = T, F, Opts) when is_function(F, 0), is_map(Opts) ->
  call2(T, F, Opts, now()).

%% @doc Call M,F,A, ensuring that it's not called more frequently than the
%% throttle would allow.
%%
%% Example:
%% <code>
%% 1> T = throttle:new(10, 1000).
%% 2> lists:foldl(fun(_,{T1,A}) ->
%%      {T2,R} = throttle:call(T1, http, get, ["google.com"]),
%%      {T2, [R|A]}
%%    end, {T,[]}, lists:seq(1, 100)).
%% </code>
call(T, M,F,A) when is_atom(M), is_atom(F), is_list(A) ->
  call(T, M,F,A, #{}, now());
call(T, F, Opts, Now) when is_function(F, 0), is_map(Opts), is_integer(Now) ->
  call2(T, F, Opts, Now).

%% @doc Call M,F,A, ensuring that it's not called more frequently than the
%% throttle would allow.
call(T, M,F,A, Now) when is_integer(Now) ->
  call(T, M,F,A, #{}, Now);
call(T, M,F,A, Opts) when is_map(Opts) ->
  call(T, M,F,A, Opts, now()).

%% @doc Call M,F,A, ensuring that it's not called more frequently than the
%% throttle would allow.
-spec call(#throttle{}, atom(), atom(), [any()], non_neg_integer(), throttle_opts()) ->
  {#throttle{}, throttle_result()}.
call(#throttle{} = T, M,F,A, Opts, Now) when is_atom(M), is_atom(F), is_list(A) ->
  call2(T, fun() -> apply(M,F,A) end, Opts, Now).

-spec call2(#throttle{}, fun(() -> any()), throttle_opts(), non_neg_integer()) ->
  {#throttle{}, throttle_result()}.
call2(#throttle{} = T, F, Opts, Now) when is_integer(Now), is_function(F, 0), is_map(Opts) ->
  Retries = maps:get(retries,     Opts, 0),
  DelayMS = maps:get(retry_delay, Opts, 1),
  Block   = maps:get(blocking,    Opts, true),
  call3(T, F, Now, Block, Retries, DelayMS).

call3(T, F, Now, Block, Retries, DelayMS) ->
  case next_timeout(T, Now) of
    0 ->
      {1, T1} = add(T, 1, Now),
      Result  = try {ok, F()} catch _:R:Trace -> {retry, {R, Trace}} end,
      case Result of
        {ok, _} ->
          {T1, Result};
        {retry, _} when Retries > 0 ->
          receive after DelayMS -> ok end,
          call3(T, F, now(), Block, Retries-1, DelayMS);
        {retry, Error} ->
          {error, Error}
      end;
    _ when not Block ->
      {error, throttled};
    WaitMS ->
      receive after WaitMS -> ok end,
      call3(T, F, now(), Block, Retries, DelayMS)
  end.

%% @doc Add one sample to the throttle
add(T)          -> add(T, 1).

%% @doc Add `Samples' to the throttle
add(T, Samples) -> add(T, Samples, now()).

%% @doc Add `Samples' to the throtlle.
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

%% @doc Return the number of available samples given `Now' current time.
available(#throttle{rate=0}=T,_Now) -> T#throttle.window;
available(#throttle{}      =T, Now) -> calc_available(T, Now).

%% @see used/2
used(T) -> used(T, now()).

%% @doc Return the number of used samples given `a_now' current time.
used(#throttle{rate = 0},     _Now) -> 0;
used(#throttle{rate = R} = T,  Now) -> R-calc_available(T, Now).

%% @doc Return the number of milliseconds to wait until the throttling
%% threshold is satisfied to fit another sample.
next_timeout(T) -> next_timeout(T, now()).
next_timeout(#throttle{next_ts = TS, step = Step, window = Win}, Now) ->
  NextTS    = TS     + Step,
  NowNextTS = Now    + Win,
  Diff      = NextTS - NowNextTS,
  if
    Diff =< 0 -> 0;
    true      -> ceil(Diff / 1000)
  end.
  
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
time(TS, US) -> erlang:universaltime_to_posixtime(TS) * 1000000 + US.

all_test() ->

  Now = time({{2015, 6, 1}, {11,59,58}}, 900000),
  Thr = throttle:new(10, 1000, Now),  %% Throttle 10 samples / sec
  ?assertEqual(100000, Thr#throttle.step),
  ?assertEqual(10,     throttle:available(Thr, Now)),
  {N, T1} = throttle:add(Thr, 1, Now),
  ?assertEqual(1, N),
  ?assertEqual(9, throttle:available(T1, Now)),
  ?assertEqual(1, throttle:used(T1, Now)),

  Now1 = time({{2015, 6, 1}, {11,59,58}}, 999999),
  ?assertEqual(9, throttle:available(T1, Now1)),
  ?assertEqual(1, throttle:used(T1, Now1)),

  Now2 = time({{2015, 6, 1}, {11,59,59}}, 0),
  ?assertEqual(10,throttle:available(T1, Now2)),
  ?assertEqual(0, throttle:used(T1, Now2)),

  Now3 = time({{2015, 6, 1}, {12,0,0}}, 0),
  % 1 second elapsed, the throttler's interval is reset, and 10 samples are available
  ?assertEqual(10,throttle:available(T1, Now3)),
  ?assertEqual(0, throttle:used(T1, Now2)),

  {N1, T2} = throttle:add(T1, 1, Now3),
  ?assertEqual(1, N1),
  ?assertEqual(9, throttle:available(T2, Now3)),
  ?assertEqual(1, throttle:used(T2, Now3)),

  TT = throttle:new(5, 1000),
  Now4 = now(),
  {_,RR} = lists:foldl(fun(_,{TT1,A}) ->
    {TT2, R} = throttle:call(TT1, erlang, system_time, [millisecond]),
    {TT2, [R|A]}
  end, {TT, []}, lists:seq(1, 15)),
  Now5 = now(),

  ?assertEqual(15, length(RR)),
  ?assert(Now5 - Now4 >= 2000000),

  TT2 = throttle:new(5, 1000),
  {_,RR1} = lists:foldl(fun(_,{TT3,A}) ->
    {TT4, R} = throttle:call(TT3, fun() -> erlang:system_time(millisecond) end),
    {TT4, [R|A]}
  end, {TT2, []}, lists:seq(1, 15)),
  Now6 = now(),

  ?assertEqual(15, length(RR1)),
  ?assert(Now6 - Now5 >= 2000000),

  ok.

retry_ok_test() ->
  Now  = now(),
  Opts = #{retries => 3, retry_delay => 100},
  TT   = throttle:new(10, 1000),
  Inc  = fun(undefined) -> 1; (N) -> N+1 end,
  F    = fun() ->
    case get(count) of
      2 -> success;
      N -> put(count, Inc(N)), erlang:error(exception)
    end
  end,
  {_, R} = throttle:call(TT, F, Opts, Now),
  erase(count),
  ?assertEqual({ok, success}, R).

retry_fail_test() ->
  Now  = now(),
  Opts = #{retries => 3, retry_delay => 100},
  TT   = throttle:new(10, 1000),
  {error, {exception, _}} = throttle:call(TT, fun() -> erlang:error(exception) end, Opts, Now),
  Diff = now() - Now,
  %% Expected delay should be around 300ms
  ?assert(Diff >= 300000),
  ?assert(Diff <  400000).

retry_fail_no_block_test() ->
  Now  = now(),
  Opts = #{retries => 3, retry_delay => 0},
  TT   = throttle:new(10, 1000),
  {error, {exception, _}} = throttle:call(TT, fun() -> erlang:error(exception) end, Opts, Now),
  Diff = now() - Now,
  %% Expected delay should be around 0ms
  ?assert(Diff >   0),
  ?assert(Diff < 100).

-endif.

