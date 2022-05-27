%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc Utility functions for date/time operations.
%%%
%%% @author  Serge Aleynikov <saleyn@gmail.com>
%%% @version $Revision$
%%%          $Date$
%%% @end
%%%------------------------------------------------------------------------
%%% Created 2006-10-18 Serge Aleynikov <saleyn@gmail.com>
%%% $URL$
%%%------------------------------------------------------------------------
-module(datetime).

-export([local_seconds_since_epoch/1, local_time/1, 
         filename/2, datetime_to_now/1, localtime_to_now/1, 
         seconds_since_epoch/1, seconds_since_epoch_to_now/1,
         datetime_to_string/1,
         time_to_string/1, time_to_string/2, time_to_msec/1, time_to_sec/1,
         string_to_time/1, parse_time/1, seconds_to_string/1]).

-define(SECS_SINCE_UTC_EPOCH, 62167219200). % calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})

%%-------------------------------------------------------------------------
%% @spec (DateTime::datetime()) -> Seconds::integer()
%% @doc Returns number of seconds since epoch to a given `DateTime' 
%%      expressed in local time.
%% @end
%%-------------------------------------------------------------------------
local_seconds_since_epoch(DateTime) ->
    [DT|_] = calendar:local_time_to_universal_time_dst(DateTime),
    GTime  = calendar:datetime_to_gregorian_seconds(DT),
    %Local  = calendar:datetime_to_gregorian_seconds(DateTime),
    %TZdiff = GTime - Local,
    %GTime - ?SECS_SINCE_UTC_EPOCH - TZdiff.
    GTime - ?SECS_SINCE_UTC_EPOCH.
    
%%-------------------------------------------------------------------------
%% @spec (Now::now()) -> {Seconds, MicroSeconds}
%% @doc Returns current local time as the number of {seconds, microseconds} since midnight.
%% @end
%%-------------------------------------------------------------------------
local_time(Now) ->
    Seconds = calendar:time_to_seconds(element(2,calendar:now_to_local_time(Now))),
    {Seconds, element(3,Now)}.

%%-------------------------------------------------------------------------
%% @spec (DateTime::datetime()) -> Seconds::integer()
%% @doc Returns number of seconds since epoch to a given `DateTime' 
%%      expressed in UTC.
%% @end
%%-------------------------------------------------------------------------
seconds_since_epoch(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?SECS_SINCE_UTC_EPOCH.

%%-------------------------------------------------------------------------
%% @spec (Secs::integer()) -> Nowtime::now()
%% @doc Convert seconds since UTC epoch to now() format.
%% @end
%%-------------------------------------------------------------------------
seconds_since_epoch_to_now(Secs) ->
    {Secs div 1000000, Secs rem 1000000, 0}.

%%-------------------------------------------------------------------------
%% @spec (DateTime::datetime()) -> Now::now()
%% @doc Convert UTC `DateTime' to now() format.
%% @end
%%-------------------------------------------------------------------------
datetime_to_now(DateTime) ->
    Secs = calendar:datetime_to_gregorian_seconds(DateTime) - ?SECS_SINCE_UTC_EPOCH,
    {Secs div 1000000, Secs rem 1000000, 0}.

%%-------------------------------------------------------------------------
%% @spec (DateTime::datetime()) -> Now::now()
%% @doc Convert local `DateTime' to now() format.
%% @end
%%-------------------------------------------------------------------------
localtime_to_now(DateTime) ->
    [DT | _] = calendar:local_time_to_universal_time_dst(DateTime),
    datetime_to_now(DT).

%%-------------------------------------------------------------------------
%% @spec (Filename::string(), Date::date()) -> string()
%% @doc Create a filename by appending "YYYYMMDD" to `Filename'.
%% @end
%%-------------------------------------------------------------------------
filename(Filename, {Y, M, D}) ->
    BaseName = conf_utils:os_path(Filename),
    lists:flatten([BaseName, io_lib:format(".~w~.2.0w~.2.0w", [Y, M, D])]).
    
%%-------------------------------------------------------------------------
%% @spec (DateTime::datetime()) -> string()
%% @doc Convert date and time to "YYYY-MM-DD HH:MM:SS".
%% @end
%%-------------------------------------------------------------------------
datetime_to_string({{Y,Mo,D},{H,M,S}}) ->
    lists:flatten([integer_to_list(Y), $-, i2l(Mo,2), $-, i2l(D,2), $ , 
                   i2l(H,2),$:,i2l(M,2),$:,i2l(S,2)]).

%%-------------------------------------------------------------------------
%% @spec (DateTime::datetime()) -> string()
%% @doc Convert date and time to "HHMMSS".
%% @end
%%-------------------------------------------------------------------------
time_to_string({H,M,S}) ->
    lists:flatten([i2l(H,2),i2l(M,2),i2l(S,2)]).

%%-------------------------------------------------------------------------
%% @spec (DateTime::datetime(), Sep::char()) -> string()
%% @doc Convert date and time to "HH:MM:SS" using separator `Sep'.
%% @end
%%-------------------------------------------------------------------------
time_to_string({H,M,S}, Sep) when is_integer(Sep) ->
    lists:flatten([i2l(H,2),Sep,i2l(M,2),Sep,i2l(S,2)]).

%%-------------------------------------------------------------------------
%% @doc Convert time to milliseconds since midnight
%% @end
%%-------------------------------------------------------------------------
-spec time_to_msec(erlang:time()) -> integer().
time_to_msec({H,M,S}) ->
    (H*3600 + M*60 + S)*1000.

%%-------------------------------------------------------------------------
%% @doc Convert time to seconds since midnight
%% @end
%%-------------------------------------------------------------------------
-spec time_to_sec(erlang:time()) -> integer().
time_to_sec({H,M,S}) ->
    H*3600 + M*60 + S.

%%-------------------------------------------------------------------------
%% @spec (Str::string()) -> time()
%% @doc Convert a string "HH:MM:SS" or "HH:MM" to {H, M, S}. Throw 
%%      {invalid_time, Time} on error.
%% @end
%%-------------------------------------------------------------------------
string_to_time(Str) when is_list(Str) ->
    case [list_to_integer(I) || I <- string:tokens(Str, ":")] of
    [H, M, S] ->
        valid_time({H, M, S});
    [H, M] ->
        valid_time({H, M, 0});
    Other ->
        throw({invalid_time, Other})
    end.

valid_time(T={H, M, S}) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 ->
    T;
valid_time(T) ->
    throw({invalid_time, T}).

%%-------------------------------------------------------------------------
%% @spec (Time) -> time()
%%          Time = time() | string()
%% @doc Convert a string "HH:MM:SS" or "HH:MM" to {H, M, S}. If time is 
%%      already a 3-element tuple, just check for valid time. Throw 
%%      {invalid_time, Time} on error.
%% @end
%%-------------------------------------------------------------------------
parse_time({_, _, _}=Time) ->
    valid_time(Time);
parse_time(Time) when is_list(Time) ->
    string_to_time(Time).
    
%%-------------------------------------------------------------------------
%% @spec (Seconds::integer()) -> string()
%% @doc Convert seconds since midnight to "HH:MM:SS".
%% @end
%%-------------------------------------------------------------------------
seconds_to_string(Secs) when is_integer(Secs) ->
    time_to_string(calendar:seconds_to_time(Secs), $:).

i2l(I,N) ->
    string:right(integer_to_list(I), N, $0).
