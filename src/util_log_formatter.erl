%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(util_log_formatter).

%% @doc Log formatter
%% Derived from //kernel/logger/logger_formatter
%%
%% This implementation is copied from logger_fomatter.erl and adds the
%% following features:
%%   * config option: time_offset = none
%%       when provided, no time zone offset will be written to the log.
%%   * time_unit = second | millisecond | microsecond
%%       controls time stamp granularity in the log.
%%   * report_prefix = string()
%%       inserts given prefix before `msg' for logging a report (default: "\n").
%%   * report_term_depth = integer()
%%       max depth of terms included in the reports. Use `infinity' for
%%       unlimited (default: 50)
%%
%%   Report printing is modified to align keys in a report to the right.
%%
%%   Additional template formatting atoms:
%%   * lev - prints "[X]" to the log to indicate the log level, where
%%           `X` is the first capitalized letter of the log level.
%%   * 'LEVEL' - same as 'level' but is printed in upper case.
%%   * modline - prints 'Module:Line' to the log.
%%   * regpid  - prints:
%%                 `*' - if the registered name of the caller's pid
%%                       matches caller's module name.
%%                 `RegisteredName' - of the calling process
%%                 `X.Y.Z' - pid of the caller with leading `0.' stripped.
%%   * regname - prints `<RegisteredName>' of the process or its pid
%%               if the process is not registered
%% @end

-export([format/2]).
-export([check_config/1]).

-include_lib("kernel/src/logger_internal.hrl").

%%%-----------------------------------------------------------------
%%% Types
-type config() :: #{chars_limit       => pos_integer() | unlimited,
                    depth             => pos_integer() | unlimited,
                    legacy_header     => boolean(),
                    max_size          => pos_integer() | unlimited,
                    report_cb         => logger:report_cb(),
                    single_line       => boolean(),
                    template          => template(),
                    time_designator   => byte(),
                    time_offset       => integer() | [byte()]
                                       | none,  %% Extension
                    %%--- Extension
                    time_unit         => atom(),
                    report_prefix     => boolean(),
                    report_term_depth => integer()
                   }.
-type template() :: [metakey() | {metakey(),template(),template()} | string()].
-type metakey() :: atom() | [atom()].

%%%-----------------------------------------------------------------
%%% API
-spec format(LogEvent,Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: config().
format(#{level:=Level,msg:=Msg0,meta:=Meta},Config0)
  when is_map(Config0) ->
    Config1 = add_default_config(Config0),
    #{single_line:=SingleLine, report_prefix:=RepPfx} = Config1,
    Config = if SingleLine ->
               Pfx = [C || C <- RepPfx, not lists:member(C, [$\n, $\r])],
               Config1#{report_prefix=>Pfx};
             true ->
               Config1
             end,
    Meta0 = maybe_add_legacy_header(Level,Meta,Config),
    Meta1 = case Msg0 of
                {report, _} -> Meta0#{report => true};
                _           -> Meta0
            end,
    Meta2 = case Config of
                #{report_cb := RCB} when is_function(RCB, 1); is_function(RCB, 2) ->
                    Meta1#{report_cb => RCB};
                #{report_term_depth:=MDep} ->
                    Meta1#{report_cb =>
                        fun(R) ->
                          RR = case R of
                                 #{label:=_, report:=R1} when is_map(R1) ->
                                   maps:to_list(R1);
                                 #{label:=_, report:=R1} when is_list(R1) ->
                                   R1;
                                 _ when is_map(R) ->
                                   maps:to_list(R);
                                 _ when is_list(R) ->
                                   R
                               end,
                          try
                            MLen = case [T || {T,_} <- RR] of
                                    [] -> 0;
                                    L  -> lists:max([length(lists:flatten(io_lib:format("~tp",[I]))) || I <- L])
                                   end,
                            format_report(integer_to_list(MLen), MDep, RR)
                          catch _:E ->
                            throw({invalid_report, E, RR})
                          end
                        end}
            end,
    Template = maps:get(template,Config),
    {BT,AT0} = lists:splitwith(fun(msg) -> false; (_) -> true end, Template),
    {DoMsg,AT} =
        case AT0 of
            [msg|Rest] -> {true,Rest};
            _ ->{false,AT0}
        end,
    B = do_format(Level,Meta2,BT,Config),
    A = do_format(Level,Meta2,AT,Config),
    MsgStr =
        if DoMsg ->
                Config1 =
                    case maps:get(chars_limit,Config) of
                        unlimited ->
                            Config;
                        Size0 ->
                            Size =
                                case Size0 - io_lib:chars_length([B,A]) of
                                    S when S>=0 -> S;
                                    _ -> 0
                                end,
                            Config#{chars_limit=>Size}
                    end,
                MsgStr0 = format_msg(Msg0,Meta2,Config1),
                case maps:get(single_line,Config) of
                    true ->
                        %% Trim leading and trailing whitespaces, and replace
                        %% newlines with ", "
                        T = lists:reverse(
                              trim(
                                lists:reverse(
                                  trim(MsgStr0,false)),true)),
                        re:replace(T,",?\r?\n\s*",", ",
                                   [{return,list},global,unicode]);
                    _false ->
                        MsgStr0
                end;
           true ->
                ""
        end,
    truncate(B,MsgStr,A,maps:get(max_size,Config)).

trim([H|T],Rev) when H==$\s; H==$\r; H==$\n ->
    trim(T,Rev);
trim([H|T],false) when is_list(H) ->
    case trim(H,false) of
        [] ->
            trim(T,false);
        TrimmedH ->
            [TrimmedH|T]
    end;
trim([H|T],true) when is_list(H) ->
    case trim(lists:reverse(H),true) of
        [] ->
            trim(T,true);
        TrimmedH ->
            [lists:reverse(TrimmedH)|T]
    end;
trim(String,_) ->
    String.

do_format(Level,Data,[lev|Format],Config) ->
    [level_to_chr(Level)|do_format(Level,Data,Format,Config)];
do_format(Level,Data,[level|Format],Config) ->
    [to_string(level,Level,Config)|do_format(Level,Data,Format,Config)];
do_format(Level,Data,['LEVEL'|Format],Config) ->
    [string:uppercase(to_string(level,Level,Config))|do_format(Level,Data,Format,Config)];
do_format(Level,#{pid:=Pid}=Data,[I|Format],Config) when I==regname; I==regpid ->
    [format_regname(I,Pid,Data)|do_format(Level,Data,Format,Config)];

do_format(Level,Data,[{Key,IfExist,Else}|Format],Config) ->
    String =
        case value(Key,Data) of
            {ok,Value} -> do_format(Level,Data#{Key=>Value},IfExist,Config);
            error -> do_format(Level,Data,Else,Config)
        end,
    [String|do_format(Level,Data,Format,Config)];
do_format(Level,Data,[Key|Format],Config)
  when is_atom(Key) orelse
       (is_list(Key) andalso is_atom(hd(Key))) ->
    String =
        case value(Key,Data) of
            {ok,Value} -> to_string(Key,Value,Config);
            error -> ""
        end,
    [String|do_format(Level,Data,Format,Config)];
do_format(Level,Data,[Str|Format],Config) ->
    [Str|do_format(Level,Data,Format,Config)];
do_format(_Level,_Data,[],_Config) ->
    [].

value(modline,#{mfa:={M,_,_},line:=L}) ->
    {ok, [atom_to_list(M),$:,integer_to_list(L)]};
value(Key,Meta) when is_map_key(Key,Meta) ->
    {ok,maps:get(Key,Meta)};
value([Key|Keys],Meta) when is_map_key(Key,Meta) ->
    value(Keys,maps:get(Key,Meta));
value([],Value) ->
    {ok,Value};
value(_,_) ->
    error.

to_string(time,Time,Config) ->
    format_time(Time,Config);
to_string(mfa,MFA,Config) ->
    format_mfa(MFA,Config);
to_string(_,Value,Config) ->
    to_string(Value,Config).

to_string(X,_) when is_atom(X) ->
    atom_to_list(X);
to_string(X,_) when is_integer(X) ->
    integer_to_list(X);
to_string(X,_) when is_pid(X) ->
    pid_to_list(X,false);
to_string(X,_) when is_reference(X) ->
    ref_to_list(X);
to_string(X,Config) when is_list(X) ->
    case printable_list(lists:flatten(X)) of
        true -> X;
        _ -> io_lib:format(p(Config),[X])
    end;
to_string(X,Config) ->
    io_lib:format(p(Config),[X]).

format_regname(Key,Pid,Data) ->
    case erlang:process_info(Pid, registered_name) of
        {_, Name} when Key==regpid, is_map_key(mfa,Data) ->
            #{mfa := {M,_,_}} = Data,
            if
                M == Name -> "*";
                true      -> atom_to_list(Name)
            end;
        {_, Name} -> atom_to_list(Name);
        _         -> pid_to_list(Pid,true)
    end.

pid_to_list(Pid,false) ->
    pid_to_list(Pid);
pid_to_list(Pid,true)  ->
    RemoveLast = fun G([_]) -> []; G([H|T]) -> [H|G(T)] end,
    RemoveLast(case pid_to_list(Pid) of
                  "<0." ++ Rest -> Rest;
                  [_|Rest]      -> Rest
               end).

printable_list([]) ->
    false;
printable_list(X) ->
    io_lib:printable_list(X).

format_msg({string,Chardata},Meta,Config) ->
    format_msg({"~ts",[Chardata]},Meta,Config);
format_msg({report,_}=Msg,Meta,#{report_cb:=Fun}=Config)
  when is_function(Fun,1); is_function(Fun,2) ->
    format_msg(Msg,Meta#{report=>true,report_cb=>Fun},maps:remove(report_cb,Config));
format_msg({report,Report},#{report_cb:=Fun}=Meta,Config) when is_function(Fun,1) ->
    try Fun(Report) of
        {Format,Args} when is_list(Format), is_list(Args) ->
            format_msg({Format,Args},maps:remove(report_cb,Meta#{report=>true}),Config);
        Other ->
            P = p(Config),
            format_msg({"REPORT_CB/1 ERROR: "++P++"; Returned: "++P,
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            P = p(Config),
            format_msg({"REPORT_CB/1 CRASH: "++P++"; Reason: "++P,
                        [Report,{C,R,S}]},
                       Meta,Config)
    end;
format_msg({report,Report},#{report_cb:=Fun}=Meta,Config) when is_function(Fun,2) ->
    try Fun(Report,maps:with([depth,chars_limit,single_line],Config)) of
        Chardata when ?IS_STRING(Chardata) ->
            try chardata_to_list(Chardata) % already size limited by report_cb
            catch _:_ ->
                    P = p(Config),
                    format_msg({"REPORT_CB/2 ERROR: "++P++"; Returned: "++P,
                                [Report,Chardata]},Meta,Config)
            end;
        Other ->
            P = p(Config),
            format_msg({"REPORT_CB/2 ERROR: "++P++"; Returned: "++P,
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            P = p(Config),
            format_msg({"REPORT_CB/2 CRASH: "++P++"; Reason: "++P,
                        [Report,{C,R,logger:filter_stacktrace(?MODULE,S)}]},
                       Meta,Config)
    end;
format_msg(Msg,Meta,#{depth:=Depth,chars_limit:=CharsLimit,
                      single_line:=Single,report_prefix:=RepPfx}) ->
    Opts = chars_limit_to_opts(CharsLimit),
    format_msg(Msg, Depth, Opts, Single, maps:get(report,Meta,false), RepPfx).

chars_limit_to_opts(unlimited) -> [];
chars_limit_to_opts(CharsLimit) -> [{chars_limit,CharsLimit}].

format_msg({Format0,Args},Depth,Opts,Single,WasReport,RepPrefix) ->
    try
        Format1 = io_lib:scan_format(Format0, Args),
        Format  = reformat(Format1, Depth, Single),
        Res     = io_lib:build_text(Format,Opts),
        if WasReport and not Single ->
            [RepPrefix | Res];
        true ->
            Res
        end
    catch C:R:S ->
            P = p(Single),
            FormatError = "FORMAT ERROR: "++P++" - "++P,
            case Format0 of
                FormatError ->
                    %% already been here - avoid failing cyclically
                    erlang:raise(C,R,S);
                _ ->
                    format_msg({FormatError,[Format0,Args]},
                               Depth,Opts,Single,WasReport,RepPrefix)
            end
    end.

-spec format_report(MaxTagLen::integer(),
                    MaxTermDepth::integer()|infinity,
                    Report) -> FormatArgs
			when Report     :: logger:report(),
           FormatArgs :: {io:format(),[term()]}.
format_report(MaxTagLen, MaxDepth, Report) when is_map(Report) ->
    format_report(MaxTagLen, MaxDepth, maps:to_list(Report));
format_report(MaxTagLen, MaxDepth, Report) when is_list(Report) ->
    case lists:flatten(Report) of
        [] ->
            {"~tp",[[]]};
        FlatList ->
            case io_lib:printable_unicode_list(FlatList) of
                true ->
                    {"~ts",[FlatList]};
                false ->
                    format_term_list(MaxTagLen, MaxDepth, Report,[],[])
            end
    end;
format_report(_MaxTagLen, infinity, Report) ->
    {"~tp",[Report]};
format_report(_MaxTagLen, MaxDepth, Report) ->
    {"~tP",[Report, MaxDepth]}.

format_term_list(MaxTagLen,MaxDepth, [{Tag,Data}|T],Format,Args) ->
    {PorS, ArgsP} =
        case string_p(Data) of
            true  -> {"s", []};
            false -> {"P", [MaxDepth]}
        end,
    format_term_list(MaxTagLen,MaxDepth, T,
        ["    ~"++MaxTagLen++"ts: ~t"++PorS|Format],ArgsP++[Data,to_string(Tag)|Args]);
format_term_list(MaxTagLen,MaxDepth, [Data|T],Format,Args) ->
    format_term_list(MaxTagLen,MaxDepth, T,["    ~tp"|Format],[Data|Args]);
format_term_list(_MaxTagLen,_MaxDepth, [],Format,Args) ->
    {lists:flatten(lists:join($\n,lists:reverse(Format))),lists:reverse(Args)}.

string_p([])                 -> true;
string_p(L) when is_list(L)  -> io_lib:printable_unicode_list(lists:flatten(L));
string_p(_)                  -> false.

to_string(S) when is_list(S) -> S;
to_string(S) when is_atom(S) -> atom_to_list(S);
to_string(S)                 -> lists:flatten(io_lib:format("~tp", [S])).

reformat(Format,unlimited,false) ->
    Format;
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $p ->
    [limit_depth(M#{width => 0}, Depth)|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $P ->
    [M#{width => 0}|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, Single) when C =:= $p; C =:= $w ->
    [limit_depth(M, Depth)|reformat(T, Depth, Single)];
reformat([H|T], Depth, Single) ->
    [H|reformat(T, Depth, Single)];
reformat([], _, _) ->
    [].

limit_depth(M0, unlimited) ->
    M0;
limit_depth(#{control_char:=C0, args:=Args}=M0, Depth) ->
    C = C0 - ($a - $A),				%To uppercase.
    M0#{control_char:=C,args:=Args++[Depth]}.

chardata_to_list(Chardata) ->
    case unicode:characters_to_list(Chardata,unicode) of
        List when is_list(List) ->
            List;
        Error ->
            throw(Error)
    end.

truncate(B,Msg,A,unlimited) ->
    [B,Msg,A];
truncate(B,Msg,A,Size) ->
    String = [B,Msg,A],
    Length = io_lib:chars_length(String),
    if Length>Size ->
            {Last,FlatString} =
                case A of
                    [] ->
                        case Msg of
                            [] ->
                                {get_last(B),lists:flatten(B)};
                            _ ->
                                {get_last(Msg),lists:flatten([B,Msg])}
                        end;
                    _ ->
                        {get_last(A),lists:flatten(String)}
                end,
            case Last of
                $\n->
                    lists:sublist(FlatString,1,Size-4)++"...\n";
                _ ->
                    lists:sublist(FlatString,1,Size-3)++"..."
            end;
       true ->
            String
    end.

get_last(L) ->
    get_first(lists:reverse(L)).

get_first([]) ->
    error;
get_first([C|_]) when is_integer(C) ->
    C;
get_first([L|Rest]) when is_list(L) ->
    case get_last(L) of
        error -> get_first(Rest);
        First -> First
    end.

i2l(I) when I < 10     -> [$0, $0+I];
i2l(I)                 -> integer_to_list(I).
i3l(I) when I < 100    -> [$0 | i2l(I)];
i3l(I)                 -> integer_to_list(I).
i4l(I) when I < 1000   -> [$0 | i3l(I)];
i4l(I)                 -> integer_to_list(I).
i5l(I) when I < 10000  -> [$0 | i4l(I)];
i5l(I)                 -> integer_to_list(I).
i6l(I) when I < 100000 -> [$0 | i5l(I)];
i6l(I)                 -> integer_to_list(I).

format_time_unit(SysTime, millisecond) -> [$. | i3l((SysTime div 1000) rem 1000)];
format_time_unit(SysTime, microsecond) -> [$. | i6l(SysTime rem 1000000)];
format_time_unit(_SysTime, _)          -> [].

usec_to_unit(USec, microsecond) -> USec;
usec_to_unit(USec, millisecond) -> USec div 1000;
usec_to_unit(USec, _)           -> USec div 1000000.

%% SysTime is the system time in microseconds
format_time(SysTime,#{time_offset:=none,time_designator:=Des,time_unit:=Unit}=Config)
  when is_integer(SysTime) ->
    Sec = SysTime div 1000000,
    UniversalTime = erlang:posixtime_to_universaltime(Sec),
    {{{Y, M, D}, {H, Mi, S}},Sfx} =
        case offset_to_utc(maps:get(time_offset,Config)) of
            true -> {UniversalTime, "Z"};
            _    -> {erlang:universaltime_to_localtime(UniversalTime), ""}
        end,
    [integer_to_list(Y), $-, i2l(M), $-, i2l(D), Des,
     i2l(H), $:, i2l(Mi), $:, i2l(S), format_time_unit(SysTime, Unit), Sfx];
    
format_time(SysTime,#{time_offset:=Offset,time_designator:=Des,time_unit:=Unit})
  when is_integer(SysTime) ->
    calendar:system_time_to_rfc3339(usec_to_unit(SysTime, Unit),
                                    [{unit,Unit},
                                     {offset,Offset},
                                     {time_designator,Des}]).

%% SysTime is the system time in microseconds
timestamp_to_datetimemicro(SysTime,Config) when is_integer(SysTime) ->
    Micro = SysTime rem 1000000,
    Sec = SysTime div 1000000,
    UniversalTime =  erlang:posixtime_to_universaltime(Sec),
    {{Date,Time},UtcStr} =
        case offset_to_utc(maps:get(time_offset,Config)) of
            true -> {UniversalTime,"UTC "};
            _ -> {erlang:universaltime_to_localtime(UniversalTime),""}
        end,
    {Date,Time,Micro,UtcStr}.

format_mfa({M,F,A},_) when is_atom(M), is_atom(F), is_integer(A) ->
    atom_to_list(M)++":"++atom_to_list(F)++"/"++integer_to_list(A);
format_mfa({M,F,A},Config) when is_atom(M), is_atom(F), is_list(A) ->
    format_mfa({M,F,length(A)},Config);
format_mfa(MFA,Config) ->
    to_string(MFA,Config).

maybe_add_legacy_header(Level,
                        #{time:=Timestamp}=Meta,
                        #{legacy_header:=true, time_unit:=Unit}=Config) ->
    #{title:=Title}=MyMeta = add_legacy_title(Level,Meta,Config),
    {{Y,Mo,D},{H,Mi,S},Micro,UtcStr} =
        timestamp_to_datetimemicro(Timestamp,Config),
    Header =
        io_lib:format("=~ts==== ~w-~s-~4w::~2..0w:~2..0w:~2..0w.~*..0w ~s===",
                      [Title,D,month(Mo),Y,H,Mi,S,time_fraction_width(Unit),Micro,UtcStr]),
    Meta#{?MODULE=>MyMeta#{header=>Header}};
maybe_add_legacy_header(_,Meta,_) ->
    Meta.

add_legacy_title(_Level,#{?MODULE:=#{title:=_}=MyMeta},_) ->
    MyMeta;
add_legacy_title(Level,Meta,Config) ->
    case maps:get(?MODULE,Meta,#{}) of
        #{title:=_}=MyMeta ->
            MyMeta;
        MyMeta ->
            TitleLevel =
                case (Level=:=notice andalso maps:find(error_logger,Meta)) of
                    {ok,_} ->
                        maps:get(error_logger_notice_header,Config);
                    _ ->
                        Level
                end,
            Title = string:uppercase(atom_to_list(TitleLevel)) ++ " REPORT",
            MyMeta#{title=>Title}
    end.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% Ensure that all valid configuration parameters exist in the final
%% configuration map
add_default_config(Config0) ->
    Default =
        #{chars_limit=>unlimited,
          error_logger_notice_header=>info,
          legacy_header=>false,
          single_line=>true,
          time_designator=>$T,
          report_prefix=>"",
          report_term_depth=>50},
    MaxSize = get_max_size(maps:get(max_size,Config0,undefined)),
    Depth = get_depth(maps:get(depth,Config0,undefined)),
    Offset = get_offset(maps:get(time_offset,Config0,undefined)),
    add_default_template(maps:merge(Default,Config0#{max_size=>MaxSize,
                                                     depth=>Depth,
                                                     time_offset=>Offset})).

add_default_template(#{template:=_}=Config) ->
    Config;
add_default_template(Config) ->
    Config#{template=>default_template(Config)}.

default_template(#{legacy_header:=true}) ->
    ?DEFAULT_FORMAT_TEMPLATE_HEADER;
default_template(#{single_line:=true}) ->
    ?DEFAULT_FORMAT_TEMPLATE_SINGLE;
default_template(_) ->
    ?DEFAULT_FORMAT_TEMPLATE.

get_max_size(undefined) ->
    unlimited;
get_max_size(S) ->
    max(10,S).

get_depth(undefined) ->
    error_logger:get_format_depth();
get_depth(S) ->
    max(5,S).

get_offset(undefined) ->
    utc_to_offset(get_utc_config());
get_offset(Offset) ->
    Offset.

utc_to_offset(true) ->
    "Z";
utc_to_offset(false) ->
    "".

get_utc_config() ->
    %% SASL utc_log overrides stdlib config - in order to have uniform
    %% timestamps in log messages
    case application:get_env(sasl, utc_log) of
        {ok, Val} when is_boolean(Val) -> Val;
        _ ->
            case application:get_env(stdlib, utc_log) of
                {ok, Val} when is_boolean(Val) -> Val;
                _ -> false
            end
    end.

offset_to_utc(Z) when Z=:=0; Z=:="z"; Z=:="Z" ->
    true;
offset_to_utc([$+|Tz]) ->
    case io_lib:fread("~d:~d", Tz) of
        {ok, [0, 0], []} ->
            true;
        _ ->
            false
    end;
offset_to_utc(_) ->
    false.

-spec check_config(Config) -> ok | {error,term()} when
      Config :: config().
check_config(Config) when is_map(Config) ->
    do_check_config(maps:to_list(Config));
check_config(Config) ->
    {error,{invalid_formatter_config,?MODULE,Config}}.

do_check_config([{Type,L}|Config]) when Type == chars_limit;
                                        Type == depth;
                                        Type == max_size ->
    case check_limit(L) of
        ok -> do_check_config(Config);
        error -> {error,{invalid_formatter_config,?MODULE,{Type,L}}}
    end;
do_check_config([{single_line,SL}|Config]) when is_boolean(SL) ->
    do_check_config(Config);
do_check_config([{legacy_header,LH}|Config]) when is_boolean(LH) ->
    do_check_config(Config);
do_check_config([{error_logger_notice_header,ELNH}|Config]) when ELNH == info;
                                                                 ELNH == notice ->
    do_check_config(Config);
do_check_config([{report_cb,RCB}|Config]) when is_function(RCB,1);
                                               is_function(RCB,2) ->
    do_check_config(Config);
do_check_config([{template,T}|Config]) ->
    case check_template(T) of
        ok -> do_check_config(Config);
        error -> {error,{invalid_formatter_template,?MODULE,T}}
    end;
do_check_config([{time_offset,Offset}|Config]) ->
    case check_offset(Offset) of
        ok ->
            do_check_config(Config);
        error ->
            {error,{invalid_formatter_config,?MODULE,{time_offset,Offset}}}
    end;
do_check_config([{time_unit,Unit}|Config]) ->
    case lists:member(Unit, [second,millisecond,microsecond]) of
        true ->
            do_check_config(Config);
        false ->
            {error,{invalid_formatter_config,?MODULE,{time_util, Unit}}}
    end;
do_check_config([{report_prefix,Pfx}|Config]) when is_list(Pfx) ->
    case io_lib:printable_latin1_list(Pfx) of
        true ->
            do_check_config(Config);
        false ->
            {error,{invalid_formatter_config,?MODULE,{report_prefix, Pfx}}}
    end;
do_check_config([{report_term_depth,I}|Config]) when is_integer(I), I > 0; I == infinity ->
    do_check_config(Config);
do_check_config([{time_designator,Char}|Config]) when Char>=0, Char=<255 ->
    case io_lib:printable_latin1_list([Char]) of
        true ->
            do_check_config(Config);
        false ->
            {error,{invalid_formatter_config,?MODULE,{time_designator,Char}}}
    end;
do_check_config([C|_]) ->
    {error,{invalid_formatter_config,?MODULE,C}};
do_check_config([]) ->
    ok.

check_limit(L) when is_integer(L), L>0 ->
    ok;
check_limit(unlimited) ->
    ok;
check_limit(_) ->
    error.

check_template([Key|T]) when is_atom(Key) ->
    check_template(T);
check_template([Key|T]) when is_list(Key), is_atom(hd(Key)) ->
    case lists:all(fun(X) when is_atom(X) -> true;
                      (_) -> false
                   end,
                   Key) of
        true ->
            check_template(T);
        false ->
            error
    end;
check_template([{Key,IfExist,Else}|T])
  when is_atom(Key) orelse
       (is_list(Key) andalso is_atom(hd(Key))) ->
    case check_template(IfExist) of
        ok ->
            case check_template(Else) of
                ok ->
                    check_template(T);
                error ->
                    error
            end;
        error ->
            error
    end;
check_template([Str|T]) when is_list(Str) ->
    case io_lib:printable_unicode_list(Str) of
        true -> check_template(T);
        false -> error
    end;
check_template([]) ->
    ok;
check_template(_) ->
    error.

check_offset(none) ->
    ok;
check_offset(I) when is_integer(I) ->
    ok;
check_offset(Tz) when Tz=:=""; Tz=:="Z"; Tz=:="z" ->
    ok;
check_offset([Sign|Tz]) when Sign=:=$+; Sign=:=$- ->
    check_timezone(Tz);
check_offset(_) ->
    error.

check_timezone(Tz) ->
    try io_lib:fread("~d:~d", Tz) of
        {ok, [_, _], []} ->
            ok;
        _ ->
            error
    catch _:_ ->
            error
    end.

p(#{single_line:=Single}) ->
    p(Single);
p(true) ->
    "~0tp";
p(false) ->
    "~tp".

time_fraction_width(microsecond) -> 6;
time_fraction_width(millisecond) -> 3;
time_fraction_width(_)           -> 0.

level_to_chr(debug)      -> $D;
level_to_chr(info)       -> $I;
level_to_chr(notice)     -> $N;
level_to_chr(warning)    -> $W;
level_to_chr(error)      -> $E;
level_to_chr(critical)   -> $C;
level_to_chr(alert)      -> $A;
level_to_chr(emergency)  -> $M;
level_to_chr(none)       -> $ .
