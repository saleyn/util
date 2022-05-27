%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc This module abstracts the process scheduling facility into a
%%% reusable behavior.  It implements a behavior that includes features of
%%% `gen_server' and `supervisor' that allows building scheduling
%%% features to run an instance of `gen_server' on given days
%%% of week/months between certain hours of day.  The `supervisor' part
%%% of the server handles all scheduling events, and the `execution'
%%% part of the server handles spawning a separate process that calls
%%% `handle_run/1' callback.
%%%   The server can execute a "daemon" like long-lived functionality
%%% or some short-lived perioric task that will be rescheduled on the
%%% following run interval.  The behavior is somewhat similar to
%%% functionality of `cron' but designed for scheduling Erlang processes.
%%%
%%% Scheduling features include abilities to:
%%% <ul>
%%% <li>Run a server on given days of week between one or more
%%%     range of hours</li>
%%% <li>Specify periodic execution. E.g. a task can run every `N'
%%%     seconds/minutes/hours between given hours of day</li>
%%% <li>Limit execution of a server to given days of month specified
%%%     from first day of month or from last day of month</li>
%%% <li>Specify task restart strategy that allows restarting the
%%%     scheduled process up until the maximum restart intensity is
%%%     reached expressed in terms of the number of restarts within
%%%     a given number of seconds and allows specifying restart delay
%%%     expressed in seconds.</li>
%%% <li>Specify child process shutdown type - immediate or in a given
%%%     interval</li>
%%% <li>Log all scheduling events to `event_logger' using an optional
%%%     custom event report type</li>
%%% </ul>
%%%
%%% The user module should export the following callbacks (note that
%%% the signature of these functions is slightly different from
%%% gen_server's by having an extra `Opaque' argument. Use `get/2'
%%% function to inspect its value):
%%% ```
%%%   init(Args)
%%%     ==> {ok, State}
%%%         {ok, State, Timeout}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   handle_call(Msg, From, State, Opaque)
%%%    ==> {reply, Reply, State}
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, Reply, State}
%%%            Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_cast(Msg, State, Opaque)
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   handle_info(Info, State, Opaque)
%%%    ==> {noreply, State}
%%%        {noreply, State, Timeout}
%%%        {stop, Reason, State}
%%%              Reason = normal | shutdown | Term, terminate(State) is called
%%%
%%%   terminate(Reason, State) Let the user module clean up
%%%        always called when server terminates
%%%    ==> ok
%%%
%%% The following callbacks happen on start/run/stop of a scheduled
%%% child process:
%%%
%%%   init(Args)
%%%     ==> {ok, State}
%%%         ignore
%%%         {stop, Reason}
%%%
%%%   handle_start(State, Opaque) ->
%%%    ==> {start, State}        continues with normal start
%%%        {skip, State}         reschedules startup for the next valid interval
%%%        {stop, Reason, State}
%%%
%%%   handle_run(State) ->
%%%    ==> void()                           when no `no_spawn' option provided
%%%        {ok, Pid} | {stop, Reason}       when `no_spawn' option is provided
%%%        {ok, Pid, Ref} | {stop, Reason}  when `no_spawn, {monitor_type, child_monitor}'
%%%                                         options are provided
%%%    valid exit values are:
%%%      * normal             - performs normal rescheduling
%%%      * shutdown           - exits the gen_server without any further rescheduling
%%%      * {shutdown, Reason} - exits gen_server with `Reason' without rescheduling 
%%%      * Term               - performs supervised recovery by applying
%%%                             given restart policy
%%%   handle_stop(Action, Reason, State, Opaque) ->
%%%               Action = continue | stop
%%%    ==> {continue, Reason, State}    continues with default rescheduling
%%%        {stop, Reason, State}
%%%
%%% '''
%%% Also note that whenever the term `supervisor' is used in this document
%%% it refers to the scheduling part of the `gen_timed_server' behavior
%%% rather than a separate parent supervisor process.
%%% ```
%%%   Examples:
%%%     Execute M:handle_run/1 every Monday between 9:30am and 5:00pm:
%%%       gen_timed_server:start_link(M, Args,
%%%           [{schedule, [{mon, [{"9:30:00", "17:00:00"}]}]}], []).
%%%
%%%     Execute M:handle_run/1 every last two days of a month between
%%%     9:30am and 5:00pm:
%%%       gen_timed_server:start_link(M, Args,
%%%           [{schedule, [{any, [{"9:30:00", "17:00:00"}]}]},
%%%            {include_days, [-1,-2]}], []).
%%%
%%%     Execute M:handle_run/1 every 5 minutes on every last ten days of
%%%     a month between 1:00am and 9:00pm:
%%%       gen_timed_server:start_link(M, Args,
%%%           [{schedule, [{any, [{"1:00:00", "21:00:00", "00:05:00"}]}]},
%%%            {include_days, [{-1,-10}]}]).
%%%
%%%     Execute M:handle_run/1 every day except for the first 5 days of
%%%     a month between 1:00am and 9:00pm:
%%%       gen_timed_server:start_link(M, Args,
%%%           [{schedule, [{any, [{"1:00:00", "21:00:00"}]}]},
%%%            {exclude_days, [{1,5}]}]).
%%%
%%%     Execute M:handle_run/1 every Monday between 9:30am and 5:00pm
%%%     repeating every 30 minutes:
%%%       gen_timed_server:start_link(M, Args,
%%%           [{schedule, [{mon, [{"9:30:00", "17:00:00", "00:30:00"}]}]}]).
%%%
%%%     Execute M:handle_run/1 every Mon,Tue,Wed between 9:30am and 5:00pm:
%%%       timed_supervisor:start_link({M,F,A},
%%%           [{schedule, [{[mon,tue,wed], [{{9,30,0}, {17,0,0}}]}]}]).
%%%
%%%     Execute M:handle_run/1 every Mon between 9am and 5pm, and Wed
%%%     between 8am and 9pm:
%%%       gen_timed_server:start_link(M, Args,
%%%           [{schedule, [{mon, [{{9,0,0}, {17,0,0}}]},
%%%                        {wed, [{{8,0,0}, {21,0,0}}]}]}]).
%%%
%%%     Execute M:handle_run/1 every Monday between 9:00am and 5:00pm, and
%%%     set the maximum restart intensity to 3 failures within 20 seconds
%%%     with delay between restarts of 4 seconds:
%%%       gen_timed_server:start_link(M, Args,
%%%             [{schedule, [{mon, [{"9:00:00", "17:00:00"}]}]},
%%%              {restart, {3, 20, 4}}]).
%%% '''
%%% @end
%%%------------------------------------------------------------------------
%%% @type schedule()    = [ DaySchedule ]
%%%         DaySchedule = {DayRange, [TimeRange::hourrange()]}
%%%         DayRange    = any | day() | [ day() ] |
%%%                       {FromDay::day(), ToDay::day()}
%%%         Hours       = [ hourrange() ].
%%% @type day()         = mon | tue | wed | thu | fri | sat | sun.
%%% @type hourrange()   = {FromTime::time(), ToTime::time()} |
%%%                       {FromTime::time(), ToTime::time(), Repeat::time()}.
%%%                       Represents the time interval between `FromTime'
%%%                       and `ToTime'. Optionally defines repetition
%%%                       period `Repeat' in hours:minutes:seconds (minimum
%%%                       repetition is one minute).
%%% @type time()        = string() |
%%%                       {Hour::integer(), Min::integer(), Sec::integer()}.
%%%                       Time can be represented as a "HH:MM:SS" string,
%%%                       "HH:MM" string or a tuple.
%%% @type timespec()    = {Mon::hourrange(), Tue::hourrange(), Wed::hourrange(),
%%%                        Thu::hourrange(), Fri::hourrange(), Sat::hourrange(),
%%%                        Sun::hourrange()}.
%%% @type sup_name()    = atom() | pid().
%%% @type sup_options() = [SupOption].
%%%     SupOption   = {schedule, Schedule::schedule()} |
%%%                   {restart, RestartSpec} |
%%%                   no_spawn | {spawn, boolean()} |
%%%                   {monitor_type, monitor | link | child_link | child_monitor} |
%%%                   {report_type, ReportType::term()} |
%%%                   {shutdown, ShutdownType} |
%%%                   {include_days, [DayOfMonth]} |
%%%                   {exclude_days, [DayOfMonth]} |
%%%                   {id, ID}
%%%     RestartSpec = {MaxR::integer(), MaxTime::integer(), Delay::integer()}}
%%%     DayOfMonth  = integer() | {FromDay::integer(), ToDay::integer()}
%%%
%%%     Timed Supervisor's options.
%%%     <dl>
%%%     <dt>Schedule</dt>
%%%         <dd>Contains a schedule when supervised child Pid needs to run.</dd>
%%%     <dt>RestartSpec</dt>
%%%         <dd>If specified, the failed child Pid will be restarted up to
%%%             `MaxR' times in `MaxTime' seconds with `Delay' number of
%%%             seconds between successive restarts. By default RestartSpec
%%%             is `{_MaxR = 0, _MaxT = 1, _Delay = 0}' (auto restart is
%%%             disabled)</dd>
%%%     <dt>no_spawn | {spawn, boolean()}</dt>
%%%         <dd>When specified, the `handle_run/1' callback is executed in
%%%             in the same gen_timed_server's process and is expected to
%%%             return `{ok, Pid}', where `Pid' is the supervided child process.
%%%             When `monitor_type' is `child_monitor' the expected return
%%%             from `handle_run/1' is `{ok, Pid::pid(), Ref::reference()}',
%%%             where `Ref' is the reference of the monitor to the spawned
%%%             child process.
%%%             In the absense of this option `handle_run/1' callback is
%%%             executed in the context of a newly spawned process.  In either
%%%             case the new `Pid' will be either monitored or linked to
%%%             depending on the presense of `monitor_type' option.</dd>
%%%     <dt>monitor_type</dt>
%%%         <dd>`monitor' - the superviser will use monitor to monitor the
%%%             child process.  The implication is that it'll
%%%             be able to detect the death of the child, but when the
%%%             supervisor process is killed with `kill' reason the child
%%%             process will remain running.
%%%             `link' - the supervisor will use a link to link to the child.
%%%             `child_link' - the child will link to this supervisor on spawn
%%%             by itself.
%%%             `child_monitor' - the child `MFA' call will set up a monitor
%%%             by itself.</dd>
%%%     <dt>ReportType</dt>
%%%         <dd>When specified, the
%%%             `error_logger:error_report({Type, ReportType}, Report)'
%%%             and `error_logger:info_report({Type, ReportType}, Report)'
%%%             will be used instead of `error_logger:*_report/1' calls
%%%             (with Type = supervisor_report | progress), so that a custom
%%%             event logger can be installed to log these events.</dd>
%%%     <dt>ShutdownType</dt>
%%%         <dd>`brutal_kill' - kill child using `exit(Pid, kill)'.</dd>
%%%         <dd>`integer() >= 0' - kill child with `exit(Pid, shutdown)'
%%%             followed by `exit(Pid, kill)' after this number of
%%%             milleseconds.</dd>
%%%     <dt>{include_days, [DayOfMonth]} or {exclude_days, [DayOfMonth]}</dt>
%%%         <dd>These options are used to include/exclude execution of a
%%%             child process on given days of a month.  The `DayOfMonth'
%%%             can be represented by a positive integer in range
%%%             [1...31] to indicate a day since the beginning of a month
%%%             or a negative integer in range [-1...-31] to indicate
%%%             a day since the end of a month (-1 is the last day of
%%%             month).  Alternativey `DayOfMonth' can be represented
%%%             as a range tuple: `{FromDay::integer(), ToDay::integer()}'</dd>
%%%     <dt>{id, ID}</dt>
%%%         <dd>Implementation-specific identifier of the child process
%%%             recorded in the report log</dd>
%%%     </dl>
%%%
%%% @author  Serge Aleynikov <saleyn@gmail.com>
%%% @version $Revision$
%%%          $Date$
%%% @end
%%%------------------------------------------------------------------------
%%% Created 2009-10-15 Serge Aleynikov <saleyn@gmail.com>
%%% $URL$
%%%------------------------------------------------------------------------
%%% ``The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Initial Developer of the Original Code is Serge Aleynikov.
%%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%%% AB. All Rights Reserved.''
%%%------------------------------------------------------------------------
-module(gen_timed_server).
-author('saleyn@gmail.com').
-id("$Id$").

-behaviour(gen_server).

%% External exports
-export([
      start/4, start/5, start_link/4, start_link/5
    , get_child_pid/1, swap_options/2, reschedule/1, restart/1
    , force_start/1
    , full_schedule/0, weekdays/0, dow_to_string/1, info/1, get/2
    , parse_time/1, split_options/1, format_valid_days/1
    , day_of_the_week/1, validate_schedule/1
    , print_timespec/1, format_time/1, now_to_datetime/1
]).

%% Internal exports
-export([
      call/2, call/3, cast/2, cast/3, reply/2
]).

%% gen_server callbacks
-export([
      init/1, handle_call/3, handle_cast/2, handle_info/2
    , terminate/2, code_change/3
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEF_VALID,     16#FFFFffff).
-define(COMPILED_SPEC, '$ts$').
-define(FMT(Fmt,Args), lists:flatten(io_lib:format(Fmt,Args))).
-define(MAX_INT, 3600).

-record(state, {
    mod,                % callback module
    mod_state,          % state maintained by callback module
    id,                 % term() - identifier written to the report log
    name,               % registered name of the server
    pid,                % Child pid
    status=scheduled,   % scheduled | failed | running | restarting | terminated
    schedule,           % Normalized scheduling mask
    next_wakeup=now,    % now | datetime(). DateTime of the next run.
    until_time,         % the job will run until this time.
    reported=false,     % True when progress report was printed at least
                        % once for next interval.
    last_start,         % now() - Time of last start
    last_fail,          % now() - Time of last failure
    intensity = 0,      % Max number of restarts allowed within period
    period    = 1,      % Time in seconds in which the number of 'restarts' is allowed
    delay     = 0,      % Delay time in seconds between successive restarts
    restarts  = 0,      % Current number of restarts in 'period'
    spawn     = true,   % boolean() - If false no child process is explicitely spawned
    valid_days =        % {FromBeginning, FromEnd} is a tuple of two binary coded
        {?DEF_VALID,    % integers representing bor'd days of month when the
         ?DEF_VALID},   % child can be run.
    monitor_type=link,  % link       - use link
                        % monitor    - use monitor
                        % child_link - child process will link to this server by itself.
    monref,             % When monitor_type is `monitor', this value is the monitor reference
    shutdown=2000,      % brutal_kill | integer() >= 0
    report_type,        % When different from `undefined', error_logger:error_report/2
                        % is used for error reports.
    start_timer,        % Timer reference used to start the child process
    stop_timer,         % Timer reference used to stop the child process
    repeat_timer,       % Timer reference used to repeat the job at given interval
    next_repeat         % time() of next repeat timer
}).

%% @hidden

%%-------------------------------------------------------------------------
%% gen_server behavior supervisor callbacks
%%-------------------------------------------------------------------------
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                    State :: term()) -> term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

%%-------------------------------------------------------------------------
%% gen_timed_server behavior callbacks
%%-------------------------------------------------------------------------
-callback handle_start(State::term(), Opaque::term()) ->
    {start, State::term()} |
    {skip,  State::term()} |
    {stop,  Reason::term(), State::term()}.

-callback handle_run(State::term()) ->
    ok |
    {ok, pid()} | {ok, pid(), Ref::reference()} | 
    {stop, Reason::term()}.

-callback handle_stop(Action::continue|stop, Reason::term(),
                      State::term(), Opaque::term()) ->
    {continue, Reason::term(), State::term()} |
    {stop, Reason::term(), State::term()}.

-optional_callbacks([format_status/2, handle_start/2, handle_stop/4]).

%%-------------------------------------------------------------------------
%% @spec (Name::sup_name(), Module, Args, SupOpts::sup_options(), Options) ->
%%              {ok, Pid::pid()} | ignore | {error, Error}
%%
%% @doc Creates a timed server process as part of a supervision tree.  This
%%      function will, among other things, ensure that the supervisor is
%%      linked to the calling process (its supervisor).  The child process
%%      started by `{M, F, A}' will only be run between time windows
%%      specified by the `schedule' option.  `RestartSpec' option allows
%%      to define MaxR number of restarts within MaxTime window
%%      (in seconds) with a restart Delay seconds.  When maximum restart
%%      intensity is reached and the process keeps failing, this supervisor
%%      process fails with reason `reached_max_restart_intensity'. Note that
%%      default `restart' option is `{_MaxR = 0, _MaxT = 1, _Delay = 0}'
%%      meaning that the child process won't be auto restarted.
%%
%% @see //stdlib/supervisor
%% @end
%%-------------------------------------------------------------------------
start_link(Name, Mod, Args, SupOpts, GenSrvOpts)
  when is_list(Args), is_list(SupOpts), is_list(GenSrvOpts) ->
    gen_server:start_link(Name, ?MODULE,
                          [Mod, Args, SupOpts, Name], GenSrvOpts).

%%-------------------------------------------------------------------------
%% @spec (Mod::atom(), Args::list(), SupOpts::sup_options(),
%%          GenSrvOpts::list()) ->
%%              {ok, Pid::pid()} | ignore | {error, Error}
%% @doc Creates a timed server as part of supervision tree without
%%      a registered name.
%% @see start_link/3
%% @end
%%-------------------------------------------------------------------------
start_link(Mod, Args, SupOpts, GenSrvOpts)
  when is_list(Args), is_list(SupOpts), is_list(GenSrvOpts) ->
    gen_server:start_link(?MODULE, [Mod, Args, SupOpts, self], GenSrvOpts).

%%-------------------------------------------------------------------------
%% @equiv start_link/3
%% @doc Creates a timed server process outside of supervision tree.
%% @see start_link/3
%% @end
%%-------------------------------------------------------------------------
start(Name, Mod, Args, SupOpts, GenSrvOpts)
  when is_list(Args), is_list(SupOpts), is_list(GenSrvOpts) ->
    gen_server:start(Name, ?MODULE,
                     [Mod, Args, SupOpts, Name], GenSrvOpts).

%%-------------------------------------------------------------------------
%% @equiv start_link/2
%% @doc Creates a timed server process outside of supervision tree.
%% @see start_link/3
%% @end
%%-------------------------------------------------------------------------
start(Mod, Args, SupOpts, GenSrvOpts)
  when is_list(Args), is_list(SupOpts), is_list(GenSrvOpts) ->
    gen_server:start(?MODULE, [Mod, Args, SupOpts, self], GenSrvOpts).

%%-------------------------------------------------------------------------
%% @spec (Name) -> State::list()
%% @doc Return supervisor's internal state.
%% @end
%%-------------------------------------------------------------------------
info(Name) ->
    gen_server:call(Name, {?COMPILED_SPEC, info}).

%%-------------------------------------------------------------------------
%% @spec (Name, SupOptions::sup_options()) -> ok | {error, Why}
%% @doc Replace child's specification and restart (according to schedule)
%%      the child process if it was running.
%% @end
%%-------------------------------------------------------------------------
swap_options(Name, SupOptions) ->
    gen_server:call(Name, {?COMPILED_SPEC, swap_options, SupOptions}).

%%-------------------------------------------------------------------------
%% @spec (Name) -> ok | {error, Why}
%% @doc Terminate child with reason `shutdown' and reschedule it for
%%      the next interval in the schedule specification.
%% @end
%%-------------------------------------------------------------------------
reschedule(Name) ->
    gen_server:call(Name, {?COMPILED_SPEC, reschedule}).

%%-------------------------------------------------------------------------
%% @spec (Name) -> ok | stopped
%% @doc Bounce child process with reason `shutdown'.  If process is not
%%      scheduled to run, nothing happens and the supervisor will activate
%%      it at the future scheduled time.
%% @end
%%-------------------------------------------------------------------------
restart(Name) ->
    gen_server:call(Name, {?COMPILED_SPEC, restart}).

%%-------------------------------------------------------------------------
%% @spec (Name) -> ok | stopped | {error, Reason}
%% @doc Force immediate start of child process if it is not currently
%%      running irrespective of the scheduling options.
%% @end
%%-------------------------------------------------------------------------
force_start(Name) ->
    gen_server:call(Name, {?COMPILED_SPEC, force_start}).

%%-------------------------------------------------------------------------
%% @spec (Name) -> pid() | undefined
%% @doc Return the pid of the process executing handle_run/2 callback or
%%      `undefined' if the process is not running.
%% @end
%%-------------------------------------------------------------------------
get_child_pid(Name) ->
    gen_server:call(Name, {?COMPILED_SPEC, get_child_pid}).

%% @equiv gen_server:call(Name, Cmd)
call(Name, Cmd)           -> gen_server:call(Name, Cmd).
%% @equiv gen_server:call(Name, Cmd, Timeout)
call(Name, Cmd, Timeout)  -> gen_server:call(Name, Cmd, Timeout).
%% @equiv gen_server:cast(Name, Cmd)
cast(Name, Cmd)           -> gen_server:cast(Name, Cmd).
%% @equiv gen_server:cast(Name, Cmd, Timeout)
cast(Name, Cmd, Timeout)  -> gen_server:cast(Name, Cmd, Timeout).
%% @equiv gen_server:reply(Client, Reply)
reply(Client, Reply)      -> gen_server:reply(Client, Reply).

%%-------------------------------------------------------------------------
%% @spec () -> schedule()
%% @doc Return a schedule covering all days of week / hours of day.  This
%%      is a convenience function for a default schedule value.
%% @end
%%-------------------------------------------------------------------------
full_schedule() ->
    [{any, [{{0,0,0},{23,59,59}}]}].

%%-------------------------------------------------------------------------
%% @spec () -> [Day::atom()]
%% @doc Return a list of weekday atoms
%% @end
%%-------------------------------------------------------------------------
weekdays() ->
    [mon,tue,wed,thu,fri].

%%-------------------------------------------------------------------------
%% @spec (Item::atom(), Opaque) -> Value | [Value]
%% @doc Get an item from `Opaque' structure passed to most of behavior
%%      callbacks.  If `Item' is `all' then `[Value]' list is returned.
%% @end
%%-------------------------------------------------------------------------
get(id,          #state{id=X})           -> X;
get(name,        #state{name=X})         -> X;
get(status,      #state{status=X})       -> X;
get(schedule,    #state{schedule=X})     -> X;
get(next_wakeup, #state{next_wakeup=X})  -> X;
get(next_repeat, #state{next_repeat=X})  -> X;
get(until_time,  #state{until_time=X})   -> X;
get(last_start,  #state{last_start=X})   -> X;
get(last_fail,   #state{last_fail=X})    -> X;
get(intensity,   #state{intensity=X})    -> X;
get(period,      #state{period=X})       -> X;
get(restarts,    #state{restarts=X})     -> X;
get(delay,       #state{delay=X})        -> X;
get(valid_days,  #state{valid_days=X})   -> format_valid_days(X);
get(monitor_type,#state{monitor_type=X}) -> X;
get(shutdown,    #state{shutdown=X})     -> X;
get(all,         #state{} = State)       ->
    [
        {pid,           State#state.pid},
        {status,        State#state.status},
        {id,            State#state.id},
        {module,        State#state.mod},
        {next_wakeup,   State#state.next_wakeup},
        {last_start,    now_to_datetime(State#state.last_start)},
        {last_fail,     now_to_datetime(State#state.last_fail)},
        {restart,       {State#state.intensity,
                         State#state.period,
                         State#state.delay}},
        {restarts,      State#state.restarts},
        {monitor_type,  State#state.monitor_type},
        {report_type,   State#state.report_type},
        {shutdown,      State#state.shutdown},
        {schedule,      State#state.schedule},
        {valid_days,    State#state.valid_days}
    ].

%%-------------------------------------------------------------------------
%% @spec (Options::list()) -> {SupOptions::sup_options(), Rest}
%% @doc Separated gen_timed_server's supervision options from other
%%      options found in the list.
%% @end
%%-------------------------------------------------------------------------
split_options(List) ->
    {L1, L2} = lists:foldl(
        fun(Opt, {TrueList, FalseList}) ->
            try
                validate_sup_opts([Opt], #state{}),
                {[Opt | TrueList], FalseList}
            catch _:_ ->
                {TrueList, [Opt | FalseList]}
            end
        end, {[], []}, List),
    {lists:reverse(L1), lists:reverse(L2)}.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% @private
init([Mod, Args, SupOpts, Name]) ->
    process_flag(trap_exit, true),

    {{module, _}, _} = {code:ensure_loaded(Mod), {line,?LINE}},
    {true, _} = {erlang:function_exported(Mod,handle_run,1), {line,?LINE}},

    try
        {?COMPILED_SPEC, DefS} = validate_schedule(full_schedule()),
        State0 = validate_sup_opts(SupOpts,
                                   #state{name=Name, mod=Mod, schedule=DefS}),
        case Mod:init(Args) of
        {ok, MS} ->
            self() ! {?COMPILED_SPEC, timer, start},
            {ok, State0#state{mod_state=MS}};
        {ok, MS, Timeout} ->
            self() ! {?COMPILED_SPEC, timer, start},
            {ok, State0#state{mod_state=MS}, Timeout};
        Other ->
            Other
        end
    catch
    ignore ->
        ignore;
    throw:Reason ->
        {stop, Reason};
    _:Reason ->
        {stop, {Reason, erlang:get_stacktrace()}}
    end.

%% @private
handle_call({?COMPILED_SPEC, swap_options, SupOptions}, From, State0) ->
    try
        State1 = validate_sup_opts(SupOptions, State0),
        check_state_change(State0, State1),
        handle_call({?COMPILED_SPEC, restart}, From, State1)
    catch throw:no_change ->
        {reply, {error, no_change}, State0};
    _:Why ->
        {reply, {error, Why}, State0}
    end;

handle_call({?COMPILED_SPEC, reschedule}, _From, State) ->
    case handle_info({?COMPILED_SPEC, timer, reschedule}, State) of
    {noreply, NewState} ->
        {reply, ok, NewState};
    {stop, Reason, NewState} ->
        {stop, Reason, stopped, NewState}
    end;

handle_call({?COMPILED_SPEC, restart}, _From, State) ->
    case handle_info({?COMPILED_SPEC, timer, restart}, State) of
    {noreply, NewState} ->
        {reply, ok, NewState};
    {stop, Reason, NewState} ->
        {stop, Reason, stopped, NewState}
    end;

handle_call({?COMPILED_SPEC, force_start}, _From, State) ->
    case handle_info({?COMPILED_SPEC, timer, force_start}, State) of
    {noreply, NewState} ->
        {reply, ok, NewState};
    {stop, Reason, NewState} ->
        {stop, Reason, stopped, NewState}
    end;

handle_call({?COMPILED_SPEC, get_child_pid}, _From, #state{pid=Pid} = State) ->
    {reply, Pid, State};

handle_call({?COMPILED_SPEC, info}, _From, State) ->
    {reply, get(all, State), State};
handle_call(Other, From, #state{mod=Mod, mod_state=MS} = State) ->
    Result = Mod:handle_call(Other, From, MS, State),
    internal_handle_result(Result, State).

%% @private
handle_cast(Msg, #state{mod=Mod, mod_state=MS} = State) ->
    Result = Mod:handle_cast(Msg, MS, State),
    internal_handle_result(Result, State).

%% @private
handle_info({?COMPILED_SPEC, timer, Type}, State)
  when Type=:=restart
     ; Type=:=scheduled_stop
     ; Type=:=reschedule
     ; Type=:=start
     ; Type=:=force_start
 -> case start_child(State, Type) of
    {ok, _, NewState} ->
        {noreply, NewState};
    {stop, Reason, NewState} ->
        {stop, Reason, NewState}
    end;
handle_info({?COMPILED_SPEC, repeat_timer, Interval} = Msg, State) ->
    case State#state.status of
    running ->
        TRef = erlang:send_after(Interval, self(), Msg),
        {noreply, State#state{repeat_timer = TRef}};
    _ ->
        % restart the child process
        handle_info({?COMPILED_SPEC, timer, restart}, State#state{repeat_timer=undefined})
    end;

handle_info({'DOWN', Ref, process, Pid, Reason}, #state{monitor_type=monitor, monref=Ref} = State) ->
    handle_info({'EXIT', Pid, Reason}, State);
handle_info({'EXIT', Pid, Reason}, #state{pid=Pid} = State) when is_pid(Pid) ->
    internal_handle_exit(Reason, State);

handle_info(Other, #state{mod=Mod, mod_state=MS} = State) ->
    Result = Mod:handle_info(Other, MS, State),
    internal_handle_result(Result, State).

%% @private
terminate(Reason, #state{mod=Mod, mod_state=MS}) ->
    Mod:terminate(Reason, MS).

%% @private
code_change(OldVsn, #state{mod=Mod, mod_state=MS} = State, Extra) ->
    {ok, NewMS} = Mod:code_change(OldVsn, MS, Extra),
    {ok, State#state{mod_state=NewMS}}.


%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

internal_handle_result({reply, Reply, MS}, State) ->
    {reply, Reply, State#state{mod_state=MS}};
internal_handle_result({reply, Reply, MS, Tout}, State)
  when is_integer(Tout); Tout=:=hibernate; Tout=:=infinity ->
    {reply, Reply, State#state{mod_state=MS}, Tout};
internal_handle_result({noreply, MS}, State) ->
    {noreply, State#state{mod_state=MS}};
internal_handle_result({noreply, MS, Tout}, State)
  when is_integer(Tout); Tout=:=hibernate; Tout=:=infinity ->
    {noreply, State#state{mod_state=MS}, Tout};
internal_handle_result({stop, Reason, MS}, State) ->
    {stop, Reason, State#state{mod_state=MS}};
internal_handle_result({stop, Reason, Reply, MS}, State) ->
    {stop, Reason, Reply, State#state{mod_state=MS}};
internal_handle_result(Other, State) ->
    {stop, {bad_return_value, Other}, State}.

%% @doc Child process exited. Perform appropriate rescheduling or
%%      restarting action and call the Mod:handle_stop/2 callback.
internal_handle_exit(normal, State) ->
    Result = do_reschedule(normal, State),
    internal_handle_exit2(normal, Result);
internal_handle_exit({Ok, MState}, State) when Ok=:=ok; Ok=:=noreply ->
    Result = do_reschedule(normal, State#state{mod_state=MState}),
    internal_handle_exit2(normal, Result);
internal_handle_exit(shutdown, State) ->
    Result = do_stop(shutdown, State),
    internal_handle_exit2(shutdown, Result);
internal_handle_exit({shutdown, _Reason} = SR, State) ->
    Result = do_stop(SR, State),
    internal_handle_exit2(SR, Result);
internal_handle_exit(Reason, State) ->
    Result = do_schedule_restart_on_failure(Reason, State),
    internal_handle_exit2(Reason, Result).

internal_handle_exit2(Reason, {noreply, State}) ->
    invoke_stop_callback(continue, Reason, State);
internal_handle_exit2(_Reason, {stop, Reason, State}) ->
    invoke_stop_callback(stop, Reason, State).

%% @spec (Action, Reason, State) -> {noreply, State} | {stop, Reason, State}
%%     Action = continue | stop
invoke_stop_callback(Action, Reason, #state{mod=Mod, mod_state=MState} = State) ->
    NewState = State#state{status=terminated},
    case erlang:function_exported(Mod,handle_stop,4) of
    true ->
        case Mod:handle_stop(Action, Reason, MState, NewState) of
        {Ok, _, NewMS} when Ok=:=continue; Ok=:=noreply; Ok=:=ok ->
            {noreply, NewState#state{mod_state=NewMS, pid=undefined}, hibernate};
        {stop, NewReason, NewMS} ->
            {stop, NewReason, NewState#state{mod_state=NewMS, pid=undefined}}
        end;
    false when Action=:=continue ->
        {noreply, NewState#state{pid=undefined}, hibernate};
    false ->
        {stop, Reason, NewState#state{pid=undefined}}
    end.

do_reschedule(Reason, State) ->
    NewState = State#state{restarts=1, status=scheduled},
    Context  = [child_rescheduled, {restart, 1, State#state.intensity}],
    report_error(Context, Reason, NewState),
    handle_info({?COMPILED_SPEC, timer, reschedule}, NewState).

do_stop(Reason, #state{restarts=C} = State) ->
    NewS = State#state{restarts=C+1, pid=undefined, status=terminated},
    do_report_shutdown([], Reason, NewS),
    {stop, Reason, NewS}.

do_schedule_restart_on_failure(Reason, State) ->
    % Handle restart
    #state{intensity=I, period=P, last_fail=T, restarts=C, delay=D} = State,
    Now = erlang:timestamp(),
    case in_period(T, Now, P) of
    true when C < I ->
        % We haven't reached restart intensity yet
        NewState = State#state{restarts=C+1, status=restarting},
        Context = [restarting_on_failure, {count, C+1, I}, {delay_s, D}],
        report_error(Context, Reason, NewState),
        erlang:send_after(D*1000, self(), {?COMPILED_SPEC, timer, restart}),
        {noreply, NewState};
    true ->
        % Restart intensity is reached - bail out
        NewS = State#state{status=failed},
        do_report_shutdown([], reached_max_restart_intensity, NewS),
        {stop, reached_max_restart_intensity, NewS};
    false when I =:= 0 ->
        NewS = State#state{status=failed},
        do_report_shutdown([no_restarts_requested], Reason, NewS),
        {stop, shutdown, NewS};
    false ->
        % This infrequent failure is outside of the period window.
        NewS = State#state{restarts=1, last_fail=Now, status=restarting},
        Context = [restarting, {count, 1, I}, {delay_s, D}],
        report_error(Context, Reason, NewS),
        erlang:send_after(D*1000, self(), {?COMPILED_SPEC, timer, restart}),
        {noreply, NewS}
    end.

do_report_shutdown(FailAction, Reason, State) ->
    Context = [child_terminated] ++ FailAction ++ [{restart, State#state.restarts, State#state.intensity}],
    report_error(Context, Reason, State),
    {stop, Reason, State#state{pid=undefined}}.

check_state_change(#state{} = S1, #state{} = S2) ->
    if  S1#state.mod          =:= S2#state.mod
      , S1#state.name         =:= S2#state.name
      , S1#state.id           =:= S2#state.id
      , S1#state.schedule     =:= S2#state.schedule
      , S1#state.valid_days   =:= S2#state.valid_days
      , S1#state.intensity    =:= S2#state.intensity
      , S1#state.period       =:= S2#state.period
      , S1#state.restarts     =:= S2#state.restarts
      , S1#state.delay        =:= S2#state.delay
      , S1#state.monitor_type =:= S2#state.monitor_type
      , S1#state.shutdown     =:= S2#state.shutdown
      , S1#state.report_type  =:= S2#state.report_type
    ->
        throw(no_change);
    true ->
        changed
    end.


validate_sup_opts([{schedule, Sched} | Tail], State) ->
    try
        {?COMPILED_SPEC, SchedSpec} = validate_schedule(Sched),
        validate_sup_opts(Tail, State#state{schedule=SchedSpec})
    catch _:Err ->
        throw({Err, Sched})
    end;
validate_sup_opts([{restart, {MaxR, MaxT, Delay}} | Tail], State) ->
    (is_integer(MaxR)  andalso MaxR  >= 0) orelse throw({invalid_intensity, MaxR}),
    (is_integer(MaxT)  andalso MaxT  >  0) orelse throw({invalid_period,    MaxT}),
    (is_integer(Delay) andalso Delay >= 0) orelse throw({invalid_delay,    Delay}),
    validate_sup_opts(Tail, State#state{intensity=MaxR, period=MaxT, delay=Delay});
validate_sup_opts([{spawn, Bool} | Tail], State) when is_boolean(Bool) ->
    validate_sup_opts(Tail, State#state{spawn=Bool});
validate_sup_opts([no_spawn | Tail], State) ->
    validate_sup_opts(Tail, State#state{spawn=false});
validate_sup_opts([{monitor_type, M} | Tail], State)
  when M=:=monitor; M=:=link; M=:=child_link; M=:=child_monitor ->
    validate_sup_opts(Tail, State#state{monitor_type=M});
validate_sup_opts([{report_type, Type}| Tail], State) ->
    validate_sup_opts(Tail, State#state{report_type=Type});
validate_sup_opts([{shutdown, Type}| Tail], State)
  when Type=:=brutal_kill; is_integer(Type), Type>=0 ->
    validate_sup_opts(Tail, State#state{shutdown=Type});
validate_sup_opts([{include_days, Days}| Tail], State) when is_list(Days) ->
    L = validate_day_range(include_days, Days),
    NewValidDays = merge_valid_days(add, L, State#state.valid_days),
    validate_sup_opts(Tail, State#state{valid_days=NewValidDays});
validate_sup_opts([{exclude_days, Days}| Tail], State) when is_list(Days) ->
    L = validate_day_range(exclude_days, Days),
    NewValidDays = merge_valid_days(remove, L, State#state.valid_days),
    validate_sup_opts(Tail, State#state{valid_days=NewValidDays});
validate_sup_opts([{id, ID}| Tail], State) ->
    validate_sup_opts(Tail, State#state{id=ID});
validate_sup_opts([Other | _Tail], _) ->
    throw({unsupported_option, Other});
validate_sup_opts([], State) ->
    State.

validate_day_range(Opt, Days) ->
    F = fun(I) when is_integer(I), I >= -31, I =< 31, I=/=0 ->
            I;
        ({I,J}) when is_integer(I), is_integer(J),
                     I >= -31, I =< 31, I =/= 0,
                     J >= -31, J =< 31, J =/= 0 ->
            if I < J -> lists:seq(I, J);
            true     -> lists:seq(J, I)
            end;
        (Other) ->
            throw(?FMT("Invalid day format ~w in ~w option", [Other, Opt]))
        end,
    lists:append([F(I) || I <- Days]).

merge_valid_days(add, L, {FromBegin, FromEnd}) ->
    lists:foldl(fun(I, {B,E}) when I > 0 -> {no_default(B) bor (1 bsl (I-1)), E};
                   (I, {B,E}) when I < 0 -> {B, no_default(E) bor (1 bsl (-I-1))}
                end, {FromBegin, FromEnd}, L);
merge_valid_days(remove, L, {FromBegin, FromEnd}) ->
    lists:foldl(fun(I, {B,E}) when I > 0 -> {no_default(B) bxor (1 bsl (I-1)), E};
                   (I, {B,E}) when I < 0 -> {B, no_default(E) bxor (1 bsl (-I-1))}
                end, {FromBegin, FromEnd}, L).

no_default(?DEF_VALID) -> 0;
no_default(Other)      -> Other.

%%-------------------------------------------------------------------------
%% @spec ({FromBegin::integer(), FromEnd::integer()}) -> ValidDays
%%    ValidDays = [ValidDay]
%%    ValidDay  = {FromDay::integer(), ToDay::integer()} | Day
%%    Day       = integer()
%% @doc Convert a `DaysMask' stored in `#state.valid_days' to a list 
%%      of days or day ranges with positive values indicating days of
%%      month from the beginning of a month, and negative values - from 
%%      the end of a month.
%% @end
%%-------------------------------------------------------------------------
format_valid_days({?DEF_VALID, ?DEF_VALID}) ->
    [{1,31}];
format_valid_days({FromBegin, FromEnd}) ->
    Positive = format_valid_days(FromBegin),
    Negative = format_valid_days(FromEnd),
    Positive ++ lists:reverse(format_valid_days_negate(Negative));

format_valid_days(?DEF_VALID) ->
    [{1,31}];
format_valid_days(0) ->
    [];
format_valid_days(Int) when is_integer(Int) ->
    format_valid_days2(Int, 1, 0, 0, []).

format_valid_days_negate([{B,E} | T]) ->
    [{-E,-B} | format_valid_days_negate(T)];
format_valid_days_negate([I | T]) ->
    [-I | format_valid_days_negate(T)];
format_valid_days_negate([]) ->
    [].

format_valid_days2(_, 32, 0, _, Acc) ->
    lists:reverse(Acc);
format_valid_days2(_, 32, N, M, Acc) ->
    lists:reverse([{N,M} | Acc]);
format_valid_days2(I, N, FirstOne, LastOne, Acc) ->
    case I band (1 bsl (N-1)) of
    0 when FirstOne > 0, FirstOne =:= LastOne ->
        format_valid_days2(I, N+1, 0, 0, [FirstOne | Acc]);
    0 when FirstOne =:= LastOne-1 ->
        format_valid_days2(I, N+1, 0, 0, [LastOne, FirstOne | Acc]);
    0 when FirstOne > 0 ->
        format_valid_days2(I, N+1, 0, 0, [{FirstOne, LastOne} | Acc]);
    0 ->
        format_valid_days2(I, N+1, 0, 0, Acc);
    _ when FirstOne =:= 0 ->
        format_valid_days2(I, N+1, N, N, Acc);
    _ when LastOne =:= (N-1) ->
        format_valid_days2(I, N+1, FirstOne, N, Acc)
    end.

%%-------------------------------------------------------------------------
%% @spec (DaySpecs::schedule()) -> {'$ts$', Result::timespec()}
%% @doc Validate day/time specification. Thow error on invalid specification
%%      format.
%% @end
%%-------------------------------------------------------------------------
validate_schedule({?COMPILED_SPEC, DaySpecs}=S) when tuple_size(DaySpecs) =:= 7 ->
    {?COMPILED_SPEC, S};
validate_schedule(DaySpecs) when tuple_size(DaySpecs) =:= 7 ->
    {?COMPILED_SPEC, DaySpecs};
validate_schedule(DaySpecs) ->
    {?COMPILED_SPEC, validate_time_spec(DaySpecs, erlang:make_tuple(7, []))}.

validate_time_spec([DaySpec | Tail], Days) ->
    NewDays = validate_day_spec(DaySpec, Days),
    validate_time_spec(Tail, NewDays);
validate_time_spec([], Days) ->
    Days.

validate_day_spec({any, TimeInt}, Days) ->
    Time = validate_time_intervals(TimeInt, []),
    lists:foldl(fun(Day, Acc) -> append_time(Day, Acc, Time) end, Days, [1,2,3,4,5,6,7]);
validate_day_spec({DayList, TimeInt}, Days) when is_list(DayList) ->
    Time = validate_time_intervals(TimeInt, []),
    lists:foldl(fun(Day, Acc) -> validate_day_spec2(Day, Acc, Time) end, Days, DayList);
validate_day_spec({{FromDay, ToDay} = DS, TimeInt}, Days)
        when is_atom(FromDay),    is_atom(ToDay)
           ; is_integer(FromDay), is_integer(ToDay) ->
    Time       = validate_time_intervals(TimeInt, []),
    IntFromDay = decode_day(FromDay),
    IntToDay   = decode_day(ToDay),
    IntFromDay =< IntToDay orelse
        throw(?FMT("Invalid day spec: ~p (FromDay must be less than ToDay)!", [DS])),
    DayList    = lists:seq(IntFromDay, IntToDay),
    lists:foldl(fun(Day, Acc) -> validate_day_spec2(Day, Acc, Time) end, Days, DayList);
validate_day_spec({Day, TimeInt}, Days) when is_list(TimeInt) ->
    Time = validate_time_intervals(TimeInt, []),
    validate_day_spec2(Day, Days, Time);
validate_day_spec(Other, _Days) ->
    throw(?FMT("Invalid day spec: ~w", [Other])).

validate_day_spec2(Day, Days, Time) ->
    IntDay = decode_day(Day),
    append_time(IntDay, Days, Time).

append_time(I, Days, TimeL2) ->
    TimeL1 = element(I, Days),
    Time = lists:keymerge(1, TimeL1, TimeL2),
    setelement(I, Days, Time).

validate_time_intervals([Interval | Tail], Acc) ->
    I = validate_time_interval(Interval),
    validate_time_intervals(Tail, I ++ Acc);
validate_time_intervals([], L) ->
    Res = lists:sort(L),
    case [{{A,B},{C,D}} || {A,B} = X1 <- Res, {C,D} = X2 <- Res, X1 < X2, A =< D, C =< B] of
    []    -> Res;
    Cross -> throw(?FMT("Found crossover times: ~w", [Cross]))
    end.

validate_time_interval({From, To}) ->
    validate_time_interval({From, To, {0,0,0}});
validate_time_interval({From, To, Repeat}) ->
    [throw(?FMT("Invalid time format: ~200p", [I]))
        || I <- [From, To, Repeat], not is_list(I) andalso not is_tuple(I)],
    validate_time_interval2(
        {parse_time(From), parse_time(To), parse_time(Repeat)});
validate_time_interval([]) ->
    [];
validate_time_interval(Other) ->
    throw(?FMT("Invalid time spec: ~200p", [Other])).

validate_time_interval2({From = {H1,M1,S1}, To = {H2,M2,S2}, _Repeat = {H3,M3,S3}})
    when is_integer(H1), H1 >= 0, H1 < 24
       , is_integer(M1), M1 >= 0, M1 < 60
       , is_integer(S1), S1 >= 0, S1 < 60
       , is_integer(H2), H2 >= 0, H2 < 24
       , is_integer(M2), M2 >= 0, M2 < 60
       , is_integer(S2), S2 >= 0, S2 < 60
       , From < To
       , is_integer(H3), H3 >= 0, H3 < 24
       , is_integer(M3), M3 >= 0, M3 < 60
       , is_integer(S3), S3 >= 0, S3 < 60
       %, (H3+M3) > 0  % Minimum repetition is 1 minute
->
    [{From, To, H3*24*60 + M3*60 + S3}].

parse_time({_,_,_} = Time) ->
    Time;
parse_time(Time) when is_list(Time) ->
    try
        case T = list_to_tuple([list_to_integer(L) || L <- string:tokens(Time, ":")]) of
        {_,_,_} -> T;
        {H,M}   -> {H,M,0}
        end
    catch _:_ ->
        throw(?FMT("Invalid time spec: ~p", [Time]))
    end.

wakeup_interval(Secs, _MSecs) when Secs > ?MAX_INT -> ?MAX_INT*1000;
wakeup_interval(Secs,  MSecs) -> Secs*1000 - MSecs.

repeat_interval_timer(_FromTime, _UntilTime, _NowTime, 0) ->
    {undefined, undefined};
repeat_interval_timer({H0,M0,S0} = _FromTime, UntilTime, {H,M,S} = _NowTime, Repeat) ->
    Interval = Repeat*1000,
    I        = Repeat - ((H-H0)*3600 + (M-M0)*60 + (S-S0)) rem Repeat,
    NextTime = normalize_time({H + I div 3600, M + (I rem 3600) div 60, S + I rem 60}),
    if I =< 0 ->
        {undefined, UntilTime};
    true ->
        {erlang:send_after(I*1000, self(), {?COMPILED_SPEC, repeat_timer, Interval}), NextTime}
    end.

normalize_time({H,M,S}) when S > 59 ->
    normalize_time({H,M+(S div 60),S rem 60});
normalize_time({H,M,S}) when M > 59 ->
    normalize_time({H+(M div 60),M rem 60,S});
normalize_time(Time) ->
    Time.

%% @spec (State::#state{}, Type) -> Result
%%          Result = {ok, Status, State} | {stop, Reason, State}
%%          Type   = start | restart | force_start | reschedule | scheduled_stop
%%          Status = started | already_running | scheduled
start_child(#state{pid=Pid} = State, Type) ->
    Now         = erlang:timestamp(),
    {Date,Time} = calendar:now_to_local_time(Now),
    MSecs       = msecs(Now),
    When        = case Type of
                  reschedule  -> get_following_time(State, {Date,Time});
                  force_start -> get_next_time(State, {Date,Time});
                  _           -> get_next_time(State, {Date,Time})
                  end,
    cancel_timer(State#state.start_timer),
    case {When, is_child_alive(Pid)} of
    {{0, 0, {_FromTime, UntilTime, _Repeat} = FUR}, true} ->
        % Request to start a process that's already running. Check if
        % UntilTime changed, if so reinitialize the termination timer.
        case State#state.until_time of
        UntilTime ->
            TRef = State#state.stop_timer;
        _ ->
            cancel_timer(State#state.stop_timer),
            Now1 = erlang:timestamp(),
            Int  = get_interval(now_to_time(Now1), UntilTime, 1)*1000 - msecs(Now1),
            TRef = erlang:send_after(Int, self(), {?COMPILED_SPEC, timer, scheduled_stop})
        end,

        NewState = State#state{
            status=running, next_wakeup=now, until_time=UntilTime, stop_timer=TRef
        },

        if Type =:= start; Type =:= restart ->
            % This is a request to swap child specification where some of the
            % child spec state changed. Kill the process and start it with the new spec
            case stop_child(NewState, Type, true) of
            {noreply, State2} ->
                try_start_child(State2#state{last_start=Now}, FUR, Time);
            Other ->
                Other
            end;
        true ->
            % The pid is already running - nothing to do
            {ok, already_running, NewState}
        end;
    {{0, 0, {_FromTime, UntilTime, _Repeat} = FUR}, false} ->
        % Time to run now up until UntilTime
        NewState = State#state{next_wakeup=now, until_time=UntilTime, last_start=Now},
        try_start_child(NewState, FUR, Time);
    {{Days, IntervalSecs, {StartTime, EndTime, Repeat}}, IsChildAlive} ->
        % Next time to run is after {Days, IntervalSecs}
        case stop_child(State, Type, IsChildAlive) of
        {noreply, NewState} ->
            GDays   = calendar:date_to_gregorian_days(Date),
            RunDate = calendar:gregorian_days_to_date(GDays + Days),
            Wakeup  = wakeup_interval(IntervalSecs, MSecs),
            Report  = [format_next_run(Wakeup, RunDate, StartTime, EndTime, Repeat)]
                   ++ format_repeat(Repeat, NewState#state.next_repeat),
                      %wakeup_sec, Wakeup div 1000}],
            report_progress(NewState#state.reported, NewState#state.name, Report,
                            NewState#state.report_type, NewState#state.id),
            cancel_timer(NewState#state.repeat_timer),
            TRef = erlang:send_after(Wakeup, self(), {?COMPILED_SPEC, timer, restart}),
            {ok, scheduled, NewState#state{start_timer=TRef, repeat_timer=undefined,
                                           pid=undefined, reported=true, status=scheduled,
                                           next_wakeup=Wakeup div 1000,
                                           until_time=EndTime, monref=undefined}};
        Other ->
            Other
        end
    end.

format_next_run(Wakeup, _RunDate, {0,0,0}, {0,0,0}, 0) ->
    {next_check, integer_to_list(Wakeup div 1000)++" s"};
format_next_run(_Wakeup, RunDate, StartTime, EndTime, 0) ->
    {next_run, to_string(RunDate, StartTime, EndTime, 0)};
format_next_run(_Wakeup, RunDate, StartTime, EndTime, Repeat) ->
    {run_window, to_string(RunDate, StartTime, EndTime, Repeat)}.

format_repeat(Repeat, Time) when Repeat=:=0; Time=:=undefined ->
    [];
format_repeat(_, Time) ->
    [{next_run, format_time(Time)}].


%% @spec (State::#state{},
%%        {FromTime::time(), UntilTime::time(), Repeat::integer()},
%%        Time::time()) ->
%%            {ok, Info::atom(), State} | {stop, Reason, State}
try_start_child(#state{mod=Mod, mod_state=MState} = State,
                {FromTime, UntilTime, _Repeat} = Opaque, Time) ->
    case erlang:function_exported(Mod,handle_start,2) of
    true ->
        State1 = State#state{next_wakeup=FromTime, until_time=UntilTime},
        case Mod:handle_start(MState, State1) of
        {Ok, NewMS} when Ok=:=noreply; Ok=:=start; Ok=:=ok ->
            try_start_child2(State#state{mod_state=NewMS}, Opaque, Time);
        {skip, NewMS} ->
            start_child(State#state{mod_state=NewMS}, reschedule);
        {stop, Reason, NewMS} ->
            {stop, Reason, State#state{mod_state=NewMS}}
        end;
    false ->
        try_start_child2(State, Opaque, Time)
    end.

try_start_child2(#state{mod=Mod, mod_state=MState, report_type=Type, name=Name} = State,
                {FromTime, UntilTime, Repeat}, Time) ->
    try
        {ChildPid, MRef} =
            case State#state.spawn of
            true ->
                case State#state.monitor_type of
                monitor       -> erlang:spawn_monitor(Mod, handle_run, [MState]);
                link          -> {erlang:spawn_link(Mod, handle_run, [MState]), undefined};
                child_link    -> throw({spawn_not_supported_for_child_link,    Mod, MState});
                child_monitor -> throw({spawn_not_supported_for_child_monitor, Mod, MState})
                end;
            false ->
                case {Mod:handle_run(MState), State#state.monitor_type} of
                {{ok, CPid}, monitor} ->
                    {CPid, erlang:monitor(process, CPid)};
                {{ok, CPid}, link} ->
                    link(CPid),
                    {CPid, undefined};
                {{ok, CPid}, child_link} ->
                    lists:member(CPid, element(2, process_info(self(), links)))
                        orelse throw({child_process_not_linked, Mod, MState}),
                    {CPid, undefined};
                {{ok, CPid, Ref}, child_monitor} when is_reference(Ref) ->
                    {CPid, Ref};
                {{error, Reason}, _} ->
                    erlang:error({error, Reason});
                {{stop, Reason}, _} ->
                    throw(Reason)
                end
            end,
        ChildName = get_registered_name(ChildPid),

        case erlang:function_exported(Mod, format_status, 2) of
        true  -> Info = Mod:format_status(normal, [[], MState]);
        false -> Info = []
        end,

        Report = [{started, [{pid, ChildName}, {mod, Mod}]},
                  {run_until, format_time(UntilTime)} | Info],

        report_progress(false, Name, Report, Type, State#state.id),
        cancel_timer(State#state.start_timer),
        cancel_timer(State#state.stop_timer),
        Now        = erlang:timestamp(),
        Interval   = get_interval(now_to_time(Now), UntilTime, 1)*1000 - msecs(Now),
        TRef       = erlang:send_after(Interval, self(), {?COMPILED_SPEC, timer, scheduled_stop}),
        {TRpt,Nxt} = case State#state.repeat_timer of
                     undefined -> repeat_interval_timer(FromTime, UntilTime, Time, Repeat);
                     OldRef    -> {OldRef, State#state.next_repeat}
                     end,
        {ok, started, State#state{
             pid=ChildPid, reported=false, monref=MRef, status=running,
             start_timer=undefined, stop_timer=TRef,
             repeat_timer=TRpt, next_repeat=Nxt
        }}
    catch throw:Why ->
        {stop, Why, State}
    end.

stop_child(#state{pid=Pid, name=SupName, report_type=Type, id=Id} = State,
           ReportReason, _IsChildAlive = true) ->
    report_shutdown(Pid, SupName, ReportReason, Type, Id),
    case monitor_child(Pid, State#state.monref) of
    {ok, MonRef} ->
        case State#state.shutdown of
        brutal_kill  -> ShutdownTime = 0;
        ShutdownTime -> exit(Pid, shutdown)  % Try to shutdown gracefully
        end,
        %
        receive
        {'DOWN', MonRef, process, Pid, _Reason} ->
            ok
        after ShutdownTime ->
            exit(Pid, kill),  % Force termination.
            receive
            {'DOWN', MonRef, process, Pid, _Reason} -> ok
            end
        end,
        invoke_stop_callback(continue, ReportReason, State);
    {error, _Why} ->
        {noreply, State}
    end;
stop_child(State, _ReportReason, false) ->
    {noreply, State}.

is_child_alive(undefined) ->
    false;
is_child_alive(Pid) when is_pid(Pid) ->
    case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
    {badrpc, _Reason} ->
        false;
    Bool ->
        Bool
    end.

get_registered_name(ChildPid) ->
    case process_info(ChildPid, registered_name) of
    {registered_name, N} -> N;
    _                    -> ChildPid
    end.

%% Help function to stop_child/3 switches from link to monitor approach
monitor_child(_Pid, MonRef) when is_reference(MonRef) ->
    {ok, MonRef};
monitor_child(Pid, undefined) ->
    %% Do the monitor operation first so that if the child dies
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    MonRef = erlang:monitor(process, Pid),
    unlink(Pid),
    receive
	%% If the child dies before the unlink we must empty
	%% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
	{'EXIT', Pid, Reason} ->
	    receive
		{'DOWN', _, process, Pid, _} ->
		    {error, Reason}
	    end
    after 0 ->
	    %% If a naughty child did unlink and the child dies before
	    %% monitor the result will be that shutdown/2 receives a
	    %% 'DOWN'-message with reason noproc.
	    %% If the child should die after the unlink there
	    %% will be a 'DOWN'-message with a correct reason
	    %% that will be handled in shutdown/2.
	    {ok, MonRef}
    end.


%%-------------------------------------------------------------------------
%% @spec (Now) -> undefined | {Date, Time}
%%           Now = undefined | erlang:timestamp().
%% @doc Same as `calendar:now_to_local_time/1' but can accept `undefined'.
%% @end
%%-------------------------------------------------------------------------
now_to_datetime(undefined) ->
    undefined;
now_to_datetime(Now) ->
    calendar:now_to_local_time(Now).

%%-------------------------------------------------------------------------
%% @spec (Time) -> string()
%% @doc Format time as "HH:MM:SS".
%% @end
%%-------------------------------------------------------------------------
format_time({H,M,S}) ->
    lists:flatten([i2l(H,2),$:,i2l(M,2),$:,i2l(S,2)]).

i2l(I,W) ->
    string:right(integer_to_list(I), W, $0).

in_period(undefined, _Now, _Period) ->
    false;
in_period(Time, Now, Period) ->
    difference(Time, Now) =< Period.

difference({CurM, TimeS, _}, {CurM, CurS, _}) ->
    CurS - TimeS;
difference({TimeM, TimeS, _}, {CurM, CurS, _}) ->
    ((CurM - TimeM) * 1000000) + (CurS - TimeS).

cancel_timer(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
cancel_timer(_) ->
    ok.

%%-------------------------------------------------------------------------
%% Progress reports
%%-------------------------------------------------------------------------
report_progress(_AlreadyReported = true, _SupName, _Opts, _ReportType, _Id) ->
    ok;
report_progress(_AlreadyReported = false, SupName, Opts, ReportType, Id) ->
    Progress = [{server, sup_name(SupName)}] ++ report_id(Id) ++ Opts,
    do_report(ReportType, info_report, progress, Progress).

report_shutdown(Pid, Name, Reason, Type, Id) ->
    Progress = [{server, sup_name(Name)}] ++ report_id(Id) ++
               [{stopped, [{pid, Pid}]}, {reason, Reason}],
    do_report(Type, info_report, progress, Progress).

report_error(Error, Reason, #state{name=Name, pid=Pid, report_type=Type, id=Id}) ->
    ErrorMsg = [{server, sup_name(Name)}] ++ report_id(Id) ++
               [{stopped, [{pid, Pid}]}, {reason, Reason}] ++
               case Reason of
               normal -> [];
               _      -> [{detail, Error}]
               end,
    do_report(Type, info_report, progress, ErrorMsg).

do_report(undefined, F, Report, Msg) ->
    error_logger:F(Report, Msg);
do_report(Other, F, Report, Msg) ->
    error_logger:F({Other, Report}, Msg).

report_id(undefined)    -> [];
report_id(Id)           -> [{id, Id}].

sup_name(self)          -> self();
sup_name({local, Name}) -> {Name, whereis(Name)};
sup_name({global,Name}) -> {Name, whereis(Name)};
sup_name(Name)          -> {Name, whereis(Name)}.

decode_day(mon)  -> 1;
decode_day(tue)  -> 2;
decode_day(wed)  -> 3;
decode_day(thu)  -> 4;
decode_day(fri)  -> 5;
decode_day(sat)  -> 6;
decode_day(sun)  -> 7;
decode_day(N)  when is_integer(N), N > 0, N =< 7 -> N;
decode_day(N)    -> throw(?FMT("Invalid day spec: ~w", [N])).

day_of_the_week(1) -> mon;
day_of_the_week(2) -> tue;
day_of_the_week(3) -> wed;
day_of_the_week(4) -> thu;
day_of_the_week(5) -> fri;
day_of_the_week(6) -> sat;
day_of_the_week(7) -> sun;
day_of_the_week({Y,M,D}) ->
    day_of_the_week(calendar:day_of_the_week({Y,M,D})).

%%-------------------------------------------------------------------------
%% @spec (DayOfWeek::integer()) -> Result::string()
%% @doc Converts a day of week to a string. Special case: when argument
%%      is 0, it returns "Today".
%% @end
%%-------------------------------------------------------------------------
dow_to_string(0) -> "Today";
dow_to_string(1) -> "Mon";
dow_to_string(2) -> "Tue";
dow_to_string(3) -> "Wed";
dow_to_string(4) -> "Thu";
dow_to_string(5) -> "Fri";
dow_to_string(6) -> "Sat";
dow_to_string(7) -> "Sun";
dow_to_string(N) -> dow_to_string(N rem 7).

to_string({Y,M,D} = Date, _From = {H1,M1,S1}, _To = {H2,M2,S2}, 0) ->
    DOW = dow_to_string(calendar:day_of_the_week(Date)),
    ?FMT("~s ~w/~.2.0w/~.2.0w [~.2.0w:~.2.0w:~.2.0w - ~.2.0w:~.2.0w:~.2.0w]",
         [DOW, Y,M,D, H1,M1,S1, H2,M2,S2]);
to_string({Y,M,D} = Date, {H1,M1,S1}, {H2,M2,S2}, Repeat) ->
    DOW = dow_to_string(calendar:day_of_the_week(Date)),
    {H3,M3,S3} = secs_to_time(Repeat),
    ?FMT("~s ~w/~.2.0w/~.2.0w "
         "[~.2.0w:~.2.0w:~.2.0w - ~.2.0w:~.2.0w:~.2.0w (every ~s)]",
         [DOW, Y,M,D, H1,M1,S1, H2,M2,S2, to_repeat_string([H3,M3,S3])]).

to_string([Dow|_] = DOWs, Times) when is_integer(Dow) ->
    Dows = string:join([dow_to_string(I) || I <- DOWs], ","),
    Tms  = string:join(
            [time_format(From, To, Repeat) || {From, To, Repeat} <- Times],
            ","),
    ?FMT("~s [~s]", [Dows, Tms]);
to_string([], _) ->
    [].

to_repeat_string([0])   -> [];
to_repeat_string([S])   -> [integer_to_list(S), $s];
to_repeat_string([0,S]) -> to_repeat_string([S]);
to_repeat_string([M,S]) -> [integer_to_list(M), $m | to_repeat_string([S])];
to_repeat_string([0|T]) -> to_repeat_string(T);
to_repeat_string([H|T]) -> [integer_to_list(H), $h | to_repeat_string(T)].

time_format({H1,M1,S1}, {H2,M2,S2}, 0) ->
    io_lib:format("~.2.0w:~.2.0w:~.2.0w-~.2.0w:~.2.0w:~.2.0w", [H1,M1,S1, H2,M2,S2]);
time_format({H1,M1,S1}, {H2,M2,S2}, Repeat) ->
    {H3,M3,S3} = secs_to_time(Repeat),
    io_lib:format("~.2.0w:~.2.0w:~.2.0w-~.2.0w:~.2.0w:~.2.0w (every ~s)",
        [H1,M1,S1, H2,M2,S2, to_repeat_string([H3,M3,S3])]).

secs_to_time(Secs) ->
    {Secs div 3600, (Secs rem 3600) div 60, Secs rem 60}.

%%-------------------------------------------------------------------------
%% @spec (TimeSpec::timespec()) -> Result::string()
%% @doc Converts a validated timespec() into a printable string.
%% @end
%%-------------------------------------------------------------------------
print_timespec({?COMPILED_SPEC, Spec}) ->
    print_timespec(Spec);
print_timespec(Spec) when is_tuple(Spec), tuple_size(Spec) =:= 7 ->
    Specs = merge_specs(tuple_to_list(Spec), 1, {1, undefined}, []),
    string:join(Specs, "; ").

merge_specs([undefined | T], N, {I, S}, Acc) ->
    merge_specs(T, N+1, {I, S}, Acc);
merge_specs([Spec | T], N, {_I, undefined}, Acc) ->
    merge_specs(T, N+1, {N, Spec}, [N|Acc]);
merge_specs([Spec | T], N, {I, Spec}, Acc) ->
    merge_specs(T, N+1, {I, Spec}, [N|Acc]);
merge_specs([Spec | T], N, {I, S}, []) ->
    [to_string([I], S) | merge_specs(T, N+1, {N, Spec}, [N])];
merge_specs([undefined | T], N, {I, S}, Acc) ->
    merge_specs(T, N+1, {I, S}, Acc);
merge_specs([Spec | T], N, {_I, []}, _Acc) ->
    merge_specs(T, N+1, {N, Spec}, []);
merge_specs([Spec | T], N, {_I, S}, Acc) ->
    [to_string(lists:reverse(Acc), S) | merge_specs(T, N+1, {N, Spec}, [])];
merge_specs([], _N, {_I, undefined}, []) ->
    [];
merge_specs([], _N, {_I, []}, _Acc) ->
    [];
merge_specs([], _N, {I, S}, Acc) ->
    [to_string(lists:reverse([I|Acc]), S)].

%%-------------------------------------------------------------------------
%% @equiv get_next_time/2
%% @doc Function returns execution time following next execution time.
%%      E.g. if the scheduling window is
%%      `[{mon, {"9:00:00", "17:00:00"}}, {tue, {"9:00:00", "17:00:00"}}]'
%%      and the function is called at 10am on Monday, it'll return Tuesday
%%      9am time (`{1, IntervalSeconds, {{9,0,0},{17,0,0}}}').
%% @end
%%-------------------------------------------------------------------------
get_following_time(State, {Date, Time}) ->
    get_next_time(State, {Date, Time}, skip_current).

%%-------------------------------------------------------------------------
%% @spec (State::#state{}, CurrentTime) -> Result
%%       Result = {Days::integer(), IntervalSeconds::integer(),
%%                      {StartTime::time(), EndTime::time()}}
%%       CurrentTime = {Date, Time}
%% @doc Function returns next execution time.
%%      `IntervalSeconds' is the total number of seconds until next
%%      run interval. `Days' is the number of days of week between `Date'
%%      and date of next run.
%% @end
%%-------------------------------------------------------------------------
get_next_time(#state{} = State, {Date, Time}) ->
    get_next_time(State, {Date, Time}, undefined).

get_next_time(#state{schedule=S, valid_days={VDB,VDE}, next_repeat=NR},
              {Date, Time}, Mode) ->
    {Y,M,D} = Date,
    DayB    = D-1,
    BOM     = case ?DEF_VALID of
              VDB -> 1;
              _   -> VDB band (1 bsl DayB)
              end,
    EOM     = case ?DEF_VALID of
              VDE -> 1;
              _   -> DayE = calendar:last_day_of_the_month(Y,M) - D,
                     VDE band (1 bsl DayE)
              end,
    DOW     = calendar:day_of_the_week(Date),
    % If current day is not in the valid_days mask (value is 0),
    % don't run child pid today.
    case {BOM, EOM} of
    {0, _} ->
        {1, 86400 - calendar:time_to_seconds(Time), {{0,0,0}, {0,0,0}, 0}};
    {_, 0} ->
        {1, 86400 - calendar:time_to_seconds(Time), {{0,0,0}, {0,0,0}, 0}};
    _ ->
        get_next_time(S, DOW, Time, 0, {Date, Time}, NR, Mode)
    end.

get_next_time(Sched, DOW, Time, N, NowDateTime, NextRepeatTime, SkipCurrent) when N < 8 ->
    case element(DOW, Sched) of
    Times when is_list(Times) ->
        case get_next_time2(N, Times, Time, NextRepeatTime, SkipCurrent) of
        false ->
            get_next_time(Sched, (DOW rem 7) + 1, {0,0,0}, N+1,
                          NowDateTime, {0,0,0}, undefined);
        {0, Secs, UntilTime} ->
            {0, Secs, UntilTime};
        {Days, Secs, RunDateTime} ->
            {_Date, Tm}     = NowDateTime,
            SecsTillNextDay = 86400 - calendar:time_to_seconds(Tm),
            TotalSecs       = (Days-1)*86400 + SecsTillNextDay + Secs,
            {Days, TotalSecs, RunDateTime}
        end;
    undefined ->
        get_next_time(Sched, (DOW rem 7) + 1, {0,0,0}, N+1,
                      NowDateTime, {0,0,0}, undefined)
    end;
get_next_time(_, _, _, _, _, _, _) ->
    throw("No runnable time interval found!").

%% [FromTime ..... ToTime]
%%             ^
%%            Now
get_next_time2(Days, [{From, To, _} = H | _], Time, _, undefined) when From =< Time, Time =< To ->
    {Days, 0, H};
get_next_time2(Days, [{From, To, Repeat} = H | _], Time, NextRepeatTime, _)
  when From =< Time, Time =< To, Repeat > 0, NextRepeatTime =< To ->
    if Time =< NextRepeatTime ->
        {Days, get_interval(Time, NextRepeatTime, 0), H};
    true ->
        % This is odd. The job likely executed past the NextRepeatTime
        % and the repeat_timer didn't fire off.
        {{H0,M0,S0}, {H1,M1,S1}} = {From, Time},
        I    = ((H1-H0)*3600 + (M1-M0)*60 + (S1-S0)) rem Repeat,
        Secs = Repeat - I,
        {Days, Secs, H}
    end;
get_next_time2(Days, [{From, To, _} | Tail], Time, NextRepeatTime, skip_current)
  when From =< Time, Time =< To ->
    get_next_time2(Days, Tail, Time, NextRepeatTime, undefined);
%% [... ToTime]
%%              ^
%%             Now
get_next_time2(_Days, [{_From, To, _Repeat}], Time, _, _) when To < Time ->
    false;
%%   [FromTime ...]
%% ^
%% Now
get_next_time2(Days, [{From, _To, _Repeat} = H | _], Time, _, _) when Time < From ->
    {Days, get_interval(Time, From, 0), H};

get_next_time2(Days, [_ | Tail], Time, NextRepeatTime, SkipCurrent) ->
    get_next_time2(Days, Tail, Time, NextRepeatTime, SkipCurrent);
get_next_time2(_, [], _Time, _, _) ->
    false.

get_interval(NowTime, ToTime, Add) ->
    ToSeconds  = calendar:time_to_seconds(ToTime),
    NowSeconds = calendar:time_to_seconds(NowTime),
    if NowTime =< ToTime ->
        ToSeconds - NowSeconds + Add;
    true ->
        % This is impossible except that the process is inspected in the debugger
        % which creates all sorts of delays
        1
    end.

msecs(_Now = {_, _, MkSecs}) ->
    MkSecs div 1000.
now_to_time(Now) ->
    element(2, calendar:now_to_local_time(Now)).

%%%------------------------------------------------------------------------
%%% Test Cases
%%%------------------------------------------------------------------------

-ifdef(EUNIT).

local_fun_test_() ->
    [ ?_assertEqual([{any, [{{0,0,0},{23,59,59}}]}]
        , full_schedule())
    , ?_assertEqual([mon,tue,wed,thu,fri]
        , weekdays())
    , ?_assertEqual({?COMPILED_SPEC, erlang:make_tuple(7, [{{0,0,0},{23,59,59},0}])}
        , validate_schedule(full_schedule()))
    , ?_assertEqual({[{{9,0,0},{10,0,0},0}],[],[],[],[],[],[]}
        , validate_day_spec({[mon], [{"9:00", "10:00"}]}, erlang:make_tuple(7, [])))
    , ?_assertEqual({[],[],[{{9,0,0},{10,0,0},0}],
                           [{{9,0,0},{10,0,0},0}],
                           [{{9,0,0},{10,0,0},0}],
                           [{{9,0,0},{10,0,0},0}],[]}
        , validate_day_spec({{wed,sat}, [{"9:00", "10:00"}]}, erlang:make_tuple(7, [])))
    , ?_assertEqual({[],[],[{{9,0,0},{10,0,0},0}],
                           [{{9,0,0},{10,0,0},0}],
                           [{{9,0,0},{10,0,0},0}],
                           [{{9,0,0},{10,0,0},0}],[]}
        , validate_day_spec({{3,6}, [{"9:00", "10:00"}]}, erlang:make_tuple(7, [])))
    , ?_assertEqual({[],[],[{{9,0,0},{10,0,0},0}],[],[],[],[{{9,0,0},{10,0,0},0}]}
        , validate_day_spec({[wed,sun], [{"9:00", "10:00"}]}, erlang:make_tuple(7, [])))
    , ?_assertEqual("Thu 2009/10/01 [10:05:00 - 10:15:00]"
        , to_string({2009,10,1}, {10,5,0}, {10,15,0}, 0))
    , ?_assertEqual("Thu 2009/10/01 [10:05:00 - 10:15:00 (every 31s)]"
        , to_string({2009,10,1}, {10,5,0}, {10,15,0}, 31))
    , ?_assertEqual("Thu 2009/10/01 [10:05:00 - 10:15:00 (every 1h1m3s)]"
        , to_string({2009,10,1}, {10,5,0}, {10,15,0}, 3663))
    , ?_assertEqual("Mon [09:00:00-10:00:00]"
        , print_timespec({[{{9,0,0},{10,0,0},0}],[],[],[],[],[],[]}))
    , ?_assertEqual("Mon [09:00:00-10:00:00 (every 30s)]"
        , print_timespec({[{{9,0,0},{10,0,0},30}],[],[],[],[],[],[]}))
    , ?_assertEqual({[{monitor_type, monitor}], [{onfailure, exit},{other, []}, option]}
        , split_options([{onfailure, exit}, {monitor_type, monitor}, {other, []}, option]))
    ].

merge_valid_days_test_() ->
    [ ?_assertEqual({7, 1879048192}
        , merge_valid_days(add, [1,2,3,-31,-30,-29], {0,0}))
    , ?_assertEqual({2, 2}
        , merge_valid_days(remove, [1,-1], {3,3}))
    , ?_assertEqual({407,0}
        , merge_valid_days(add, [1,2,3,5,8,9], {0,0}))
    ].

next_time_test_() ->
    Spec = {[],[{{9,0,0},{10,0,0},30},
                {{13,0,0},{14,0,0},0}],
            [],[],[],[],[]},
    [ ?_assertEqual({3,262800,{{9,0,0},{10,0,0},30}}
        , get_next_time(#state{schedule=Spec, next_repeat={8,0,0}},  {{2009,10,10}, {8,0,0}}))
    , ?_assertEqual({0,3600,{{9,0,0},{10,0,0},30}}
        , get_next_time(#state{schedule=Spec, next_repeat={8,0,0}},  {{2009,10,13}, {8,0,0}}))
    , ?_assertEqual({0,0,{{9,0,0},{10,0,0},30}}
        , get_next_time(#state{schedule=Spec, next_repeat={10,0,0}}, {{2009,10,13}, {10,0,0}}))
    , ?_assertEqual({0,7200,{{13,0,0},{14,0,0},0}}
        , get_next_time(#state{schedule=Spec, next_repeat={11,0,0}}, {{2009,10,13}, {11,0,0}}))
    , ?_assertEqual({6,511200,{{9,0,0},{10,0,0},30}}
        , get_next_time(#state{schedule=Spec, next_repeat={11,0,0}}, {{2009,10,14}, {11,0,0}}))
    , ?_assertEqual({0,0,{{9,0,0},{10,0,0},30}}
        , get_next_time(#state{schedule=Spec, next_repeat={9,0,30}}, {{2009,10,13}, {9,0,0}}))
    , ?_assertEqual({0,0,{{9,0,0},{10,0,0},30}}
        , get_next_time(#state{schedule=Spec, next_repeat={9,0,30}}, {{2009,10,13}, {9,0,10}}))
    , ?_assertEqual({0,0,{{9,0,0},{10,0,0},30}}
        , get_next_time(#state{schedule=Spec, next_repeat={9,0,30}}, {{2009,10,13}, {10,0,0}}))
    , ?_assertEqual({0,10799,{{13,0,0},{14,0,0},0}}
        , get_next_time(#state{schedule=Spec, next_repeat={9,0,30}}, {{2009,10,13}, {10,0,1}}))
    ].

following_time_test_() ->
    Spec = {[],[{{9,0,0}, {10,0,0},30},
                {{13,0,0},{14,0,0},0},
                {{15,0,0},{16,0,0},0}],
            [],[],[],[],[]},
    [ ?_assertEqual({0,3600,{{9,0,0},{10,0,0},30}}
        , get_following_time(#state{schedule=Spec, next_repeat={9,0,10}}, {{2009,10,13}, {8,0,0}}))
    , ?_assertEqual({0,0,{{9,0,0},{10,0,0},30}}
        , get_following_time(#state{schedule=Spec, next_repeat={9,0,0}}, {{2009,10,13}, {9,0,0}}))
    , ?_assertEqual({0,30,{{9,0,0},{10,0,0},30}}
        , get_following_time(#state{schedule=Spec, next_repeat={9,0,30}}, {{2009,10,13}, {9,0,0}}))
    , ?_assertEqual({0,29,{{9,0,0},{10,0,0},30}}
        , get_following_time(#state{schedule=Spec, next_repeat={9,0,30}}, {{2009,10,13}, {9,0,31}}))
    , ?_assertEqual({0,10800,{{13,0,0},{14,0,0},0}}
        , get_following_time(#state{schedule=Spec, next_repeat={10,0,30}}, {{2009,10,13}, {10,0,0}}))
    , ?_assertEqual({0,7200,{{15,0,0},{16,0,0},0}}
        , get_following_time(#state{schedule=Spec, next_repeat={13,0,0}}, {{2009,10,13}, {13,0,0}}))
    , ?_assertEqual({0,3599,{{15,0,0},{16,0,0},0}}
        , get_following_time(#state{schedule=Spec, next_repeat={14,0,1}}, {{2009,10,13}, {14,0,1}}))
    , ?_assertEqual({7,579600,{{9,0,0},{10,0,0},30}}
        , get_following_time(#state{schedule=Spec, next_repeat={16,0,0}}, {{2009,10,13}, {16,0,0}}))
    ].

valid_days_test_() ->
    State = #state{schedule={[],[],[],[],[],[{{9,0,0},{10,0,0},30},{{13,0,0},{14,0,0},0}],[]}},
    VDS = {?DEF_VALID, ?DEF_VALID},
    [ ?_assertEqual({0,28800,{{9,0,0},{10,0,0},30}}
        , get_next_time(State#state{valid_days=merge_valid_days(add, [1,2,3], VDS)}, {{2009,1,3},{1,0,0}}))
    , ?_assertEqual({0,28800,{{9,0,0},{10,0,0},30}}
        , get_next_time(State#state{valid_days=merge_valid_days(add, [-1], VDS)}, {{2009,1,31},{1,0,0}}))
    , ?_assertEqual({1,82800,{{0,0,0},{0,0,0},0}}
        , get_next_time(State#state{valid_days=merge_valid_days(add, [1,2], VDS)}, {{2009,1,3},{1,0,0}}))
    ].

normalize_time_test_() ->
    [ ?_assertEqual({ 1,59,59}, normalize_time({ 1,59,59}))
    , ?_assertEqual({ 1,59, 0}, normalize_time({ 1,58,60}))
    , ?_assertEqual({ 3, 0, 0}, normalize_time({ 2,59,60}))
    , ?_assertEqual({24, 0, 0}, normalize_time({23,59,60}))
    ].

format_valid_days_test_() ->
    [ ?_assertEqual([{1,3},5],         format_valid_days(merge_valid_days(add, [1,2,3,5], {0,0})))
    , ?_assertEqual([{1,3},5,{10,12}], format_valid_days(merge_valid_days(add, [1,2,3,5,10,11,12], {0,0})))
    , ?_assertEqual([1,2,{29,31}],     format_valid_days(merge_valid_days(add, [1,2,29,30,31], {0,0})))
    , ?_assertEqual([29,-2,-1],        format_valid_days(merge_valid_days(add, [-1,-2,29], {0,0})))
    , ?_assertEqual([{-7,-5},-2,-1],   format_valid_days(merge_valid_days(add, [-1,-2,-5,-6,-7], {0,0})))
    , ?_assertEqual([{1,31}],          format_valid_days(merge_valid_days(add, [], {?DEF_VALID,?DEF_VALID})))
    , ?_assertEqual([{1,31}],          format_valid_days(merge_valid_days(add, [], {?DEF_VALID,0})))
    ].

-endif.
