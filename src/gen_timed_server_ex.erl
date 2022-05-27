%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc Sample implementation of a gen_timed_server.  Implements a server
%%%      that executes for a given number of milliseconds and then exits
%%%      to be rescheduled in accordance with provided supervision options.
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
-module(gen_timed_server_ex).
-author('saleyn@gmail.com').
-id("$Id$").

-behaviour(gen_timed_server).

%% External exports
-export([
    start/3, start/2, start_link/3, start_link/2, state/1
]).

%% Internal exports
-export([
      init/1, handle_call/4, handle_cast/3, handle_info/3
    , code_change/3, terminate/2, format_status/2
    , handle_start/2, handle_run/1, handle_stop/4
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
      delay
    , name
    , failure          % if true - process failure is simulated
    , count = 0
    , notify           % pid() to notify on start with {started, pid()}
    , start_time
    , end_time
}).

%%-------------------------------------------------------------------------
%% @spec (Name::sup_name(), ChildMFA::mfa(), SupOpts::sup_options()) ->
%%              {ok, Pid::pid()} | ignore | {error, Error}
%% @doc Creates a server process that will execute the handle_run/1
%%      callback that will sleep Delay seconds.
%% @see //stdlib/supervisor
%% @end
%%-------------------------------------------------------------------------
start_link(Name, Opts, SupOpts) ->
    gen_timed_server:start_link(
        {local, Name}, ?MODULE, [Opts], SupOpts, []).

%%-------------------------------------------------------------------------
%% @spec (ChildMFA::mfa(), SupOpts::sup_options()) ->
%%              {ok, Pid::pid()} | ignore | {error, Error}
%% @doc Creates a timed supervisor as part of supervision tree without
%%      a registered name.
%% @see start_link/3
%% @end
%%-------------------------------------------------------------------------
start_link(Opts, SupOpts) ->
    gen_timed_server:start_link(?MODULE, [Opts], SupOpts, []).

%%-------------------------------------------------------------------------
%% @equiv start_link/3
%% @doc Creates a timed supervisor process outside of supervision tree.
%% @see start_link/3
%% @end
%%-------------------------------------------------------------------------
start(Name, Opts, SupOpts) ->
    gen_timed_server:start(
        {local, Name}, ?MODULE, [Opts], SupOpts, []).

%%-------------------------------------------------------------------------
%% @equiv start_link/2
%% @doc Creates a timed supervisor process outside of supervision tree.
%% @see start_link/3
%% @end
%%-------------------------------------------------------------------------
start(Opts, SupOpts) ->
    gen_timed_server:start(?MODULE, [Opts], SupOpts, []).

%%-------------------------------------------------------------------------
%% @spec (Name::pid()) -> #state{}
%% @end
%%-------------------------------------------------------------------------
state(Name) ->
    gen_timed_server:call(Name, state).

%%-------------------------------------------------------------------------
%% Callback functions from gen_timed_server
%%-------------------------------------------------------------------------

%% @private
init([Opts]) ->
    Delay   = proplists:get_value(delay,   Opts, 100),
    Name    = proplists:get_value(name,    Opts),
    Notify  = proplists:get_value(notify,  Opts),
    Failure = proplists:get_value(failure, Opts, false),
    {ok, #state{delay=Delay, name=Name, notify=Notify, failure=Failure}}.

%% @private
handle_call(state, _From, State, _Opaque) ->
    {reply, State, State};
handle_call(Req, _From, State, _Opaque) ->
    {stop, {unhandled_call, Req}, State}.

%% @private
handle_cast(Req, State, _Opaque) ->
    {stop, {unhandled_cast, Req}, State}.

handle_info(Msg, State, _Opaque) ->
    error_logger:warning_msg("Unhandled msg: ~p\n", [Msg]),
    {noreply, State}.

handle_start(#state{notify=Notify} = State, _Opaque) ->
    % This callback is executed just before a separate process
    % is spawned calling handle_run/1 callback
    Notify ! {starting, self()},
    %?debugFmt("~p --> ~p Starting", [gen_timed_server:format_time(time()), self()]),
    {start, State#state{start_time=erlang:timestamp(), count=State#state.count+1}}.

handle_run(#state{name=Name, delay=Timeout, notify=Notify, failure=Failure}) ->
    % This callback is executed in a separate process
    if Name =:= undefined -> ok; true -> register(Name, self()) end,
    if is_pid(Notify) -> Notify ! {started, self()}; true -> ok end,
    %?debugFmt("~p --> ~p Running", [gen_timed_server:format_time(time()), self()]),
    case Failure of
    true ->
        timer:sleep(100),
        exit(simulated_failure);
    false ->
        timer:sleep(Timeout)
    end.

handle_stop(Action, Reason, #state{notify=Notify, count=N} = State, _Opaque) ->
    % This callback is executed after the process calling handle_run/1
    % callback exited
    Notify ! {stopped, self(), N, Reason},
    %?debugFmt("~p --> ~p Stopped\n    ~p", [
    %    gen_timed_server:format_time(time()), self()
    %    ,[{next_wakeup,gen_timed_server:get(next_wakeup, _Opaque)}
    %     ,{next_repeat,gen_timed_server:get(next_repeat, _Opaque)}
    %     ,{last_start, gen_timed_server:now_to_datetime(gen_timed_server:get(last_start, _Opaque))}
    %     ,{last_fail,  gen_timed_server:now_to_datetime(gen_timed_server:get(last_fail,  _Opaque))}]
    %    ]),
    {Action, Reason, State#state{end_time=erlang:timestamp(), start_time=undefined}}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
format_status(_Opt, [_PDict, #state{}=State]) ->
    Names = record_info(fields, state),
	Vals  = tl(tuple_to_list(State)),
    {state, lists:zip(Names, Vals)}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

-ifdef(EUNIT).

-define(DELAY, 2000).

% Since test setup and execution happen in different processes we use
% a forwarding process as a proxy to forward messages to a given Parent 
% process.  Using `Where' equal `local' is not convenient becase we 
% want a guaranteed cleanup.
forwarder_link() ->
    forwarder ! {set, self()},
    link(whereis(forwarder)).

forwarder() ->
    true = register(forwarder, self()),
    loop(undefined, []).

loop(Parent, L) ->
    receive
    {set, Pid} ->
        [Pid ! M || M <- lists:reverse(L)],
        loop(Pid, []);
    stop ->
        ok;
    Msg when is_pid(Parent) ->
        Parent ! Msg,
        loop(Parent, [])
    end.

cleanup(Pid) ->
    erlang:monitor(process, Pid),
    exit(whereis(forwarder), kill),
    exit(Pid, kill),
    receive {'DOWN', _, process, Pid, _} -> ok end.

%% @private
no_schedule_test_() ->
    F = fun(Pid) -> fun() ->
        forwarder_link(),
        receive {starting, P} -> ?assertEqual(Pid, P) after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started,CPid}-> ok                   after ?DELAY -> CPid = undefined        end,

        ?assert(is_pid(CPid)),
        ?assertEqual(CPid, gen_timed_server:get_child_pid(Pid)),

        MRef = erlang:monitor(process, CPid),

        receive {'DOWN',MRef,_,_,I} -> ?assertEqual(normal, I) after ?DELAY -> throw(timeout) end,
        receive {stopped,P1,1,_}    -> ?assertEqual(Pid, P1) after ?DELAY -> throw({?LINE, timeout}) end,

        ?assert(erlang:is_process_alive(Pid)),
        true
    end end,

    {setup,
        % Setup
        fun() ->
            Forwarder = spawn(fun() -> forwarder() end),
            {ok,Pid} = ?MODULE:start(?MODULE,
                          [{delay,250},{name,first_test},{notify,Forwarder}], []),
            Pid
        end,
        % Cleanup
        fun(Pid) -> cleanup(Pid) end,
        % Tests
        fun(Pid) -> {timeout, 10, F(Pid)} end
    }.

%% @private
repeat_test_() ->
    F = fun(Pid) -> fun() ->
        forwarder_link(),
        timer:sleep(1000),
        receive {starting,P1}     -> ?assertEqual(Pid,P1) after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started,  _}     -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {stopped,Pid,1,_} -> ok after ?DELAY -> throw({?LINE, timeout}) end,

        timer:sleep(1000),
        receive {starting,P2}     -> ?assertEqual(Pid,P2) after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started,  _}     -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {stopped,Pid,2,_} -> ok after ?DELAY -> throw({?LINE, timeout}) end,

        timer:sleep(1000),
        receive {starting, Pid}   -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started, CPid}   -> ok after ?DELAY -> CPid = undefined        end,
        receive {stopped,Pid,3,_} -> ok after ?DELAY -> throw({?LINE, timeout}) end,

        MRef = erlang:monitor(process, CPid),

        receive
        {'DOWN', MRef, _, _, Info} ->
            ?assertEqual(noproc, Info)
        after ?DELAY ->
            throw(timeout)
        end,

        ?assert(erlang:is_process_alive(Pid)),
        true
    end end,

    {setup,
        fun() ->
            Forwarder = spawn(fun() -> forwarder() end),
            DOW   = gen_timed_server:day_of_the_week(date()),
            Time  = calendar:time_to_seconds(time()),
            Start = calendar:seconds_to_time(Time+1),
            End   = calendar:seconds_to_time(Time+8),
            Schedule = [{DOW, [{Start, End, "00:00:02"}]}],
            {ok,Pid} = ?MODULE:start(?MODULE,
                          [{delay,100},{name,repeat_test},{notify,Forwarder}],
                          [{schedule, Schedule}]),
            Pid
        end,
        % Cleanup
        fun(Pid) -> cleanup(Pid) end,
        % Tests
        fun(Pid) -> {timeout, 10, F(Pid)} end
    }.

%% @private
failure_test_() ->
    F = fun(Pid) -> fun() ->
        forwarder_link(),
        MRef = erlang:monitor(process, Pid),

        receive {starting,Pid}    -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started, _}      -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {stopped,Pid,1,E1}->
            ?assertEqual(simulated_failure, E1)
        after ?DELAY ->
            throw({?LINE, timeout})
        end,

        receive {starting,Pid}    -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started, _}      -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {stopped,Pid,2,E2}->
            ?assertEqual(simulated_failure, E2)
        after ?DELAY ->
            throw({?LINE, timeout})
        end,

        receive {starting,Pid}    -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started, _}      -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {stopped,Pid,3,E3}->
            ?assertEqual(reached_max_restart_intensity, E3)
        after ?DELAY ->
            throw({?LINE, timeout})
        end,

        receive {'DOWN', MRef, process, Pid, Info} ->
            ?assertEqual(reached_max_restart_intensity, Info)
        end,
        true
    end end,

    {setup,
        fun() ->
            Forwarder = spawn(fun() -> forwarder() end),
            {ok,Pid} = ?MODULE:start(?MODULE,
                          [{name,failure_test},{notify,Forwarder},{failure,true}],
                          [{restart, {2, 5, 0}}]),
            Pid
        end,
        % Cleanup
        fun(Pid) -> cleanup(Pid) end,
        % Tests
        fun(Pid) -> {timeout, 20, F(Pid)} end
    }.

%% @private
restart_test_() ->
    F = fun(Pid) -> fun() ->
        forwarder_link(),
        receive {starting,Pid}    -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started, _}      -> ok after ?DELAY -> throw({?LINE, timeout}) end,

        ?assertEqual(ok, gen_timed_server:restart(Pid)),

        receive {stopped,Pid,1,_} -> ok after ?DELAY -> throw({?LINE, timeout}) end,

        receive {starting,Pid}    -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started, CPid}   -> ok after ?DELAY -> CPid=undefined, throw({?LINE, timeout}) end,

        ?assert(is_pid(CPid)),
        ?assertEqual(CPid, gen_timed_server:get_child_pid(Pid)),

        MRef = erlang:monitor(process, CPid),

        receive
        {'DOWN',MRef,_,_,I} -> ?assert(I=:=normal orelse I=:=noproc)
        after
            ?DELAY -> throw(timeout)
        end,
        receive {stopped,P1,2,_} -> ?assertEqual(Pid, P1) after ?DELAY -> throw({?LINE, timeout}) end,

        ?assert(erlang:is_process_alive(Pid)),
        true
    end end,

    {setup,
        fun() ->
            Forwarder = spawn(fun() -> forwarder() end),
            {ok,Pid} = ?MODULE:start(?MODULE,
                          [{delay,200},{name,restart_test},{notify,Forwarder}],
                          []),
            Pid
        end,
        % Cleanup
        fun(Pid) -> cleanup(Pid) end,
        % Tests
        fun(Pid) -> {timeout, 10, F(Pid)} end
    }.

%% @private
reschedule_test_() ->
    F = fun(Pid) -> fun() ->
        forwarder_link(),
        receive {starting,Pid}    -> ok after ?DELAY -> throw({?LINE, timeout}) end,
        receive {started, _}      -> ok after ?DELAY -> throw({?LINE, timeout}) end,

        ?assertEqual(ok, gen_timed_server:reschedule(Pid)),

        receive {stopped,Pid,1,_} -> ok after ?DELAY -> throw({?LINE, timeout}) end,

        ?assertEqual(undefined, gen_timed_server:get_child_pid(Pid)),
        ?assertEqual(3600, proplists:get_value(next_wakeup, gen_timed_server:info(Pid))),

        ?assert(erlang:is_process_alive(Pid)),

        ?assertEqual(timeout, receive M -> M after 0 -> timeout end),

        true
    end end,

    {setup,
        fun() ->
            Forwarder = spawn(fun() -> forwarder() end),
            {ok,Pid} = ?MODULE:start(?MODULE,
                          [{delay,200},{name,reschedule_test},{notify,Forwarder}],
                          []),
            Pid
        end,
        % Cleanup
        fun(Pid) -> cleanup(Pid) end,
        % Tests
        fun(Pid) -> {timeout, 10, F(Pid)} end
    }.

-endif.
