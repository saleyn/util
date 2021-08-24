%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%------------------------------------------------------------------
%% This is a minor extension of the stdlib/supervisor_bridge module
%% that exposes three additional functions:
%%      Mod:handle_call/3
%%      Mod:handle_cast/2
%%      Mod:handle_info/2
%%------------------------------------------------------------------
-module(sup_bridge).

-behaviour(gen_server).

%% External exports
-export([start_link/2, start_link/3, get_child_pid/1]).
-export([behaviour_info/1]).
%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).

behaviour_info(callbacks) ->
    [{init,1},{terminate,2},{handle_call,3},{handle_cast,2},{handle_info,2}];
behaviour_info(_Other) ->
    undefined.

%%%-----------------------------------------------------------------
%%% This is a rewrite of supervisor_bridge from BS.3.
%%%
%%% This module is built to function as process code
%%% for a process sitting inbetween a real supervisor
%%% and a not start&recovery complient server/system
%%% The process inbetween simulates start&recovery
%%% behaviour of the server/system below.
%%%
%%% The supervisor_bridge behaviour must export the following
%%% functions:
%%%    init(Args) -> {ok, Pid, State} | {error, Reason} | ignore
%%%       where Pid is the child process
%%%    terminate(Reason, State) -> ok
%%%-----------------------------------------------------------------
-record(state, {mod, pid, child_state, name}).

start_link(Mod, StartArgs) ->
    gen_server:start_link(?MODULE, [Mod, StartArgs, self], []).

start_link(Name, Mod, StartArgs) ->
    gen_server:start_link(Name, ?MODULE, [Mod, StartArgs, Name], []).

get_child_pid(Name) ->
    gen_server:call(Name, get_child_pid).
    
%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
init([Mod, StartArgs, Name0]) ->  
    process_flag(trap_exit, true),
    Name = supname(Name0, Mod),
    case Mod:init(StartArgs) of
	{ok, Pid, ChildState} when is_pid(Pid) ->
	    link(Pid),
	    report_progress(Pid, Mod, StartArgs, Name),
	    {ok, #state{mod = Mod, pid = Pid,
			child_state = ChildState, name = Name}};
	ignore ->
	    ignore;
	{error, Reason} ->
	    {stop, Reason}
    end.

supname(self, Mod) -> {self(),Mod};
supname(N, _)      -> N.

%% A supervisor *must* answer the supervisor:which_children call.
handle_call(which_children, _From, State) ->
    {reply, [], State};
handle_call(get_child_pid, _From, #state{pid = Pid} = State) ->
    {reply, {ok, Pid}, State};
handle_call(Req, From, #state{mod = Mod, child_state = ChildState} = State) ->
    try Mod:handle_call(Req, From, ChildState) of
    {reply, Reply, NewState} ->
        {reply, Reply, State#state{child_state = NewState}};
    {reply, Reply, NewState, Timeout} ->
        {reply, Reply, State#state{child_state = NewState}, Timeout};
    {noreply, NewState} ->
        {noreply, State#state{child_state = NewState}};
    {noreply, NewState, Detail} ->
        {noreply, State#state{child_state = NewState}, Detail};
    {stop, Reason, NewState} ->
        {stop, Reason, State#state{child_state = NewState}};
    {stop, Reason, Reply, NewState} ->
        {stop, Reason, Reply, State#state{child_state = NewState}}
    catch error:{undef, _} ->
        {reply, {error, badcall}, State};
    _:Reason ->
        {stop, Reason, State}
    end.

handle_cast(Req, #state{mod = Mod, child_state = ChildState} = State) ->
    try Mod:handle_cast(Req, ChildState) of
    {noreply, NewState} ->
        {noreply, State#state{child_state = NewState}};
    {noreply, NewState, Timeout} ->
        {noreply, State#state{child_state = NewState}, Timeout};
    {stop, Reason, NewState} ->
        {stop, Reason, State#state{child_state = NewState}}
    catch error:{undef, _} ->
        {noreply, State};
    _:Reason ->
        {stop, Reason, State}
    end.

handle_info({'EXIT', Pid, Reason}, State) when State#state.pid =:= Pid ->
    report_error(child_terminated, Reason, State),
    {stop, Reason, State#state{pid = undefined}};
handle_info(Req, #state{mod = Mod, child_state = ChildState} = State) ->
    try Mod:handle_info(Req, ChildState) of
    {noreply, NewState} ->
        {noreply, State#state{child_state = NewState}};
    {noreply, NewState, Timeout} ->
        {noreply, State#state{child_state = NewState}, Timeout};
    {stop, Reason, NewState} ->
        {stop, Reason, State#state{child_state = NewState}}
    catch error:{undef, _} ->
        {noreply, State};
    _:Reason ->
        {stop, Reason, State}
    end.

terminate(_Reason, #state{pid = undefined}) ->
    ok;
terminate(Reason, State) ->
    terminate_pid(Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% This function is supposed to terminate the 'real' server.
terminate_pid(Reason, #state{mod = Mod, child_state = ChildState}) ->
    Mod:terminate(Reason, ChildState).

report_progress(Pid, Mod, StartArgs, SupName) ->
    Progress = [{supervisor, SupName},
		{started, [{pid, Pid}, {mfa, {Mod, init, [StartArgs]}}]}],
    error_logger:info_report(progress, Progress).

report_error(Error, Reason, #state{name = Name, pid = Pid, mod = Mod}) ->
    ErrorMsg = [{supervisor, Name},
		{errorContext, Error},
		{reason, Reason},
		{offender, [{pid, Pid}, {mod, Mod}]}],
    error_logger:error_report(supervisor_report, ErrorMsg).
