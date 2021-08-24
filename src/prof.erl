%%% vim:ts=4:sw=4:et
%%%----------------------------------------------------------------------------
%%% @doc Profiling functions
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2015 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2015-05-13
%%%----------------------------------------------------------------------------
-module(prof).
-author('saleyn@gmail.com').

%% API
-export([start/1, stop/0]).
-export([apply/2, apply/3]).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc Begin profiling a list of pids
%% @end
%%-----------------------------------------------------------------------------
-spec start([pid() | atom()]) -> ok.
start(Pids) when is_list(Pids) ->
    fprof:trace([start, {procs, Pids}]).

%%-----------------------------------------------------------------------------
%% @doc Finish profiling a list of pids and save results to a file called
%%      `"fprof.analysis"'.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    fprof:trace([stop]),
    fprof:profile(),
    fprot:analyse([totals, {dest, "fprof.analysis"}]),
    fprof:stop().

%%-----------------------------------------------------------------------------
%% @doc Run a function analysis and save it to `"fprof-apply.analysis"'.
%% @end
%%-----------------------------------------------------------------------------
apply(Fun, Args) when is_list(Args) ->
    fprof:apply(Fun, Args),
    fprop:profile(),
    fprof:analyse([{dest, "fprof-apply.analysis"}]),
    fprof:stop().

%%-----------------------------------------------------------------------------
%% @doc Run a function analysis and save it to `"fprof-apply.analysis"'.
%% @end
%%-----------------------------------------------------------------------------
apply(M, F, Args) when is_atom(M), is_atom(F), is_list(Args) ->
    fprof:apply(M, F, Args),
    fprop:profile(),
    fprof:analyse([{dest, "fprof-apply.analysis"}]),
    fprof:stop().

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
