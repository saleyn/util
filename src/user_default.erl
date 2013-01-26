%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc This is an extension of the shell commands
%%%      to do all the work!  Either place this file in the
%%%      path accessible to Erlang (via ERL_LIBS) or
%%%      add this line to the ~/.erlang file:
%%%      ``code:load_abs(os:getenv("HOME") ++ "/.erlang/user_default").''
%%%
%%% @author  Serge Aleynikov <saleyn@gmail.com>
%%% @version $Revision$
%%%          $Date$
%%% @end
%%%------------------------------------------------------------------------
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
-module(user_default).
-author('saleyn@gmail.com').

-export([help/0,dbgtc/1, dbgon/1, dbgon/2,
         dbgadd/1, dbgadd/2, dbgdel/1, dbgdel/2, dbgoff/0,
         p/1, l/0, nl/0, mm/0, tc/2, tc/4]).

-import(io, [format/1, format/2]).

help() ->
    shell_default:help(),
    format("** user extended commands **~n"),
    format("dbgtc(File)   -- use dbg:trace_client() to read data from File\n"),
    format("dbgon(M)      -- enable dbg tracer on all funs in module M\n"),
    format("dbgon(M,Fun)  -- enable dbg tracer for module M and function F\n"),
    format("dbgon(M,File) -- enable dbg tracer for module M and log to File\n"),
    format("dbgadd(M)     -- enable call tracer for module M\n"),
    format("dbgadd(M,F)   -- enable call tracer for function M:F\n"),
    format("dbgdel(M)     -- disable call tracer for module M\n"),
    format("dbgdel(M,F)   -- disable call tracer for function M:F\n"),
    format("dbgoff()      -- disable dbg tracer (calls dbg:stop/0)\n"),
    format("p(Term)       -- print term using io:format(\"~s\\n\", [Term])\n", ["~p"]),
    format("l()           -- load all changed modules\n"),
    format("nl()          -- load all changed modules on all known nodes\n"),
    format("mm()          -- list modified modules\n"),
    format("tc(N,M,F,A)   -- evaluate {M,F,A} N times and return {TotMkSecs, MkSecs/call, Result}\n"),
    format("tc(N,F)       -- evaluate F N times and return {MkSecs, MkSecs/call, Result}\n"),
    true.

%% these are in alphabetic order it would be nice if they
%% were to *stay* so!

dbgtc(File) ->
   Fun = fun({trace,_,call,{M,F,A}}, _)          -> io:format("call: ~w:~w~w~n", [M,F,A]);
            ({trace,_,return_from,{M,F,A},R}, _) -> io:format("retn: ~w:~w/~w -> ~w~n", [M,F,A,R]);
            (A,B)                                -> io:format("~w: ~w~n", [A,B]) end,
   dbg:trace_client(file, File, {Fun, []}).

dbgon(Module) ->
   case dbg:tracer() of
   {ok,_} ->
      dbg:p(all,call),
      dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
      ok;
   Else ->
      Else
   end.

dbgon(Module, Fun) when is_atom(Fun) ->
   {ok,_} = dbg:tracer(),
   dbg:p(all,call),
   dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]),
   ok;

dbgon(Module, File) when is_list(File) ->
   {ok,_} = dbg:tracer(file, dbg:trace_port(file, File)),
   dbg:p(all,call),
   dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
   ok.

dbgadd(Module) ->
   dbg:tpl(Module, [{'_',[],[{return_trace}]}]),
   ok.

dbgadd(Module, Fun) ->
   dbg:tpl(Module, Fun, [{'_',[],[{return_trace}]}]),
   ok.

dbgdel(Module) ->
   dbg:ctpl(Module),
   ok.

dbgdel(Module, Fun) ->
   dbg:ctpl(Module, Fun),
   ok.

dbgoff() ->
   dbg:stop().

%% @doc Term printer
p(Term) ->
   io:format("~p\n", [Term]).

%% @doc Load all changed modules.

l() ->
    lists:foreach(
      fun(M) ->
          io:format("Loading ~p -> ~p~n", [M, c:l(M)])
      end,
      mm()
     ).

%% @doc Load all changed modules on all visible nodes

nl() ->
    lists:foreach(
      fun(M) ->
          io:format("Network loading ~p -> ~p~n", [M, c:nl(M)])
      end,
      mm()
     ).

%% Vladimir Sekissov's fine code

mm() ->
    modified_modules().

modified_modules() ->
    [M || {M, _} <-  code:all_loaded(), module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
    {file, preloaded} ->
        false;
    {file, Path} ->
        CompileOpts = proplists:get_value(compile, Module:module_info()),
        CompileTime = proplists:get_value(time, CompileOpts),
        Src = proplists:get_value(source, CompileOpts),
        module_modified(Path, CompileTime, Src);
    _ ->
        false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
    false ->
        false;
    ModPath ->
        {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
        CompileOpts =  binary_to_term(CB),
        CompileTime = proplists:get_value(time, CompileOpts),
        Src = proplists:get_value(source, CompileOpts),
        not (CompileTime == PrevCompileTime) and (Src == PrevSrc)
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
    {ok, _} ->
        Path;
    _ ->
        %% maybe the path was changed?
        %% NB: the following does not exist ... but the whole works
        %% fairly well anyway
        case code:where_is_file(filename:basename(Path)) of
        non_existing ->
            false;
        NewPath ->
            NewPath
        end
    end.

% Profiling functions inspired by Ulf Wiger post:
% http://www.erlang.org/pipermail/erlang-questions/2007-August/028462.html

tc(N, F) when N > 0 ->
    time_it(fun() -> exit(call(N, N, F, erlang:now())) end).

tc(N, M, F, A) when N > 0 ->
    time_it(fun() -> exit(call(N, N, M, F, A, erlang:now())) end).

time_it(F) -> 
    Pid  = spawn_opt(F, [{min_heap_size, 16384}]),
    MRef = erlang:monitor(process, Pid),
    receive
    {'DOWN', MRef, process, _, Result} -> Result
    end.

call(1, X, F, Time1) ->
    Res = (catch F()),
    return(X, Res, Time1, erlang:now());
call(N, X, F, Time1) ->
    (catch F()),
    call(N-1, X, F, Time1).

call(1, X, M, F, A, Time1) ->
    Res = (catch apply(M, F, A)),
    return(X, Res, Time1, erlang:now());
call(N, X, M, F, A, Time1) ->
    catch apply(M, F, A),
    call(N-1, X, M, F, A, Time1).

return(N, Res, Time1, Time2) ->
    Int   = timer:now_diff(Time2, Time1),
    {Int, Int / N, Res}.

