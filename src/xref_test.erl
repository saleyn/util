%% Code for testing during development. Finds misspelled function
%% names and incorrect arity usage. It requiers that the modules
%% tested, have already been debug compiled - use erlc with the
%% +debug_info flag, e.g. :
%%
%% erlc +debug_info xxxx.erl
%%
%% This code assumes that the current directory (when calling
%% xref_test:run) is the build folder (the directory where the .beam
%% files are).
%% This code has been tested on my (hsten) local machine, with a
%%
%% > cd src
%% > erlc -W +debug_info *.erl
%% > erl
%% 1> xref_test:run().
%%
%% in the source directory, but should also work in the build
%% directory using the yxa Makefile to compile it.
%%--------------------------------------------------------------------
%% LICENSE: This file is part of the YXA open-source project at:
%%          http://www.stacken.kth.se/project/yxa/
%%--------------------------------------------------------------------
-module(xref_test).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
    run/0,
    run/1,
    run/2,
    run_shell/0
]).

%%--------------------------------------------------------------------
%% Function: run()
%% Descrip.: run xref on yxa code, to look for bugs
%% Returns : -
%%--------------------------------------------------------------------
run() ->
    run([]).
    
run(Dirs) when is_list(Dirs) ->
    run(Dirs, []).

run(Dirs, AddAnalysis) when is_list(Dirs) ->
    Xref = foobar,

    %% stop any old xref process
    try xref:stop(Xref)
    catch
    throw: _ -> ok;
      error: _ -> ok;
      exit: _ -> ok
    end,
    %% start new "empty" xref process
    xref:start(Xref, {xref_mode, functions}),

    %% add path to OTP modules - they should not be detected as unkown modules
    OTP = code:get_path(),
    xref:set_library_path(Xref, OTP, [{verbose, true}]),

    AddOptions = [
          {builtins, false},
          {recurse, false},
          {verbose, true},
          {warnings, true}
    ],

    Dir = 
        case filelib:wildcard("*.beam") of
        [] ->
            case filelib:is_dir("../ebin") of
            true  -> "../ebin";
            false -> "."
            end;
        _ ->
            "."
        end,
    
    %% tell xref where to look for modules to check
    Res = lists:foldl(fun(D, Acc) ->
        case xref:add_directory(Xref, D, AddOptions) of
        {ok, Mods} -> 
            Mods ++ Acc;
        _ ->
            Acc
        end
    end, [], [Dir] ++ Dirs),
    
    io:format("add_directory:~n  ~p~n", [Res]),

    %% determine which properties to check with xref
    Analysis = [
        undefined_function_calls,
        undefined_functions,
        locals_not_used,

        %% this lists lots of functions - some are exported
        %% behaviour callbacks, others are unused functions intended
        %% for future use (to expose a useful interface to the module)
        %% and some are probably callback functions not related to
        %% behaviours.

        %% exports_not_used,
        deprecated_function_calls,
        deprecated_functions

        %% {deprecated_function_calls, DeprFlag},
        %% {deprecated_functions, DeprFlag},
        %% {call, FuncSpec},
        %% {use, FuncSpec},
        %% {module_call, ModSpec},
        %% {module_use, ModSpec},
        %% {application_call, AppSpec},
        %% {application_use, AppSpec},
        %% {release_call, RelSpec},
        %% {release_use, RelSpec}
    ] ++ AddAnalysis,

    %% format analysis results
    Options = [{verbose, true}],
    F = fun(AnalysisKind) ->
        ARes = filter(xref:analyze(Xref, AnalysisKind, Options), AnalysisKind),
        case ARes of
        {ok, L} -> L;
        L       -> L
        end,
        io:format("~n----------------------------------------------------"),
        io:format("~n- ANALYSIS       ~p", [AnalysisKind]),
        io:format("~n----------------------------------------------------"),
        io:format("~n~p~n", [L]),
        L
    end,
    lists:append(lists:map(F, Analysis)) =:= [].

run_shell() ->
    case run() of
    true ->
        erlang:halt(0);
    false ->
        erlang:halt(1)
    end.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: filter(Res, AnalysisKind)
%%           Res          = term(), return value of xref:analyze
%%           AnalysisKind = atom(), the xref:analyze kind
%% Descrip.: remove certain xref:analyze output that only appears
%%           to be wrong
%% Returns : list() of term()
%%--------------------------------------------------------------------
%% filter out the all calls to local:xxxx/yyy
filter(Res, undefined_function_calls) ->
    {ok, L} = Res,
    F = fun(E, Acc) ->
        case E of
            {_, {local,_,_}} -> Acc;
            _ -> [E | Acc]
        end
    end,
    lists:reverse(lists:foldl(F, [], L));

%% filter out the all calls to local:xxxx/yyy
filter(Res, undefined_functions) ->
    {ok, L} = Res,
    F = fun(E, Acc) ->
        case E of
            {local,_,_} -> Acc;
            _ -> [E | Acc]
        end
    end,
    lists:reverse(lists:foldl(F, [], L));

filter(Res, _AnalysisKind) ->
    Res.
