%%%------------------------------------------------------------------------
%%% @doc Module and function decompiler
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @end
%%%------------------------------------------------------------------------
%%% Copyright (c) 2009 Serge Aleynikov
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without restriction,
%%% including without limitation the rights to use, copy, modify, merge,
%%% publish, distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do
%%% so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%------------------------------------------------------------------------
-module(decompiler).
-author('saleyn@gmail.com').

-export([run/1, run/2, fun_src/1, fun_src/2]).

%% @doc Decompile a beam file
-spec run(BeamFilename::string()) -> ok | {error, any()}.
run(BeamFName) when is_list(BeamFName) ->
    run(BeamFName, []).

%% @doc Decompile a beam file and optionally save it to disk
-spec run(string(), [verbose | erl | ast]) -> ok | {error, any()}.
run(BeamFName, Options) when is_list(BeamFName) ->
    case get_abstract_code(BeamFName) of
    {ok, Module, Basename, Forms} ->
        Ast = erl_syntax:form_list(tl(Forms)),
        save_file(lists:member(ast, Options), ast, Options, Basename,
            fun(Fd) -> io:fwrite(Fd, "~p.\n", [Ast]) end),
        Src = erl_prettypr:format(Ast),
        save_file(lists:member(erl, Options) orelse not
                  lists:member(ast, Options),
            erl, Options, Basename,
            fun(Fd) ->
                case [I || I = {attribute,1,module,_} <- Forms] of
                [] -> io:fwrite(Fd, "-module(~w).\n", [Module]);
                _  -> ok
                end,
                io:fwrite(Fd, "~s\n", [Src])
            end);
    {ok,{_,[{abstract_code,no_abstract_code}]}} ->
        print(verbose, Options, "Error: file ~s has no abstract code!\n", [BeamFName]),
        lists:member(verbose, Options) orelse throw(no_abstract_code),
        {error, no_abstract_code};
    Error ->
        lists:member(verbose, Options) orelse throw(Error),
        Error
    end.

%% @doc Decompile a function to its source text
fun_src(Fun) when is_function(Fun) ->
    fun_src(Fun, []).

%% @doc Decompile a function to its source text
-spec fun_src(function(), Options :: [verbose | ast]) -> string().
fun_src(Fun, Options) when is_function(Fun), is_list(Options) ->
    {module, Mod} = erlang:fun_info(Fun, module),
    {name, Name}  = erlang:fun_info(Fun, name),
    {ok, Module, Beam, Forms} = get_abstract_code(Mod),
    {F, Arity, Pos} = fun_name(Name),
    print(verbose, Options, "Module: ~w, Beam: ~s, Name: ~w (~w)\n",
        [Module, Beam, Name, F]),
    fun_src(Module, Name, F, Arity, Pos, Forms, Fun, Options).

print(Opt, Options, Fmt, Args) ->
    case lists:member(Opt, Options) of
    true ->
        io:format(Fmt, Args);
    false ->
        ok
    end.

save_file(false, _Type, _Options, _Basename, _Fun) ->
    false;
save_file(true, erl, Options, Basename, Fun) ->
    ErlFName = Basename ++ ".erl",
    write(ErlFName, Options, Fun);

save_file(true, ast, Options, Basename, Fun) ->
    FName = Basename ++ ".AST",
    write(FName, Options, Fun).

write(Filename, Options, Fun) ->
    {ok, Fd} = file:open(Filename, [write]),
    Fun(Fd),
    file:close(Fd),
    print(verbose, Options, "File: ~s\n", [Filename]), 
    {ok, Filename}.

get_abstract_code(Module) when is_atom(Module) ->
    {module,_} = code:ensure_loaded(Module),
    Beam = code:which(Module),
    get_abstract_code(Beam);
get_abstract_code(Beam) when is_list(Beam) ->
    Basename = filename:basename(Beam, ".beam"),
    case beam_lib:chunks(Beam, [abstract_code]) of
    {ok, {Module,[{abstract_code,{_,AC}}]}} ->
       {ok, Module, Basename, AC};
    Other ->
       Other
    end.

fun_src(erl_eval, _Name, expr, _Arity, _Pos, _Forms, Fun, Options) ->
    {env, [_, _, _, Abst | _]} = erlang:fun_info(Fun, env),
    Ast = erl_syntax:form_list(Abst),
    fun_src2(format, Ast, Options);
fun_src(_Module, _Name, F, Arity, Pos, Forms, _Fun, Options) ->
    Clauses = [Cs || {function, _, Fun, A, Cs} <- Forms, Fun == F, A == Arity],
    Funs    = funs(lists:concat(Clauses)),
    Ast     = lists:nth(Pos, Funs),
    fun_src2(undefined, Ast, Options).

fun_src2(Envelope, Ast, Options) ->
    print(ast, Options, "Ast: ~p\n", [Ast]),
    Text = erl_prettypr:format(Ast),
    case Envelope of
    format ->
        "fun " ++ Text ++ " end.";
    _ ->
        Text ++ "."
    end.

fun_name(Name) ->
    [Fs, As, _, Rs] = string:tokens(atom_to_list(Name), "-/"),
    {list_to_atom(Fs), list_to_integer(As), list_to_integer(Rs)+1}.

funs(L) ->
    lists:reverse(lists:foldl(fun
        ({'fun',_,_} = F, A)    -> [F | A];
        (T, A) when is_tuple(T) -> funs(lists:flatten(tuple_to_list(T))) ++ A;
        (_, A)                  -> A
    end, [],  L)).
