%%% ============================================================== [ smerl.erl ]
%%% @doc Simple Metaprogramming for Erlang
%%% @author Yariv Sadan
%%% @copyright 2006-2007, 2016 AUTHORS
%%%
%%% Smerl is an Erlang library
%%% that simplifies the creation and manipulation of Erlang modules in
%%% runtime.
%%%
%%% You don't need to know Smerl in order to use ErlyWeb; Smerl
%%% is included in ErlyWeb because ErlyWeb uses it internally.
%%%
%%% Smerl uses Erlang's capabilities for hot code swapping and
%%% abstract syntax tree transformations to do its magic. Smerl is inspired by
%%% the rdbms_codegen.erl module in the RDBMS application written by
%%% Ulf Wiger. RDBMS is part of Jungerl ([http://jungerl.sf.net]).
%%%
%%% Here's a quick example illustrating how to use Smerl:
%%% ```
%%% test_smerl() ->
%%%   M1 = smerl:new(foo),
%%%   {ok, M2} = smerl:add_func(M1, "bar() -> 1 + 1."),
%%%   smerl:compile(M2),
%%%   foo:bar(),   % returns 2``
%%%   smerl:has_func(M2, bar, 0). % returns true
%%% '''
%%%
%%% New functions can be expressed either as strings of Erlang code
%%% or as abstract forms. For more information, read the Abstract Format
%%% section in the ERTS User's guide
%%%  ([http://erlang.org/doc/doc-5.5/erts-5.5/doc/html/absform.html#4]).
%%%
%%% Using the abstract format, the 3rd line of the above example
%%% would be written as
%%%  ```
%%%    {ok,M2} = smerl:add_func(M1, {function,1,bar,0,
%%%                             [{clause,1,[],[],
%%%                              [{op,1,'+',{integer,1,1},{integer,1,1}}]}]).
%%%  '''
%%%
%%%  <p>The abstact format may look more verbose in this example, but
%%%  it's also easier to manipulate in code.</p>
%%% @end
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
%%% ==================================================================== [ EOH ]
-module(smerl).
-author("Yariv Sadan (yarivsblog@gmail.com, http://yarivsblog.com").

%% Public API.
-export([new/1,
         for_module/1, for_module/2, for_module/3,
         for_file/1, for_file/2, for_file/3,
         get_module/1, set_module/2,
         get_forms/1, set_forms/2,
         get_exports/1, set_exports/2, get_export_all/1, set_export_all/2,
         remove_export/3,
         get_attribute/2,
         add_func/2, add_func/3, remove_func/3, has_func/3, get_func/3,
         replace_func/2, % replace_func/3,
         compile/1, compile/2,
         rename/2,
         curry/2, curry/4, curry/5,
         curry_add/3, curry_add/4, curry_add/5, curry_add/6,
         curry_replace/3, curry_replace/4,
         embed_args/2, embed_args/4, embed_args/5, embed_all/2,
         extend/2, extend/3, extend/4,
         to_src/1, to_src/2
        ]).

%%% ================================================================== [ Types ]

-export_type([args/0, export/0, exports/0,
              func_form/0, func_forms/0,
              meta_mod/0,
              result/1, result/2,
              ok_t/1, error_t/1]).

%% TODO: write docstring
-type args() :: term() | [term()].

%% TODO: write docstring
-type export() :: {Function :: atom(), Arity :: arity()}.

-type exports() :: [export()].
%% A list of `export()'s.

-type func_form() :: erl_parse:abstract_form().
%% The abstract form for the function, as described
%% in the ERTS Users' manual.

-type func_forms() :: [func_form()].
%% A list of `func_form()'s.

%% The record type holding the abstract representation for a module.
-record(meta_mod, {module             :: module(),
                   file               :: undefined | file:filename(),
                   exports    = []    :: exports(),
                   forms      = []    :: func_forms(),
                   export_all = false :: boolean()
                  }).

-type meta_mod() :: #meta_mod{}.
%% A data structure holding the abstract representation for a module.

-type result(Value) :: result(Value, term()).

-type result(Value, Error) :: {ok, Value} | {error, Error}.

-type ok_t(Value) :: {ok, Value} | error.

-type error_t(Error) :: ok | {error, Error}.

%%% ================================================================= [ Macros ]

-define(IF(Test, Then, Else), case Test of true -> Then; false -> Else end).

%%% ============================================================= [ Public API ]

-include_lib("kernel/include/file.hrl").

%% @doc Create a new meta_mod for a module with the given name.
-spec new(Module :: module()) -> meta_mod().
new(ModuleName) when is_atom(ModuleName) ->
  #meta_mod{module = ModuleName}.

%% @equiv for_module(ModuleName, [])
for_module(ModuleName) ->
  for_module(ModuleName, []).

%% @equiv for_module(ModuleName, IncludePaths, [])
for_module(ModuleName, IncludePaths) ->
  for_module(ModuleName, IncludePaths, []).

%% @doc Create a meta_mod tuple for an existing module. If ModuleName is a
%% string, it is interpreted as a file name (this is the same as calling
%% {@link for_file/3}). If ModuleName is an atom, <em>Smerl</em> attempts to
%% find its abstract represtation either from its source file or from
%% its .beam file directly (if it has been compiled with debug_info).
%% If the abstract representation can't be found, this function returns
%% an error.
%%
%% The `IncludePaths' argument is used when `ModuleName' is a file name.
-spec for_module(ModuleName, IncludePaths, Macros) -> result(meta_mod) when
    ModuleName   :: atom() | string(),
    IncludePaths :: [string()],
    Macros       :: [{atom(), term()}].
for_module(ModuleName, IncludePaths, Macros) when is_list(ModuleName) ->
  for_file(ModuleName, IncludePaths, Macros);
for_module(ModuleName, IncludePaths, Macros) when is_atom(ModuleName) ->
  [_Exports, _Imports, _Attributes,
   {compile, [_Options, _Version, _Time, {source, Path}]}] =
    ModuleName:module_info(),
  case for_file(Path, IncludePaths, Macros) of
    {ok, _Mod} = Res -> Res;
    _Error           -> get_module_forms(ModuleName)
  end.

%% @equiv for_file(SrcFilePath, [])
for_file(SrcFilePath) ->
  for_file(SrcFilePath, []).

%% @equiv for_file(SrcFilePath, IncludePaths, [])
for_file(SrcFilePath, IncludePaths) ->
  for_file(SrcFilePath, IncludePaths, []).

%% @doc Create a meta_mod for a module from its source file.
-spec for_file(SrcFilePath, IncludePaths, Macros) -> Result when
    SrcFilePath  :: file:filename(),
    IncludePaths :: [file:filename()],
    Macros       :: [{module(), atom()}],
    Result       :: result(meta_mod(), invalid_module).
for_file(SrcFilePath, IncludePaths, Macros) ->
  case epp:parse_file(SrcFilePath, [filename:dirname(SrcFilePath) |
                                    IncludePaths], Macros) of
    {ok, Forms} ->
      mod_for_forms(Forms);
    _Err ->
      {error, {invalid_module, SrcFilePath}}
  end.

%% @doc Return the module name for the meta_mod.
-spec get_module(MetaMod :: meta_mod()) -> module().
get_module(MetaMod) ->
  MetaMod#meta_mod.module.

%% @doc Set the meta_mod's module name.
-spec set_module(MetaMod, NewName) -> NewMod when
    MetaMod :: meta_mod(),
    NewName :: module(),
    NewMod  :: meta_mod().
set_module(MetaMod, NewName) ->
  MetaMod#meta_mod{module = NewName}.

%% @doc Return the list of function forms in the meta_mod.
-spec get_forms(MetaMod :: meta_mod()) -> func_forms().
get_forms(MetaMod) ->
  MetaMod#meta_mod.forms.

-spec set_forms(MetaMod, Forms) -> NewMod when
    MetaMod :: meta_mod(),
    Forms   :: func_forms(),
    NewMod  :: meta_mod().
set_forms(MetaMod, Forms) ->
  MetaMod#meta_mod{forms = Forms}.

%% @doc Return the list of exports in the meta_mod.
-spec get_exports(MetaMod :: meta_mod()) -> exports().
get_exports(MetaMod) ->
  ?IF(not MetaMod#meta_mod.export_all, MetaMod#meta_mod.exports,
      lists:foldl(
        fun({function, _L, Name, Arity, _Clauses}, Exports) ->
            [{Name, Arity} | Exports];
           (_Form, Exports) ->
            Exports
        end, [], MetaMod#meta_mod.forms)).

%% @doc Set the `MetaMod''s export list to `Exports'.
-spec set_exports(MetaMod, Exports) -> NewMod when
    MetaMod :: meta_mod(),
    Exports :: exports(),
    NewMod  :: meta_mod().
set_exports(MetaMod, Exports) ->
  MetaMod#meta_mod{exports = Exports}.

%% @doc Get the `export_all' value for `MetaMod'.
-spec get_export_all(MetaMod :: meta_mod()) -> boolean().
get_export_all(MetaMod) ->
  MetaMod#meta_mod.export_all.

%% @doc Set the `export_all' value for `MetaMod'.
-spec set_export_all(MetaMod, Value) -> NewMod when
    MetaMod :: meta_mod(),
    Value   :: boolean(),
    NewMod  :: meta_mod().
set_export_all(MetaMod, Val) ->
  MetaMod#meta_mod{export_all = Val}.

%% @doc Remove an export `{Function, Arity}'
%% from the list of `exports' in `MetaMod'.
-spec remove_export(MetaMod, Function, Arity) -> NewMod when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    NewMod   :: meta_mod().
remove_export(MetaMod, FuncName, Arity) ->
  MetaMod#meta_mod{exports =
                     lists:delete({FuncName, Arity},
                                  MetaMod#meta_mod.exports)}.

%% @doc Get the value of `MetaMod''s `Key' attribute.
-spec get_attribute(MetaMod :: meta_mod(), Key :: atom()) -> result(term()).
get_attribute(MetaMod, Key) ->
  case lists:keyfind(Key, 3, get_forms(MetaMod)) of
    {attribute, _, _, Val} -> {ok, Val};
    _                      -> error
  end.

%% @doc Add a new exported function to `MetaMod'.
%% @equiv add_func(MetaMod, Form, true)
-spec add_func(MetaMod, Form) -> result(meta_mod(), parse_error) when
    MetaMod :: meta_mod(),
    Form    :: func_form() | string().
add_func(MetaMod, Form) ->
  add_func(MetaMod, Form, true).

%% @doc Add `Function' to `MetaMod' and return the new `meta_mod()'. If
%% `Export' is `true', add `Function' to `MetaMod''s `exports'.
-spec add_func(MetaMod, Func, Export) -> result(meta_mod(), parse_error) when
    MetaMod :: meta_mod(),
    Func    :: func_form() | string(),
    Export  :: boolean().
add_func(MetaMod, Func, Export) when is_list(Func) ->
  case parse_func_string(Func) of
    {ok, Form} ->
      add_func(MetaMod, Form, Export);
    Err ->
      Err
  end;
add_func(MetaMod, {function, _Line, FuncName, Arity, _Clauses} = Form, true) ->
  Foo = {ok, MetaMod#meta_mod{
               exports = [{FuncName, Arity} | MetaMod#meta_mod.exports],
               forms = [Form | MetaMod#meta_mod.forms]
              }},
  Foo;
add_func(MetaMod, {function, _Line, _Func, _Arity, _Clauses} = Form, false) ->
  {ok, MetaMod#meta_mod{forms = [Form | MetaMod#meta_mod.forms]}};
add_func(_, _, _) ->
  {error, parse_error}.

%% @doc Try to remove `Function' from `MetaMod'.
%% If the function exists, return the new `meta_mod()'.
%% Otherwise, return `MetaMod'.
-spec remove_func(MetaMod, Function, Arity) -> NewMod when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    NewMod   :: meta_mod().
remove_func(MetaMod, Function, Arity) ->
  Forms = [ Form || {function, _L, F, A, _Cs} = Form <- MetaMod#meta_mod.forms,
                    F =:= Function, A =:= Arity ],
  Exports = [ {F, A} || {F, A} <- MetaMod#meta_mod.exports,
                        F =:= Function, A =:= Arity ],
  MetaMod#meta_mod{forms = Forms, exports = Exports}.

%% @doc Check whether `MetaMod' has a function `Function'/`Arity'.
-spec has_func(MetaMod, Function, Arity) -> boolean() when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity().
has_func(MetaMod, FuncName, Arity) ->
  lists:any(fun({function, _Line, FuncName1, Arity1, _Clauses})
                when FuncName1 == FuncName, Arity1 == Arity ->
                true;
               (_) ->
                false
            end, MetaMod#meta_mod.forms).

%% @doc Attempt to get the `func_form()' for `MetaMod':`Function'/`Arity'.
-spec get_func(MetaMod, Function, Arity) -> result(func_form()) when
    MetaMod  :: meta_mod() | module(),
    Function :: atom(),
    Arity    :: arity().
get_func(Module, FuncName, Arity) when is_atom(Module) ->
  case smerl:for_module(Module) of
    {ok, C1} ->
      get_func(C1, FuncName, Arity);
    Err ->
      Err
  end;
get_func(MetaMod, FuncName, Arity) ->
  do_get_func(MetaMod#meta_mod.forms, FuncName, Arity).

%% @doc Replace an existing function with a new one. If a matching function
%% doesn't exist, add `Function' to `MetaMod'. This is tantamount to calling
%% {@link remove_func/3} followed by {@link add_func/2}.
-spec replace_func(MetaMod, Function) -> result(meta_mod()) when
    MetaMod  :: meta_mod(),
    Function :: string() | func_form().
replace_func(MetaMod, Function) when is_list(Function) ->
  case parse_func_string(Function) of
    {ok, Form} ->
      replace_func(MetaMod, Form);
    Err ->
      Err
  end;
replace_func(MetaMod, {function, _Line, FuncName, Arity, _Clauses} = Form) ->
  Mod1 = remove_func(MetaMod, FuncName, Arity),
  add_func(Mod1, Form);
replace_func(_MetaMod, _) ->
  {error, parse_error}.

%% @doc Compile `MetaMod' and load the resulting BEAM into the emulator.
%% @equiv compile(MetaMod, [])
-spec compile(MetaMod :: meta_mod()) -> error_t(term()).
compile(MetaMod) ->
  compile(MetaMod, []).

%% @doc Compile `MetaMod' and load the resulting BEAM into the emulator.
%% `Options' is a list of options as described in the `compile' module in the
%% Erlang documentation.
%% If an `outdir' is provided, write the `.beam' file to it.
%% @equiv compile(MetaMod, [report_errprs, report_warnings, return_errors])
-spec compile(MetaMod, Options) -> error_t(term()) when
    MetaMod :: meta_mod(),
    Options :: [proplists:property()].
compile(MetaMod, []) ->
  compile(MetaMod, [report_errors, report_warnings,
                    return_errors]);

compile(MetaMod, Options) ->
  Forms = [{attribute, 2, module, MetaMod#meta_mod.module},
           {attribute, 3, export, get_exports(MetaMod)}],
  FileName = case MetaMod#meta_mod.file of
               undefined -> atom_to_list(get_module(MetaMod));
               Val       -> Val
             end,
  Forms1 = [{attribute, 1, file, {FileName, 1}} | Forms],
  Forms2 = Forms1 ++ lists:reverse(MetaMod#meta_mod.forms),
  compile(MetaMod#meta_mod.module, Forms2, Options).

%% @doc Change the name of the function represented by `Form' to `NewName'.
-spec rename(Form :: func_form(), NewName :: atom()) -> func_form().
rename({function, Line, _Name, Arity, Clauses}, NewName) ->
  {function, Line, NewName, Arity, Clauses}.

%% @doc Get the curried form for `Form' with `Args'.
%% Here, "currying" involves replacing one or more of the function's leading
%% arguments with predefined values.
-spec curry(Form :: func_form(), Args :: args()) -> result(func_form()).
curry(Form, Arg) when not is_list(Arg) ->
  curry(Form, [Arg]);
curry({function, _Line, _Name, Arity, _Clauses}, Args)
  when length(Args) > Arity ->
  {error, too_many_args};
curry({function, Line, Name, Arity, Clauses}, NewArgs) ->
  NewClauses =
    lists:foldl(
      fun(Clause, Clauses1) ->
          [curry_clause(Clause, NewArgs) | Clauses1]
      end, [], Clauses),
  {ok, {function, Line, Name, Arity-length(NewArgs), NewClauses}}.

%% @doc Curry `Module':`Function'/`Arity' with the given `Args'.
-spec curry(Module, Function, Arity, Args) -> result(func_form()) when
    Module   :: module() | meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Args     :: args().
curry(ModName, Name, Arity, Args) when is_atom(ModName) ->
  case for_module(ModName) of
    {ok, MetaMod} ->
      curry(MetaMod, Name, Arity, Args);
    Err ->
      Err
  end;
curry(MetaMod, Name, Arity, Args) ->
  case get_func(MetaMod, Name, Arity) of
    {ok, Form} ->
      curry(Form, Args);
    Err ->
      Err
  end.

%% @doc Curry `Module':`Function'/`Arity'with the given `Args',
%% renaming it to `NewName' and return the renamed form.
-spec curry(Module, Function, Arity, Args, NewName) -> result(func_form()) when
    Module   :: module() | meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Args     :: args(),
    NewName  :: atom().
curry(Module, Name, Arity, Args, NewName) ->
  case curry(Module, Name, Arity, Args) of
    {ok, NewForm} ->
      {ok, rename(NewForm, NewName)};
    Err ->
      Err
  end.

%% @doc Add `Form' curried with `Args' to `MetaMod'.
-spec curry_add(MetaMod, Form, Args) -> result(meta_mod()) when
    MetaMod :: meta_mod(),
    Form    :: func_form(),
    Args    :: args().
curry_add(MetaMod, {function, _Line, Name, Arity, _Clauses}, Args) ->
  curry_add(MetaMod, Name, Arity, Args).

%% @doc Add `Function'/`Arity' curried with `Args' to `MetaMod'.
-spec curry_add(MetaMod, Function, Arity, Args) -> result(meta_mod()) when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Args     :: args().
curry_add(MetaMod, Name, Arity, Args) ->
  curry_change(MetaMod, Name, Arity, Args, false).

%% @doc Curry `MetaMod':`Function'/`Arity' and add it to `MetaMod' as `NewName'.
-spec curry_add(MetaMod, Function, Arity, Args, NewName) -> Result when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Args     :: args(),
    NewName  :: atom(),
    Result   :: result(meta_mod(), parse_error).
curry_add(MetaMod, Name, Arity, Args, NewName) ->
  curry_add(MetaMod, MetaMod, Name, Arity, Args, NewName).

%% @doc Curry `Module':`Function'/`Arity' and add it to `MetaMod' as `NewName'.
-spec curry_add(MetaMod, Module, Function, Arity, Args, NewName) -> Result when
    MetaMod  :: meta_mod(),
    Module   :: module() | meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Args     :: args(),
    NewName  :: atom(),
    Result   :: result(meta_mod()).
curry_add(MetaMod, Module, Name, Arity, Args, NewName) ->
  case curry(Module, Name, Arity, Args, NewName) of
    {ok, Form} ->
      add_func(MetaMod, Form);
    Err ->
      Err
  end.

%% @doc Replace the function represented by `Form' in `MetaMod'
%% with its curried form.
-spec curry_replace(MetaMod, Form, Args) -> result(meta_mod()) when
    MetaMod :: meta_mod(),
    Form    :: func_form(),
    Args    :: args().
curry_replace(MetaMod, {function, _Line, Name, Arity, _Clauses}, Args) ->
  curry_replace(MetaMod, Name, Arity, Args).

%% @doc Replace `Function'/`Arity' in `MetaMod' with its curried form.
-spec curry_replace(MetaMod, Function, Arity, Args) -> result(meta_mod()) when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Args     :: args().
curry_replace(MetaMod, Name, Arity, Args) ->
  curry_change(MetaMod, Name, Arity, Args, true).

%% @doc Replace the arguments of the function represented by `Form',
%% where the argument's `Name' matches an element from `Vals'
%% with the corresponding `Value'.
-spec embed_args(Form, Vals) -> NewForm when
    Form    :: func_form(),
    Vals    :: [{Name :: atom(), Value :: term()}],
    NewForm :: func_form().
embed_args({function, L, Name, Arity, Clauses}, Vals) ->
  NewClauses = new_clauses(Clauses, Vals),
  NewArity = case NewClauses of
               [{clause, _L2, Args, _Guards, _Exprs}|_] ->
                 length(Args);
               _ ->
                 Arity
             end,
  {function, L, Name, NewArity, NewClauses}.

%% @equiv embed_args(MetaMod, Name, Arity, Values, Name)
-spec embed_args(MetaMod, Function, Arity, Values) -> result(meta_mod()) when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Values   :: proplists:proplist().
embed_args(MetaMod, Name, Arity, Values) ->
  embed_args(MetaMod, Name, Arity, Values, Name).

%% @doc Apply {@link embed_args/2} to `MetaMod':`Function'/`Arity' and
%% add the resulting function to `MetMod', after renaming it to `NewName'.
%% @see rename/2
-spec embed_args(MetaMod, Function, Arity, Values, NewName) -> Result when
    MetaMod  :: meta_mod(),
    Function :: atom(),
    Arity    :: arity(),
    Values   :: proplists:proplist(),
    NewName  :: atom(),
    Result   :: result(meta_mod()).
embed_args(MetaMod, Name, Arity, Values, NewName) ->
  case get_func(MetaMod, Name, Arity) of
    {ok, Form} ->
      NewForm = embed_args(Form, Values),
      add_func(MetaMod, rename(NewForm, NewName));
    Err ->
      Err
  end.

%% @doc Apply {@link embed_args/2} with `Values' to all forms in `MetaMod'.
%% `exports' for functions whose arities change are preserved.
-spec embed_all(MetaMod, Values) -> NewMod when
    MetaMod :: meta_mod(),
    Values  :: [{Name :: atom(), Value :: term()}],
    NewMod  :: meta_mod().
embed_all(MetaMod, Vals) ->
  Forms = get_forms(MetaMod),
  Exports = get_exports(MetaMod),
  {NewForms, Exports3, NewExports} =
    lists:foldl(
      fun({function, _L, Name, Arity, _Clauses} = Form,
          {Forms1, Exports1, NewExports1}) ->
          {function, _, _, NewArity, _} = NewForm =
            embed_args(Form, Vals),
          Exports2 = lists:delete({Name, Arity}, Exports1),
          NewExports2 =
            ?IF(length(Exports2) == length(Exports1),
                NewExports1,
                [{Name, NewArity} | NewExports1]),
          {[NewForm | Forms1], Exports2, NewExports2};
         (Form, {Forms1, Exports1, NewExports1}) ->
          {[Form | Forms1], Exports1, NewExports1}
      end, {[], Exports, []}, Forms),
  #meta_mod{module = get_module(MetaMod),
            exports = Exports3 ++ NewExports,
            forms = lists:reverse(NewForms),
            export_all = get_export_all(MetaMod)}.

%% @doc Add aliases for `Parent''s functions missing from `Child' to `Child'.
%% The new functions in `Child' are shallow, i.e. they have the name and arity
%% of the corresponding functions in `Parent', but instead of implementing their
%% logic they call the `Parent' functions.
-spec extend(Parent, Child) -> NewChildMod when
    Parent      :: module() | meta_mod(),
    Child       :: module() | meta_mod(),
    NewChildMod :: meta_mod().
extend(Parent, Child) ->
  extend(Parent, Child, 0).

%% @doc Similar to {@link extend/2}, with the addition of `ArityDiff', which
%% indicates the difference in arities <em>Smerl</em> should use when figuring
%% out which functions to generate based on the modules' exports. This is
%% sometimes useful when calling {@link extend/3} followed by {@link
%% embed_all/2}.
-spec extend(Parent, Child, ArityDiff) -> NewChildMod when
    Parent      :: module() | meta_mod(),
    Child       :: module() | meta_mod(),
    ArityDiff   :: non_neg_integer(),
    NewChildMod :: meta_mod().
extend(Parent, Child, ArityDiff) ->
  extend(Parent, Child, ArityDiff, []).

-spec extend(Parent, Child, ArityDiff, Options) -> NewChildMod when
    Parent      :: module() | meta_mod(),
    Child       :: module() | meta_mod(),
    ArityDiff   :: non_neg_integer(),
    Options     :: [proplists:property()],
    NewChildMod :: meta_mod().
extend(Parent, Child, ArityDiff, Options) ->
  {{ParentName, ParentExports, ParentMod}, ChildMod} =
    get_extend_data(Parent, Child, Options),
  ChildExports = get_exports(ChildMod),
  ChildExports1 = [{ExportName, ExportArity + ArityDiff} ||
                    {ExportName, ExportArity} <-
                      ChildExports],
  ExportsDiff = ParentExports -- ChildExports1,
  NewChild =
    lists:foldl(
      fun({FuncName, Arity}, ChildMod1) ->
          Func =
            case lists:member(copy, Options) of
              true ->
                {ok, ParentFunc} =
                  smerl:get_func(ParentMod, FuncName, Arity),
                ParentFunc;
              _ ->
                Args = get_args(
                         ParentMod, FuncName, Arity),
                Clause1 =
                  {clause, 1, Args, [],
                   [{call, 1,
                     {remote, 1, {atom, 1, ParentName},
                      {atom, 1, FuncName}},
                     Args}]},
                {function, 1, FuncName, Arity, [Clause1]}
            end,
          {ok, ChildMod2} = add_func(ChildMod1, Func),
          ChildMod2
      end, ChildMod, ExportsDiff),
  NewChild.

%% @doc Return the pretty-printed source code for `MetaMod'.
-spec to_src(MetaMod :: meta_mod()) -> Source :: string().
to_src(MetaMod) ->
  ExportsForm = {attribute, 1, export, get_exports(MetaMod)},
  AllForms = [{attribute, 1, module, get_module(MetaMod)}, ExportsForm |
              get_forms(MetaMod)],
  erl_prettypr:format(erl_syntax:form_list(AllForms)).

%% @equiv file:write_file(Filename , to_src(MetaMod))
-spec to_src(MetaMod, Filename) -> error_t(term()) when
    MetaMod  :: meta_mod(),
    Filename :: file:filename().
to_src(MetaMod, Filename) ->
  file:write_file(Filename, to_src(MetaMod)).

%%% ===================================================== [ Internal functions ]

get_module_forms(ModuleName) ->
  case code:which(ModuleName) of
    Path when is_list(Path) ->
      case get_forms(ModuleName, Path) of
        {ok, Forms} -> mod_for_forms(Forms);
        _Error      -> {error, {invalid_module, ModuleName}}
      end;
    _Error ->
      {error, {invalid_module, ModuleName}}
  end.

mod_for_forms([{attribute, _, file, {FileName, _FileNum}},
               {attribute, _, module, ModuleName}|Forms]) ->
  {Exports, OtherForms, ExportAll} =
    lists:foldl(
      fun({attribute, _, export, ExportList},
          {ExportsAcc, FormsAcc, ExportAll}) ->
          {ExportList ++ ExportsAcc, FormsAcc, ExportAll};
         ({attribute, _, compile, export_all},
          {ExportsAcc, FormsAcc, _ExportAll}) ->
          {ExportsAcc, FormsAcc, true};
         ({eof, _}, Acc) ->
          Acc;
         (Form, {ExportsAcc, FormsAcc, ExportAll}) ->
          {ExportsAcc, [Form | FormsAcc], ExportAll}
      end, {[], [], false}, Forms),
  {ok, #meta_mod{module = ModuleName,
                 file = FileName,
                 exports = Exports,
                 forms = OtherForms,
                 export_all = ExportAll
                }};
mod_for_forms(Mod) ->
  {error, {invalid_module, Mod}}.

%% @doc Get the abstract representation, if available, of `Module'.
%%
%% Strategy:
%% <ol>
%%   <li>Try to get the abstract code from `Module' if it's compiled with
%%    `debug_info'.</li>
%%   <li>Look for the source file in the beam file's directory.</li>
%%   <li>If the file's directory ends with `ebin', then search in
%%   `[beamdir]/../src'</li>
%% </ol>
get_forms(Module, Path) ->
  case beam_lib:chunks(Path, [abstract_code]) of
    {ok, {_, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
      {ok, Forms};
    _Err ->
      case filelib:find_source(Module) of
        {error, _} = Err ->
          get_forms_from_binary(Module, Err);
        {ok, Filename} ->
          epp:parse_file(Filename, [filename:dirname(Filename)], [])
      end
  end.

%% @doc Try to infer module source files from the beam code path.
get_forms_from_binary(Module, OrigErr) ->
  Ret =
    case code:where_is_file(atom_to_list(Module) ++ ".beam") of
      non_existing ->
        OrigErr;
      Filename ->
        %% We could automatically obtain a list of all dirs under this dir,
        %%but we just do a hack for now.
        Basedir = filename:dirname(Filename),
        Lastdir = filename:basename(Basedir),
        case Lastdir of
          "ebin" ->
            Rootdir = filename:dirname(Basedir),
            DirList0 = [Rootdir ++ "/src"],
            DirList = DirList0 ++ get_dirs_in_dir(Rootdir ++ "/src"),
            get_forms_from_file_list(Module, Rootdir, DirList);
          _ ->
            DirList = [Basedir],
            get_forms_from_file_list(Module, Basedir, DirList)
        end
    end,
  case Ret of
    [] -> OrigErr;
    _ -> Ret
  end.

get_dirs_in_dir(Dir) ->
  case list_dir(Dir) of
    {ok, Listing} ->
      lists:filtermap(fun(Name) -> is_directory(Dir, Name) end, Listing);
    {error, _} ->
      undefined
  end.

%% @equiv is_directory(filename:join(Directory, Filename))
is_directory(Directory, Filename) ->
  is_directory(filename:join(Directory, Filename)).

is_directory(Filename) ->
  case read_file_info(Filename) of
    {ok, #file_info{type=directory}} -> true;
    _                                -> false
  end.

get_forms_from_file_list(_Module, _Basedir, []) ->
  [];
get_forms_from_file_list(Module, Basedir, [H|T]) ->
  Filename = H ++ "/" ++ atom_to_list(Module) ++ ".erl",
  case read_file_info(Filename) of
    {ok, #file_info{type=regular}} ->
      epp:parse_file(Filename, [filename:dirname(Filename)], []);
    _ ->
      get_forms_from_file_list(Module, Basedir, T)
  end.

do_get_func([], FuncName, Arity) ->
  {error, {function_not_found, {FuncName, Arity}}};
do_get_func([{function, _Line, FuncName, Arity, _Clauses} = Form | _Rest],
            FuncName, Arity) ->
  {ok, Form};
do_get_func([_Form|Rest], FuncName, Arity) ->
  do_get_func(Rest, FuncName, Arity).

parse_func_string(Func) ->
  case erl_scan:string(Func) of
    {ok, Toks, _} ->
      case erl_parse:parse_form(Toks) of
        {ok, _Form} = Res ->
          Res;
        _Err ->
          {error, parse_error}
      end;
    _Err ->
      {error, parse_error}
  end.

compile(ModName, Forms, Options) ->
  case compile:forms(Forms, Options) of
    {ok, Module, Bin} ->
      OutDir = proplists:get_value(outdir, Options),
      maybe_write_beam_file({ModName, Module}, Bin, OutDir);
    Err ->
      Err
  end.

maybe_write_beam_file({_ModName, Module}, Bin, undefined) ->
  Filename = case code:which(Module) of
               non_existing -> atom_to_list(Module) ++ ".erl";
               Path         -> Path
             end,
  code:purge(Module),
  case code:load_binary(Module, Filename, Bin) of
    {module, _Module} -> ok;
    Err               -> Err
  end;
maybe_write_beam_file({ModName, _Module}, Bin, OutDir) ->
  BeamFile = atom_to_list(ModName) ++ ".beam",
  file:write_file(filename:join(OutDir, BeamFile), Bin).

curry_clause({clause, L1, ExistingArgs, Guards, _Exprs} = Clause, NewArgs) ->
  {FirstArgs, LastArgs} = lists:split(length(NewArgs), ExistingArgs),
  Vals = [ {Name, erl_parse:abstract(NewVal)}
           || {{var, _ , Name}, NewVal} <- lists:zip(FirstArgs, NewArgs) ],
  NewExprs = replace_vars(Clause, Vals),
  {clause, L1, LastArgs, Guards, NewExprs}.

replace_vars(Clause, Vals) ->
  Tree =
    erl_syntax_lib:map(
      fun({var, _L2, Name} = Expr) ->
          case proplists:lookup(Name, Vals) of
            none ->
              Expr;
            {_, Val} ->
              Val
          end;
         (Expr) ->
          Expr
      end, Clause),
  {clause, _, _, _, NewExprs} = erl_syntax:revert(Tree),
  NewExprs.

curry_change(MetaMod, Name, Arity, Args, Remove) ->
  case get_func(MetaMod, Name, Arity) of
    {ok, OldForm} ->
      case curry(OldForm, Args) of
        {ok, NewForm} ->
          MetaMod1 =
            ?IF(Remove, remove_func(MetaMod, Name, Arity), MetaMod),
          add_func(MetaMod1, NewForm);
        Err ->
          Err
      end;
    Err ->
      Err
  end.

%% @see embed_args/2
new_clauses(Clauses, Vals) ->
  [ begin
      {EmbeddedVals, OtherArgs} =
        lists:foldr(
          fun({var, _, VarName} = Arg, {Embedded, Rest}) ->
              case proplists:lookup(VarName, Vals) of
                none ->
                  {Embedded, [Arg | Rest]};
                {_, Val} ->
                  {[{VarName, erl_parse:abstract(Val)} |
                    Embedded], Rest}
              end;
             (Arg, {Embedded, Rest}) ->
              {Embedded, [Arg | Rest]}
          end, {[], []}, Args),
      NewExprs = replace_vars(Clause, EmbeddedVals),
      {clause, L1, OtherArgs, Guards, NewExprs}
    end || {clause, L1, Args, Guards, _Exprs} = Clause <- Clauses].

get_extend_data(Parent, Child, Options) when is_atom(Parent) ->
  SrcDir = proplists:get_value(src_dir, Options),
  do_get_extend_data(Parent, Child, Options, SrcDir);
get_extend_data(Parent, Child, Options) when is_record(Parent, meta_mod) ->
  Data = {get_module(Parent), get_exports(Parent), Parent},
  get_extend_data(Data, Child, Options);
get_extend_data(Parent, Child, Options) when is_list(Parent) ->
  case for_file(Parent) of
    {ok, M1} ->
      get_extend_data(M1, Child, Options);
    Err ->
      Err
  end;
get_extend_data({_, _, _} = ParentData, Child, _Options)
  when is_atom(Child); is_list(Child) ->
  case for_module(Child) of
    {ok, MetaMod} ->
      {ParentData, MetaMod};
    Err ->
      Err
  end;
get_extend_data(ParentData, Child, _Options) when is_record(Child, meta_mod) ->
  {ParentData, Child}.

do_get_extend_data(Parent, Child, Options, undefined) ->
  [{exports, Exports} |_] = Parent:module_info(),
  Exports1 = Exports -- [{module_info, 0}],
  Exports2 = Exports1 -- [{module_info, 1}],
  ParentMod = case smerl:for_module(Parent) of
                {ok, M}    -> M;
                {error, _} -> undefined
              end,
  get_extend_data({Parent, Exports2, ParentMod}, Child, Options);
do_get_extend_data(Parent, Child, Options, Dir) ->
  Filename = filename:join(Dir, atom_to_list(Parent) ++ ".erl"),
  %% Check if file exists
  case read_file_info(Filename) of
    {ok, _FileInfo} ->
      ParentMod = case smerl:for_file(Filename) of
                    {ok, MetaMod} -> MetaMod;
                    {error, _} -> undefined
                  end,
      get_extend_data(ParentMod, Child, Options);
    Error ->
      Error
  end.

get_args(_, _, 0) -> [];
get_args(undefined, _FuncName, Arity) ->
  [{var, 1, list_to_atom("P" ++ integer_to_list(Num))}
   || Num <- lists:seq(1, Arity)];
get_args(ParentMod, FuncName, Arity) ->
  {ok, {function, _L, _Name, _Arity,
        [{clause, _, Args, _Guards, _Exprs} | _]}} =
    get_func(ParentMod, FuncName, Arity),
  Args.

-spec list_dir(Dir :: file:dirname()) -> Result when
    Result :: result([file:filename()], erl_prim_loader).
list_dir(Dir) ->
  handle_list_dir(erl_prim_loader:list_dir(Dir)).

handle_list_dir(error)           -> {error, erl_prim_loader};
handle_list_dir({ok, Filenames}) -> {ok, Filenames}.

-spec read_file_info(Filename :: file:filename()) -> Result when
    Result :: result(file:file_info(), file:posix() | badarg).
read_file_info(Filename) ->
  erl_prim_loader:read_file_info(Filename).

%%% ==================================================================== [ EOF ]
