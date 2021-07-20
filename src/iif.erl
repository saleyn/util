%%%-----------------------------------------------------------------------------
%%% @doc Conditional expression functions
%%% When using as a parse transform, include `{parse_transform, iif}' option.
%%% In that case all calls to `iif(A, B, C)' will get translated to:
%%% `case A of true -> B; _ -> C end' and all calls to `iif(A,B,C,D)' will get
%%% translated to `case A of B -> C; _ -> D end'.
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 Serge Aleynikov
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
%%%-----------------------------------------------------------------------------
-module(iif).

% If using this module as a parse transform, we need to export the following:
-export([parse_transform/2]).
% Otherwise use as a regular module
-export([ife/2, ife/3, ifne/2, ifne/3, iif/3, iif/4, nvl/2, format_ne/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

parse_transform(Ast, Opt) ->
  Debug = proplists:get_value(debug, Opt, false),
	Debug andalso io:format("AST Before:\n~1024p~n",[Ast]),
	%After = [parse(X) || X <- Ast],
  Tree = erl_syntax:form_list(Ast),
	ModifiedTree = recurse(Tree),
	After = erl_syntax:revert_forms(ModifiedTree),
	Debug andalso io:format("AST After:\n~1024p~n",[After]),
	After.


%% @doc Return `Value' if first argument is one of: `[]', `false', `undefined'.
%%      Otherwise return the value of the first argument.
ife([],         Value) -> execute([], Value);
ife(false,      Value) -> execute([], Value);
ife(undefined,  Value) -> execute([], Value);
ife(Test,      _Value) -> Test.

nvl(Value, IfNull)     -> ife(Value, IfNull).

%% @doc Return `Empty' if first argument is one of: `[]', `false', `undefined'.
%%      Otherwise, if `NotEmpty' is `fun()', evaluate it, or if it's `fun(Arg)'
%%      evaluate it with `Value' argument.
ife([],         Empty,_NotEmpty) -> execute([], Empty);
ife(false,      Empty,_NotEmpty) -> execute([], Empty);
ife(undefined,  Empty,_NotEmpty) -> execute([], Empty);
ife(Value,     _Empty, NotEmpty) -> execute(Value, NotEmpty).

%% @doc Return `Value' if first argument is not one of: `[]', `false', `undefined'.
%%      Otherwise, if `Value' is `fun()', evaluate it, or if it's `fun(Arg)'
%%      evaluate it with `Test' argument.
% If not empty
ifne([],       _Value) -> [];
ifne(false,    _Value) -> [];
ifne(undefined,_Value) -> [];
ifne(Test,      Value) -> execute(Test, Value).

%% @doc Return `NotEmpty' if first argument is not one of: `[]', `false', `undefined'.
%%      Otherwise, if `NotEmpty' is `fun()', evaluate it, or if it's `fun(Arg)'
%%      evaluate it with `Value' argument.
ifne([],       _NotEmpty, Empty) -> execute([], Empty);
ifne(false,    _NotEmpty, Empty) -> execute([], Empty);
ifne(undefined,_NotEmpty, Empty) -> execute([], Empty);
ifne(Value,     NotEmpty,_Empty) -> execute(Value, NotEmpty).

%% @doc Return `True' if first argument is `true' or return `False' if
%%      the first argument is one of: `[]', `false', `undefined'.
iif([],            _True, False) -> execute([], False);
iif(false,         _True, False) -> execute([], False);
iif(undefined,     _True, False) -> execute([], False);
iif(true,           True,_False) -> execute([], True).

%% @doc Return `True' if first two arguments match
iif(Value, Value,   True,_False) -> execute(Value, True);
iif(Value,_Other,  _True, False) -> execute(Value, False).

%% @doc Format if first argument is not empty 
format_ne(false, _Fmt, _Args) -> [];
format_ne([],    _Fmt, _Args) -> [];
format_ne(_True,  Fmt,  Args) -> io_lib:format(Fmt, Args).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

execute(_, F) when is_function(F,0) -> F();
execute(V, F) when is_function(F,1) -> F(V);
execute(_, V)                       -> V.

%% Parse transform support
recurse(Tree) ->
  update(case erl_syntax:subtrees(Tree) of
           []   -> Tree;
           List -> erl_syntax:update_tree(Tree, [[recurse(Subtree) || Subtree <- Group] || Group <- List])
         end).

update(Node) ->
 	case erl_syntax:type(Node) of
		application ->
      case erl_syntax:application_operator(Node) of
        {atom, _, iif} ->
          %io:format("Application: Op=~p ~1024p\n", [erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node)]),
          case erl_syntax:application_arguments(Node) of
            [A,B,C] ->
              %% This is a call to iif(A, B, C).
              %% Replace it with a case expression: case A of true -> B; _ -> C end
              erl_syntax:case_expr(A, [erl_syntax:clause([erl_syntax:atom(true)],    [], [B]),
                                       erl_syntax:clause([erl_syntax:variable('_')], [], [C])]);
            [A,B,C,D] ->
              %% This is a call to iif(A, B, C, D).
              %% Replace it with a case expression: case A of B -> C; _ -> D end
              erl_syntax:case_expr(A, [erl_syntax:clause([B],                        [], [C]),
                                       erl_syntax:clause([erl_syntax:variable('_')], [], [D])]);
            _ ->
              Node
          end;
        _ ->
          Node
      end;
		_ ->
			Node
	end.

%%%-----------------------------------------------------------------------------
%%% Unit Tests
%%%-----------------------------------------------------------------------------

-ifdef(EUNIT).

ife_test() ->
    ?assertEqual(abc, ife ([],        abc)),
    ?assertEqual(abc, ife (false,     abc)),
    ?assertEqual(abc, ife (undefined, abc)),
    ?assertEqual(xxx, ife (xxx,       abc)),
    ?assertEqual(ok,  ife (false,     fun()  -> ok end)),
    ?assertEqual([],  ife (false,     fun(V) -> V  end)),

    ?assertEqual(abc, ife ([],        abc, efg)),
    ?assertEqual(abc, ife (false,     abc, efg)),
    ?assertEqual(abc, ife (undefined, abc, efg)),
    ?assertEqual(efg, ife (xxx,       abc, efg)),
    ?assertEqual(xxx, ife (xxx,       abc, fun(V) -> V  end)).

ifne_test() ->
    ?assertEqual([],  ifne([],        abc)),
    ?assertEqual([],  ifne(false,     abc)),
    ?assertEqual([],  ifne(undefined, abc)),
    ?assertEqual(abc, ifne(xxx,       abc)),
    ?assertEqual(ok,  ifne(false,     abc, fun()  -> ok end)),
    ?assertEqual(x,   ifne(xxx,       fun()  -> x  end, efg)),
    ?assertEqual(xxx, ifne(xxx,       fun(V) -> V  end, efg)),

    ?assertEqual(efg, ifne([],        abc, efg)),
    ?assertEqual(efg, ifne(false,     abc, efg)),
    ?assertEqual(efg, ifne(undefined, abc, efg)),
    ?assertEqual(abc, ifne(xxx,       abc, efg)),
    ?assertEqual(xxx, ifne(xxx,       fun(V) -> V  end, efg)).

iif_test() ->
    ?assertEqual(abc, iif(x, x, abc, efg)),
    ?assertEqual(ok,  iif(x, x, fun() -> ok end, efg)),
    ?assertEqual(x,   iif(x, x, fun(X) -> X end, efg)),
    ?assertEqual(efg, iif(x, y, abc, efg)),
    ?assertEqual(ok,  iif(x, y, abc, fun() -> ok end)),
    ?assertEqual(x,   iif(x, y, abc, fun(X) -> X end)).

-endif.
