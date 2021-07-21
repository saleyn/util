%%%-----------------------------------------------------------------------------
%%% @doc Conditional expression functions
%%% When using this as a parse transform, include `{parse_transform,iif}' option.
%%% In that case the following code transforms will be done:
%%% ```
%%% iif(A, B, C)   -> case A of true -> B; _ -> C end
%%% iif(A,B,C,D)   -> case A of B -> C; _ -> D end
%%% ife(A,B)       -> case A of false -> B; undefined -> B; [] -> B; _ -> A end
%%% ife(A,B,C)     -> case A of false -> B; undefined -> B; [] -> B; _ -> C end
%%% '''
%%% For debugging the AST of the resulting transform, use `iif_debug'
%%% command-line option:
%%% ```
%%% erlc -Diif_debug=1 ...    % Prints AST before the transform
%%% erlc -Diif_debug=2 ...    % Prints AST after the transform
%%% erlc -Diif_debug[=3] ...  % Prints AST before/after the transform
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

%% `Opt' are compiler options passed from command line. E.g.:
%% ```
%% erlc -Diif_debug=N ...  ->  Opts = [{d,debug,N}|_]
%% erlc -Diif_debug ...    ->  Opts = [{d,debug}|_]
parse_transform(Ast, Opts) ->
  Debug = case lists:keyfind(iif_debug, 2, Opts) of
            {d,iif_debug}              -> 3;
            {d,iif_debug,N} when N > 0 -> N;
            _                          -> 0
          end,
	(Debug band 1) > 0 andalso io:format("AST:\n  ~p~n",[Ast]),
  Tree = erl_syntax:form_list(Ast),
	(Debug band 4) > 0 andalso io:format("AST Tree:\n  ~p~n",[Tree]),
  put(count, 1),
  try
    ModifiedTree = recurse(Tree),
    erase(count),
    Res = erl_syntax:revert_forms(ModifiedTree),
    (Debug band 2) > 0 andalso io:format("AST After:\n  ~p~n",[Res]),
    Res
  catch E:R:S ->
    io:format(standard_error, "Error transforming AST: ~p\n  ~p\n", [R, S]),
    erlang:raise(E,R,S)
  end.


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
           List -> erl_syntax:update_tree(Tree, [[recurse(Subtree) || Subtree <- Group]
                                                 || Group <- List])
         end).

%set_pos([H|T], Line) ->
%  [set_pos(H, Line) | set_pos(T, Line)];
%set_pos([], _Line) ->
%  [];
%set_pos(T, Line) when is_tuple(T), tuple_size(T) > 1 ->
%  setelement(2, T, Line).
  
clause3(A,B,C, Line) ->
  erl_syntax:set_pos(erl_syntax:clause(A,B,C), Line).

syn_atom (A, Line)          -> erl_syntax:set_pos(erl_syntax:atom(A), Line).
syn_var  (V, Line)          -> erl_syntax:set_pos(erl_syntax:variable(V), Line).
syn_if   (V,Then,Else,Line) -> erl_syntax:set_pos(erl_syntax:if_expr(
                                                    [clause3([],[[V]],[Then],Line),
                                                     clause3([],[[syn_atom('true',Line)]],[Else], Line)]),
                                                  Line).
syn_case (A, Clauses, Line) -> erl_syntax:set_pos(erl_syntax:case_expr(A, Clauses), Line).
syn_block(Clauses,    Line) -> erl_syntax:set_pos(erl_syntax:block_expr(Clauses), Line).
syn_match(A, B,       Line) -> erl_syntax:set_pos(erl_syntax:match_expr(A, B), Line).

make_var_name({I,_} = Line) ->
  K = get(count),
  put(count, K+1),
  syn_var(list_to_atom(lists:append(["__I",integer_to_list(I),"_",integer_to_list(K)])), Line).

update(Node) ->
 	case erl_syntax:type(Node) of
		application ->
      case erl_syntax:application_operator(Node) of
        {atom, Line, iif} ->
          %io:format("Application: Op=~p ~1024p\n", [erl_syntax:application_operator(Node), erl_syntax:application_arguments(Node)]),
          case erl_syntax:application_arguments(Node) of
            [A,B,C] ->
              %% This is a call to iif(A, B, C).
              %% Replace it with:
              %%   begin
              %%     _V = A,
              %%     if _V -> B; _ -> C end
              %%   end
              Var = make_var_name(Line),
              syn_block([
                  syn_match(Var, A, Line),
                  syn_if(Var, B, C, Line)
                ], Line);
            [A,B,C,D] ->
              %% This is a call to iif(A, B, C, D).
              %% Replace it with:
              %%   begin
              %%     _V = A,
              %%     case _V of B -> C; _ -> D end
              %%   end
              Var = make_var_name(Line),
              syn_block([
                  syn_match(Var, A, Line), 
                  syn_case(Var,
                    [clause3([B],                  [], [C], Line),
                     clause3([syn_var('_', Line)], [], [D], Line)],
                    Line)
                ], Line);
            _ ->
              Node
          end;
        {atom, Line, ife} ->
          case erl_syntax:application_arguments(Node) of
            [A,B] ->
              %% This is a call to ife(A, B).
              %% Replace it with a case expression:
              %%   begin
              %%     _V = A,
              %%     case _V of false -> B; undefined -> B; [] -> B; _ -> A end
              %%   end
              Var = make_var_name(Line),
              syn_block([
                syn_match(Var, A, Line),
                syn_case(Var,
                  [clause3([syn_atom(false,     Line)],[],[B], Line),
                   clause3([syn_atom(undefined, Line)],[],[B], Line),
                   clause3([erl_syntax:set_pos(erl_syntax:nil(),Line)], [], [B], Line),
                   clause3([syn_var('_',        Line)],[],[A], Line)],
                  Line)],
                Line);
            [A,B,C] ->
              %% This is a call to ife(A, B, C).
              %% Replace it with a case expression:
              %%   begin
              %%     _V = A,
              %%     case _V of false -> B; undefined -> B; [] -> B; _ -> C end
              %%   end
              Var = make_var_name(Line),
              syn_block([
                syn_match(Var, A, Line),
                syn_case(Var,
                  [clause3([syn_atom(false,     Line)],[],[B], Line),
                   clause3([syn_atom(undefined, Line)],[],[B], Line),
                   clause3([erl_syntax:set_pos(erl_syntax:nil(),Line)], [], [B], Line),
                   clause3([syn_var('_',        Line)],[],[C], Line)],
                  Line)],
                Line);
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
