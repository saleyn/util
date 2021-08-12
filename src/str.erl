%%%-----------------------------------------------------------------------------
%%% @doc Parse transform that implements `str/2'
%%%
%%% Use `{parse_transform,str}' compiler's option to use this transform.
%%% ```
%%% str(Fmt, Args)     -> lists:flatten(io_lib:format(Fmt, Args))
%%% throw(Fmt, Args)   -> throw(lists:flatten(io_lib:format(Fmt, Args))
%%% i2l(Int)           -> integer_to_list(Int)
%%% b2l(Bin)           -> binary_to_list(Bin)
%%% str(Term)          -> str:str(Term)
%%% '''
%%% @author Serge Aleynikov <saleyn(at)gmail(dot)com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2021 Serge Aleynikov
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
-module(str).

% If using this module as a parse transform, we need to export the following:
-export([parse_transform/2]).
-export([str/1, str/2, reset_float_fmt/0, set_float_fmt/1, get_float_fmt/0]).

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%% @doc Parse transform to be used by providing `{parse_transform, str}' option.
parse_transform(Ast, _Opts) ->
  Tree = erl_syntax:form_list(Ast),
  ModifiedTree = recurse(Tree),
  erase(line),
  erl_syntax:revert_forms(ModifiedTree).

-spec str(term()) -> string().
str(I) when is_list(I)    ->
  lists:flatten(
    try          io_lib:format("~s", [I])
    catch _:_ -> io_lib:format("~p", [I])
    end);
str(I) when is_integer(I) -> integer_to_list(I);
str(I) when is_binary(I)  -> binary_to_list(I);
str(I) when is_float(I)   -> str(I, get_float_fmt());
str(I) when is_atom(I)    -> atom_to_list(I);
str(I) ->
  lists:flatten(io_lib:format("~p", [I])).

str(I, undefined) when is_float(I) ->
  float_to_list(I);
str(I, Opts) when is_float(I) ->
  float_to_list(I, Opts).

%% @doc Erase custom float format from the process dictionary
reset_float_fmt()   -> erase(float_fmt).

%% @doc Store custom float format in the process dictionary
%%      Return previously stored format.
%% Also see float_to_list/2 [http://erlang.org/doc/man/erlang.html#float_to_list-2]
set_float_fmt(Opts) -> V=get(float_fmt), put(float_fmt, Opts), V.

%% @doc Get custom float format from the process dictionary
get_float_fmt()     -> get(float_fmt).

%%%-----------------------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------------------

%% Parse transform support
recurse(Tree) ->
  update(case erl_syntax:subtrees(Tree) of
           []   -> Tree;
           List -> erl_syntax:update_tree(Tree, [[recurse(Subtree) || Subtree <- Group]
                                                 || Group <- List])
         end).

syn_atom(A, Line) -> erl_syntax:set_pos(erl_syntax:atom(A), Line).
syn_call(F,A)     -> L=get(line),
                     erl_syntax:set_pos(
                       erl_syntax:application(syn_atom(F, L), A), L).
syn_call(M,F,A)   -> L=get(line),
                     erl_syntax:set_pos(
                       erl_syntax:application(syn_atom(M, L), syn_atom(F, L), A), L).

update(Node)                     -> update2(Node, erl_syntax:type(Node)).
update2(Node, application)       -> update3(Node, erl_syntax:application_operator(Node));
update2(Node, _)                 -> Node.

update3(Node, {atom, Line, Fun}) -> update4(Fun, Node, Line);
update3(Node, _)                 -> Node.

update4(str, Node, Line) ->
  %% Replace str(A, B) -> lists:flatten(io_lib:format(A, B)).
  %%         str(A)    -> str:str(A).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A,B] ->
      %% This is a call to str(Fmt, Args).
      %% Replace it with:
      %%   lists:flatten(io_libs:format(Fmt, Args)
      syn_call(lists, flatten, [syn_call(io_lib, format, [A,B])]);
    [A] ->
      %% This is a call to str(Arg).
      %% Replace it with:
      %%   sprintf:str(Args)
      syn_call(str, str, [A]);
    _ ->
      Node
  end;
update4(throw, Node, Line) ->
  %% Replace throw(A, B) -> throw(lists:flatten(io_lib:format(A, B))).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A,B] ->
      syn_call(throw, [syn_call(lists, flatten, [syn_call(io_lib, format, [A,B])])]);
    _ ->
      Node
  end;
update4(i2l, Node, Line) ->
  %% Replace i2l(A) -> integer_to_list(A).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A] -> syn_call(integer_to_list, [A]);
    _   -> Node
  end;
update4(b2l, Node, Line) ->
  %% Replace b2l(A) -> binary_to_list(A).
  put(line, Line),
  case erl_syntax:application_arguments(Node) of
    [A] -> syn_call(binary_to_list, [A]);
    _   -> Node
  end;
update4(_, Node, _) ->
  Node.
