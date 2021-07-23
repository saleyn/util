%%%-----------------------------------------------------------------------------
%%% @doc Parse transform that implements `sprintf/2'
%%%
%%% Use `{parse_transform,sprintf}' compiler's option to use this transform.
%%% ```
%%% sprintf(Fmt, Args) -> lists:flatten(io_lib:format(Fmt, Args))
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
-module(sprintf).

% If using this module as a parse transform, we need to export the following:
-export([parse_transform/2]).
-export([to_string/1, to_string/2, reset_float_fmt/0, set_float_fmt/1, get_float_fmt/0]).

%%%-----------------------------------------------------------------------------
%%% External API
%%%-----------------------------------------------------------------------------

%% @doc Parse transform to be used by providing `{parse_transform, sprintf}' option.
parse_transform(Ast, _Opts) ->
  Tree = erl_syntax:form_list(Ast),
  ModifiedTree = recurse(Tree),
  erase(line),
  erl_syntax:revert_forms(ModifiedTree).

-spec to_string(term()) -> string().
to_string(I) when is_integer(I) -> integer_to_list(I);
to_string(I) when is_binary(I)  -> binary_to_list(I);
to_string(I) when is_float(I)   -> to_string(I, get_float_fmt());
to_string(I) when is_atom(I)    -> atom_to_list(I);
to_string(I) when is_list(I)    ->
  try
    lists:flatten(io_lib:format("~s", [I]))
  catch _:_ ->
    lists:flatten(io_lib:format("~p", [I]))
  end;
to_string(I) ->
  lists:flatten(io_lib:format("~p", [I])).

to_string(I, Opts) when is_float(I) ->
  float_to_list(I, Opts).

%% @doc Erase custom float format from the process dictionary
reset_float_fmt()   -> erase(float_fmt).

%% @doc Store custom float format in the process dictionary
%% @see //kernel/erlang/float_to_list/2
set_float_fmt(Opts) -> put(float_fmt, Opts).

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
syn_call(M,F,A)   -> L=get(line),
                     erl_syntax:set_pos(
                       erl_syntax:application(syn_atom(M, L), syn_atom(F, L), A), L).

update(Node) ->
 	case erl_syntax:type(Node) of
		application ->
      case erl_syntax:application_operator(Node) of
        {atom, Line, sprintf} ->
          put(line, Line),
          case erl_syntax:application_arguments(Node) of
            [A,B] ->
              %% This is a call to sprintf(Fmt, Args).
              %% Replace it with:
              %%   lists:flatten(io_libs:format(Fmt, Args)
              syn_call(lists, flatten, [syn_call(io_lib, format, [A,B])]);
            [A] ->
              %% This is a call to sprintf(Arg).
              %% Replace it with:
              %%   sprintf:to_string(Args)
              syn_call(sprintf, to_string, [A]);
            _ ->
              Node
          end;
        _ ->
          Node
      end;
		_ ->
			Node
	end.
