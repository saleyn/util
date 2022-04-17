%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Creation of module aliases
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2021 Serge Aleynikov
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created 2021-06-01
%%%-----------------------------------------------------------------------------
-module(alias).
-export([create/2, create/3]).

%% @doc Create a module `Alias' for the given `OrigMod' module.
%% This function is useful for creating in-memory Elixir wrapper modules without
%% needing to maintain persistent files with such wrappers.
%% `IncFuns' argument can be `all' or a list of `{Fun, Arity}' values which
%% only export a subset of functions from the `OrigMod'.

create(Alias, OrigMod) ->
  create(Alias, OrigMod, all).
create(Alias, OrigMod, IncFuns) when IncFuns == all; is_list(IncFuns) ->
  Exports = [T || T <- OrigMod:module_info(exports)
                , element(1, T) /= module_info
                , IncFuns==all orelse lists:member(T, IncFuns)],
  AST     = [{attribute, ?LINE, module, Alias}
            ,{attribute, ?LINE, export, Exports}
            |[begin
                % AST of a function call from the OrigMod module
                Remote = {remote, ?LINE, {atom, ?LINE, OrigMod}, {atom, ?LINE, Fun}},
                Args   = [{var,   ?LINE, list_to_atom("A" ++ integer_to_list(N))}
                          || N <- lists:seq(1, Arity)],
                {function, ?LINE, Fun, Arity, [{clause, ?LINE, Args, [], [{call, ?LINE, Remote, Args}]}]}
              end || {Fun, Arity} <- Exports]
            ],
  {ok, Alias, Bin} = compile:forms(AST),
  {module,  Alias} = code:load_binary(Alias, "/dev/null", Bin),
  {ok,      Alias}.
