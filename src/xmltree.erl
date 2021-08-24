%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc Parse XML into a hierarchical Erlang term
%%% ```
%%% % Example xml:
%%%
%%%  <?xml version="1.0" encoding="UTF-8" ?>
%%%  <root id="1">
%%%    <ele id="2"/>
%%%    <ele id="3">vvv\nxxx\n</ele>
%%%  </root>
%%%
%%%
%%% # Usage example
%%% 1> xmltree:file(L).
%%% {root,[{id,<<"1">>}],
%%%      [{ele,[{id,<<"2">>}],[]},
%%%       {ele,[{id,<<"3">>}],<<"vvv\nxxx\n">>}]}
%%%
%%% 2> Rules = {root, [{id,integer}], [{ele, [{id,integer}], string}]},
%%% 2> xmltree:string(L, Rules).
%%% {root,[{id,1}],
%%%    [{ele,[{id,2}],[]},{ele,[{id,3}],"vvv\nxxx\n"}]}
%%% '''
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
%%% Date:   2015-12-10
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 Serge Aleynikov
%%%-----------------------------------------------------------------------------
-module(xmltree).
-export([file/1, file/2, string/1, string/2, xml/1, xml/2]).

-include_lib("xmerl/include/xmerl.hrl").

file(Filename) when is_list(Filename) ->
  {Xml, _} = xmerl_scan:file(Filename),
  xml(Xml).
file(Filename, RulesFile) when is_list(Filename), is_list(RulesFile) ->
  {Xml, _} = xmerl_scan:file(Filename),
  case file:consult(RulesFile) of
    {ok, [Rules]} -> xml(Xml, Rules);
    {ok,  Rules } -> xml(Xml, Rules)
  end;
file(Filename, Rules) when is_list(Filename), is_tuple(Rules) ->
  {Xml, _} = xmerl_scan:file(Filename),
  xml(Xml, Rules).

string(XmlS) when is_list(XmlS) ->
  {Xml, _} = xmerl_scan:string(XmlS),
  xml(Xml).
string(XmlS, Rules) when is_list(XmlS), is_tuple(Rules) ->
  {Xml, _} = xmerl_scan:string(XmlS),
  xml(Xml, Rules).


xml(#xmlElement{name = N, attributes = A, content = C}) ->
  {N, process_attributes(A), xml(C)};
xml([#xmlElement{} = E | T]) ->
  [xml(E) | xml(T)];
xml([#xmlComment{} | T]) ->
  xml(T);
xml([#xmlText{value = V} | T]) ->
  case [C || C <- V, not lists:member(C, "\n ")] of
    [] -> xml(T);
    _  -> [unicode:characters_to_binary(V, utf8) | xml(T)]
  end;
xml([]) ->
  [].

xml(#xmlElement{name = N, attributes = A, content = C}, {N, AttrRules, ChildRules}) ->
  {N, process_attributes(A, AttrRules), xml(C, ChildRules)};
xml([#xmlElement{name = N} = E | T], ChildRules) ->
  case lists:keyfind(N, 1, ChildRules) of
    false -> throw({no_rule_for_element, N});
    Rule  -> [xml(E, Rule) | xml(T, ChildRules)]
  end;
xml([#xmlComment{} | T], ChildRules) ->
  xml(T, ChildRules);
xml([#xmlText{value = V} | T], ChildRules) ->
  case [C || C <- V, not lists:member(C, "\n ")] of
    [] -> xml(T, ChildRules);
    _  -> [process_value(V, ChildRules) | xml(T, ChildRules)]
  end;
xml([], _) ->
  [].

process_attributes([#xmlAttribute{name=N, value=V} | T]) ->
  [{N, unicode:characters_to_binary(V, utf8)} | process_attributes(T)];
process_attributes([]) ->
  [].

process_attributes([#xmlAttribute{name=N, value=V} | T], Rules) ->
  [{N, process_value(V, proplists:get_value(N, Rules))} | process_attributes(T, Rules)];
process_attributes([], _) ->
  [].


process_value(Value, Fun) when is_function(Fun ,1) -> Fun(Value);
process_value(Value, atom)    -> list_to_atom   (Value);
process_value(Value, boolean) -> A = list_to_existing_atom(Value),
                                 if is_boolean(A) -> A;
                                    true          -> throw({value_is_not_boolean, Value})
                                 end;
process_value(Value, integer) -> list_to_integer(Value);
process_value(Value, float)   -> list_to_float  (Value);
process_value(Value, binary)  -> list_to_binary (Value);
process_value(Value, string)  -> Value;
process_value(Value, _)       -> unicode:characters_to_binary(Value, utf8).
