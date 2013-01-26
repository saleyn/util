%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is exprecs-0.2.
%%%
%%% The Initial Developer of the Original Code is Ericsson AB.
%%% Portions created by Ericsson are Copyright (C), 2006, Ericsson AB.
%%% All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.

%%%-------------------------------------------------------------------
%%% File    : exprecs.erl
%%% @author  : Ulf Wiger <ulf.wiger@ericsson.com>
%%% @end
%%% Description : 
%%%
%%% Created : 13 Feb 2006 by Ulf Wiger <ulf.wiger@ericsson.com>
%%%-------------------------------------------------------------------

%%% @doc Parse transform for generating record access functions
%%% <p>This parse transform can be used to reduce compile-time 
%%% dependencies in large systems.</p>
%%% <p>In the old days, before records, Erlang programmers often wrote
%%% access functions for tuple data. This was tedious and error-prone.
%%% The record syntax made this easier, but since records were implemented
%%% fully in the pre-processor, a nasty compile-time dependency was 
%%% introduced.</p>
%%% <p>This module automates the generation of access functions for 
%%% records. While this method cannot fully replace the utility of 
%%% pattern matching, it does allow a fair bit of functionality on 
%%% records without the need for compile-time dependencies.</p>
%%% <p>Whenever record definitions need to be exported from a module,
%%% inserting a compiler attribute,
%%% <code>export_records([RecName|...])</code> causes this transform
%%% to lay out access functions for the exported records:</p>
%%%
%%% <pre>
%%% -record(a, {a, b, c}).
%%% -export_records([a]).
%%% -export(['#info-'/2,
%%%          '#get-'/2, '#set-'/2,
%%%          '#new-a'/0, '#new-a'/1,
%%%          '#get-a'/2, '#set-a'/2,
%%%          '#info-a'/1]).
%%%
%%% '#info-'(Info, Rec) when is_record(Rec, a) -&gt;
%%%     '#info-a'(Info).
%%%
%%% '#get-'(Attrs, Rec) when is_record(Rec, a) -&gt;
%%%     '#get-a'(Attrs, Rec).
%%%
%%% '#set-'(Attrs, Rec) when is_record(Rec, a) -&gt;
%%%     '#set-a'(Attrs, Rec).
%%%
%%% '#new-a'() -&gt; #a{}.
%%% '#new-a'(Vals) -&gt; '#set-a'(Vals, #a{}).
%%%
%%% '#get-a'(Attrs, R) when is_list(Attrs) -&gt;
%%%     ['#get-a'(A, R) || A &lt;- Attrs];
%%% '#get-a'(a, R) -&gt; R#a.a;
%%% '#get-a'(b, R) -&gt; R#a.b;
%%% '#get-a'(c, R) -&gt; R#a.c.
%%%
%%% '#set-a'(Vals, Rec) -&gt;
%%%     F = fun ([], R, _F1) -&gt; R;
%%%             ([{a, V} | T], R, F1) -&gt; F1(T, R#a{a = V}, F1);
%%%             ([{b, V} | T], R, F1) -&gt; F1(T, R#a{b = V}, F1);
%%%             ([{c, V} | T], R, F1) -&gt; F1(T, R#a{c = V}, F1)
%%%         end,
%%%     F(Vals, Rec, F).
%%%
%%% '#info-a'(size) -&gt; record_info(size, a);
%%% '#info-a'(fields) -&gt; record_info(fields, a).
%%% </pre>
%%% <p>The generated accessor functions are:</p>
%%% <table border="1">
%%% <tr><td><code>'#new-R'() -&gt; #R{}</code></td>
%%%     <td>Instantiates a new record of type `R'.</td></tr>
%%% <tr><td><code>'#new-R'(Data) -&gt; #R{}</code></td>
%%%     <td>Exactly equivalent to calling
%%%         <code>'#set-R'(Data,'#new-R'())</code></td></tr>
%%% <tr><td><code>'#info-R'(Info) -&gt;<br/>
%%%              &#160;&#160;[FldName]<br/>
%%%              Info :: fields | size</code></td>
%%%     <td>Equivalent to `record_info(fields, R)' for the
%%%         given record type R.</td></tr>
%%% <tr><td><code>'#info-'(Info, Rec) -&gt;<br/>
%%%               &#160;&#160;[FldName]</code></td>
%%%     <td>Detects the record type of `Rec', and calls the corresponding
%%%         <code>'#info-R'/1</code> function.</td></tr>
%%% <tr><td><code>'#get-R'(A, Rec) -&gt;<br/>
%%%               &#160;&#160;Value | [Value]</code></td>
%%%     <td>Returns the value (if `A' is an atom) of the given field,
%%%         in `Rec' (which must be a record of type `R'),
%%%         or a list of values (if `A' is a list of atoms).</td></tr>
%%% <tr><td><code>'#get-'(A, Rec) -&gt;<br/>
%%%               &#160;&#160;Value | [Value]</code></td>
%%%     <td>Detects the record type of `Rec' and calls the corresponding
%%%         <code>'#get-R'(A, Rec)</code> function.</td></tr>
%%% <tr><td><code>'#set-R'(Data, Rec) -&gt;<br/>
%%%               &#160;&#160;Data | [{Attr::atom(), Value}]</code></td>
%%%     <td>Takes a list of `{Attr,Value}' tuples and sets the corresponding
%%%         attributes in the record `Rec' (which must be of type `R').
%%%         Each `Attr' in the list must correspond to an actual attribute
%%%         in the record `R'.</td></tr>
%%% <tr><td><code>'#set-'(Data, Rec) -&gt;<br/>
%%%               &#160;&#160;Value | [Value]</code></td>
%%%     <td>Detects the record type of `Rec' and calls the corresponding
%%%         <code>'#set-R'(Data, Rec)</code> function.</td></tr>
%%% </table>
%%% @end

-module(exprecs).

-export([parse_transform/2,
	 format_error/1,
	 transform/3,
	 context/2]).

-record(context, {module,
		  function,
		  arity}).

-record(pass1, {exports = [],
		generated = false,
		records = []}).

-define(HERE, {?MODULE, ?LINE}).

-define(ERROR(R, F, I),
        begin
            rpt_error(R, F, I),
            
            throw({error,get_pos(I),{unknown,R}})
        end).

get_pos(I) ->
    case proplists:get_value(form, I) of
	undefined ->
	    0;
	Form ->
	    erl_syntax:get_pos(Form)
    end.

parse_transform(Forms, Options) ->
    [File|_] = [F || {attribute,_,file,{F,_}} <- Forms],
    try do_transform(Forms, Options) of
	Res ->
%%	    io:format("Res = ~p~n", [Res]),
	    Res
    catch
	throw:{error, Ln, What} ->
	    {error, [{File, [{Ln, ?MODULE, What}]}], []}
    end.

do_transform(Forms, _Options) ->
    %%
    %% 1st pass - collect record info
    %%
    Fun1 =
	fun(attribute, {attribute,_L,record,RecDef}=Form, _Ctxt, Acc) ->
		Recs0 = Acc#pass1.records,
		{Form, false, Acc#pass1{records = [RecDef|Recs0]}};
	   (attribute, {attribute,_L,export_records, E}=Form, _Ctxt, Acc) ->
		Exports0 = Acc#pass1.exports,
		NewExports = Exports0 ++ E,
		{Form, false, Acc#pass1{exports = NewExports}};
	   (_Type, Form, _Context, Acc) ->
		{Form, false, Acc}
	end,
    {Forms1, Acc1} = pass(Forms, Fun1, _Acc = #pass1{}),
    %%
    %% 2nd pass - generate accessor functions
    %%
    Fun2 = 
	fun(attribute, {attribute,L,export_records,Es} = Form, _Ctxt,
	    #pass1{exports = [_|_] = Es} = Acc) ->
		Exports = [{list_to_atom(fname_prefix(info)), 2},
			   {list_to_atom(fname_prefix(get)), 2},
			   {list_to_atom(fname_prefix(set)), 2} |
			   lists:concat(
			     lists:map(
			       fun(Rec) ->
				       FNew = fname(new, Rec),
				       [{FNew, 0}, {FNew,1},
					{fname(get, Rec), 2},
					{fname(set, Rec), 2},
					{fname(info, Rec), 1}]
			       end, Es))],
		{[],
		 Form,
		 [{attribute,L,export,Exports}],
		 false, Acc};
	   (function, Form, _Ctxt, #pass1{exports = [_|_],
					  generated = false} = Acc) ->
		%% Layout record funs before first function
		L = element(2, Form),
		Funs = generate_accessors(L, Acc),
		{Funs, Form, [], false, Acc#pass1{generated = true}};
	   (_Type, Form, _Ctxt, Acc) ->
		{Form, false, Acc}
	end,
    {Forms2, Acc2} = pass(Forms1, Fun2, Acc1),
    case Acc2#pass1.generated of
	true ->
	    Forms2;
	false ->
	    case Acc2#pass1.exports of
		[] ->
		    Forms2;
		[_|_] ->
		    [{eof,Last}|RevForms] = lists:reverse(Forms2),
		    [{function, NewLast, _, _, _}|_] = RevAs = 
			lists:reverse(generate_accessors(Last, Acc2)),
		    lists:reverse([{eof, NewLast+1} | RevAs] ++ RevForms)
	    end
    end.


pass(Forms, Fun, Acc) ->
    {NewTree, NewAcc} = transform(Forms, Fun, Acc),
    NewForms = [erl_syntax:revert(T) || T <- lists:flatten(NewTree)],
    {NewForms, NewAcc}.


generate_accessors(L, Acc) ->
    [f_info(Acc, L),
     f_get(Acc, L),
     f_set(Acc, L) |
     lists:concat(
       lists:map(
	 fun(Rname) ->
		 Fields = get_flds(Rname, Acc),
		 [f_new_0(Rname, L),
		  f_new_1(Rname, L),
		  f_get_2(Rname, Fields, L),
		  f_set_2(Rname, Fields, L),
		  f_info_1(Rname, L)]
	 end, Acc#pass1.exports))].

get_flds(Rname, #pass1{records = Rs}) ->
    {value, {_, Flds}} = lists:keysearch(Rname, 1, Rs),
    lists:map(
      fun({record_field,_, {atom,_,N}}) -> N;
	 ({record_field,_, {atom,_,N}, _}) -> N
      end, Flds).



fname_prefix(Op) ->
    case Op of
	new -> "#new-";
	get -> "#get-";
	set -> "#set-";
	info -> "#info-"
    end.

fname(Op, Rname) ->
    Prefix = fname_prefix(Op),
    list_to_atom(Prefix ++ atom_to_list(Rname)).

%%% Accessor functions
%%%
f_new_0(Rname, L) ->
    {function, L, fname(new, Rname), 0,
     [{clause, L, [], [],
       [{record, L, Rname, []}]}]}.


f_new_1(Rname, L) ->
    {function, L, fname(new, Rname), 1,
     [{clause, L, [{var, L, 'Vals'}], [],
       [{call, L, {atom, L, fname(set, Rname)},
	 [{var, L, 'Vals'},
	  {record, L, Rname, []}
	 ]}]
       }]}.

f_set_2(Rname, Flds, L) ->
    {function, L, fname(set, Rname), 2,
     [{clause, L, [{var, L, 'Vals'}, {var, L, 'Rec'}], [],
       [{match, L, {var, L, 'F'},
	 {'fun', L, 
	  {clauses, 
	   [{clause, L, [{nil,L},
			 {var,L,'R'},
			 {var,L,'_F1'}],
	     [],
	     [{var, L, 'R'}]} |
	    [{clause, L, 
	      [{cons, L, {tuple, L, [{atom, L, Attr},
				     {var,  L, 'V'}]},
		{var, L, 'T'}},
	       {var, L, 'R'},
	       {var, L, 'F1'}],
	      [],
	      [{call, L, {var, L, 'F1'},
		[{var,L,'T'},
		 {record, L, {var,L,'R'}, Rname,
		  [{record_field, L,
		    {atom, L, Attr},
		    {var, L, 'V'}}]},
		 {var, L, 'F1'}]}]} || Attr <- Flds]]}}},
	{call, L, {var, L, 'F'}, [{var, L, 'Vals'},
				  {var, L, 'Rec'},
				  {var, L, 'F'}]}]}]}.

f_get_2(Rname, Flds, L) ->
    FName = fname(get, Rname),
    {function, L, FName, 2,
     [{clause, L, [{var, L, 'Attrs'}, {var, L, 'R'}],
       [[{call, L, {atom, L, is_list}, [{var, L, 'Attrs'}]}]],
       [{lc, L, {call, L, {atom, L, FName}, [{var, L, 'A'}, {var, L, 'R'}]},
	 [{generate, L, {var, L, 'A'}, {var, L, 'Attrs'}}]}]
       } |
      [{clause, L, [{atom, L, Attr}, {var, L, 'R'}], [],
	[{record_field, L, {var, L, 'R'}, Rname, {atom, L, Attr}}]} ||
	  Attr <- Flds]]
    }.


f_info(Acc, L) ->
    Fname = list_to_atom(fname_prefix(info)),
    {function, L, Fname, 2,
     [{clause, L,
       [{var, L, 'Info'}, {var, L, 'Rec'}],
       [[{call, L,
	  {atom, L, is_record},
	  [{var, L, 'Rec'}, {atom, L, R}]}]],
       [{call, L, {atom, L, fname(info, R)}, [{var, L, 'Info'}]}]} ||
	 R <- Acc#pass1.exports]}.


f_get(Acc, L) ->
    f_getset(get, Acc, L).

f_set(Acc, L) ->
    f_getset(set, Acc, L).

f_getset(Mode, Acc, L) when Mode == get; Mode == set ->
    Fname = list_to_atom(fname_prefix(Mode)),
    {function, L, Fname, 2,
     [{clause, L,
       [{var, L, 'Attrs'},
	{var, L, 'Rec'}],
       [[{call, L,
	  {atom, L, is_record},
	  [{var, L, 'Rec'}, {atom, L, R}]}]],
       [{call, L, {atom, L, fname(Mode, R)}, [{var, L, 'Attrs'},
					      {var, L, 'Rec'}]}]} ||
	 R <- Acc#pass1.exports]}.

f_info_1(Rname, L) ->
    {function, L, fname(info, Rname), 1,
     [{clause, L, [{atom, L, fields}], [],
       [{call, L, {atom, L, record_info},
	 [{atom, L, fields}, {atom, L, Rname}]}]
      },
      {clause, L, [{atom, L, size}], [],
       [{call, L, {atom, L, record_info},
	 [{atom, L, size}, {atom, L, Rname}]}]
      }]}.

%%% ========== generic parse_transform stuff ==============

context(module,   #context{module = M}  ) -> M;
context(function, #context{function = F}) -> F;
context(arity,    #context{arity = A}   ) -> A.


transform(Forms, F, Acc) ->
    case  [{L,M} || {attribute, L, module, M} <- Forms] of
	[{_,Module}] ->
	    transform(Forms, F, #context{module = Module}, Acc);
	[] ->
	    ?ERROR(missing_module_attribute, ?HERE, []);
	[_|_] = Multiple ->
	    ?ERROR(multiple_module_attributes, ?HERE,
		   [{L,{module,M}} || {L,M} <- Multiple])
    end.

transform(Forms, F, Context, Acc) ->
    F1 =
	fun(Form, Acc0) ->
		Type = erl_syntax:type(Form),
		{Before1, Form1, After1, Recurse, Acc1} =
		    try F(Type, Form, Context, Acc0) of
			{F1, Rec1, A1} ->
			    {[], F1, [], Rec1, A1};
			{_Be1, _F1, _Af1, _Rec1, _Ac1} = Res1 ->
			    Res1
		    catch
			error:Reason ->
			    ?ERROR(Reason,
				   ?HERE,
				   [{type, Type},
				    {context, Context},
				    {acc, Acc},
				    {form, Form}])
		    end,
		if Recurse == true ->
			case erl_syntax:subtrees(Form1) of
			    [] ->
				{Before1, Form1, After1, Acc1};
			    ListOfLists ->
				{NewListOfLists, NewAcc} =
				    mapfoldl(
				      fun(L, AccX) ->
					      transform(
						L, F, 
						new_context(
						  Form1, Context), AccX)
				      end, Acc1, ListOfLists),
				NewForm =
				    erl_syntax:update_tree(
				      Form, NewListOfLists),
				{Before1, NewForm, After1, NewAcc}
			end;
		   true ->
			{Before1, Form1, After1, Acc1}
		end
	end,
    mapfoldl(F1, Acc, Forms).


new_context(Form, Context0) ->
    case erl_syntax:type(Form) of
	function ->
	    {Fun, Arity} =
		erl_syntax_lib:analyze_function(Form),
	    Context0#context{function = Fun,
			     arity = Arity};
	_ ->
	    Context0
    end.




%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
mapfoldl(F, Accu0, [Hd|Tail]) ->
    {Before, Res, After, Accu1} =
	case F(Hd, Accu0) of
	    {Be, _, Af, _} = Result when is_list(Be), is_list(Af) ->
		Result;
	    {R1, A1} ->
		{[], R1, [], A1}
	end,
    {Rs, Accu2} = mapfoldl(F, Accu1, Tail),
    {Before ++ [Res| After ++ Rs], Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[], Accu}.



rpt_error(Reason, Fun, Info) ->
    Fmt = lists:flatten(
	    ["*** ERROR in parse_transform function:~n"
	     "*** Reason     = ~p~n",
             "*** Location: ~p~n",
	     ["*** ~10w = ~p~n" || _ <- Info]]),
    Args = [Reason, Fun | 
	    lists:foldr(
	      fun({K,V}, Acc) ->
		      [K, V | Acc]
	      end, [], Info)],
    io:format(Fmt, Args).


format_error({_Cat, Error}) ->
    Error.
