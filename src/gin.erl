%% @doc Guard in
%% @author Michael Uvarov (freeakk@gmail.com)
%% Source:  https://github.com/mad-cocktail/gin
%% License: MIT

-module(gin).
-author('freeakk@gmail.com').

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    F1 = local_function(numeric_in, 2, in_transform('==')),
    F2 = local_function(in, 2, in_transform('=:=')),
    F3 = local_function(beetween, 3, fun beetween_transform/1),
    F  = foldl_functions([F1, F2, F3, fun erl_syntax:revert/1]),
    X = [erl_syntax_lib:map(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


%% ==================================================================
%% In
%% ==================================================================

%% It is curry (from Haskell) for `in_transform/2'.
in_transform(Op) ->
    fun(Node) ->
        in_transform(Op, Node)
        end.


%% @doc Replace `in(X, List)' with `(X =:= E1) andalso (X =:= E2)' 
%%      when `List' is `[E1, E2]' and `Op' is `=:='.
%%
%%      The caller checks, that the function name is valid.
%%      `in' can be any function, for example, `in2' is valid too.
-spec in_transform(Op, Node) -> Node when
    Op :: '==' | '=:=',
    Node :: erl_syntax_lib:syntaxTree().

in_transform(Op, Node) ->
    Pos = erl_syntax:get_pos(Node),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, ListForm] = erl_syntax:application_arguments(Node),
    Elems =
        case erl_syntax:type(ListForm) of
        string ->
            Str = erl_syntax:string_value(ListForm),
            [erl_syntax:char(C) || C <- Str];
        list ->
            %% Extract the list of the valid values.
            erl_syntax:list_elements(ListForm)
        end,
    case Elems of
    [] ->
        %% Always `false'.
        New(erl_syntax:atom(false));
    
    _  ->
        EqOp = New(erl_syntax:operator(Op)),
        OrOp = New(erl_syntax:operator('orelse')),
        %% `X' is `Subject =:= Xs'.
        [X|Xs] = [New(erl_syntax:infix_expr(E, EqOp, SubjectForm)) || E <- Elems],
        F = fun(Right, Left) -> New(erl_syntax:infix_expr(Left, OrOp, Right)) end,
        GuardAST = New(erl_syntax:parentheses(lists:foldl(F, X, Xs))),
        erl_syntax:revert(GuardAST)
    end.


%% ==================================================================
%% Beetween
%% ==================================================================

%% @doc Transforms `beetween(Subject, Start, To)'.
%% Subject is a term, but usually it is a number.
%% `From' and `To' can be wrapped with the `open(_)' call.
%% It meand, that this value is not inluded in the interval.
%%
%% `beetween(X, F, T)' is replaced with `((X =< F) andalso (X >= T))'.
%% `beetween(X, open(F), T)' is replaced with `((X < F) andalso (X >= T))'.
beetween_transform(Node) ->
    Pos = erl_syntax:get_pos(Node),
    %% Call it fore all new nodes.
    New = fun(NewNode) -> erl_syntax:set_pos(NewNode, Pos) end,
    %% Extract arguments of the `in' function.
    [SubjectForm, FromForm, ToForm] = 
        erl_syntax:application_arguments(Node),
    GtEqOp = New(erl_syntax:operator(greater(is_open(FromForm)))),
    LoEqOp = New(erl_syntax:operator(less(is_open(ToForm)))),
    AndOp  = New(erl_syntax:operator('andalso')),
    Exp1 = New(erl_syntax:infix_expr(SubjectForm, GtEqOp, clean_open(FromForm))),
    Exp2 = New(erl_syntax:infix_expr(SubjectForm, LoEqOp, clean_open(ToForm))),
    Exp3 = New(erl_syntax:infix_expr(Exp1, AndOp, Exp2)),
    GuardAST = New(erl_syntax:parentheses(Exp3)),
    erl_syntax:revert(GuardAST).


%% @doc Returns an operator name.
-spec less(IsExcluded) -> Op when
    IsExcluded :: boolean(), 
    Op :: atom().

less(true)  -> '<';
less(false) -> '=<'.


-spec greater(IsExcluded) -> Op when
    IsExcluded :: boolean(), 
    Op :: atom().

greater(true)  -> '>';
greater(false) -> '>='.

%% @doc Return true, if `Node' is wrapped by `open(_)'.
is_open(Node) ->
    is_local_function(open, 1, Node).


%% @doc Convert the call of `open(Body)' to `Body'.
clean_open(Node) ->
    case is_open(Node) of
        true ->  hd(erl_syntax:application_arguments(Node));
        false -> Node
    end.


foldl_functions(Fs) ->
    fun(Node) ->
        Apply = fun(F, N) -> F(N) end,
        lists:foldl(Apply, Node, Fs)
    end.


local_function(FunName, FunArity, TransFun) ->
    fun(Node) ->
        IsFun = is_local_function(FunName, FunArity, Node),
        if IsFun -> TransFun(Node);
            true -> Node
            end
        end.

%% @doc Return `true', `Node' is a function call of the `FunName/FunArity' function.
is_local_function(FunName, FunArity, Node) -> 
    erl_syntax:type(Node) =:= application
        andalso always(Op = erl_syntax:application_operator(Node))
        andalso erl_syntax:type(Op) =:= atom
        andalso erl_syntax:atom_value(Op) =:= FunName
        andalso application_arity(Node) =:= FunArity.

always(_) -> true.


%% @doc Return arity of the called function inside `Node'.
application_arity(Node) ->
    length(erl_syntax:application_arguments(Node)).
