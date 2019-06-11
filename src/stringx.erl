%%% vim:ts=2:sw=2:et
%%%------------------------------------------------------------------------
%%% @doc Implements miscelaneous string functions
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2006 Serge Aleynikov
%%% @end
%%% Some implementation authored by Evan Miller:
%%% http://www.evanmiller.org/joy-of-erlang.html
%%%------------------------------------------------------------------------
%%% Created 2005-09-25
%%%------------------------------------------------------------------------
-module(stringx).
-author('saleyn@gmail.com').

%% External API
-export([titlecase/1, wordcount/1, wordwrap/2]).
-export([pretty_table/1, pretty_table/2, pretty_table/3]).
-export([pretty_print_table/1, pretty_print_table/2, pretty_print_table/3]).
-export([align_rows/1, align_rows/2, aligned_format/2, aligned_format/3]).

-include("stringx.hrl").

%%%------------------------------------------------------------------------
%%% External API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc Convert words in a string to capitalize first letter of each word.
%% @end
%%-------------------------------------------------------------------------
-spec titlecase(string()) -> string().
titlecase(S) when is_list(S) ->
  titlecase(S, []).

%%-------------------------------------------------------------------------
%% @doc Count number of words in a string
%% @end
%%-------------------------------------------------------------------------
-spec wordcount(string()) -> string().
wordcount(S) ->
  wordcount(S, 0).

%%-------------------------------------------------------------------------
%% @doc Wrap words words in a string
%% @end
%%-------------------------------------------------------------------------
-spec wordwrap(string(), integer()) -> string().
wordwrap(S, Margin) when is_list(S), is_integer(Margin) ->
  wordwrap(S, [], [], 0, Margin).

%%-------------------------------------------------------------------------
%% @doc Pretty print list of maps to list.
%% @see pretty_table/3.
%% @end
%%-------------------------------------------------------------------------
-spec pretty_table([map()]) -> list().
pretty_table([Map|_] = LofMaps0) when is_map(Map) ->
  pretty_table(lists:sort(maps:keys(Map)), LofMaps0, #opts{}).

%%-------------------------------------------------------------------------
%% @doc Pretty print table of lists/tuples/maps to list.
%% @see pretty_table/3.
%% @end
%%-------------------------------------------------------------------------
-spec pretty_table([string()], [Row :: tuple()|list()|map()]) -> list().
pretty_table(HeaderRowKeys, Rows) ->
  pretty_table(HeaderRowKeys, Rows, #opts{}).

%%-------------------------------------------------------------------------
%% @doc Pretty print table of lists/tuples/maps to list.
%% The following options control formatting behavior:
%% <dl>
%% <dt>number_pad</dt>
%%      <dd>Leading padding character used for numbers</dd>
%% <dt>th_dir</dt>
%%      <dd>Table header row padding direction (both|leading|trailing)</dd>
%% <dt>td_dir</dt>
%%      <dd>Table row padding direction (both|leading|trailing)</dd>
%% <dt>td_start</dt>
%%      <dd>Don't print columns less than this (e.g. use 2 for records)</dd>
%% <dt>td_exclulde</dt>
%%      <dd>List of column ID's (starting with 1) or names to exclude</dd>
%% <dt>td_sep</dt>
%%      <dd>Column separator (default `" | "')</dd>
%% <dt>tr_sep</dt>
%%      <dd>Row separator (default `"-"')</dd>
%% <dt>tr_sep_td</dt>
%%      <dd>Column delimiter used in separating rows (`"+"`)</dd>
%% <dt>prefix</dt>
%%      <dd>Prepend each row with this string</dd>
%% <dt>td_formats</dt>
%%      <dd>A tuple containing column formats. Each value is either
%%          a format string passed to `io_lib:format/2' or a function
%%          `fun(Value) -> {number|string, FormattedValue::string()}'</dd>
%% </dl>
%% Example:
%% ```
%% 1> stringx:pretty_print_table(
%%      {a,b,c,d}, [{a, 10, ccc}, {bxxx, 200.00123, 'Done'}, {abc, 100.0, xx}],
%%      #opts{td_dir=both, td_exclude=[d], td_formats=
%%          {undefined, fun(V) when is_integer(V) -> {number, integer_to_list(V)};
%%                         (V) when is_float(V)   -> {number, float_to_list(V, [{decimals, 5}])}
%%                      end, "~w"}}).
%%  a   |     b     |   c
%% -----+-----------+-------
%%  a   |        10 |  ccc
%% bxxx | 200.00123 | 'Done'
%% -----+-----------+-------
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec pretty_table([string()], [Row :: tuple()|list()|map()], #opts{}) -> list().
pretty_table(HeaderRowKeys, Rows, #opts{} = Opts) ->
  pretty_table1(HeaderRowKeys, Rows, Opts).

pretty_print_table([Map|_] = LofMaps0) when is_map(Map) ->
  io:put_chars(pretty_table1(lists:sort(maps:keys(Map)), LofMaps0, #opts{})).

pretty_print_table(HeaderRowKeys, Rows) ->
  io:put_chars(pretty_table1(HeaderRowKeys, Rows, #opts{})).

pretty_print_table(HeaderRowKeys, Rows, Opts) ->
  io:put_chars(pretty_table1(HeaderRowKeys, Rows, Opts)).

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

titlecase([], Acc) ->
  lists:reverse(Acc);
titlecase([C | T], [] = Acc) when C >= $a, C =< $z ->
  titlecase(T, [C + ($A - $a) | Acc]);
titlecase([C | T], [$  |_] = Acc) when C >= $a, C =< $z ->
  titlecase(T, [C + ($A - $a) | Acc]);
titlecase([C | T], Acc) ->
  titlecase(T, [C | Acc]).

wordcount([],  N)                    -> N;
wordcount([C], N) when C =/= $       -> N+1;
wordcount([C,$ |T], N) when C =/= $  -> wordcount([$ |T], N+1);
wordcount([_   |T], N)               -> wordcount(T, N).

wordwrap([], Acc, WordAcc, _LineLen, _Margin) ->
  lists:reverse(WordAcc ++ Acc);

% Premature newline
wordwrap([$\n | Rest], Acc, WordAcc, _LineLen, Margin) ->
  wordwrap(Rest, [$\n | dropws(WordAcc, Acc)], [], 0, Margin);

% Reached the wrap length at a space character - add $\n
wordwrap([$  | Rest], Acc, WordAcc, Margin, Margin) ->
  wordwrap(Rest, [$\n | dropws(WordAcc, Acc)], [], 0, Margin);

% Found space character before the wrap length - continue
wordwrap([$  | Rest], Acc, WordAcc, LineLen, Margin) ->
  wordwrap(Rest, [$  | WordAcc ++ Acc], [], LineLen + 1 + length(WordAcc), Margin);

% Overflowed the current line while building a word
wordwrap([C | Rest], Acc, WordAcc, 0, Margin) when erlang:length(WordAcc) > Margin ->
  wordwrap(Rest, Acc, [C | WordAcc], 0, Margin);
wordwrap([C | Rest], Acc, WordAcc, LineLen, Margin) 
  when erlang:length(WordAcc) + LineLen > Margin ->
  wordwrap(Rest, [$\n | dropws(Acc, [])], [C | WordAcc], 0, Margin);

% Just building a word...
wordwrap([C | Rest], Acc, WordAcc, LineLen, Margin) ->
  wordwrap(Rest, Acc, [C | WordAcc], LineLen, Margin).

dropws(Word, Acc) -> dropws2(dropws1(Word), Acc).

dropws1([$ |T]) -> dropws1(T);
dropws1(L     ) -> L.

dropws2([],   Acc) -> dropws1(Acc);
dropws2(Word, Acc) -> Word ++ Acc.

translate_excludes(ColNames, ExcludeNamesAndPos, StartPos) ->
  {IDs, Names} = lists:partition(fun(I) -> is_integer(I) end, ExcludeNamesAndPos),
  Cols         = if is_tuple(ColNames) -> ColNames; true -> list_to_tuple(ColNames) end,
  TransNames   = fun G([_|L], I, T) when I > tuple_size(Cols) -> G(L,1,T);
                     G([A|L], I, T) when element(I,Cols) == A -> G(L,I+1,setelement(I, T, true));
                     G([],   _I, T) -> T;
                     G(L,     I, T) -> G(L,I+1,T)
                 end,
  BaseExcludes = erlang:make_tuple(tuple_size(Cols), false, [{I, true} || I <- IDs]),
  Excludes     = TransNames(Names, 1, BaseExcludes),
  take_nth(StartPos, tuple_to_list(Excludes)).

filter_out([], _)              -> [];
filter_out([_|T1], [true |T2]) -> filter_out(T1, T2);
filter_out([H|T1], [false|T2]) -> [H|filter_out(T1, T2)].

pretty_table1(Keys0, Rows0, #opts{} = Opts) when is_tuple(Keys0) ->
  pretty_table1(tuple_to_list(Keys0), Rows0, Opts);
pretty_table1(Keys0, Rows0, #opts{} = Opts) when is_list(Keys0), is_list(Rows0) ->
  Exclude = translate_excludes(Keys0, Opts#opts.td_exclude, Opts#opts.td_start),
  KeyStrs = take_nth(Opts#opts.td_start, [element(2, to_string(Key)) || Key <- Keys0]),
  Rows    = [take_nth(Opts#opts.td_start, to_strings(Keys0, V, Opts#opts.td_formats)) || V <- Rows0],
  Ws      = ws(Rows, [{undefined, string:length(Key)} || Key <- KeyStrs]),
  Col     = fun({_,Str}, {Type, Width}) ->
              {Dir,Pad} = case Type of
                            number -> {leading, Opts#opts.number_pad};
                            _      -> {Opts#opts.td_dir, $\s}
                          end,
              string:pad(Str, Width, Dir, Pad)
            end,
  AddSpLn = length([C || C <- Opts#opts.td_sep, C == $\s]),
  AddSpH  = string:copies(Opts#opts.tr_sep, AddSpLn div 2),
  AddSpT  = string:copies(Opts#opts.tr_sep, AddSpLn - (AddSpLn div 2)),
  Row     = fun(Row) ->
              R0 = filter_out([Col(Str, W) || {Str,W} <- lists:zip(Row, Ws)], Exclude),
              [Opts#opts.prefix, lists:join(Opts#opts.td_sep, R0)]
            end,
  Header0 = [{string:pad(Str, W, Opts#opts.th_dir), string:copies(Opts#opts.tr_sep, W)}
              || {Str,{_,W}} <- lists:zip(KeyStrs, Ws)],
  Header  = if Opts#opts.out_header ->
              [lists:join(Opts#opts.td_sep, filter_out([H || {H,_} <- Header0], Exclude)),$\n];
            true ->
              []
            end,
  Delim   = if Opts#opts.out_sep ->
              [lists:join(AddSpH ++ [Opts#opts.tr_sep_td] ++ AddSpT,
                filter_out([T || {_,T} <- Header0], Exclude)),$\n];
            true ->
              []
            end,
  [Opts#opts.prefix, Header, Delim, string:join(filter_out([Row(R) || R <- Rows], Exclude), "\n"),
   if Delim==[] -> ""; true -> "\n" end, Delim].

aligned_format(Fmt, Rows) ->
  {match, FF} = re:run(Fmt, "(~-?s)", [global, {capture, [1], list}]),
  Directions  = [case F of
                   "~s"  -> trailing;
                   "~-s" -> leading
                 end || [F] <- FF],
  Fmt1 = lists:append(string:replace(Fmt, "~-s", "~s", all)),
  aligned_format(Fmt1, Rows, Directions).

aligned_format(Fmt, Rows, Directions) when is_list(Rows), is_list(Directions) ->
  Aligned = align_rows(Rows, Directions),
  Fun     = fun(T) when is_tuple(T) -> tuple_to_list(T);
               (L)                  -> case all_columns_simple(L) of
                                          true  -> [L];
                                          false ->  L
                                       end
            end,
  [io_lib:format(Fmt, Fun(Row)) || Row <- Aligned].

align_rows(Rows) ->
  align_rows(Rows, []).

%%-------------------------------------------------------------------------
%% @doc Align rows of terms by stringifying them to uniform column width.
%%      If some row doesn't need to be aligned, pass its value as a binary.
%% `Options' can be:
%%
-spec align_rows(Rows    :: [tuple()|binary()|list()],
                 Options :: [{pad,    Dir::[trailing|leading|both|
                                            {Pos::integer()|last,trailing|leading|both|none}]} |
                             {exclude,Cols::[integer()]} |
                             {return, Ret::tuple|list}   |
                             {prefix,       string()}    |
                             {ignore_empty, boolean()}]) ->
       [AlignedRow::tuple()|list()].
%% `Rows' is a list. All rows must have the same arity except if a row is
%%  a binary.
%% `Options' contain:
%% <dl>
%% <dt>{pad, Direction}</dt>
%%     <dd>Column padding direction, where `Direction' is one of `leading',
%%         `trailing', `{Position::integer(), leading|trailing|none}',
%%         `{last, leading|trailing|none}'</dd>
%% <dt>{return, tuple|list}</dt>
%%     <dd>Return result rows as lists or tuples</dd>
%% <dt>{prefix, string()}</dt>
%%     <dd>Prefix first item in each row with this string</dd>
%% <dt>{ignore_empty, boolean()}</dt>
%%     <dd>Don't pad trailing empty columns if this option is true</dd>
%% <dt>{exclude, [integer()]}</dt>
%%     <dd>Exclude given column numbers</dd>
%% </dt>
%% @end
%%-------------------------------------------------------------------------
align_rows([], _Options) ->
  [];
align_rows(Rows, Options) when is_list(Rows), is_list(Options) ->
  case lists:any(fun(I) -> is_tuple(I) end, Rows) of
    true ->
      FF = fun
            (I) when is_binary(I) -> I;
            (I) when is_tuple(I)  -> tuple_to_list(I);
            (I) when is_list(I)   -> I
           end,
      RR = [FF(R) || R <- Rows],
      LL = align_rows1(RR, Options),
      case proplists:get_value(return, Options) of
        I when I==undefined; I==tuple ->
          [case is_binary(R) of
             true -> binary_to_list(R);
             _    -> list_to_tuple(R)
           end || R <- LL];
        list ->
          [case is_binary(R) of
             true  -> binary_to_list(R);
             false -> R
           end || R <- LL]
      end;
    false ->
      align_rows1(Rows, Options)
  end.
align_rows1(Rows, Options) when is_list(Rows), is_list(Options) ->
  Simple = all_columns_simple(Rows),
  {N, L, Unlist} = if
                     Simple -> {1, [[I] || I <- Rows], true};
                     true   -> {length(hd([R || R <- Rows, not is_binary(R)])), Rows, false}
                   end,
  lists:filter(fun
                (R) when is_list(R)   -> length(R) =/= N;
                (R) when is_binary(R) -> false
               end, L) =/= []
    andalso throw({all_rows_must_have_same_arity, N,
                   [I || I <- L, not is_binary(I), length(I) /= N]}),
  RR  = [case is_binary(R) of
           true -> R;
           _    -> [lists:flatten(element(2,to_string1(I))) || I <- R]
         end || R <- L],
  Ln  = [case is_binary(R) of
           true -> R;
           _    -> list_to_tuple([length(I) || I <- R])
         end || R <- RR],
  Max = fun(I) -> lists:max([case is_binary(R) of
                               true -> 0;
                               _    -> element(I, R)
                             end || R <- Ln]) end,
  ML  = fun
          Loop(0, A) -> A;
          Loop(I, A) -> Loop(I-1, [Max(I) | A])
        end,
  LW  = ML(N, []),
  Dir = proplists:get_value(pad, Options, []),
  DD  = if
          length(Dir) == N, is_atom(hd(Dir)) ->
            Dir;
          true ->
            T0 = erlang:make_tuple(N, trailing),
            {_,T1} = lists:foldl(
                  fun
                    (D, {I, A})     when   D == trailing orelse
                                           D == leading  orelse
                                           D == both     orelse
                                           D == none ->
                      {I+1, setelement(I, A, D)};
                    ({last,D}, {I,A}) when
                                          (D == trailing orelse
                                           D == leading  orelse
                                           D == both     orelse
                                           D == none) ->
                      {I+1, setelement(N, A, D)};
                    ({J,D}, {I, A}) when (is_integer(J) andalso J =< N) andalso
                                          (D == trailing orelse
                                           D == leading  orelse
                                           D == both     orelse
                                           D == none) ->
                      {I+1, setelement(J, A, D)}
                  end, {1, T0}, Dir),
            tuple_to_list(T1)
        end,
  HasLastNone = lists:member({last,none}, Dir),
  IE  = proplists:get_value(ignore_empty, Options, false),
  SkE = fun
          (List) when not IE ->
            List;
          (List) ->
            RL0 = lists:reverse(List),
            RL1 = lists:dropwhile(fun({_Wid, Row, _Pad}) -> Row == [] end, RL0),
            case {length(RL0) == length(RL1), HasLastNone, RL1} of
              {false, true, [{Wid, Row, _} | Rest]} ->
                lists:reverse([{Wid, Row, none} | Rest]);
              _ ->
                lists:reverse(RL1)
            end
        end,
  PAD = fun
          (S,_W, none) -> S;
          (S, W, D)    -> string:pad(S, W, D)
        end,
  LL  = [case is_binary(R) of
          true  -> R;
          false -> [lists:flatten(PAD(S, W, D)) || {W,S,D} <- SkE(lists:zip3(LW,R,DD))]
         end || R <- RR],
  LL1 = case [I || I <- proplists:get_value(exclude, Options, []), is_integer(I), I > 0] of
          [] -> LL;
          EX -> lists:map(fun(LR) ->
                  [I || I <- tuple_to_list(
                              lists:foldl(fun(I, T) ->
                                setelement(I, T, false)
                                end, list_to_tuple(LR), EX)), I =/= false]
                  end, LL)
        end,
  case  proplists:get_value(prefix,  Options, []) of
    [] when Unlist ->
      [I || [I] <- LL1];
    [] ->
      LL1;
    Pfx ->
      LL2 = [case R of
               _ when is_binary(R) ->
                 <<(list_to_binary(Pfx))/binary, R/binary>>;
               [HH|TT] when is_binary(HH) ->
                 [<<(list_to_binary(Pfx))/binary, HH/binary>> | TT];
               [HH|TT] when is_list(HH) ->
                 [Pfx ++ HH | TT]
             end || R <- LL1],
      if
        Unlist -> [I || [I] <- LL2];
        true   -> LL2
      end
  end.

all_columns_simple(Rows) ->
  lists:all(fun(I) ->
             is_atom(I)    orelse
             is_integer(I) orelse
             is_float(I)   orelse
             is_binary(I)  orelse
             io_lib:printable_list(I)
            end, Rows).

take_nth(I, L) when I < 2 -> L;
take_nth(_,[])            -> [];
take_nth(I,[_|T])         -> take_nth(I-1, T).

ws([H|T], Ws)             -> ws(T, ws1(H, Ws));
ws([], Ws)                -> Ws.

ws1([{T,V}|Vs], [{Type,Max}|Ms]) ->
  [{type(T,Type),max(string:length(V),Max)}|ws1(Vs,Ms)];
ws1([], []) -> [].

type(T, undefined) -> T;
type(T, T)         -> T;
type(_, _)         -> string.

to_strings(Keys, Values, undefined) ->
  to_strings1(Keys, Values, tuple_to_list(erlang:make_tuple(length(Keys), undefined)));
to_strings(Keys, Values, Formats) when is_tuple(Formats), tuple_size(Formats) == length(Keys) ->
  to_strings1(Keys, Values, tuple_to_list(Formats));
to_strings(Keys, Values, Formats) when is_list(Formats), length(Formats) == length(Keys) ->
  to_strings1(Keys, Values, Formats).

to_strings1([], _, _) ->
  [];
to_strings1([K|Keys], Map, [F|Formats]) when is_map(Map) ->
  [to_string(maps:get(K, Map), F) | to_strings1(Keys, Map, Formats)];
to_strings1(_Keys, List, Formats) when is_list(List), is_list(Formats) ->
  [to_string(Entry, F) || {Entry, F} <- lists:zip(List, Formats)];
to_strings1(Keys, Tuple, Formats) when is_tuple(Tuple), is_list(Formats) ->
  to_strings1(Keys, tuple_to_list(Tuple), Formats).

to_string(V) ->
  to_string(V, undefined).

to_string(V, Fmt) when is_list(Fmt) ->
  {guess_type(V), io_lib:format(Fmt, [V])};
to_string(V, Fmt) when is_function(Fmt, 1) ->
  case Fmt(V) of
    R when is_tuple(R)
         , tuple_size(R)==2
         , (element(1,R)==number orelse element(1,R)==string) ->
      R;
    R when is_tuple(R) ->
      throw({invalid_format_function_return, V, R});
    R ->
      {string, R}
  end;
to_string(V, undefined) ->
  to_string1(V).
  
to_string1(Int)   when is_integer(Int) -> {number, integer_to_list(Int)};
to_string1(Float) when is_float(Float) -> {number, io_lib:format("~.4f",[Float])};
to_string1(Str)   when is_list(Str)    -> {string, Str};
to_string1(Bin)   when is_binary(Bin)  -> {string, binary_to_list(Bin)};
to_string1(T)                          -> {string, io_lib:format("~tp", [T])}.

guess_type(V)     when is_integer(V)   -> number;
guess_type(V)     when is_float(V)     -> number;
guess_type(_)                          -> string.
