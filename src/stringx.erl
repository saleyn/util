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
%%% format_number/3,4, format_price/1,2,3 functions are taken from
%%% [https://github.com/DOBRO/uef-lib/blob/master/src/uef_format.erl]
%%% Copyright (c) 2019, Sergei Semichev <chessvegas@chessvegas.com>
%%%------------------------------------------------------------------------
-module(stringx).
-author('saleyn@gmail.com').

%% External API
-export([titlecase/1, wordwrap/2, wordwrap/3]).
-export([pretty_table/1, pretty_table/2, pretty_table/3]).
-export([pretty_print_table/1, pretty_print_table/2, pretty_print_table/3]).
-export([align_rows/1, align_rows/2, aligned_format/2, aligned_format/3]).

-export([format/2, format_binary/2]).
-export([format_integer/1, format_integer/2, format_number/3, format_number/4]).
-export([format_price/1, format_price/2, format_price/3]).
-export([round_price/1, round_number/2, binary_join/2]).
-export([parse_csv/1, parse_csv/2, batch_split/2]).

-type formatted_number() :: binary().
-type precision()        :: integer().
-type decimals()         :: 0..253. % see types for erlang:float_to_binary/2
-type ccy_sym()          :: binary() | string().
-type format_number_opts() :: #{
	thousands     => binary() | string(),
	decimal_point => binary() | string(),  % Default: "."
	ccy_sym       => ccy_sym(),
	ccy_pos       => left | right,
	ccy_sep       => binary() | string(),
  return        => binary | list         % Default: binary
}.

-type pretty_print_opts() :: #{
  number_pad => char(),    % Padding character for numbers
  header     => boolean(), % Output header row
  unicode    => boolean(), % When true, use unicode outline characters
  outline    => none | full | [top | left | bottom | right], % outline sides
  th_dir     => both|leading|trailing, % table header padding dir
  td_dir     => both|leading|trailing, % table row    padding dir
  td_pad     => #{integer() => both|leading|trailing}, % column padding dir
  td_start   => integer(), % Start printing from this field number
  td_exclude => list(),    % Exclude columns (start with 1) or names
  td_sep     => string(),  % Column separator
  tr_sep     => string(),
  tr_sep_td  => string(),  % Delimiter header/footer column sep
  prefix     => string(),  % Use this prefix in front of each row
  thousands  => string()|binary(),       % Thousands separator for numbers
  translate  => fun((term()) -> term()), % Value translation function `(Val) -> any()`
  footer_rows=> integer(), % Number of footer rows
  td_formats => tuple()|
                fun((ColVal::term()) -> {string, string()}|
                                        {number, string()|number()}|
                                        {number, Decimals::integer(), ColVal::number()}|
                                        {ccy,    number()}),  % Optional tuple containing value format for columns
  thousands  => string()|binary(),       % Number thousands separator
  ccy_sym    => string()|binary(),       % Currency prefix/suffix
  ccy_sep    => string()|binary(),       % Currency symbol separator
  ccy_pos    => left|right               % Currency symbol position
}.

-export_type([format_number_opts/0, pretty_print_opts/0]).

-include("stringx.hrl").
-include("iif.hrl").

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
%% @doc Wrap words words in a string
%% @end
%%-------------------------------------------------------------------------
-spec wordwrap(string(), integer()) -> string().
wordwrap(S, Margin) when is_list(S), is_integer(Margin) ->
  wordwrap(S, [], [], 0, Margin).

%%-------------------------------------------------------------------------
%% @doc Wrap words words in a string to multiple lines that fit the margin
%% Example:
%% <code>
%% 1> stringx:wordwrap(["abc", "efg", "exdf"], 8, ",").
%% ["abc,efg,","exdf"]
%% </code>
%% @end
%%-------------------------------------------------------------------------
-spec wordwrap([Word], integer(), Word) -> [string()|binary()]
        when Word :: string()|binary().
wordwrap(Words, Margin, Delim) ->
  wrap(Words, [[]], Margin, Delim).

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
-spec pretty_table([binary()|string()|atom()],
                   [Row :: tuple()|list()|map()]) -> list().
pretty_table(HeaderRowKeys, Rows) ->
  pretty_table(HeaderRowKeys, Rows, #opts{}).

-spec pretty_table([string()|binary()|atom()]|tuple(),
                   [Row :: tuple()|list()|map()],
                   Opts::map()|#opts{}) -> list().
pretty_table(HeaderRowKeys, Rows, MapOpts) when is_map(MapOpts) ->
  pretty_table0(HeaderRowKeys, Rows, MapOpts);

pretty_table(HeaderRowKeys, Rows, #opts{} = Opts) ->
  pretty_table1(HeaderRowKeys, Rows, Opts).

%%-------------------------------------------------------------------------
%% @doc Pretty print table of lists/tuples/maps to list.
%% The following options control formatting behavior:
%% <dl>
%% <dt>header</dt>
%%      <dd>When true (default), output header row</dd>
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
%% <dt>td_pad</dt>
%%      <dd>A map containing column padding directions
%%      `#{Col::integer() => both|leading|trailing'.</dd>
%% <dt>td_sep</dt>
%%      <dd>Column separator (default `" | "')</dd>
%% <dt>tr_sep</dt>
%%      <dd>Row separator (default `"-"')</dd>
%% <dt>tr_sep_td</dt>
%%      <dd>Column delimiter used in separating rows (`"+"')</dd>
%% <dt>prefix</dt>
%%      <dd>Prepend each row with this string</dd>
%% <dt>td_formats</dt>
%%      <dd>A tuple containing column formats. Each value is either
%%          a format string passed to `io_lib:format/2' or a function taking
%%          either one argument
%%          `fun(Value) -> {number|string, FormattedValue::string()}'
%%          or three arguments
%%          `fun(Key,Value,Row::tuple()|map()) -> {number|string, FormattedValue::string()}'.
%%          This three argument function can perform calculation of the field
%%          value based on values of other fields in the `Row'.
%%      </dd>
%% <dt>unicode</dt>
%%      <dd>Use unicode outline characters</dd>
%% <dt>outline</dt>
%%      <dd>Draw top, left and line box outline (by default only the bottom one is drawn).
%%          Values:
%%          <ul>
%%          <li>`none' - on outline box</li>
%%          <li>`full' - outline box on all 4 sides</li>
%%          <li>`[top, left, bottom, right]' - outline box on given sides</li>
%%          </ul>
%%      </dd>
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
-spec pretty_print_table([map()]) -> ok.
pretty_print_table([Map|_] = LofMaps0) when is_map(Map) ->
  io:put_chars(pretty_table1(lists:sort(maps:keys(Map)), LofMaps0, #opts{})).

-spec pretty_print_table([string()|binary()|atom()]|tuple(), [map()|list()]) -> ok.
pretty_print_table(HeaderRowKeys, Rows) ->
  io:put_chars(pretty_table1(HeaderRowKeys, Rows, #opts{})).

-spec pretty_print_table([string()|binary()|atom()]|tuple(), [map()|list()], #opts{}|map()) -> ok.
pretty_print_table(HeaderRowKeys, Rows, Opts) ->
  io:put_chars(pretty_table0(HeaderRowKeys, Rows, Opts)).

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


% * All done, return the result
wrap([], Result, _Margin, _Delim) ->
  lists:map(fun
    ([])  -> [];
    ([L]) -> [L];
    (L)   -> lists:flatten(L)
  end, lists:reverse(Result));

wrap([Word | Rest], [CurrLine | PrevLines], Margin, Delim) ->
  Width = iolist_size(Word) + iolist_size(CurrLine) + iolist_size(Delim),
  Fits  = Width =< Margin,
  if
    % Adding word(s) to an empty line
    CurrLine == [], Rest == [] ->
      % This is the last word
      wrap([],   [Word | PrevLines], Margin, Delim);
    CurrLine == [] ->
      wrap(Rest, [[Word, Delim] | PrevLines], Margin, Delim);

    % Adding to a partially filled line, where the word fits the margin?
    Fits, Rest == [] ->
      % This is the last word
      wrap([],   [[CurrLine, Word] | PrevLines], Margin, Delim);
    Fits ->
      wrap(Rest, [[CurrLine, Word, Delim] | PrevLines], Margin, Delim);

    % The word does not fit the line, append it to the new one.
    not Fits, Rest == [] ->
      % This is the last word
      wrap([],   [Word, CurrLine | PrevLines], Margin, Delim);
    not Fits ->
      wrap(Rest, [[Word, Delim], CurrLine | PrevLines], Margin, Delim);
    true ->
      erlang:error(logic_error)
  end.

dropws(Word, Acc) -> dropws2(dropws1(Word), Acc).

dropws1([$ |T]) -> dropws1(T);
dropws1(L     ) -> L.

dropws2([],   Acc) -> dropws1(Acc);
dropws2(Word, Acc) -> Word ++ Acc.

translate_excludes(_,  I, _) when I==undefined; I==[] -> [];
translate_excludes([], _, _)                          -> [];
translate_excludes(ColNames, ExcludeNamesAndPos, StartPos) ->
  {IDs, Names} = lists:partition(fun(I) -> is_integer(I) end, ExcludeNamesAndPos),
  Cols         = list_to_tuple(ColNames),
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
filter_out([H|T1], [false|T2]) -> [H|filter_out(T1, T2)];
filter_out([H|T1], []) ->         [H|filter_out(T1, [])].

-spec pretty_table0(tuple()|[string()|binary()|atom()], list(), #opts{}|map()) -> iolist().
pretty_table0(HdrRowKeys, Rows, #opts{} = Opts) ->
  pretty_table1(HdrRowKeys, Rows, Opts);
pretty_table0(HdrRowKeys, Rows, MapOpts) when is_map(MapOpts) ->
  DefTup  = #opts{},
  DefOpts = maps:from_list(lists:zip(record_info(fields, opts), tl(tuple_to_list(DefTup)))),
  MOpts   = maps:merge(DefOpts, MapOpts),
  #{number_pad:=NP, header:=OH,      th_dir:=THD, td_dir:=TDD, td_pad:=TDA,
    td_start:=TDST, td_exclude:=TDE, td_sep:=TDS, tr_sep:=TRS, tr_sep_td:=TRSTD,
    prefix:=Prf,    translate :=TR,  footer_rows:=FR,          td_formats:=TDF,
    thousands:=THS, ccy_sym:=CCY,    ccy_sep    :=CS,          ccy_pos:=CP,
    unicode  :=UNI, outline:=OUTLINE} = MOpts,
  Opts = #opts{
    number_pad = NP,
    header     = OH,
    th_dir     = THD,
    td_dir     = TDD,
    td_pad     = TDA,
    td_start   = TDST,
    td_exclude = TDE,
    td_sep     = ?IIF(UNI andalso TDS   == DefTup#opts.td_sep,   " │ ", TDS),
    tr_sep     = ?IIF(UNI andalso TRS   == DefTup#opts.tr_sep,   "─",   TRS),
    tr_sep_td  = ?IIF(UNI andalso TRSTD == DefTup#opts.tr_sep_td,"┼",   TRSTD),
    prefix     = Prf,
    translate  = TR,
    footer_rows= FR,
    td_formats = TDF,
    thousands  = THS,
    ccy_sym    = CCY,
    ccy_sep    = CS,
    ccy_pos    = CP,
    outline    = OUTLINE,
    unicode    = UNI
  },
  pretty_table1(HdrRowKeys, Rows, Opts).

pretty_table1(Keys0, Rows0, Opts) when is_tuple(Keys0) ->
  pretty_table1(tuple_to_list(Keys0), Rows0, Opts);
pretty_table1(Keys0, Rows0, #opts{unicode = Uni} = Opts) when is_list(Keys0), is_list(Rows0) ->
  Exclude = translate_excludes(Keys0, Opts#opts.td_exclude, Opts#opts.td_start),
  KeyStrs = take_nth(Opts#opts.td_start, [element(2, to_string(Key)) || Key <- Keys0]),
  Rows    = [if is_binary(V) -> V; true -> take_nth(Opts#opts.td_start, to_strings(Keys0, V, Opts)) end || V <- Rows0],
  Ws      = ws(Rows, [{undefined, string:length(Key)} || Key <- KeyStrs]),
  NCols   = length(Keys0),
  Pad     = fun (I, DefPad) -> maps:get(I, Opts#opts.td_pad, DefPad) end,
  Col     = fun
              ({number,Str}, {_, Width}, I) ->
                string:pad(Str, Width, Pad(I, leading), Opts#opts.number_pad);
              ({_,undefined}, {_,Width}, I) when is_atom(Opts#opts.td_dir) ->
                string:pad("", Width,  Pad(I, Opts#opts.td_dir), $\s);
              ({_,Str}, {_, Width}, I) when is_atom(Opts#opts.td_dir) ->
                string:pad(Str, Width, Pad(I, Opts#opts.td_dir), $\s);
              ({_,_Str}, {_, _Width}, _I) ->
                throw({invalid_option, td_dir, Opts#opts.td_dir})
            end,
  #{top:=BoxT, bottom:=BoxB, left:=BoxL,right:=BoxR} = to_outline(Opts#opts.outline),
  {BoxTL,BoxTR,BoxBHL,BoxEHL,BoxBOL,BoxEOL,BoxBL,BoxBR,BoxTC,BoxBC} =
    case {BoxT or BoxB or BoxL or BoxR, Uni} of
      {false,_} -> {"", "", "",  "", "",  "", "", "",  "", "|"};
      {_, true} -> {?IIF(BoxT and BoxL,"┌─",""),  ?IIF(BoxT and BoxR,"─┐",""),
                    ?IIF(BoxL,"├─",""), ?IIF(BoxR,"─┤",""),
                    ?IIF(BoxL,"│ ",""), ?IIF(BoxR," │",""), ?IIF(BoxB and BoxL,"└─",""),
                    ?IIF(BoxB and BoxR,"─┘",""),
                    ?IIF(BoxT,"┬",""),  ?IIF(BoxB,"┴","")};
      {_,false} -> {?IIF(BoxT and BoxL, "+-",""), ?IIF(BoxT and BoxR,"-+",""),
                    ?IIF(BoxL,"+-",""), ?IIF(BoxR,"-+",""),
                    ?IIF(BoxL,"| ",""), ?IIF(BoxR," |",""), ?IIF(BoxB and BoxL,"+-",""),
                    ?IIF(BoxB and BoxR,"-+",""),
                    ?IIF(BoxT, "+",""), ?IIF(BoxB,"+","")}
    end,
  AddSpLn = length([C || C <- Opts#opts.td_sep, C == $\s]),
  AddSpH  = string:copies(Opts#opts.tr_sep, AddSpLn div 2),
  AddSpT  = string:copies(Opts#opts.tr_sep, AddSpLn - (AddSpLn div 2)),
  Nums    = lists:seq(1, NCols),
  Row     = fun
              (Row) when is_binary(Row) ->
                [Opts#opts.prefix, BoxBOL, Row, BoxEOL, $\n];
              (Row) ->
                R0 = filter_out([Col(Str, W, I) || {Str,W,I} <- lists:zip3(Row, Ws, Nums)], Exclude),
                [Opts#opts.prefix, BoxBOL, lists:join(Opts#opts.td_sep, R0), BoxEOL, $\n]
            end,
  Header0 = [{string:pad(Str, W, Opts#opts.th_dir), string:copies(Opts#opts.tr_sep, W)}
              || {Str,{_,W}} <- lists:zip(KeyStrs, Ws)],
  Top     = ?IIF(BoxT, [Opts#opts.prefix, BoxTL, lists:join(AddSpH ++ BoxTC ++ AddSpT,
                        filter_out([T || {_,T} <- Header0], Exclude)), BoxTR, $\n], ""),
  Header  = ?IIF(Opts#opts.header,
              [Opts#opts.prefix, BoxBOL,
               lists:join(Opts#opts.td_sep, filter_out([H || {H,_} <- Header0], Exclude)),
               BoxEOL, $\n], ""),
  Delim   = ?IIF(Opts#opts.header,
              [Opts#opts.prefix, BoxBHL, lists:join(AddSpH ++ [Opts#opts.tr_sep_td] ++ AddSpT,
                filter_out([T || {_,T} <- Header0], Exclude)), BoxEHL, $\n], ""),
  Footer  = ?IIF(BoxB,
              [Opts#opts.prefix, BoxBL, lists:join(AddSpH ++ BoxBC ++ AddSpT,
                filter_out([T || {_,T} <- Header0], Exclude)), BoxBR, $\n], ""),
  [Top, Header, Delim, filter_out([Row(R) || R <- Rows], Exclude), Footer].

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
%% `Rows' is a list. All rows must have the same arity except if a row is
%%  a binary.
%% `Options' contain:
%% <dl>
%% <dt>{pad, Direction}</dt>
%%     <dd>Column padding direction, where `Direction' is one of `leading',
%%         `trailing', `{Position::integer(), leading|trailing|none}',
%%         `{last, leading|trailing|none}'</dd>
%% <dt>{type, binary|charlist}</dt>
%%     <dd>Return columns in the result rows as binaries or charlists
%%     (default)</dd>
%% <dt>{return, tuple|list}</dt>
%%     <dd>Return result rows as lists or tuples</dd>
%% <dt>{prefix, string()}</dt>
%%     <dd>Prefix first item in each row with this string</dd>
%% <dt>{ignore_empty, boolean()}</dt>
%%     <dd>Don't pad trailing empty columns if this option is true</dd>
%% <dt>{exclude, [integer()]}</dt>
%%     <dd>Exclude given column numbers</dd>
%% </dl>
%% @end
%%-------------------------------------------------------------------------
-spec align_rows(
        Rows    :: [tuple()|binary()|list()],
        Options :: [{pad,    Dir::[trailing|leading|both|
                                   {Pos::integer()|last,
                                     trailing|leading|both|none}]} |
                   {exclude,Cols::[integer()]} |
                   {return, Ret::tuple|list}   |
                   {type,   binary|charlist}   |
                   {prefix,       string()}    |
                   {ignore_empty, boolean()}]) ->
       [AlignedRow::tuple()|list()].
align_rows([], _Options) ->
  [];
align_rows(Rows, Options) when is_list(Rows), is_list(Options) ->
  TP = proplists:get_value(type, Options, charlist),
  case lists:any(fun(I) -> is_tuple(I) end, Rows) of
    true ->
      FF = fun
            (I) when is_binary(I) -> I;
            (I) when is_tuple(I)  -> tuple_to_list(I);
            (I) when is_list(I)   -> I
           end,
      RR = [FF(R) || R <- Rows],
      LL = align_rows1(RR, TP, Options),
      case proplists:get_value(return, Options) of
        I when I==undefined; I==tuple ->
          [case {is_binary(R), TP} of
             {true, charlist} -> binary_to_list(R);
             {true, binary}   -> R;
             {false, _}       -> list_to_tuple(R)
           end || R <- LL];
        list ->
          [case {is_binary(R), TP} of
             {true, charlist} -> binary_to_list(R);
             {true, binary}   -> R;
             {false, _}       -> R
           end || R <- LL]
      end;
    false ->
      align_rows1(Rows, TP, Options)
  end.
align_rows1(Rows, TP, Options) when is_list(Rows), is_list(Options) ->
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
  case  proplists:get_value(prefix, Options, []) of
    [] when Unlist ->
      [to_type(TP, I) || [I] <- LL1];
    [] ->
      LL1;
    Pfx0 ->
      Pfx = if is_binary(Pfx0) -> Pfx0;
               is_list(Pfx0)   -> list_to_binary(Pfx0);
               true            -> error({invalid_prefix, Pfx0})
            end,
      LL2 = [case R of
               _ when is_binary(R) ->
                 <<Pfx/binary, R/binary>>;
               [HH|TT] when is_binary(HH) ->
                 [to_type(TP, <<Pfx/binary, HH/binary>>) | [to_type(TP, I) || I <- TT]];
               [HH|TT] when is_list(HH) ->
                 [to_type(TP, <<Pfx/binary, (list_to_binary(HH))/binary>>) | [to_type(TP, I) || I <- TT]]
             end || R <- LL1],
      if
        Unlist -> [to_type(TP, I) || [I] <- LL2];
        true   -> LL2
      end
  end.

to_type(binary,   V) when is_binary(V) -> V;
to_type(binary,   V) when is_list(V)   -> list_to_binary(V);
to_type(charlist, V) when is_binary(V) -> binary_to_list(V);
to_type(charlist, V) when is_list(V)   -> V.

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

ws([H|T], Ws) when is_binary(H) -> ws(T, Ws);
ws([H|T], Ws)                   -> ws(T, ws1(H, Ws));
ws([], Ws)                      -> Ws.

ws1([{T,undefined}|Vs], [{Type,Max}|Ms]) -> [{type(T,Type),Max}|ws1(Vs,Ms)];
ws1([{T,nil}|Vs],       [{Type,Max}|Ms]) -> [{type(T,Type),Max}|ws1(Vs,Ms)];
ws1([{T,V}|Vs],         [{Type,Max}|Ms]) -> [{type(T,Type),max(string:length(V),Max)}|ws1(Vs,Ms)];
ws1([], [])                              -> [].

type(T, undefined) -> T;
type(T, T)         -> T;
type(_, _)         -> string.

to_strings(Keys, Values, #opts{td_formats=undefined}) ->
  to_strings1(Keys, Values, tuple_to_list(erlang:make_tuple(length(Keys), undefined)), #opts{});
to_strings(Keys, Values, #opts{td_formats=Formats}=O) when is_tuple(Formats), tuple_size(Formats) == length(Keys) ->
  to_strings1(Keys, Values, tuple_to_list(Formats), O);
to_strings(Keys, Values, #opts{td_formats=Formats}=O) when (is_list(Formats) andalso length(Formats)==length(Keys))
                                                           orelse is_function(Formats, 1)
                                                           orelse is_function(Formats, 3) ->
  to_strings1(Keys, Values, Formats, O).

to_strings1([], _, _, _) ->
  [];
to_strings1([K|Keys], Map, [F|Formats], Opts) when is_map(Map) ->
  [to_string(K, maps:get(K, Map), Map, F, Opts) | to_strings1(Keys, Map, Formats, Opts)];
to_strings1([K|Keys], Map, Fmt, Opts) when is_map(Map) andalso (is_function(Fmt,1) orelse is_function(Fmt, 3)) ->
  [to_string(K, maps:get(K, Map), Map, Fmt, Opts) | to_strings1(Keys, Map, Fmt, Opts)];
to_strings1(_Keys, List, Fmt, Opts) when is_list(List) andalso (is_function(Fmt,1) orelse is_function(Fmt,3)) ->
  Row = list_to_tuple(List),
  [to_string(undefined, Entry, Row, Fmt, Opts) || Entry <- List];
to_strings1(_Keys, List, Formats, Opts) when is_list(List), is_list(Formats) ->
  Row = list_to_tuple(List),
  [to_string(undefined, Entry, Row, F, Opts) || {Entry, F} <- lists:zip(List, Formats)];
to_strings1(Keys, Row, Fmt, Opts) when is_tuple(Row) andalso (is_function(Fmt,1) orelse is_function(Fmt,3)) ->
  [to_string(K, Entry, Row, Fmt, Opts) || {K, Entry} <- lists:zip(Keys, tuple_to_list(Row))];
to_strings1(Keys, Row, Formats, Opts) when is_tuple(Row), is_list(Formats) ->
  List = tuple_to_list(Row),
  [to_string(K, Entry, Row, F, Opts) || {K, Entry, F} <- lists:zip3(Keys, List, Formats)];
to_strings1(_, Row, _, _) when is_binary(Row) ->
  Row.

to_string(V) ->
  to_string(undefined, V, undefined, undefined, undefined).

to_string(_K, V, _Row, Fmt, _Opts) when is_list(Fmt) ->
  {guess_type(V), io_lib:format(Fmt, [V])};
to_string(_K, V, _Row, Fmt, Opts) when is_function(Fmt, 1) ->
  Res = Fmt(V),
  to_string2(Res, Opts);
to_string(K, V, Row, Fmt, Opts) when is_function(Fmt, 3) ->
  Res = Fmt(K, V, Row),
  to_string2(Res, Opts);
to_string(_K, V, _Row, undefined, Opts) ->
  to_string1(V, Opts).

to_string2({number,R}, Opts) when is_number(R) -> to_string1(R, Opts);
to_string2({number, Dec, I}, Opts) when is_number(Dec) ->
  {number, format_number(I, Dec, Dec, #{thousands=>Opts#opts.thousands, return=>list})};
to_string2({number,L}=R,_Opts) when is_list(L) -> R;
to_string2({string,L},   Opts)                 -> to_string1(L, Opts);
to_string2({ccy,I},Opts)     when is_number(I) -> {number, format_ccy(I, 2, Opts)};
to_string2({ccy,Decimals,I},Opts) when is_integer(Decimals)
                                , is_number(I) -> {number, format_ccy(I, Decimals, Opts)};
to_string2(R,_) when is_tuple(R)               -> throw({invalid_format_function_return, R});
to_string2(R,_) when is_list(R)                -> {string, R}.

to_string1(V) ->
  to_string1(V, undefined).

to_string1(I, #opts{thousands=undefined}) when is_number(I) -> to_string1(I, undefined);
to_string1(I, #opts{thousands=Thousands}) when is_number(I) -> to_string1(I, Thousands);
to_string1(I, undefined) when is_integer(I)  -> {number, integer_to_list(I)};
to_string1(I, Thousands) when is_integer(I)
                            ,(is_binary(Thousands) orelse is_list(Thousands))
                                             -> {number, format_integer(I, #{thousands=>Thousands,
                                                                             return=>list})};
to_string1(F, undefined) when is_float(F)    -> {number, io_lib:format("~.4f",[F])};
to_string1(F, Thousands) when is_float(F)
                            ,(is_binary(Thousands) orelse is_list(Thousands))
                                             -> {number, format_number(F, 4,4, #{thousands=>Thousands,
                                                                                 return=>list})};
to_string1(Str,_Opts)   when is_list(Str)    -> {string, Str};
to_string1(Bin,_Opts)   when is_binary(Bin)  -> {string, binary_to_list(Bin)};
to_string1(undefined,_Opts)                  -> {string, ""};
to_string1(nil,_Opts)                        -> {string, ""};
to_string1(T,_Opts)                          -> {string, io_lib:format("~tp", [T])}.

format_ccy(I, Decimals, #opts{ccy_sym=Sym, ccy_sep=Sep, ccy_pos=Pos, thousands=Th}) ->
  format_number(I, Decimals, Decimals, #{ccy_sym=>Sym, ccy_sep=>Sep, ccy_pos=>Pos,
                                         thousands=>Th, return=>list}).

guess_type(V)     when is_integer(V)   -> number;
guess_type(V)     when is_float(V)     -> number;
guess_type(_)                          -> string.

%% @doc Convert format and arguments to binary/list shortening .
%% This function can be used by Elixir, which is missing the equivalent of `io_lib.format/2'
-spec format(binary()|string(), list()) -> binary()|string().
format(Fmt, Args) when is_list(Args) ->
  Res = io_lib:format(Fmt, Args),
  format1(Res).

%% @doc Convert format and arguments to binary/list shortening .
%% This function can be used by Elixir, which is missing the equivalent of `io_lib.format/2'
-spec format_binary(binary()|string(), list()) -> binary().
format_binary(Fmt, Args) when is_binary(Fmt) or is_list(Fmt) ->
  iolist_to_binary(format(Fmt, Args)).

-define(DEFAULT_PRICE_PRECISION, 2).
-define(DEFAULT_PRICE_DECIMALS, 2).
-define(THOUSANDS_SEP, <<"">>).
-define(DECIMAL_POINT, <<".">>).

-spec format_integer(integer()) -> formatted_number().
format_integer(Integer) ->
  format_integer(Integer, #{}).
-spec format_integer(integer(), format_number_opts()) -> formatted_number().
format_integer(Integer, Opts) when is_integer(Integer), is_map(Opts) ->
  do_format_number(Integer, 0, Opts).

%% @doc
%% The same as uef_format:format_number/4 with #{} as the forth argument.
%% @see format_number/4
%% @end
-spec format_number(number(), precision(), decimals()) -> formatted_number().
format_number(N, Precision, Decimals) when (is_float(N) orelse is_integer(N))
                                         ,  is_integer(Precision)
                                         ,  is_integer(Decimals) ->
	format_number(N, Precision, Decimals, #{});
format_number(N, Precision, Opts) when is_map(Opts) ->
  format_number(N, Precision, Precision, Opts).

%% @doc
%% Formats Number by adding thousands separator between each set of 3
%% digits to the left of the decimal point, substituting Decimals for
%% the decimal point, and rounding to the specified Precision.
%% Returns a binary value.
%% @end
-spec format_number(number(), precision(), decimals(), format_number_opts()) ->
        formatted_number().
format_number(Number, Precision, Decimals, Opts) when is_integer(Number) ->
	format_number(erlang:float(Number), Precision, Decimals, Opts);
format_number(Number, Precision, Decimals, Opts) when is_float(Number) ->
	Precision2 = case Precision > 0 andalso Decimals < Precision of
		true  -> Decimals;
		false -> Precision
	end,
	Rounded = round_number(Number, Precision2), % round to Precision2 before formatting
	do_format_number(Rounded, Decimals, Opts).

%% @doc
%% Formats Number in price-like style.
%% Returns a binary containing FormattedPrice formatted with a precision
%% of 2 and decimal digits of 2. The same as format_price/2 with a precision
%% of 2 as the second argument. See uef_format:format_price/2 docs.
%% @end
-spec format_price(Number:: number()) -> FormattedPrice :: formatted_number().
format_price(Price) ->
	format_price(Price, ?DEFAULT_PRICE_PRECISION).

%% @doc
%% Formats Number in price-like style.
%% Returns a binary containing FormattedPrice formatted with a specified
%% precision as the second argument and decimal digits of 2.
%% The same as uef_format:format_price/3 with #{} as the third argument.
%% @see format_price/3
%% @end
-spec format_price(Number::number(), Precision::precision()) -> formatted_number().
format_price(Price, Precision) ->
	format_price(Price, Precision, #{}).

%% format_price/3
-spec format_price(Number::number(), Precision::precision(),
        CcySymbol_OR_Options::format_number_opts() | ccy_sym()) ->
          FormattedPrice::formatted_number().
%% @doc
%% Formats Number in price-like style.
%% Returns a binary containing FormattedPrice formatted with a specified
%% precision as the second argument, decimal digits of 2,
%% and with ccy symbol (or options) as the third argument.
%% If CcySymbol_OR_Options is a map the functions works as format_number/4
%% with decimal digits of 2 as the third argument and with options as the forth one.
%% If CcySymbol_OR_Options is a binary or a string, the corresponding
%% ccy symbol is added to the left.
%% @end
format_price(Price, Precision, Opts) when is_map(Opts) ->
	format_number(Price, Precision, ?DEFAULT_PRICE_DECIMALS, Opts);
format_price(Price, Precision, CurSymbol) when is_binary(CurSymbol) orelse is_list(CurSymbol) ->
	format_number(Price, Precision, ?DEFAULT_PRICE_DECIMALS, #{ccy_sym => CurSymbol});
format_price(Price, Precision, Opts) ->
	erlang:error({badarg, Opts}, [Price, Precision, Opts]).

%% round_price/1
-spec round_price(Number :: number()) -> float().
%% @doc Rounds the number to the precision of 2.
round_price(Price) -> round_number(Price, 2).

%% round_number/2
-spec round_number(Number :: number(), Precision :: integer()) -> float().
%% @doc Rounds the number to the specified precision.
round_number(Number, Precision) ->
	P = math:pow(10, Precision),
	erlang:round(Number * P) / P.

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

till_quote([$\\, $' | T], Acc) -> till_quote(T, [$', $\\ | Acc]);
till_quote([$'      | T], Acc) -> {lists:reverse(Acc), T};
till_quote([C       | T], Acc) -> till_quote(T, [C | Acc]);
till_quote([],            Acc) -> {lists:reverse(Acc), []}.

append_after_next_word([32|T],S,false)    -> [S, 32 | T];
append_after_next_word([32|T],S,true)     -> [32 | append_after_next_word(T,S,true)];
append_after_next_word([$'|T],S,false)    -> [$' | append_after_next_word(T,S,true)];
append_after_next_word([$\\,$'|T],S,true) -> [$\\,$' | append_after_next_word(T,S,true)];
append_after_next_word([$'|T],S,true)     -> [$',S | T];
append_after_next_word([$"|T],S,false)    -> [$" | append_after_next_word(T,S,true)];
append_after_next_word([$\\,$"|T],S,true) -> [$\\,$' | append_after_next_word(T,S,true)];
append_after_next_word([$"|T],S,true)     -> [$",S | T];
append_after_next_word([H|T],S,X)         -> [H, 32 | append_after_next_word(T, S, X)];
append_after_next_word([],S,_)            -> S.

format1(["'Elixir."++R|T])        -> {S,T1} = till_quote(R, []), [[S], format1(T1) | format1(T)];
format1([$'|T])                   -> {S,T1} = till_quote(T, []), [[S], format1(T1)];
format1("#{__struct__ => " ++ T)  -> T1 = string:trim(T, leading),
                                     T2 = append_after_next_word(T1, ${, false),
                                     format1(T2);
format1([H|T]) when is_integer(H) -> [H          | format1(T)];
format1([H|T]) when is_list(H)    -> [format1(H) | format1(T)];
format1([H|T])                    -> [H          | format1(T)];
format1([])                       -> [].

%% do_format_number/3
-spec do_format_number(number(), decimals(), format_number_opts()) -> formatted_number().
do_format_number(Number, Decimals, Opts) when is_float(Number), is_integer(Decimals), is_map(Opts) ->
	PositiveNumber = case Number < 0 of
		false -> Number;
		true  -> erlang:abs(Number)
	end,
	BinNum = erlang:float_to_binary(PositiveNumber, [{decimals, Decimals}]),
	{IntegerPart, DecimalPart} = case binary:split(BinNum, <<".">>) of
		[I, D] -> {I, D}; % ex: <<"12.345">> -> [<<"12">>, <<"345">>]
		[I] -> {I, <<>>} % ex: <<"12345">> -> [<<"12345">>] (when Precision < 1)
	end,
  do_format_num(Number, IntegerPart, DecimalPart, Opts);

do_format_number(Number, _, Opts) when is_integer(Number) ->
  do_format_num(Number, integer_to_binary(Number), <<>>, Opts).

do_format_num(Number, IntegerPart, DecimalPart, Opts) when is_map(Opts) ->
	HeadSize = erlang:byte_size(IntegerPart) rem 3,
	<<Head:HeadSize/binary, IntRest/binary>> = IntegerPart, % ex: <<"12", "345678">> = <<"12345678">>
	ThousandParts = split_thousands(IntRest), % ex: <<"345678">> -> [<<"345">>, <<678>>]
	AllIntegerParts = case HeadSize > 0 of
		true  -> [Head|ThousandParts];
		false -> ThousandParts
	end,
	ThousandsSep = maybe_to_binary(maps:get(thousands, Opts, ?THOUSANDS_SEP)),
	% Join with thousands separator
	FormattedIntegerPart = <<(binary_join(AllIntegerParts, ThousandsSep))/binary>>,
	PositiveFormattedNumber = case DecimalPart of
		<<>> ->
			FormattedIntegerPart;
		_    ->
			DecimalPoint = maybe_to_binary(maps:get(decimal_point, Opts, ?DECIMAL_POINT)),
			<<FormattedIntegerPart/binary, DecimalPoint/binary, DecimalPart/binary>>
	end,
	% Insert "-" before number if negative
	FormattedNumber1 = case Number < 0 of
		false -> PositiveFormattedNumber;
		true  -> <<"-", PositiveFormattedNumber/binary>>
	end,
	% Format with ccy options
	format_number_with_ccy(FormattedNumber1, Opts).

%% format_number_with_ccy/2
-spec format_number_with_ccy(binary(), map()) -> binary().
format_number_with_ccy(FmtNum, #{ccy_sym := CurSymbol0} = Opts) ->
	CurSym = maybe_to_binary(CurSymbol0),
	CurSep = maybe_to_binary(maps:get(ccy_sep, Opts, <<"">>)),
  format_number_return(
    case maps:get(ccy_pos, Opts, left) of
      left -> <<CurSym/binary, CurSep/binary, FmtNum/binary>>;
      _ -> <<FmtNum/binary, CurSep/binary, CurSym/binary>>
    end,
    maps:get(return, Opts, binary));
format_number_with_ccy(FmtNum, Opts) ->
	format_number_return(FmtNum, maps:get(return, Opts, binary)).

format_number_return(Bin, binary) when is_binary(Bin) ->
  Bin;
format_number_return(Bin, list) when is_binary(Bin) ->
  binary_to_list(Bin);
format_number_return(_Bin, Type) ->
  erlang:error({badarg, {return, Type}}).

%% maybe_to_binary/1
-spec maybe_to_binary(binary() | string()) -> binary().
maybe_to_binary(B) when is_binary(B) -> B;
maybe_to_binary(L) when is_list(L) ->
	case unicode:characters_to_binary(L, utf8, utf8) of
		B when is_binary(B) -> B;
		_ -> erlang:error({badarg, L})
	end;
maybe_to_binary(T)->
	erlang:error({badarg, T}).

%% split_thousands/1
-spec split_thousands(binary()) -> [<<_:24>>].
split_thousands(Bin) ->
	split_thousands(Bin, []).

%% split_thousands/2
-spec split_thousands(binary(), [<<_:24>>]) -> [<<_:24>>].
split_thousands(<<>>, List) ->
	lists:reverse(List);
split_thousands(Bin, List) ->
	<<B:3/binary, Rest/binary>> = Bin,
	split_thousands(Rest, [B | List]).

binary_join([], _Sep) -> <<>>;
binary_join([Bin], _Sep) -> Bin;
binary_join([Head|Tail], Sep) ->
	lists:foldl(fun(Value, Acc) ->
		<<Acc/binary, Sep/binary, Value/binary>>
	end, Head, Tail).

%% @doc Parse a given CSV file.
-spec parse_csv(string()) -> [[string()]].
parse_csv(File) when is_list(File) ->
  csv:parse(File).

%% @doc Parse a given CSV file.
-spec parse_csv(string(), [fix_lengths | {open, Opts::list()}]) -> [[string()]].
parse_csv(File, Opts) when is_list(File), is_list(Opts) ->
  csv:parse(File, Opts).

%% @doc Split a list into batches of N items
-spec batch_split(integer(), list()) -> [list()].
batch_split(N, L) when is_integer(N), is_list(L) ->
  batch_split(N, N, L, [], []).

batch_split(_, _, [], A, Out) ->
  lists:reverse([lists:reverse(A) | Out]);
batch_split(N, 0, L, A, Out) ->
  batch_split(N, N, L, [], [lists:reverse(A) | Out]);
batch_split(N, I, [H|T], A, Out) ->
  batch_split(N, I-1, T, [H|A], Out).

to_outline(none) -> #{top=>false,bottom=>false,left=>false,right=>false};
to_outline(full) -> #{top=>true, bottom=>true, left=>true, right=>true};
to_outline(L) when is_list(L) ->
  (L -- [top,bottom,left,right] /= []) andalso erlang:error({invalid_outline, L}),
  #{top   =>lists:member(top,   L),
    bottom=>lists:member(bottom,L),
    left  =>lists:member(left,  L),
    right =>lists:member(right, L)};
to_outline(#{} = M) ->
  #{top   =>maps:get(top,   M, false),
    bottom=>maps:get(bottom,M, false),
    left  =>maps:get(left,  M, false),
    right =>maps:get(right, M, false)}.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

word_wrap_test() ->
  ?assertEqual(["abc,efg,","exdf"],
    stringx:wordwrap(["abc", "efg", "exdf"], 8, ",")).

align_rows_test() ->
  ?assertEqual(
    [{"cc    ","x1"},{"'Done'","x2"},{"xx    ","x3"}],
    stringx:align_rows([{a, 10, cc,x1}, {bxxx, 200.00123, 'Done',x2},
                        {abc, 100.0, xx,x3}],
                       [{exclude, [1,2]}])

  ),
  ?assertEqual(
    [{"x1"},{"x2"},{"x3"}],
    stringx:align_rows([{a, 10, cc,x1}, {bxxx, 200.00123, 'Done',x2},
                        {abc, 100.0, xx,x3}], [{exclude, [1,2,3]}])
  ),
  ?assertEqual(
    [{"a   ","10      ","cc    ","x1"},
     {"bxxx","200.0012","'Done'","x2"},
     {"abc ","100.0000","xx    ","x3"}],
    stringx:align_rows([{a, 10, cc,x1}, {bxxx, 200.00123, 'Done',x2},
                        {abc, 100.0, xx,x3}], [])
  ).

pretty_table_test() ->
  ?assertEqual(
    " a   |     b    \n"
    "-----+----------\n"
    " a   |        10\n"
    "bxxx | 200.00123\n"
    "-----+----------\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c,d},
      [{a, 10, cc,x1}, {bxxx, 200.00123, 'Done',x2}, {abc, 100.0, xx,x3}],
      #opts{td_dir=both, td_exclude=[3,4],
        td_formats={
          undefined,
          fun(V) when is_integer(V) -> {number, integer_to_list(V)};
             (V) when is_float(V)   -> {number, float_to_list(V, [{decimals, 5}])} end,
          "~w",
          undefined}}))).

pretty_table_unicode_test() ->
  ?assertEqual(
    "a  | b  |  c  \n"
    "---+----+-----\n"
    "10 | 20 |    0\n"
    "30 | 40 | 1000\n"
    "---+----+-----\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => false}))),
  ?assertEqual(
    "a  │ b  │  c  \n"
    "───┼────┼─────\n"
    "10 │ 20 │    0\n"
    "30 │ 40 │ 1000\n"
    "───┴────┴─────\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => true}))),
  ?assertEqual(
    "───┬────┬─────\n"
    "a  │ b  │  c  \n"
    "───┼────┼─────\n"
    "10 │ 20 │    0\n"
    "30 │ 40 │ 1000\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => true, outline => [top]}))),
  ?assertEqual(
    "a  │ b  │  c  \n"
    "───┼────┼─────\n"
    "10 │ 20 │    0\n"
    "30 │ 40 │ 1000\n"
    "───┴────┴─────\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => true, outline => [bottom]}))),
  ?assertEqual(
    "│ a  │ b  │  c  \n"
    "├────┼────┼─────\n"
    "│ 10 │ 20 │    0\n"
    "│ 30 │ 40 │ 1000\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => true, outline => [left]}))),
  ?assertEqual(
    "a  │ b  │  c   │\n"
    "───┼────┼──────┤\n"
    "10 │ 20 │    0 │\n"
    "30 │ 40 │ 1000 │\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => true, outline => [right]}))),
  ?assertEqual(
    "───┬────┬─────\n"
    "a  │ b  │  c  \n"
    "───┼────┼─────\n"
    "10 │ 20 │    0\n"
    "30 │ 40 │ 1000\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => true, outline => [top]}))),
  ?assertEqual(
    "┌────┬────┬──────┐\n"
    "│ a  │ b  │  c   │\n"
    "├────┼────┼──────┤\n"
    "│ 10 │ 20 │    0 │\n"
    "│ 30 │ 40 │ 1000 │\n"
    "└────┴────┴──────┘\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c}, [{10, 20, 0}, {30, 40, 1000}], #{unicode => true, outline => full}))).

pretty_table_ccy_thousands_test() ->
  ?assertEqual(
    " a   |    b     |  c  \n"
    "-----+----------+-----\n"
    "$ 10 | 1,000.00 |    1\n"
    " $ 2 | 1,123.56 | 1000\n"
    "-----+----------+-----\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c},
      [{10, 1000.0, 1}, {2, 1123.56, 1000}],
      #opts{td_dir=both, thousands=",", ccy_sym="$", ccy_sep=" ",
        td_formats=
          fun(a,V,_) when is_integer(V) -> {ccy, 0, V};
             (_,V,_) when is_integer(V) -> {number, integer_to_list(V)};
             (_,V,_) when is_float(V)   -> {number, 2, V} end
          }))
  ).

pretty_table_calc_format_test() ->
  ?assertEqual(
    "a  | b  | c \n"
    "---+----+---\n"
    "10 | 20 | 30\n"
    "30 | 40 | 70\n"
    "---+----+---\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c},
      [{10, 20, 0}, {30, 40, 0}],
      #opts{td_dir=both,
        td_formats={
          undefined,
          undefined,
          fun(c,V,Row) when is_integer(V) -> {number, integer_to_list(lists:sum(tuple_to_list(Row)))};
             (_,V,_)   when is_integer(V) -> {number, integer_to_list(V)} end
        }}))).

pretty_table_map_opts_test() ->
  ?assertEqual(
    " a   |     b     |   c   \n"
    "-----+-----------+-------\n"
    " a   |        10 |     cc\n"
    "bxxx | 200.00123 | 'Done'\n"
    "     | 100.00000 |     xx\n"
    "-----+-----------+-------\n",
    lists:flatten(stringx:pretty_table(
      {a,b,c,d},
      [{a, 10, cc,x1}, {<<"bxxx">>, 200.00123, 'Done',x2}, {nil, 100.0, xx,x3}],
      #{td_dir     => both,
        td_pad     => #{3 => leading},
        td_exclude => [4],
        td_formats => {
          undefined,
          fun(V) when is_integer(V) -> {number, integer_to_list(V)};
             (V) when is_float(V)   -> {number, float_to_list(V, [{decimals, 5}])} end,
          "~w",
          undefined}}))).

format_number_test_() -> [
	?_assertEqual(<<"1.00">>, format_number(1, 2, 2, #{})),
	?_assertEqual(<<"1.99">>, format_number(1.99, 2, 2, #{})),
	?_assertEqual(<<"2.00">>, format_number(1.99, 1, 2, #{})),
	?_assertEqual(<<"1">>,    format_integer(1)),
	?_assertEqual(<<"1,000">>,format_integer(1000, #{thousands => <<",">>})),
	?_assertEqual(<<"2,000,000">>,    format_integer(2000000, #{thousands => <<",">>})),
	?_assertEqual(<<"1 000 999.00">>, format_number(1000999, 2, 2, #{thousands => <<" ">>})),
	?_assertEqual(<<"2,000,000.00">>, format_number(2000000, 2, 2, #{thousands => <<",">>})),
	?_assertEqual(<<"9 999 999 999">>, format_integer(9999999999, #{thousands => <<" ">>})),
	?_assertEqual(<<"9 999 999 999.00">>, format_number(9999999999, 2, 2, #{thousands => <<" ">>})),
	?_assertEqual(<<"99 999 999 999.99">>, format_price(99999999999.99, 2, #{thousands => <<" ">>})),
	?_assertEqual(<<"999 999 999 999.99">>, format_price(999999999999.99, 2, #{thousands => <<" ">>})),
	?_assertEqual(<<"999,999,999,999.99">>, format_price(999999999999.99,  2, #{thousands => <<",">>})),
	?_assertEqual(<<"USD 1,234,567,890==4600">>,
    format_number(1234567890.4567, 2, 4, #{thousands => ",",
      decimal_point => "==", ccy_sym => "USD", ccy_sep => " ", ccy_pos => left})),
	?_assertEqual(<<"$1000.88">>, format_price(1000.8767, 4, "$")),
	?_assertEqual(<<"1000.88 руб."/utf8>>, format_price(1000.8767, 4,
    #{ccy_sym => <<"руб."/utf8>>, ccy_sep => " ", ccy_pos => right})),
	?_assertEqual(<<"1000.88 руб."/utf8>>, format_price(1000.8767, 4,
    #{ccy_sym => "руб.", ccy_sep => " ", ccy_pos => right})),
	?_assertEqual(<<"€€1000.00"/utf8>>, format_price(1000, 4,
    #{ccy_sym => "€", ccy_sep => "€", ccy_pos => left})),
	?_assertEqual(format_number(100, 2, 3), format_number(100, 2, 3, #{})),
	?_assertEqual(format_price(1000), format_price(1000, 2)),
	?_assertEqual(format_price(1000), format_price(1000, 2, <<>>)),
	?_assertEqual(format_price(1000), format_number(1000, 2, 2, #{}))
].

round_number_test_() ->	[
	?_assertEqual(1.0,     round_price(1)),
	?_assertEqual(1.01,    round_price(1.01)),
	?_assertEqual(1.01,    round_price(1.015)),
	?_assertEqual(1.02,    round_price(1.025)),
	?_assertEqual(1.02,    round_price(1.0155)),
	?_assertEqual(1.015,   round_number(1.015, 3)),
	?_assertEqual(2.0,     round_number(1.9999, 1)),
	?_assertEqual(2.0,     round_number(1.9999, 2)),
	?_assertEqual(1.9999,  round_number(1.9999, 4)),
	?_assertEqual(-1.9999, round_number(-1.9999, 4)),
	?_assertEqual(-2.0,    round_number(-1.9999, 3)),
	?_assertEqual(10000.0, round_number(9999.999999, 5))
].
-endif.
