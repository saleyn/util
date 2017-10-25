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

% Options for table pretty printing
-record(opts, {
  number_pad = $\s,     % padding character for numbers
  th_dir     = both     :: both|leading|trailing, % table header padding dir
  td_dir     = trailing :: both|leading|trailing, % table row    padding dir
  start_col  = 1,       % Start printing from this field (use 2 for records)
  c_sep      = " | ",   % Column separator
  d_sep      = "+",     % Delimiter header/footer column separator
  r_sep      = "-",
  prefix     = ""       % Use this prefix in front of each row
}).

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
%% @doc Pretty print table
%% @end
%%-------------------------------------------------------------------------
pretty_table([Map|_] = LofMaps0) when is_map(Map) ->
  pretty_table(lists:sort(maps:keys(Map)), LofMaps0, #opts{}).

pretty_table(HeaderRowKeys, Rows) ->
  pretty_table(HeaderRowKeys, Rows, #opts{}).

pretty_print_table([Map|_] = LofMaps0) when is_map(Map) ->
  io:put_chars(pretty_table(lists:sort(maps:keys(Map)), LofMaps0, #opts{})).

pretty_print_table(HeaderRowKeys, Rows) ->
  io:put_chars(pretty_table(HeaderRowKeys, Rows, #opts{})).

pretty_print_table(HeaderRowKeys, Rows, Opts) ->
  io:put_chars(pretty_table(HeaderRowKeys, Rows, Opts)).

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

pretty_table(Keys0, Rows0, #opts{} = Opts) when is_tuple(Keys0) ->
  pretty_table(tuple_to_list(Keys0), Rows0, Opts);
pretty_table(Keys0, Rows0, #opts{} = Opts) when is_list(Keys0), is_list(Rows0) ->
  KeyStrs = take_nth(Opts#opts.start_col, [element(2, to_string(Key)) || Key <- Keys0]),
  Rows    = [take_nth(Opts#opts.start_col, to_strings(Keys0, V))      || V   <- Rows0],
  Ws      = ws(Rows, [{undefined, string:length(Key)} || Key <- KeyStrs]),
  Col     = fun({_,Str}, {Type, Width}) ->
              {Dir,Pad} = case Type of
                            number -> {leading, Opts#opts.number_pad};
                            _      -> {Opts#opts.td_dir, $\s}
                          end,
              string:pad(Str, Width, Dir, Pad)
            end,
  AddSpLn = length([C || C <- Opts#opts.c_sep, C == $\s]),
  AddSpH  = string:copies(Opts#opts.r_sep, AddSpLn div 2),
  AddSpT  = string:copies(Opts#opts.r_sep, AddSpLn - (AddSpLn div 2)),
  Row     = fun(Row) ->
              R0 = [Col(Str, W) || {Str,W} <- lists:zip(Row, Ws)],
              [Opts#opts.prefix, lists:join(Opts#opts.c_sep, R0)]
            end,
  Header0 = [{string:pad(Str, W, Opts#opts.th_dir), string:copies(Opts#opts.r_sep, W)}
              || {Str,{_,W}} <- lists:zip(KeyStrs, Ws)],
  Header  = lists:join(Opts#opts.c_sep, [H || {H,_} <- Header0]),
  Delim   = lists:join(AddSpH ++ [Opts#opts.d_sep] ++ AddSpT, [T || {_,T} <- Header0]),
  [Opts#opts.prefix, Header, $\n, Delim, $\n, lists:join($\n, [Row(R) || R <- Rows]), $\n, Delim, $\n].

take_nth(I, L) when I < 2 -> L;
take_nth(_,[])            -> [];
take_nth(I,[_|T])         -> take_nth(I-1, T).

ws([Row|Rs], Ws)   -> ws(Rs, ws_1(Row, Ws));
ws([], Ws)         -> Ws.

ws_1([{T0,V}|Vs], [{Type,Max}|Ms]) ->
  [{type(T0,Type),max(string:length(V),Max)}|ws_1(Vs,Ms)];
ws_1([], []) -> [].

type(T, undefined) -> T;
type(T, T)         -> T;
type(_, _)         -> string.

to_strings(Keys, Map) when is_map(Map) ->
  [to_string(maps:get(Key, Map, undefined)) || Key <- Keys];
to_strings(_Keys, List) when is_list(List) ->
  [to_string(Entry) || Entry <- List];
to_strings(_Keys, Tuple) when is_tuple(Tuple) ->
  [to_string(Entry) || Entry <- tuple_to_list(Tuple)].

to_string(Int)   when is_integer(Int) -> {number, integer_to_list(Int)};
to_string(Float) when is_float(Float) -> {number, io_lib:format("~.4f",[Float])};
to_string(Str)   when is_list(Str)    -> {string, Str};
to_string(Bin)   when is_binary(Bin)  -> {string, Bin};
to_string(T)                          -> {string, io_lib:format("~tp", [T])}.
