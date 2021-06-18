%%% vim:ts=2:sw=2:et
%%%-----------------------------------------------------------------------------
%%% @doc CSV file parsing functions
%%%
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2021 Serge Aleynikov
%%% @end
%%%-----------------------------------------------------------------------------
%%% Created 2021-06-01
%%%-----------------------------------------------------------------------------
-module(csv).
-author('saleyn@gmail.com').

-export([parse/1, parse/2, parse_line/1]).
-export([max_field_lengths/2]).

%%------------------------------------------------------------------------------
%% CSV parsing
%%------------------------------------------------------------------------------
-spec parse(string()) -> [[string()]].
parse(File) when is_list(File) ->
  parse(File, []).

%% @doc Parse a given CSV file.
%% Options:
%% <dl>
%%   <dt>fix_lengths</dt><dd>if a record has a column count
%% greater than what's found in the header row, those extra columns will be
%% dropped, and if a row has fewer columns, empty columns will be added.</dd>
%%   <dt>{open, list()}</dt><dd>Options given to file:open/2</dd>
%% </dl>
-spec parse(string(), [fix_lengths | {open, Opts::list()}]) -> [[string()]].
parse(File, Opts) when is_list(File), is_list(Opts) ->
  FileOpts = proplists:get_value(open, Opts, []),
  {ok, F}  = file:open(File, [read, raw]++FileOpts),
  Res      = parse_csv_file(F, 1, file:read_line(F), []),
  case lists:member(fix_lengths, Opts) of
    true when hd(Res) == hd(Res) ->  %% Not an empty list
      HLen = length(hd(Res)),
      [fix_length(HLen, length(R), R) || R <- Res];
    false ->
      Res
  end.

parse_csv_file(F, _, eof, Done) ->
  file:close(F),
  lists:reverse(Done);

parse_csv_file(F, LineNo, {ok, Line}, Done) ->
  parse_csv_file(F, LineNo+1, file:read_line(F), [parse_line(string:trim(Line, trailing, "\r\n"))|Done]);

parse_csv_file(_F, LineNo, {error, Reason}, _) ->
  throw({error, [{line, LineNo}, {reason, file:format_error(Reason)}]}).

parse_line(Line) -> parse_line(Line, []).

parse_line([],          Fields) -> lists:reverse(Fields);
parse_line("," ++ Line, Fields) -> parse_csv_field(Line, Fields);
parse_line(Line,        Fields) -> parse_csv_field(Line, Fields).

parse_csv_field("\"" ++ Line, Fields) -> parse_csv_field_q(Line, Fields);
parse_csv_field(Line, Fields) -> parse_csv_field(Line, [], Fields).

parse_csv_field(","   ++ _ = Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_csv_field([C|Line],          Buf, Fields) -> parse_csv_field(Line, [C|Buf], Fields);
parse_csv_field([],                Buf, Fields) -> parse_line([], [lists:reverse(Buf)|Fields]).

parse_csv_field_q(Line,                Fields) -> parse_csv_field_q(Line, [], Fields).
parse_csv_field_q("\"\"" ++ Line, Buf, Fields) -> parse_csv_field_q(Line, [$"|Buf], Fields);
parse_csv_field_q("\\"   ++ Line, Buf, Fields) -> parse_csv_field_q(Line, [$\\|Buf], Fields);
parse_csv_field_q("\""   ++ Line, Buf, Fields) -> parse_line(Line, [lists:reverse(Buf)|Fields]);
parse_csv_field_q([C|Line],       Buf, Fields) -> parse_csv_field_q(Line, [C|Buf], Fields).

fix_length(HLen, HLen, Data) -> Data;
fix_length(HLen, DLen, Data) when HLen < DLen ->
  {RR, _} = lists:split(HLen, Data),
  RR;
fix_length(HLen, DLen, Data) when HLen > DLen ->
  RR = lists:duplicate(HLen-DLen, ""),
  Data ++ RR.

%% Get max field lengths for a list obtained by parsing a CSV file with `parse_csv_file(File,[fix_lengths])'.
-spec max_field_lengths(HasHeaderRow::boolean(), Rows::[Fields::list()]) -> [Len::integer()].
max_field_lengths(true = _HasHeaders, [_Headers | Rows ] = _CSV) ->
  max_field_lengths(false, Rows);
max_field_lengths(false = _HasHeaders, CsvRows) ->
  % Calculate length of a field in a row:
  %   CSV -> [[R1FieldLen0, ..., R1FieldLenN], ...,
  %           [RnFieldLen0, RnFieldLenN]]
  F     = fun(Row) -> [length(I) || I <- Row] end,
  CsvL  = [F(Row) || Row <- CsvRows],
  MLens =
    fun
      G([[] | _], Acc) ->
        lists:reverse(Acc);
      G(RRows, Acc) ->
        {MaxLen, F1Nrows} = lists:foldl(fun([H|T], {A,L}) -> {max(H,A),[T|L]} end, {0,[]}, RRows),
        G(F1Nrows, [MaxLen | Acc])
    end,
  MLens(CsvL, []).

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
  Lines = ["a,bb,ccc",
           "xx,yyy,zzzz",
           "xxx,yyyy,zzzzz"],
  CSV   = [parse_line(L) || L <- Lines],
  Res   = [["a",   "bb",   "ccc"],
           ["xx",  "yyy",  "zzzz"],
           ["xxx", "yyyy", "zzzzz"]],
  ?assertEqual(Res, CSV).

max_lens_test() ->
  Lines = [["a",   "bb",   "ccc"],
           ["xx",  "yyy",  "zzzz"],
           ["xxx", "yyyy", "zzzzz"]],
  Res   = max_field_lengths(true, Lines),
  ?assertEqual([3,4,5], Res).

-endif.
