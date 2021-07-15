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
-export([max_field_lengths/2, load_to_mysql/4]).

%%------------------------------------------------------------------------------
%% CSV parsing
%%------------------------------------------------------------------------------
-spec parse(string()) -> [[string()]].
parse(File) when is_list(File) ->
  parse(File, []).

%%------------------------------------------------------------------------------
%% @doc Parse a given CSV file.
%% Options:
%% <dl>
%%   <dt>fix_lengths</dt><dd>if a record has a column count
%% greater than what's found in the header row, those extra columns will be
%% dropped, and if a row has fewer columns, empty columns will be added.</dd>
%%   <dt>{open, list()}</dt><dd>Options given to file:open/2</dd>
%% </dl>
%% @end
%%------------------------------------------------------------------------------
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
parse_csv_field_q([C|Line],       Buf, Fields) -> parse_csv_field_q(Line, [C|Buf], Fields);
parse_csv_field_q([],             Buf, Fields) -> parse_line([], [lists:reverse(Buf)|Fields]).

fix_length(HLen, HLen, Data) -> Data;
fix_length(HLen, DLen, Data) when HLen < DLen ->
  {RR, _} = lists:split(HLen, Data),
  RR;
fix_length(HLen, DLen, Data) when HLen > DLen ->
  RR = lists:duplicate(HLen-DLen, ""),
  Data ++ RR.

%%------------------------------------------------------------------------------
%% @doc Get max field lengths for a list obtained by parsing a CSV file with
%%      `parse_csv_file(File,[fix_lengths])'.
%% @end
%%------------------------------------------------------------------------------
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
%% @doc Load CSV data from File to MySQL database
%% @end
%%------------------------------------------------------------------------------
-spec load_to_mysql(File::string(), Tab::string(),
                    MySqlPid::pid(), Opts::[{batch_size, integer()}|
                                            {blob_size,  integer()}|
                                            {save_create_sql_to_file, string()}|
                                            {encoding,   string()|atom()}|
                                            {verbose,    boolean()}]) ->
        {Columns::list(), RecCount::integer()}.
load_to_mysql(File, Tab, MySqlPid, Opts)
    when is_list(File), is_list(Tab), is_pid(MySqlPid), is_list(Opts) ->
  BatSz  = proplists:get_value(batch_size, Opts, 100), % Insert this many records per call
  BlobSz = proplists:get_value(blob_size,  Opts, 1000),% Length at which VARCHAR becomes BLOB
  Enc    = encoding(proplists:get_value(encoding, Opts, undefined)),
  Verbose= proplists:get_value(verbose,    Opts, false),
  CSV0   = parse(File, [fix_lengths]),
  CSV    = [[cleanup_header(S) || S <- hd(CSV0)] | tl(CSV0)],
  MLens  = max_field_lengths(true, CSV),
  HLens  = lists:zip(hd(CSV), MLens),
  TmpTab = Tab ++ "_tmp",
  OldTab = Tab ++ "_OLD",
  CrTab  =  lists:flatten([
              "DROP TABLE IF EXISTS `", TmpTab, "`;\n",
              "CREATE TABLE `", TmpTab, "` (",
                string:join([if
                               I > BlobSz -> io_lib:format("`~s` BLOB", [S]);
                               true       -> io_lib:format("`~s` VARCHAR(~w)", [S,I])
                             end || {S,I} <- HLens], ","),
              ");\n"]),

  case proplists:get_value(save_create_sql_to_file, Opts, false) of
    false ->
      ok;
    true ->
      SQLF  = filename:join(filename:dirname(File), filename:basename(File, ".csv") ++ ".sql"),
      ok    = file:write_file(SQLF, CrTab);
    SQLF when is_list(SQLF) ->
      ok    = file:write_file(SQLF, CrTab)
  end,
 
  Verbose andalso io:format(standard_error, "SQL:\n====\n~s\n", [CrTab]),

  case mysql:query(MySqlPid, CrTab) of
    ok -> ok;
    {error, {Code, _, Msg}} ->
      throw({error_creating_table, Code, binary_to_list(Msg), CrTab})
  end,

  [HD|RRR]  = CSV,
  [_|QQQ0s] = string:copies(",?", length(HD)),
  Heads     = string:join(["`"++S++"`" || S <- HD], ","),
  QQQs      = lists:append([",(", QQQ0s, ")"]),
  Rows      = [if length(I) > 1000 -> list_to_binary(I); true -> I end || I <- RRR],
  BatchRows = stringx:batch_split(BatSz, Rows),

  FstBatLen = length(hd(BatchRows)),
  if Enc   /= [] ->
    Verbose andalso io:format(standard_error, "SQL:\n====\n~s\n", [Enc]),
    ok      = mysql:query(MySqlPid, Enc);
  true ->
    ok
  end,
  PfxSQL    = lists:append(["INSERT INTO ", TmpTab, " (", Heads, ") VALUES "]),
  [_|SfxSQL]= string:copies(QQQs, FstBatLen),
  Verbose andalso io:format(standard_error, "SQL:\n====\n~s~s\n", [PfxSQL, tl(QQQs)]),
  {ok,Ref}  = mysql:prepare(MySqlPid, PfxSQL++SfxSQL),

  lists:foldl(fun(Batch, I) ->
    NRows = length(Batch),
    {SqlRef, Unprepare} =
      if NRows == FstBatLen ->
        {Ref, false};
      true ->
        [_|Sfx2] = string:copies(QQQs, NRows),
        {ok,R}   = mysql:prepare(MySqlPid, PfxSQL++Sfx2),
        {R, true}
      end,
    Row = lists:append(Batch),
    Res = mysql:execute(MySqlPid, SqlRef, Row),
    Unprepare andalso mysql:unprepare(MySqlPid, SqlRef),
    case Res of
      ok ->
        I + NRows;
      {error, {_Code, _, _Msg}} ->
        SQL2 = PfxSQL++tl(QQQs),
        try
          lists:foldl(fun(R, J) ->
            case mysql:query(MySqlPid, SQL2, R) of
              ok ->
                J+1;
              {error, {Code1, _, Msg1}} ->
                throw({error_inserting_records, Code1, binary_to_list(Msg1), {row, J, R}})
            end
          end, I, Batch)
        catch _:E ->
          mysql:unprepare(MySqlPid, Ref),
          throw(E)
        end
    end
  end, 1, BatchRows),

  mysql:unprepare(MySqlPid, Ref),

  SQL = lists:flatten(
          io_lib:format("DROP TABLE IF EXISTS `~s`;\n"
                        "-- Check if the real prod table exists\n"
                        "SELECT count(*) INTO @exists\n"
                        "  FROM information_schema.tables\n"
                        " WHERE table_schema=database() AND\n"
                        "       table_name='~s';\n"
                        "-- If so, atomically rename it to another table,\n"
                        "-- rename temp table to prod table, and drop the old table\n"
                        "-- Otherwise, just atomically rename the temp table into prod\n"
                        "SET @query = IF(@exists>0,\n"
                        "  'RENAME TABLE `~s` TO `~s`, `~s` TO `~s`',\n"
                        "  'RENAME TABLE `~s` TO `~s`');\n"
                        "PREPARE stmt FROM @query;\n"
                        "EXECUTE stmt;\n"
                        "DEALLOCATE PREPARE stmt;\n"
                        "DROP TABLE IF EXISTS `~s`;\n",
                        [OldTab, Tab,
                         Tab, OldTab, TmpTab, Tab,
                         TmpTab, Tab,
                         OldTab])),

  Verbose andalso io:format(standard_error, "SQL:\n====\n~s\n", [SQL]),

  ok = mysql:query(MySqlPid, SQL),

  {HD, length(CSV)-1}.

encoding(undefined) -> [];
encoding(A) when is_atom(A) -> encoding2(atom_to_list(A));
encoding(L) when is_list(L) -> encoding2(L).
encoding2(L) ->
  ["SET NAMES ", L, ";\n", encoding3(L)].
encoding3("utf8"++_) ->
  "SET CHARACTER SET utf8;\n";
encoding3(Other) ->
  ["SET CHARACTER SET ", Other].

cleanup_header([$ |T]) -> [$_|cleanup_header(T)];
cleanup_header([C|T]) when (C >= $a andalso C =< $z);
                           (C >= $A andalso C =< $Z);
                           (C >= $0 andalso C =< $9);
                           (C == $_)
                       -> [C|cleanup_header(T)];
cleanup_header([_|T])  -> cleanup_header(T);
cleanup_header([])     -> [].

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
