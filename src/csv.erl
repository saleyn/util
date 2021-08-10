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
-export([max_field_lengths/2, guess_data_types/2, guess_data_type/1, load_to_mysql/4]).

-type load_options() ::
  [{batch_size, integer()}|
   {blob_size,  integer()}|
   {save_create_sql_to_file, string()}|
   {guess_types,boolean()}|
   {guess_limit_rows, integer()}|
   {max_nulls_pcnt, float()}|
   {primary_key,PKColumns::binary()|[binary()|list()]}|
   {encoding,   string()|atom()}|
   {verbose,    boolean()}].
%% Options for loading data to a database.
%% <dl>
%% <dt>{batch_size, Size}</dt>
%%   <dd>Number of records per SQL insert/update/replace call</dd>
%% <dt>{blob_size, Size}</dt>
%%   <dd>Threshold in number of bytes at which a VARCHAR field is defined as BLOB</dd>
%% <dt>{save_create_sql_to_file, Filename::string()}</dt>
%%   <dd>Save CREATE TABLE sql statement to a file</dd>
%% <dt>guess_types</dt>
%%   <dd>When specified, the function will try to guess the type of data in columns
%%       instead of treating all data as string fields. The possible data typed guessed:
%%       integer, float, date, datetime, number, string</dd>
%% <dt>{guess_limit_rows, Limit}</dt>
%%   <dd>Limit the number of rows for guessing the column data types</dd>
%% <dt>{max_nulls_pcnt, Percent}</dt>
%%   <dd>A percentage threshold of permissible NULLs in a column (0-100), above which
%%       the column data type is forced to be treated as `string'</dd>
%% <dt>{primary_key, Fields}</dt>
%%   <dd>Names of primary key fields in the created table</dd>
%% <dt>{encoding, Encoding}</dt>
%%   <dd>The name of the encoding to use for storing data. For the list of permissible
%%       values [see this
%%       link](https://dev.mysql.com/doc/refman/8.0/en/charset-unicode-sets.html)</dd>
%% <dt>verbose</dt>
%%   <dd>Print additional details to stdout</dd>
%% </dl>
-export_type([load_options/0]).

%%------------------------------------------------------------------------------
%% CSV parsing
%%------------------------------------------------------------------------------
%% @doc Parse a CSV file using default options.
-spec parse(string()) -> [[binary()]].
parse(File) when is_list(File); is_binary(File) ->
  parse(File, []).

%%------------------------------------------------------------------------------
%% @doc Parse a given CSV file.
%% Options:
%% <dl>
%%   <dt>fix_lengths</dt><dd>if a record has a column count
%% greater than what's found in the header row, those extra columns will be
%% dropped, and if a row has fewer columns, empty columns will be added.</dd>
%%   <dt>{open, list()}</dt><dd>Options given to file:open/2</dd>
%%   <dt>{open, list()}</dt><dd>Options given to file:open/2</dd>
%%   <dt>binary</dt><dd>Return fields as binaries (default)</dd>
%%   <dt>list</dt><dd>Return fields as lists</dd>
%% </dl>
%% @end
%%------------------------------------------------------------------------------
-spec parse(binary()|string(), [fix_lengths | binary | list |
                                {open, Opts::list()}]) -> [[string()]].
parse(File, Opts) when is_binary(File) ->
  parse(binary_to_list(File), Opts);
parse(File, Opts) when is_list(File), is_list(Opts) ->
  FileOpts = proplists:get_value(open,   Opts, []),
  Mode     = case proplists:get_value(binary, Opts,  true) andalso
              not proplists:get_value(list,   Opts, false)
             of
               true  -> binary;
               false -> list
             end,
  {ok, F}  = file:open(File, [read, raw, binary]++FileOpts),
  FstLine  = case file:read_line(F) of
               {ok, <<I/utf8, Line/binary>>} when I == 65279 -> %% Utf-8  <<239,187,191,...>>
                 {ok, Line};
               Other ->
                 Other
             end,
  Res      = case parse_csv_file(F, 1, FstLine, []) of
               L when Mode == binary ->
                 L;
               L ->
                 [[binary_to_list(B) || B <- Row] || Row <- L]
             end,
  case lists:member(fix_lengths, Opts) of
    true when hd(Res) == hd(Res) ->  %% Not an empty list
      HLen = length(hd(Res)),
      [fix_length(HLen, length(R), R) || R <- Res];
    _ ->
      Res
  end.

parse_csv_file(F, _, eof, Done) ->
  file:close(F),
  lists:reverse(Done);

parse_csv_file(F, LineNo, {ok, Line}, Done) ->
  parse_csv_file(F, LineNo+1, file:read_line(F), [parse_line(LineNo, Line)|Done]);

parse_csv_file(_F, LineNo, {error, Reason}, _) ->
  throw({error, [{line, LineNo}, {reason, file:format_error(Reason)}]}).

trim_eol(Line) ->
  trim_eol(0, Line).

trim_eol(N, Line) when N < byte_size(Line) ->
  Z = byte_size(Line),
  M = N+1,
  case binary:at(Line, Z-M) of
    C when C == $\r; C == $\n; C == $  ->
      trim_eol(M, Line);
    _ ->
      Z-N
  end;
trim_eol(N, Line) ->
  byte_size(Line) - N.

%% @doc Parse a CSV line
-spec parse_line(binary()) -> list().
parse_line(Line) ->
  parse_line(1, Line).

parse_line(LineNo, Line) ->
  try
    parse_csv_field(0, trim_eol(Line), Line, 0,0, [], false)
  catch E:R:S ->
    erlang:raise(E, {line_parse_error, LineNo, Line, R}, S)
  end.

field(Line, Pos, Len, true = _HasEscDblQuote) ->
  binary:replace(binary:part(Line, Pos, Len), <<"\"\"">>, <<"\"">>, [global]);
field(Line, Pos, Len, _) ->
  binary:part(Line, Pos, Len).

trim(_Line, I, I, _Inc) ->
  I;
trim(Line, I, End, Inc) ->
  case binary:at(Line, I) of
    $  ->
      trim(Line, I+Inc, End, Inc);
    _ ->
      I
  end.

trim(Line, Pos, End) when Pos < End ->
  io:format("Line ~p, pos=~w, end=~w\n", [Line, Pos, End]),
  Front = trim(Line, Pos, End,    1),
  Back  = trim(Line, End, Front, -1),
  {Front, Back+1};
trim(_Line, Pos, End) ->
  {Pos, End+1}.

parse_csv_field(From, To, Line, Pos,Len, Fields, HasEscDblQuote) when From >= To ->
  {Start, End} = trim(Line, Pos, Pos+Len-1),
  lists:reverse([field(Line, Start, End-Start, HasEscDblQuote) | Fields]);
parse_csv_field(From, To, Line, Pos,Len, Fields, HasEscDblQuote) ->
  case Line of
    <<_:From/binary, "\"", _/binary>> ->
      parse_csv_field_q(From+1, To, Line, From+1, 0, Fields, HasEscDblQuote);
    <<_:From/binary, ",", _/binary>> ->
      {Start, End} = trim(Line, Pos, Pos+Len-1),
      parse_csv_field(From+1,To,Line, From+1, 0, [field(Line,Start,End-Start,HasEscDblQuote)|Fields], false);
    _ ->
      parse_csv_field(From+1,To,Line, Pos,Len+1, Fields, HasEscDblQuote)
  end.

parse_csv_field_q(From, To, Line, Pos,Len, Fields, HasEscDblQuote) when From < To ->
  case Line of
    <<_:From/binary, "\"\"", _/binary>> ->
      parse_csv_field_q(From+2, To, Line, Pos, Len+2, Fields, true);
    <<_:From/binary, "\\",   _/binary>> ->
      parse_csv_field_q(From+1, To, Line, Pos, Len+1, Fields, HasEscDblQuote);
    <<_:From/binary, "\"",   _/binary>> ->
      parse_csv_field  (From+1, To, Line, Pos, Len,   Fields, HasEscDblQuote);
    _ ->
      parse_csv_field_q(From+1, To, Line, Pos, Len+1, Fields, HasEscDblQuote)
  end;
parse_csv_field_q(From, To, Line, Pos,Len, Fields, HasEscDblQuote) ->
  parse_csv_field(From, To, Line, Pos,Len, Fields, HasEscDblQuote).

fix_length(HLen, HLen, Data) -> Data;
fix_length(HLen, DLen, Data) when HLen < DLen ->
  {RR, _} = lists:split(HLen, Data),
  RR;
fix_length(HLen, DLen, Data) when HLen > DLen ->
  Value = if is_binary(hd(Data)) -> <<"">>; true -> "" end,
  RR    = lists:duplicate(HLen-DLen, Value),
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
  F     = fun(Row) -> [if is_binary(I) -> byte_size(I); true -> length(I) end || I <- Row] end,
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
%% @doc Guess data types of fields in the given CSV list of rows obtained by
%%      parsing a CSV file with `parse(File,[fix_lengths])'.
%%      The function returns a list of tuples `{Type, MaxFieldLen, NumOfNulls}',
%%      where the `Type' is a field type, `MaxFieldLen' is the max length of
%%      data in this column, and `NumOfNulls' is the number of rows with empty
%%      values in this column.
%% @end
%%------------------------------------------------------------------------------
-spec guess_data_types(HasHeaderRow::boolean(), Rows::[Fields::[binary()]]) ->
        {Type::string|integer|number|float|date|datetime,
         MaxFieldLen::integer(),
         NumOfNulls::integer()}.
guess_data_types(HasHeaders, CSV) when is_boolean(HasHeaders), is_list(CSV) ->
  guess_data_types(HasHeaders, CSV, 20.0, 1_000_000).

-spec guess_data_types(HasHeaderRow::boolean(), Rows::[Fields::list()], number(), integer()) ->
        {[{FieldType::string|date|datetime|integer|float|number,
           MaxFieldLength::integer(),
           NullsCount::integer()}],
         [Row::[term()]]}.
guess_data_types(true = _HasHeaders, [Headers | Rows ] = _CSV, NullsMaxPcnt, SniffRows) ->
  {TpLenNulls, DataRows} = guess_data_types(false, Rows, NullsMaxPcnt, SniffRows),
  {TpLenNulls, [Headers | DataRows]};
guess_data_types(false = _HasHeaders, CsvRows, NullsMaxPcnt, SniffRows)
    when is_number(NullsMaxPcnt), NullsMaxPcnt >= 0.0, NullsMaxPcnt =< 100.0
       , is_integer(SniffRows) ->
  %   CSV -> [[R1Field0, ..., R1FieldN], ..., [RnField0, RnFieldN]]
  NRows = length(CsvRows),
  F     = fun(UseString, Row) ->
            [guess_data_type2(I,UseString) || I <- Row]
          end,
  CsvL  = [F(I > SniffRows, Row) || {I, Row} <- lists:zip(lists:seq(1,NRows), CsvRows)],
  Res   = scan_column(CsvL, NullsMaxPcnt, NRows),
  Data  = [ [V || {_,V,_} <- R] || R <- CsvL],
  {Res, Data}.

sort_fun(A,A)    -> true;
sort_fun(null,_) -> true;
sort_fun(_,null) -> false;
sort_fun(A,B)    -> A =< B.

scan_column([], _NullsMaxPcnt, _NRows) ->
  [];
scan_column(L, NullsMaxPcnt, NRows) when is_number(NullsMaxPcnt) ->
  scan_column(L, NRows, NullsMaxPcnt, []).

scan_column([[]|_], _NRows, _NullsMaxPcnt, Acc) ->
  lists:reverse(Acc);
scan_column(L, NRows, NullsMaxPcnt, Acc) ->
  {_Type, _MLen, _Nulls} = Res = scan_mlt(L, NRows, NullsMaxPcnt),
  Acc1 = [Res | Acc],
  case L of
    [[_|_]|_] ->
      scan_column([tl(I) || I <- L], NRows, NullsMaxPcnt, Acc1);
    _ ->
      scan_column([[]], NRows, NullsMaxPcnt, Acc1)
  end.

scan_mlt(L, NRows, NullsMaxPcnt) when is_number(NullsMaxPcnt) ->
  {CTypes, MLen, Nulls, _} = scan_mlt2(L, {#{}, 0, 0, false}),
  PcntNulls = if NRows == 0 -> 100; true -> Nulls / NRows * 100 end,
  Fun = fun
          ([T]) when T /= null -> T;
          ([date,datetime])    -> datetime; 
          ([float,integer])    -> number;
          (_)                  -> string
        end,
  Type =
    case lists:sort(fun sort_fun/2, maps:keys(CTypes)) of
      [null|T] when PcntNulls =< NullsMaxPcnt ->
        Fun(T);
      T ->
        Fun(T)
    end,
  {Type, MLen, Nulls}.

scan_mlt2([[{I,_,V} | _] | T], {ATypes, MLen, Nulls, HasStrings}) ->
  case I of
    null ->
      scan_mlt2(T, {ATypes#{null => 1}, MLen, Nulls+1, HasStrings});
    string when HasStrings ->
      scan_mlt2(T, {ATypes, max(byte_size(V),MLen), Nulls, true});
    string ->
      scan_mlt2(T, {ATypes#{string => 1}, max(byte_size(V),MLen), Nulls, true});
    Type ->
      scan_mlt2(T, {ATypes#{Type => 1}, max(byte_size(V),MLen), Nulls, HasStrings})
  end;
scan_mlt2([[]|_], Acc) ->
  Acc;
scan_mlt2([], Acc) ->
  Acc.

%%------------------------------------------------------------------------------
%% @doc Load CSV data from a `File' to a `MySQL' database.
%% `Tab' is the name of a table where to load data.  `MySqlPid' is the pid of a
%% MySQL database connection returned by `mysql:start_link/1'.
%% The data from the file is loaded atomically - i.e. either the whole file
%% loading succeeds or fails.  This is accomplished by first loading data to a
%% temporary table, and then using the database's ACID properties to replace the
%% target table with the temporary table.
%%
%% Presently the table is entirely replaced with the data from file.
%%
%% NOTE: this function requires [https://github.com/mysql-otp/mysql-otp.git]
%% @end
%%------------------------------------------------------------------------------
-spec load_to_mysql(File::string(), Tab::string(),
                    MySqlPid::pid(), Opts::load_options()) ->
        {Columns::list(), RecCount::integer()}.
load_to_mysql(File, Tab, MySqlPid, Opts)
    when is_list(File), is_list(Tab), is_pid(MySqlPid), is_list(Opts) ->
  BatSz  = proplists:get_value(batch_size, Opts, 100), % Insert this many records per call
  BlobSz = proplists:get_value(blob_size,  Opts, 1000),% Length at which VARCHAR becomes BLOB
  Enc    = encoding(proplists:get_value(encoding, Opts, undefined)),
  Verbose= proplists:get_value(verbose,    Opts, false),
  PKey   = case proplists:get_value(primary_key,  Opts, undefined) of
             K when is_binary(K) ->
               [K];
             [H|_] = K when is_binary(H); is_list(H) ->
               [to_binary(I) || I <- K];
             undefined ->
               [];
             [] ->
               [];
             Other ->
               throw({badarg, {primary_key, Other}})
           end,
  CSV0   = parse(File, [fix_lengths, binary]),
  ColCnt = length(hd(CSV0)),
  CSV1   = [[list_to_binary(cleanup_header(to_string(S))) || S <- hd(CSV0)] | tl(CSV0)],
  Heads  = hd(CSV1),
  PKey  /= [] andalso
     lists:foreach(fun(K) ->
                     lists:member(K, Heads) orelse throw({primary_key_not_found, K, Heads})
                   end, PKey),
  Merge  = fun M([],     [])                        -> [];
               M([H|T1], [{T,I,J}|T2])              -> [{H,T,I,J}|M(T1,T2)];
               M([H|T1], [I|T2]) when is_integer(I) -> [{H,string,I,0}|M(T1,T2)]
           end,
  {ColNmTpMxLens, CSV} =
    case proplists:get_value(guess_types, Opts, false) of
      true ->
        SniffRows    = proplists:get_value(guess_limit_rows, Opts, 1_000_000),
        MaxNullsPcnt = proplists:get_value(max_nulls_pcnt,   Opts, 40.0),
        {TLN, CSV2}  = guess_data_types(true, CSV1, MaxNullsPcnt, SniffRows),
        {Merge(Heads, TLN), CSV2};
      false ->
        {Merge(Heads, max_field_lengths(true, CSV1)), CSV1}
    end,
  Verbose andalso
    io:format(standard_error, "Columns:\n  ~p\n", [ColNmTpMxLens]),
  TmpTab = Tab ++ "_tmp",
  OldTab = Tab ++ "_OLD",
  CrTab  =  lists:flatten([
              "DROP TABLE IF EXISTS `", TmpTab, "`;\n",
              "CREATE TABLE `", TmpTab, "` (",
                string:join([case T of
                               _ when I > BlobSz -> io_lib:format("`~s` BLOB",     [to_string(S)]);
                               date              -> io_lib:format("`~s` DATE",     [to_string(S)]);
                               datetime          -> io_lib:format("`~s` DATETIME", [to_string(S)]);
                               integer           -> io_lib:format("`~s` BIGINT",   [to_string(S)]);
                               float             -> io_lib:format("`~s` DOUBLE",   [to_string(S)]);
                               number            -> io_lib:format("`~s` DOUBLE",   [to_string(S)]);
                               _                 -> io_lib:format("`~s` VARCHAR(~w)", [to_string(S),I])
                             end || {S,T,I,_} <- ColNmTpMxLens], ","),
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

  [HD|Rows] = CSV,
  [_|QQQ0s] = string:copies(",?", ColCnt),
  HHHs      = string:join(["`"++binary_to_list(S)++"`" || S <- HD], ","),
  QQQs      = lists:append([",(", QQQ0s, ")"]),
  BatchRows = stringx:batch_split(BatSz, Rows),

  FstBatLen = length(hd(BatchRows)),
  if Enc   /= [] ->
    Verbose andalso io:format(standard_error, "SQL:\n====\n~s\n", [Enc]),
    ok      = mysql:query(MySqlPid, Enc);
  true ->
    ok
  end,
  PfxSQL    = lists:append(["INSERT INTO ", TmpTab, " (", HHHs, ") VALUES "]),
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

  PKSql = case PKey of
            [] -> "";
            _  -> SS = string:join(["`"++binary_to_list(S)++"`" || S <- PKey], ","),
                  io_lib:format("ALTER TABLE `~s` ADD PRIMARY KEY (~s);\n", [TmpTab, SS])
          end,
  SQL = lists:flatten(
          io_lib:format("DROP TABLE IF EXISTS `~s`;\n"
                        "~s"
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
                        [OldTab, PKSql,  Tab,
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

to_string(L) when is_binary(L) -> binary_to_list(L);
to_string(L) when is_list(L)   -> L.

to_binary(I) when is_binary(I) -> I;
to_binary(L) when is_list(L)   -> list_to_binary(L).

cleanup_header([$ |T]) -> [$_|cleanup_header(T)];
cleanup_header([C|T]) when (C >= $a andalso C =< $z);
                           (C >= $A andalso C =< $Z);
                           (C >= $0 andalso C =< $9);
                           (C == $_)
                       -> [C|cleanup_header(T)];
cleanup_header([_|T])  -> cleanup_header(T);
cleanup_header([])     -> [].

%% @doc Guess the type of data by its value
-spec guess_data_type(binary()) ->
  {null | date | datetime | integer | float | string, term(), string()}.
guess_data_type(S) ->
  guess_data_type2(S, false).

guess_data_type2(<<"">>, _) ->
  {null, null, <<"">>};
guess_data_type2(S, true) ->
  {string, S, S};
guess_data_type2(S, _) ->
  guess_data_type3(S).

guess_data_type3(<<C, _/binary>> = S) when C >= $0, C =< $9
                                         ; C == $-; C == $+ ->
  try
    I = binary_to_integer(S),
    %case binary_to_integer(S) of
    %  I when I < -2147483648; I > 2147483647 ->
    %    {integer, I, S};
    %  I ->
    %    {integer, I, S}
    %end;
    {integer, I, S}
  catch _:_ ->
    try F = binary_to_float(S), {float, F, S} catch _:_ ->
      guess_data_type4(S)
    end
  end;
guess_data_type3(S) ->
  guess_data_type4(S).

guess_data_type4(<<Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2, _/binary>> = V)
  when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9, Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9
     , M1 >= $0, M1 =< $9, M2 >= $0, M2 =< $9
     , D1 >= $0, D1 =< $9, D2 >= $0, D2 =< $9
     ->
  Y = i(Y1)*1000 + i(Y2)*100 + i(Y3)*10 + i(Y4),
  M = i(M1)*10   + i(M2),
  D = i(D1)*10   + i(D2),
  if Y < 1000; Y > 2500; M < 1; M > 12; D < 1; D > 31 ->
    def_type(V);
  true ->
    case V of
      <<_:10/binary, C,H1,H2,$:,N1,N2,$:,S1,S2, _/binary>>
        when (C == $  orelse C == $T)
           , H1 >= $0, H1 =< $9, H2 >= $0, H2 =< $9
           , N1 >= $0, N1 =< $9, N2 >= $0, N2 =< $9
           , S1 >= $0, S1 =< $9, S2 >= $0, S2 =< $9 ->
        H = i(H1)*10 + i(H2),
        N = i(N1)*10 + i(N2),
        S = i(S1)*10 + i(S2),
        if H < 0; H > 23; N < 0; N > 59; S < 0; S > 59 ->
          def_type(V);
        true ->
          {datetime, {{Y,M,D},{H,N,S}}, V}
        end;
      _ when byte_size(V) == 10 ->
        {date, {Y,M,D}, V};
      _ ->
        def_type(V)
    end
  end;
guess_data_type4(S) ->
  def_type(S).

def_type(V) ->
  {string, V, V}.

i(C) -> C - $0.

%%------------------------------------------------------------------------------
%% Tests
%%------------------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
  ?assertEqual([<<>>],            parse_line(<<"\n">>)),
  ?assertEqual([<<>>],            parse_line(<<"\r\n">>)),
  ?assertEqual([<<>>],            parse_line(<<"\n\r">>)),
  ?assertEqual([<<>>],            parse_line(<<"\n\r\n">>)),
  ?assertEqual([<<"a">>],         parse_line(<<"a\r\n">>)),
  ?assertEqual([<<>>,<<>>],       parse_line(<<",\r\n">>)),
  ?assertEqual([<<>>,<<>>],       parse_line(<<",">>)),
  ?assertEqual([<<>>,<<"a">>],    parse_line(<<",a">>)),
  ?assertEqual([<<"a">>,<<"b">>], parse_line(<<"a , b">>)),
  ?assertEqual([<<"a">>,<<"b">>], parse_line(<<" a , b ">>)),
  ?assertEqual([<<"a">>,<<"b">>], parse_line(<<" a, b ">>)),
  ?assertEqual([<<"a">>,<<"b">>], parse_line(<<" a,b ">>)),
  ?assertEqual([<<"a">>,<<"b">>], parse_line(<<" a, \"b\" ">>)),
  ?assertEqual([<<"a">>,<<"b">>], parse_line(<<" a , b \r\n">>)),
  ?assertEqual([<<"a">>,<<"b \r\nbb">>,<<"c">>],parse_line(<<"\"a\",\"b \r\nbb\",\"c\"\r\n">>)),
  ?assertEqual([<<"a\"b">>,<<"c\"">>],          parse_line(<<"\"a\"\"b\",\"c\"\"\"">>)),
  ?assertEqual([<<"a,b">>],                     parse_line(<<"\"a\,b\"">>)),
  ?assertEqual([<<"a, b">>],                    parse_line(<<"\"a\, b\"">>)),
  Lines = [<<"a,bb,ccc">>,
           <<",b,c">>,
           <<",b,\"c,d\"">>,
           <<"\"c,d\"\r">>,
           <<"xx,yyy,zzzz">>,
           <<"xxx,yyyy,zzzzz">>,
           <<"a,b\n">>,
           <<"x,y\r">>,
           <<"z\r\n">>],
  CSV   = [parse_line(L) || L <- Lines],
  Res   = [[<<"a">>,   <<"bb">>,   <<"ccc">>],
           [<<"">>,    <<"b">>,    <<"c">>],
           [<<"">>,    <<"b">>,    <<"c,d">>],
           [<<"c,d">>],
           [<<"xx">>,  <<"yyy">>,  <<"zzzz">>],
           [<<"xxx">>, <<"yyyy">>, <<"zzzzz">>],
           [<<"a">>,<<"b">>],
           [<<"x">>,<<"y">>],
           [<<"z">>]
          ],
  ?assertEqual(Res, CSV).

max_lens_test() ->
  Lines = [[<<"a">>,   <<"bb">>,   <<"ccc">>],
           [<<"xx">>,  <<"yyy">>,  <<"zzzz">>],
           [<<"xxx">>, <<"yyyy">>, <<"zzzzz">>]],
  Res   = max_field_lengths(true, Lines),
  ?assertEqual([3,4,5], Res).

guess_type_test() ->
  ?assertEqual({integer, 1,   <<"1">>},   guess_data_type(<<"1">>)),
  ?assertEqual({float,   1.0, <<"1.0">>}, guess_data_type(<<"1.0">>)),
  ?assertEqual({date,    {2021,1,1}, <<"2021-01-01">>},                          guess_data_type(<<"2021-01-01">>)),
  ?assertEqual({datetime,{{2021,1,1},{0,0,0}}, <<"2021-01-01 00:00:00">>},       guess_data_type(<<"2021-01-01 00:00:00">>)),
  ?assertEqual({datetime,{{2021,1,1},{0,0,0}}, <<"2021-01-01 00:00:00+01:00">>}, guess_data_type(<<"2021-01-01 00:00:00+01:00">>)),
  ?assertEqual({datetime,{{2021,1,1},{0,0,0}}, <<"2021-01-01 00:00:00-01:00">>}, guess_data_type(<<"2021-01-01 00:00:00-01:00">>)),
  ?assertEqual({string,  <<"abc">>, <<"abc">>}, guess_data_type(<<"abc">>)),
  ?assertEqual({null,    null,  <<"">>},        guess_data_type(<<"">>)).

col_types_test() ->
  LineS = [["a", "b",   "c",   "d",          "e",                  "f",   "g", "h",  "i"],
           ["1", "1.0", "1.0", "2021-01-02", "2021-01-02",         "abc", "1", "1.0","2021-01-02"],
           ["1", "2.0", "3.0", "2021-03-02", "2021-03-02",         "abc", "1", "3.0","2023-01-02"],
           ["1", "2.0", "3.0", "2021-03-02", "2021-03-02",         "abc", "1", "3.0","2023-01-02"],
           ["1", "2.0", "3.0", "2021-03-02", "2021-03-02",         "abc", "1", "3.0","2023-01-02"],
           ["2", "2",   "2.0", "2021-02-03", "2021-01-02 00:01:02","efg", "",  "",   ""]],
  Lines = [[list_to_binary(I) || I <- Row] || Row <- LineS],
  Res   = guess_data_types(true, Lines),
  ?assertEqual({[{integer,  1,0},
                 {number,   3,0},
                 {float,    3,0},
                 {date,    10,0},
                 {datetime,19,0},
                 {string,   3,0},
                 {integer,  1,1},
                 {float,    3,1},
                 {date,    10,1}],
                [
                  [<<"a">>,<<"b">>,<<"c">>,<<"d">>,<<"e">>,<<"f">>,<<"g">>,<<"h">>,<<"i">>],
                  [1,1.0,1.0,{2021,01,02},{2021,01,02},           <<"abc">>, 1, 1.0, {2021,01,02}],
                  [1,2.0,3.0,{2021,03,02},{2021,03,02},           <<"abc">>, 1, 3.0, {2023,01,02}],
                  [1,2.0,3.0,{2021,03,02},{2021,03,02},           <<"abc">>, 1, 3.0, {2023,01,02}],
                  [1,2.0,3.0,{2021,03,02},{2021,03,02},           <<"abc">>, 1, 3.0, {2023,01,02}],
                  [2,2,  2.0,{2021,02,03},{{2021,01,02},{0,1,2}}, <<"efg">>, null, null, null]
                ]},
               Res),
  Res2 = guess_data_types(true, Lines, 10.0, 5),
  ?assertEqual({[{integer,  1,0},
                 {number,   3,0},
                 {float,    3,0},
                 {date,    10,0},
                 {datetime,19,0},
                 {string,   3,0},
                 {string,   1,1},
                 {string,   3,1},
                 {string,  10,1}],
                [
                  [<<"a">>,<<"b">>,<<"c">>,<<"d">>,<<"e">>,<<"f">>,<<"g">>,<<"h">>,<<"i">>],
                  [1,1.0,1.0,{2021,01,02},{2021,01,02},           <<"abc">>, 1, 1.0, {2021,01,02}],
                  [1,2.0,3.0,{2021,03,02},{2021,03,02},           <<"abc">>, 1, 3.0, {2023,01,02}],
                  [1,2.0,3.0,{2021,03,02},{2021,03,02},           <<"abc">>, 1, 3.0, {2023,01,02}],
                  [1,2.0,3.0,{2021,03,02},{2021,03,02},           <<"abc">>, 1, 3.0, {2023,01,02}],
                  [2,2,  2.0,{2021,02,03},{{2021,01,02},{0,1,2}}, <<"efg">>, null, null, null]
                ]},
               Res2).

-endif.
