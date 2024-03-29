%%% vim:ts=2:sw=2:et

% Options for table pretty printing (see stringx:pretty_print_table/3)
-record(opts, {
  number_pad = $\s      :: char(),     % Padding character for numbers
  header     = true     :: boolean(),  % Output header row
  th_dir     = both     :: both|leading|trailing, % table header padding dir
  td_dir     = trailing :: both|leading|trailing, % table row    padding dir
  td_pad     = #{}      :: map(),      % Map of column padding directions #{Col::integer() => both|leading|trailing}
  td_start   = 1        :: integer(),  % Start printing from this field number
  td_exclude = []       :: list(),     % Exclude columns (start with 1) or names
  td_sep     = " | "    :: string(),   % Column separator
  tr_sep     = "-"      :: string(),
  tr_sep_td  = "+"      :: string(),   % Delimiter header/footer column sep
  prefix     = ""       :: string(),   % Use this prefix in front of each row
  translate,                           % Value translation function `(Val) -> any()`
  footer_rows= 0        :: integer(),  % Number of footer rows
  td_formats    :: undefined|tuple(),  % Optional tuple containing value format for columns
                                       % (each item is either a Fmt string or fun(Value)).
  thousands :: undefined|string()|binary(),% Number thousands separator
  ccy_sym   :: undefined|string()|binary(),% Currency prefix/suffix
  ccy_sep = <<"">>:: string()|binary(),% Currency separator
  ccy_pos = left  :: left|right,       % Currency symbol position
  outline = [bottom]::none|full|[top|bottom|left|right]|map(),% Draw outline box on all sides of a table [top,bottom,left,right]
  unicode = false :: boolean()         % Use unicode symbols for box borders
}).


