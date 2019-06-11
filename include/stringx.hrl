%%% vim:ts=2:sw=2:et

% Options for table pretty printing (see stringx:pretty_print_table/3)
-record(opts, {
  number_pad = $\s      :: char(),    % Padding character for numbers
  out_header = true     :: boolean(), % Output header row
  out_sep    = true     :: boolean(), % Output separator rows
  th_dir     = both     :: both|leading|trailing, % table header padding dir
  td_dir     = trailing :: both|leading|trailing, % table row    padding dir
  td_start   = 1        :: integer(), % Start printing from this field number
  td_exclude = []       :: list(),    % Exclude columns (start with 1) or names
  td_sep     = " | "    :: string(),  % Column separator
  tr_sep     = "-"      :: string(),
  tr_sep_td  = "+"      :: string(),  % Delimiter header/footer column sep
  prefix     = ""       :: string(),  % Use this prefix in front of each row
  translate,                          % Value translation function `(Val) -> any()`
  footer_rows= 0        :: integer(), % Number of footer rows
  td_formats :: tuple() % Optional tuple containing value format for columns
                        % (each item is either a Fmt string or fun(Value)).
}).


