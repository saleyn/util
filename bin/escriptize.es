#!/usr/bin/env escript
%%! -env ERL_CRASH_DUMP /dev/null +sbtu +A0 -mode minimal

%%-------------------------------------------------------------------
%% This script compiles an escript into a binary executable
%%-------------------------------------------------------------------

-record(args, {
  input,
  output
}).

main(Args) ->
  try
    #args{input=In, output=Out} = parse(Args, #args{}),
    In == undefined
      andalso throw("Missing input file name!"),
    Output = case Out of
               undefined -> output_name(In);
               _         -> Out
             end,
    case escript:extract(In, [compile_source]) of
      {ok, S} ->
        is_binary(proplists:get_value(beam, S))
          andalso throw("Script " ++ In ++ " is already compiled!"),
        case escript:create(Output, S) of
          ok ->
            ok = file:change_mode(Output, 8#0755),
            io:format("~s executable file created\n", [Output]);
          {error, {Reason, _}} ->
            throw(io_lib:format(
                    "Cannot create file '~s': ~s", [Output, file:format_error(Reason)]));
          {error, Reason} ->
            throw(io_lib:format(
                    "Cannot create file '~s': ~p", [Output, Reason]))
        end;
      {error, Reason} when is_list(Reason) ->
        throw(io_lib:format(
                "Cannot create file '~s': ~s", [Output, Reason]));
      {error, Reason} ->
        throw(io_lib:format(
                "Cannot create file '~s': ~p", [Output, Reason]))
    end
  catch _:Why ->
    io:format(standard_error, "~s\n", [Why]),
    halt(1)
  end.

parse([],    Args)                          -> Args;
parse(["-f", File|T], Args)                 -> parse(T, Args#args{input=File});
parse(["-o", File|T], Args)                 -> parse(T, Args#args{output=File});
parse([[C|_] = File|T], Args) when C /= $-  -> parse(T, Args#args{input=File});
parse([H|_], _) when H=="-h"; H=="--help"   ->
  io:format(standard_error,
    "Compile source escript to a binary executable\n"
    "Author: Serge Aleynikov <serge(at)gmail(dot)com>\n\n"
    "Usage: ~s [-h|--help] [-o OutputName] [-f] ScriptName\n", [escript:script_name()]),
  halt(1);
parse([H|_], _) ->
  throw("Invalid argument: " ++ H).

output_name(File) ->
  re:replace(File,"\.(es|escript)$",".bin",[{return,list}]).
