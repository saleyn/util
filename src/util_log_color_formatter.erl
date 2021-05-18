%% @doc
%% Implementation of coloring handler for the Erlang's logger.
%% @see https://github.com/hauleth/logger_colorful
%%
%% == Options ==
%%
%% <ul>
%%      <li>`formatter' - parent formatter which will be used for message
%%      formatting</li>
%%      <li>`colors' - map of `logger:log_level()' and colors in form atom
%%      describing color name, or 2-ary or 3-ary tuple with 1 or 2 modifiers for
%%      the color</li>
%% </ul>
%%
%% Available colors:
%%
%% <ul>
%%      <li>`black'</li>
%%      <li>`red'</li>
%%      <li>`green'</li>
%%      <li>`yellow'</li>
%%      <li>`blue'</li>
%%      <li>`magenta'</li>
%%      <li>`cyan'</li>
%%      <li>`white'</li>
%% </ul>
%%
%% Available modifiers:
%%
%% <ul>
%%      <li>`fg' - set foreground color</li>
%%      <li>`bg' - set background color</li>
%%      <li>`bright' - use bright color</li>
%% </ul>
%%
%% @end
-module(util_log_color_formatter).

-define(RESET, "\e[m").
-define(CLREOL, "\e[K").
-define(CSI(Code), ["\e[", integer_to_binary(Code), $m]).

-export([check_config/1,
         format/2]).

%% @hidden
check_config(Config0) ->
    Formatter = maps:get(formatter, Config0, logger_formatter),
    Colors = maps:get(colors, Config0, #{}),
    case check_colors(Colors) of
        true ->
            Config = maps:without([formatter, colors], Config0),
            try_callback_call(Formatter, ?FUNCTION_NAME, [Config], ok);
        _ ->
            {error, invalid_colors}
    end.

check_colors(Colors) when is_map(Colors) ->
    lists:all(fun check_value/1, maps:to_list(Colors));
check_colors(_Colors) -> false.

check_value({Level, Color}) ->
    lists:member(Level, [emergency, alert, critical, error, warning, notice, info, debug])
    andalso is_color(Color).

-define(color(C), lists:member(C, [black, red, green, yellow, blue, magenta, cyan, white])).
-define(mod(C), lists:member(C, [fg, bg, bright])).

is_color(normal) -> true;
is_color({Mod1, Mod2, Color}) ->
    ?mod(Mod1) andalso ?mod(Mod2) andalso ?color(Color);
is_color({Mod, Color}) ->
    ?mod(Mod) andalso ?color(Color);
is_color(Color) ->
    ?color(Color).

%% @hidden
format(#{level := Level} = Event, Config0) ->
    Formatter = maps:get(formatter, Config0, logger_formatter),
    Color = color(Level, Config0),
    Config = maps:without([formatter, colors], Config0),
    Formatted = Formatter:format(Event, Config),
    {Leading, Trailing} = string:take(Formatted, "\n", false, trailing),
    FullLines = string:replace(Leading, "\n", [$\n, ?CLREOL], all),
    [?RESET, Color, FullLines, ?CLREOL, ?RESET, Trailing].

color(Level, Config) ->
    Color = case Config of
                #{colors := #{Level := C}} -> C;
                _ -> default(Level)
            end,
    color_to_escape(Color).

default(emergency) -> {bg, red};
default(alert) -> {bg, red};
default(critical) -> {bg, red};
default(error) -> red;
default(warning) -> yellow;
default(debug) -> blue;
default(_) -> normal.

color_to_escape(normal) -> [];
color_to_escape({Mod, Color}) -> ?CSI(mod(Mod) + code(Color));
color_to_escape({Mod1, Mod2, Color}) ->
    ?CSI(mod(Mod1) + mod(Mod2) + code(Color));
color_to_escape(Color) -> ?CSI(code(Color)).

mod(fg)     -> 0;
mod(bg)     -> 10;
mod(bright) -> 60.

code(black)   -> 30;
code(red)     -> 31;
code(green)   -> 32;
code(yellow)  -> 33;
code(blue)    -> 34;
code(magenta) -> 35;
code(cyan)    -> 36;
code(white)   -> 37.

try_callback_call(Module, Function, Args, DefRet) ->
    try apply(Module, Function, Args)
    catch
        error:undef:S ->
            case S of
                [{Module, Function, Args, _}|_] ->
                    DefRet;
                _ ->
                    erlang:raise(error, undef, S)
            end
    end.
