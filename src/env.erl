%%%----------------------------------------------------------------------------
%%% @doc Environment utils
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2012 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2012-09-21
%%%----------------------------------------------------------------------------
-module(env).
-author('saleyn@gmail.com').

%% API
-export([subst_env_path/1,   subst_env_path/2, 
         replace_env_vars/1, replace_env_vars/2,
         get_env/3, home_dir/0,
         normalize_path/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-deprecated([{subst_env_path,1,"use replace_env_vars/1 instead"}]).
-deprecated([{subst_env_path,2,"use replace_env_vars/2 instead"}]).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

-spec subst_env_path(list() | binary()) -> list() | binary().
subst_env_path(OsPath) ->
    replace_env_vars(OsPath, []).

-spec subst_env_path(list() | binary(), [{atom() | string(), string()}]) ->
    list() | binary().
subst_env_path(OsPath, Bindings) when is_list(Bindings) ->
    replace_env_vars(OsPath, Bindings).

%%------------------------------------------------------------------------
%% @spec (OsPath) -> Path::string()
%%         OsPath = string() | binary()
%% @doc Perform replacement of environment variable values in the OsPath.
%% ```
%% Example:
%%   env:replace_env_vars("~/app")       -> "/home/cuser/app"
%%   env:replace_env_vars("${HOME}/app") -> "/home/cuser/app"
%%   env:replace_env_vars("$USER/app")   -> "cuser/app"
%% '''
%% @see os:getenv/1
%% @end
%%------------------------------------------------------------------------
-spec replace_env_vars(list() | binary()) -> list() | binary().
replace_env_vars(OsPath) ->
    replace_env_vars(OsPath, []).


%%------------------------------------------------------------------------
%% @spec (OsPath, Bindings) -> Path::string()
%%          OsPath   = string() | binary()
%%          Bindings = [{Name, Value::string()}]
%%          Name     = atom() | string()
%% @doc Perform replacement of environment variable values in the OsPath.
%%      This function also allows to provide a list of `Bindings' that
%%      override the environment (they are checked before environment
%%      variables are looked up).
%% ```
%% Example:
%%   env:replace_env_vars("~/",   [{"HOME", "/home/cu"}]) -> "/home/cu/"
%%   env:replace_env_vars("~/",   [{home,   "/home/cu"}]) -> "/home/cu/"
%%   env:replace_env_vars("$A/",  [{a, "/aaa"}]) -> "/aaa/"
%%   env:replace_env_vars("${A}/",[{a, "/aaa"}]) -> "/aaa/"
%% '''
%% @see os:getenv/1
%% @end
%%------------------------------------------------------------------------
-spec replace_env_vars(list() | binary(), [{atom() | string(), string()}]) ->
    list() | binary().
replace_env_vars(OsPath, Bindings) when is_list(Bindings) ->
    element(2, env_subst(OsPath, Bindings)).

%%-----------------------------------------------------------------------------
%% @doc Get application configuration
%% @end
%%-----------------------------------------------------------------------------
-spec get_env(atom(), atom(), any()) -> any().
get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
    {ok, Val} ->
        Val;
    _ ->
        Default
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

internal_var("RELEASES")    -> {ok, get_rels_dir()};
internal_var("ROOTDIR")     -> {ok, get_root_dir()};
internal_var("RELPATH")     -> {ok, get_rel_path()};     % Releases/Version
internal_var(_)             -> undefined.

env_var(OS, "$$", _) when OS==unix; OS==win ->
    "$";
env_var(OS, [$~] = Key, Bindings) when OS==unix; OS==win ->
    case get_var(OS, "HOME", Bindings) of
    {ok, Value} -> {Key, Value};
    _           -> Key
    end;
env_var(OS, [$~ | User] = Word, Bindings) when OS==unix; OS==win ->
    case get_var(OS, "HOME", Bindings) of
    {ok, Value} -> {[$~], filename:join(filename:dirname(Value), User)};
    _           -> Word
    end;
env_var(OS, [$$, ${ | Env] = Word, Bindings) when OS==unix; OS==win ->
    Key = string:strip(Env, right, $}),
    case get_var(OS, Key, Bindings) of
    {ok, Value} -> {Key, Value};
    _           -> Word
    end;
env_var(OS, [$$ | Key] = Word, Bindings) when OS==unix; OS==win ->
    case get_var(OS, Key, Bindings) of
    {ok, Value} -> {Key, Value};
    _           -> Word
    end;
env_var(win, "%%", _) ->
    "%";
env_var(win, [$% | Env] = Word, Bindings) ->
    Key = string:strip(Env, right, $%),
    case get_var(win, Key, Bindings) of
    {ok, Value} -> {Key, Value};
    _           -> Word
    end;
env_var(_, Other, _Bindings) ->
    Other.

get_var(OS, Name, Bindings) ->
    case try_get_binding(Name, Bindings) of
    undefined when Name == "HOME" ->
        {ok, home_dir(OS)};
    undefined ->
        case os:getenv(Name) of
        false -> internal_var(Name);
        Value -> {ok, Value}
        end;
    Value ->
        {ok, Value}
    end.

try_get_binding(Name, Bindings) ->
    case proplists:get_value(Name, Bindings) of
    undefined when is_list(Name) ->
        try
            A = list_to_existing_atom(string:to_lower(Name)),
            try_get_binding(A, Bindings)
        catch _:_ ->
            undefined
        end;
    Value ->
        Value
    end.

env_subst(Bin, Bindings) when is_binary(Bin) ->
    {VarList, NewText} = env_subst(binary_to_list(Bin), Bindings),
    {VarList, list_to_binary(NewText)};
env_subst(Text, Bindings) when is_list(Text) ->
    env_subst(Text, os:type(), Bindings).

env_subst(Text, {unix, _}, Bindings) ->
    env_subst(Text, unix,
        "(?|(?:\\$\\$)|(?:~[^/$]*)|(?:\\${[A-Za-z][A-Za-z_0-9]*})|(?:\\$[A-Za-z][A-Za-z_0-9]*))",
        Bindings);
env_subst(Text, {win32, _}, Bindings) ->
    env_subst(Text, win, "(?|(?:\\%\\%)|(?:%[A-Za-z][A-Za-z_0-9]*%)|(?:~[^/$]*)|(?:\\${[A-Za-z][A-Za-z_0-9]*})|(?:\\$[A-Za-z][A-Za-z_0-9]*))", Bindings).

env_subst(Text, OsType, Pattern, Bindings) ->
    case re:run(Text, Pattern, [global,{capture, all}]) of
    {match, Matches} ->
        {Vars, TextList, Last} =
            lists:foldl(fun
                ([{Start, Length}], {Dict, List, Prev}) when Start >= 0 ->
                    Pos = Start+1,
                    Match = string:substr(Text, Pos, Length),
                    case env_var(OsType, Match, Bindings) of
                    {Key, Val} ->
                        case orddict:is_key(Key, Dict) of
                        true ->
                            NewDict = Dict;
                        false ->
                            NewDict = orddict:append(Key, Val, Dict)
                        end;
                    Val ->
                        NewDict = Dict
                    end,
                    NewList = [Val, string:substr(Text, Prev, Pos - Prev) | List],
                    NewPrev = Pos + Length,
                    {NewDict, NewList, NewPrev};
                (_, Acc) -> Acc
            end, {orddict:new(), [], 1}, Matches),
        VarList = [{K, V} || {K, [V|_]} <- orddict:to_list(Vars)],
        NewText = lists:concat(lists:reverse([string:substr(Text, Last) | TextList])),
        {VarList, NewText};
    nomatch ->
        {[], Text}
    end.

get_rel_ver() ->
    % release and version
    try [T || T <- release_handler:which_releases(), element(4, T) =:= permanent] of
    [{Name, Vsn, _Apps, permanent} | _] ->
        {Name, Vsn};
    _Other ->
        throw
    catch _:_ ->
        false
    end.

get_root_dir() ->
    case os:getenv("ROOTDIR") of
    Str when is_list(Str) -> Str;
    _ -> ""
    end.

get_rels_dir() ->
    case application:get_env(sasl, releases_dir) of
    {ok, Path} ->
        Path;
    _ ->
        filename:join(get_root_dir(), "releases")
    end.

get_rel_path() ->
    % version
    {_, Ver} = get_rel_ver(),
    % current release dir
    filename:join(get_rels_dir(), Ver).

home_dir() ->
  case os:type() of
    {win32,_} -> home_dir(win);
    {_,_}     -> home_dir(linux)
  end.

home_dir(win) ->
  normalize_path(os:getenv("USERPROFILE"));
home_dir(_) ->
  os:getenv("HOME").

normalize_path(Path) ->
  case os:type() of
    {win32,_} ->
 			F = fun Norm([$\\ | T]) -> [$/ | Norm(T)];
              Norm([H   | T]) -> [H  | Norm(T)];
              Norm([]       ) -> []
          end,
      F(Path);
    {_,_} ->
      Path
  end.

  
%%%----------------------------------------------------------------------------
%%% Test Cases
%%%----------------------------------------------------------------------------

-ifdef(EUNIT).

run_test_() -> 
    [
        ?_assertEqual("/abc/$/efg", replace_env_vars("/abc/$$/efg")),
        ?_assertEqual(true, os:putenv("X", "x")),
        ?_assertEqual("/" ++ os:getenv("X") ++ "/dir", replace_env_vars("/$X/dir")),
        ?_assertEqual(os:getenv("X") ++ "/dir", replace_env_vars("${X}/dir")),
        ?_assertEqual(os:getenv("HOME") ++ "/dir", replace_env_vars("~/dir")),
        ?_assertEqual("/aaa/dir", replace_env_vars("/$X/dir", [{"X", "aaa"}])),
        ?_assertEqual("/aaa/dir", replace_env_vars("/$X/dir", [{x, "aaa"}])),
        ?_assertEqual("/xxx/dir", replace_env_vars("$HOME/dir",  [{"HOME", "/xxx"}])),
        ?_assertEqual("/xxx/dir", replace_env_vars("~/dir", [{"HOME", "/xxx"}])),
        ?_assertEqual("/xxx/dir", replace_env_vars("~/dir", [{home, "/xxx"}]))
    ].

-endif.
