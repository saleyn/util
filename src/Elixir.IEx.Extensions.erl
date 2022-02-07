-module('Elixir.IEx.Extensions').

-compile([no_auto_import]).

-export([tc/2, tc/4]).

tc(_n@1, _f@1) -> user_default:tc(_n@1, _f@1).

tc(_n@1, _m@1, _f@1, _a@1) ->
    user_default:tc(_n@1, _m@1, _f@1, _a@1).
