-module('Elixir.IEx.Extensions').

-compile([no_auto_import]).

-export([tc/2, tc/4, dbg/0, dbgtc/1, dbgon/1, dbgon/2,
         dbgadd/1, dbgadd/2, dbgdel/1, dbgdel/2, dbgoff/0]).

tc(N, F)      -> user_default:tc(N, F).
tc(N, M,F,A)  -> user_default:tc(N, M,F,A).
dbg()         -> user_default:dbg().
dbgtc(File)   -> user_default:dbgtc(File).
dbgon(M)      -> user_default:dbgon(M).      
dbgon(M,F)    -> user_default:dbgon(M,F). % F = file | fun
dbgadd(M)     -> user_default:dbgadd(M).
dbgadd(M,F)   -> user_default:dbgadd(M,F).
dbgdel(M)     -> user_default:dbgdel(M).
dbgdel(M,F)   -> user_default:dbgdel(M,F).
dbgoff()      -> user_default:dbgoff().

