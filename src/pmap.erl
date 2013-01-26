%%%------------------------------------------------------------------------
%%% File: $Id$
%%%------------------------------------------------------------------------
%%% @doc Parallel map and multicall.
%%%
%%% @author  Serge Aleynikov <saleyn@gmail.com> (multicall)
%%%          Luke Gorries (http://lukego.livejournal.com) (pmap)
%%% @version $Rev$
%%%          $Date: 2008/07/02 03:29:58 $
%%% @end
%%%
%%%------------------------------------------------------------------------
%%% Created: 2008/02/11 10:07:15
%%%------------------------------------------------------------------------
-module(pmap).
-author('saleyn@gmail.com').
-id    ("$Id$").

-export([pmap/2, pmap/3, multicall/2, reply/2]).

-ifdef(DEBUG).
-export([test/0, test1/0, test2/0, test3/0]).
-endif.

%%-------------------------------------------------------------------------
%% @spec (F, MultiArgs) -> [Reply]
%%          F = () -> Term
%%          MultiArgs = [Args]
%% @doc Evaluate the `MultiArgs' list by calling `F' on each argument in
%%      the list concurrently.
%% @see pmap/3
%% @end
%%-------------------------------------------------------------------------
pmap(F, List) ->
    pmap(F, List, infinity).

%%-------------------------------------------------------------------------
%% @spec (F, MultiArgs, Timeout) -> [Reply]
%%          F         = (Args) -> Term
%%          MultiArgs = [Args]
%%          Timeout   = integer() | infinity
%% @doc Evaluate the `MultiArgs' list by calling `F' on each argument in
%%      the list concurrently.  Same as pmap/2 but has a `Timeout' option.
%% @end
%%-------------------------------------------------------------------------
pmap(F, List, Timeout) ->
    Ref = make_ref(),
    [wait_result(Ref, Worker, Timeout) || Worker <- [spawn_worker(self(),Ref,F,E) || E <- List]].

spawn_worker(Parent, Ref, F, E) ->
    erlang:spawn_monitor(fun() -> reply({Parent, Ref}, F(E)) end).

wait_result(Ref, {Pid,MonRef}, Timeout) ->
    receive
    {'DOWN', MonRef, _, _, normal} -> 
        receive 
        {{Pid, Ref}, Result} ->
            Result
        after Timeout ->
            {error, timeout}
        end;
    {'DOWN', MonRef, _, _, Reason} ->
        {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @spec (PidMsgs, Timeout::timeout()) -> {Success, Error}
%%          PidMsgs = [{Pid, Msg}]
%%          Success = [OkReply]
%%          Error   = [ErrorReply]
%% @doc Send messages to pids and wait for replies.
%%      Each Pid would get a message in the form:
%%      `{{FromPid, Ref}, Msg}' and would have to reply with:
%%      `FromPid ! {{self(), Ref}, Reply}'. The function aggregates all 
%%      replies into `Success' and `Error' lists. The error list is in the 
%%      form: `{Pid, ErrorReason}'.
%% @end
%%-------------------------------------------------------------------------
multicall([], _Timeout) ->
    {[], []};
multicall(PidMsgs, Timeout) when is_list(PidMsgs) ->
    Ref = make_ref(),
    Fun = fun(_) ->
        {Refs, Errors, Monitors} = lists:foldl(
            fun({Pid, Msg}, {Refs, Err, Mons}) ->
                case node(Pid) =/= node() orelse erlang:is_process_alive(Pid) of
                true  -> 
                    reply({Pid, Ref}, Msg),
                    MonRef = erlang:monitor(process, Pid),
                    {gb_sets:add({Pid, Ref}, Refs), Err, [{MonRef, Pid} | Mons]};
                false -> 
                    {Refs, [{Pid, {error, no_process}} | Err], Mons}
                end
            end,
            {gb_sets:empty(), [], []},
            PidMsgs),
            
        gather_results(Refs, [], Errors, Monitors, Timeout)
    end,
    lists:foldl(
        fun({Ok, Err}, {AccOk, AccErr}) -> 
            {Ok ++ AccOk, Err ++ AccErr};
        (Other, _Acc) ->
            erlang:error({?MODULE, unexpected_result, Other})
        end,
        {[], []},
        pmap(Fun, [[]], Timeout)
    ).

gather_results({0,nil}, Replies, Errors, _Monitors, _Timeout) ->
    {lists:reverse(Replies), Errors};
gather_results(Set, Replies, Errors, Monitors, Timeout) ->
    receive
    {{Pid, Ref}, Result} ->
        try 
            NewSet = gb_sets:delete({Pid, Ref}, Set),
            gather_results(NewSet, [{Pid, Result} | Replies], Errors, Monitors, Timeout)
        catch _:_ ->
            gather_results(Set, Replies, Errors, Monitors, Timeout)
        end;
    {'DOWN', _MonRef, _, _, normal} ->
        % Ideally we'd have to remove the _MonRef from the Monitors list,
        % but since the multicall/2 call is executed in its own process (via pmap call)
        % at the end of this multicall all monitors are cleaned up as the process dies.
        gather_results(Set, Replies, Errors, Monitors, Timeout);
    {'DOWN', MonRef, _, _, Reason} ->
        case lists:keytake(MonRef, 1, Monitors) of
        {value, {_, Pid}, NewMonitors} ->
            NewSet = gb_sets:filter(fun({P, _}) -> P =/= Pid end, Set),
            gather_results(NewSet, Replies, 
                [{Pid, {error, {process_disconnected, Reason}}} | Errors], 
                NewMonitors, Timeout);
        false ->
            gather_results(Set, Replies, Errors, Monitors, Timeout)
        end
    after Timeout ->
        {Replies, gb_sets:fold(fun({Pid, _}, Acc) -> [{Pid, {error, timeout}} | Acc] end, [], Set) ++ Errors}
    end.

%%-------------------------------------------------------------------------
%% @spec (From, Reply) -> ok
%%          From = {FromPid::pid(), Ref::reference()}
%% @doc Send a reply back to sender.
%% @end
%%-------------------------------------------------------------------------
reply({FromPid, Ref}, Msg) ->
    catch FromPid ! {{self(), Ref}, Msg},
    ok.

%%%------------------------------------------------------------------------
%%% TEST CASES
%%%------------------------------------------------------------------------

-ifdef(DEBUG).
test() ->
    Expected = lists:seq(1, 20),
    F = fun() -> receive {{_Pid, _Ref} = From, Msg} -> pmap:reply(From, Msg) end end,
    Msgs = [{spawn(F), I} || I <- Expected],
    {Oks, []} = pmap:multicall(Msgs, 10000),
    Expected = lists:sort([I || {_Pid, I} <- Oks]).
    
test1() ->
    F = fun() -> 
            receive 
            {_From, {_I, X}} when X == 3 -> 
                timer:sleep(1000), 
                exit(killed);
            {{From, Ref}, {I, _X}} -> 
                From ! {{self(), Ref}, I}, 
                timer:sleep(2000) 
            end 
        end,
    {A,B,C}=now(), random:seed(A,B,C),
    Msgs = [{spawn(F), {I, random:uniform(3)}} || I <- lists:seq(1, 20)],
    pmap:multicall(Msgs, 10000).

test2() ->
    [1,4,9,16,25] = pmap:pmap(fun(I) -> I*I end, [I || I <- lists:seq(1,5)]).

test3() ->
    F = fun() -> receive {{From, Ref}, Msg} -> timer:sleep(2000), From ! {{self(), Ref}, Msg}, timer:sleep(1000) end end,
    Msgs = [{spawn(F), I} || I <- lists:seq(1, 5)],
    multicall(Msgs, 1000).
    
-endif.
