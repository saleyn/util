-module(plists_test).

-include_lib("eunit/include/eunit.hrl").

nodename() ->
  {ok, Host} = inet:gethostname(),
  list_to_atom("b@" ++ Host).

this_nodename() ->
  {ok, Host} = inet:gethostname(),
  list_to_atom("a@" ++ Host).

setup_network() ->
    Cookie = atom_to_list(erlang:get_cookie()),
    Node   = nodename(),
    case net_kernel:nodename() of
      ignored ->
        {ok, _} = net_kernel:start([this_nodename(), shortnames]);
      _ ->
        ok
    end,
    case net_adm:ping(Node) of
      pong ->
        ok;
      _ ->
        Cmd  = "erl -pa ../ebin -noshell -sname " ++ atom_to_list(Node) ++
               (if Cookie=="nocookie" -> ""; true -> " -setcookie " ++ Cookie end) ++
               " -noinput",
        io:format(standard_error, "Starting:\n  ~s\n  ~s\n", [Cmd, element(2, file:get_cwd())]),
        Port = open_port({spawn, Cmd}, []),
        link(Port)
    end,
    ok = wait_until_running(10, Node),
    Node.

wait_until_running(0, _) -> throw({error, could_not_connect});
wait_until_running(N, Node) ->
    case net_adm:ping(Node) of
    pong ->
        ok;
    pang ->
        timer:sleep(50),
        wait_until_running(N - 1, Node)
    end.

close_network(Node) ->
    rpc:call(Node, erlang, halt, [0]).

plists_test_() ->
    {setup
    ,fun setup_network/0
    ,fun (Node) -> close_network(Node) end
    ,fun (Node) ->
             do_tests(1)
                 ++ do_tests(4)
                 ++ do_tests({processes, 2})
                 ++ do_tests([4, {processes, 2}])
                 ++ do_tests({processes, schedulers})
                 ++ do_tests({timeout, 4000})
                 ++ do_tests({nodes, [{node(), 2}, node(), {node(), schedulers}]})
                 ++ do_tests({nodes, [{Node, 2}, Node,
                                      {Node, schedulers}]})
                 ++ do_tests([{nodes, [{node(), 2}, Node]}, {timeout, 4000}, 4])
             %% Ignore the ERROR REPORTs above, they are supposed to be there.
     end
    }.

do_tests(Malt) ->
    io:format(user, "Testing with malt: ~p~n", [Malt]),
    test_mapreduce(Malt)
        ++ test_all(Malt)
        ++ test_any(Malt)
        ++ test_filter(Malt)
        ++ test_fold(Malt)
        ++ test_foreach(Malt)
        ++ test_map(Malt)
        ++ test_partition(Malt)
        ++ test_sort(Malt)
        ++ test_usort(Malt)
        ++ [?_assertEqual(nil, check_leftovers())]
        ++ do_error_tests(Malt).

do_error_tests(Malt) ->
    MaltList = case is_list(Malt) of
                   true -> Malt;
                   false -> [Malt]
               end,
    [?_assertError(badarith, plists:map(fun (X) -> 1/X end, [1,2,3,0,4,5,6], Malt))
    ,?_assertEqual(nil, check_leftovers())
    ,?_assertError(timeout, test_mapreduce([{timeout,0}|MaltList]))
    ,?_assertEqual(nil, check_leftovers())
    ,?_assertError(timeout, plists:foreach(fun (_X) -> timer:sleep(1000) end, [1,2,3], [{timeout,40}|MaltList]))
    ,?_assertEqual(nil, check_leftovers())
    ].

check_leftovers() ->
    receive
	{'EXIT', _, _} ->
	    % plists doesn't start processes with spawn_link, so we
	    % know these aren't our fault.
	    check_leftovers();
	M ->
	    io:format("Leftover messages:~n~p~n", [M]),
	    print_leftovers()
    after 0 ->
	    nil
    end.

print_leftovers() ->
    receive
	M ->
	    io:format("~p~n", [M]),
	    print_leftovers()
    after 0 ->
	    exit(leftover_messages)
    end.

test_mapreduce(Malt) ->
    Ans = plists:mapreduce(fun (X) -> lists:map(fun (Y) -> {Y, X} end, lists:seq(1, X-1)) end, [2,3,4,5], Malt),
    % List1 consists of [2,3,4,5]
    List1 = dict:fetch(1, Ans),
    true = lists:all(fun (X) -> lists:member(X, List1) end, [2,3,4,5]),
    false = lists:any(fun (X) -> lists:member(X, List1) end, [1,6]),
    % List3 consists of [4,5]
    List3 = dict:fetch(3, Ans),
    true = lists:all(fun (X) -> lists:member(X, List3) end, [4,5]),
    false = lists:any(fun (X) -> lists:member(X, List3) end, [1,2,3,6]),
    Text = "how many of each letter",
    TextAns = plists:mapreduce(fun (X) -> {X, 1} end, Text, Malt),
    TextAns2 = dict:from_list(lists:map(fun ({X, List}) ->
						{X, lists:sum(List)} end,
			     dict:to_list(TextAns))),
    3 = dict:fetch($e, TextAns2),
    2 = dict:fetch($h, TextAns2),
    1 = dict:fetch($m, TextAns2),
    [].

test_all(Malt) ->
    [?_assert(plists:all(fun even/1, [2,4,6,8], Malt))
    ,?_assert(not plists:all(fun even/1, [2,4,5,8], Malt))
    ].

even(X) when X rem 2 =:= 0 ->
    true;
even(_) ->
    false.

test_any(Malt) ->
    [?_assert(plists:any(fun even/1, [1,2,3,4,5], Malt))
    ,?_assert(not plists:any(fun even/1, [1,3,5,7], Malt))
    ].

test_filter(Malt) ->
    [?_assertEqual([2,4,6], plists:filter(fun even/1, [1,2,3,4,5,6], Malt))
    ].

some_folder(X, A) -> max(X * X, A).
fuse(A1, A2) -> max(A1, A2).

test_fold(Malt) ->
    List = lists:seq(-5, 4),
    [?_assertEqual(15, plists:fold(fun (A, B) -> A+B end, 0, [1,2,3,4,5], Malt))
    ,?_assertEqual(25, plists:fold(fun some_folder/2, fun fuse/2, -10000, List, Malt))
    ,?_assertEqual(25, plists:fold(fun some_folder/2, {recursive,fun fuse/2}, -10000, List, Malt))
    ].

test_foreach(_Malt) ->
    [].

test_map(Malt) ->
    [?_assertEqual([2,4,6,8,10], plists:map(fun (X) -> 2*X end, [1,2,3,4,5], Malt))
     %% edge cases
    ,?_assertEqual([2], plists:map(fun (X) -> 2*X end, [1], Malt))
    ,?_assertEqual([], plists:map(fun (X) -> 2*X end, [], Malt))
    ].

test_partition(Malt) ->
    [?_assertEqual({[2,4,6],[1,3,5]}, plists:partition(fun even/1, [1,2,3,4,5,6], Malt))
    ].

test_sort(Malt) ->
    Fun = fun erlang:'=<'/2,
    [?_assertEqual([1,2,2,3,4,5,5], plists:sort(Fun, [2,4,5,1,2,5,3], Malt))
     %% edge cases
    ,?_assertEqual([1], plists:sort(Fun, [1], Malt))
    ,?_assertEqual([], plists:sort(Fun, [], Malt))
    ].

test_usort(Malt) ->
    Fun = fun erlang:'=<'/2,
    [?_assertEqual([1,2,3,4,5], plists:usort(Fun, [2,4,5,1,2,5,3], Malt))
    ,?_assertEqual([1,2,3,4,5], plists:usort(Fun, [2,4,5,1,2,5,3], Malt))
    ].
