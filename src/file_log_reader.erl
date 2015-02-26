%%% vim:ts=4:sw=4:et
%%%----------------------------------------------------------------------------
%%% @doc Periodically read an append-only log file and parse newly added data.
%%%
%%% The user controls the interval in msec how often to check for file
%%% modifications. When new data is appended to file it triggers invocation of
%%% the user-defined parsing function that deliminates the file, and the result
%%% is delivered to the consumer by calling the consumer callback function.
%%%
%%% The log reader can be started as a `gen_server' or can be controlled
%%% synchronously by using `init/3', `run/1', and `close/1' methods.
%%%
%%% @author    Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2015 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2015-02-12
%%%----------------------------------------------------------------------------
-module(file_log_reader).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/3, start_link/4, start/3, start/4, stop/1,
         position/1, pstate/1]).
-export([init/3, run/1, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-ifdef(TEST).
-define(D(Fmt,Args), io:format(Fmt, Args)).
-include_lib("eunit/include/eunit.hrl").
-else.
-define(D(Fmt,Args), ok).
-endif.

-define(MAX_READ_SIZE, 32*1024*1024).

-type consumer() ::
    fun((Msg :: {'$end_of_file', string(), Res::any()} | any(),
         Pos::integer(), State::any()) -> NewState::any()).
-type options()  :: [
    {pos,       StartPos::integer()}           |
    {end_pos,   ReadUntilPos::integer() | eof} |
    {max_size,  MaxReadSize::integer()  | eof} |
    {timeout,   MSec::integer()}               |
    {retry_sec, Sec::integer()}                |
    {parser, fun((Data::binary(), ParserState::any()) ->
                {ok, Msg::any(), Tail::binary(), NewParserState::any()} |
                {incomplete, NewParserState::any()} |
                {skip, Tail::binary(), NewParserState::any()}) |
             {Mod::atom(), Fun::atom()}}       |
    {pstate, fun((File::string(), consumer(), Options::list()) -> any()) | any()}].
%% Details:
%% <dl>
%% <dt>pos</dt>
%%   <dd>Start reading from this position (default: 0)</dd>
%% <dt>end_pos</dt>
%%   <dd>Read until this position and stop. If provided and file position reaches
%%       `end_pos', the consumer() callback given to the reader will be called as:
%%       ``Consumer({'$end_of_file', Filename::string(), Result}, Pos::integer(), State)''
%%       where `Result' is `ok' or `{error|exit|exception, Error::any(), StackTrace}'
%%       if an error occured.
%%   </dd>
%% <dt>max_size</dt>
%%   <dd>Maximum chunk size to read from file in a single pass (default: 32M).</dd>
%% <dt>timeout</dt>
%%   <dd>Number of milliseconds between successive file scanning (default: 1000)</dd>
%% <dt>retry_sec</dt>
%%   <dd>Number of seconds between successive retries upon failure to open the
%%       market data file passed to one of the `start*/{3,4}' functions (default: 15).
%%       The value of 0 means that the file must exist or else the process won't
%%       start.</dd>
%% <dt>parser</dt>
%%   <dd>Is the function to be called when the next chunk is read from file. The
%%       function must return:
%%       <dl>
%%       <dt>`{ok, Msg, Tail, State}'</dt>
%%          <dd>invoke `Consumer' callback passing it the parsed message `Msg',
%%              and continue parsing the `Tail' binary</dd>
%%       <dt>`{incomplete, State}'</dt>
%%          <dd>the data contains no complete messages - wait until there's more</dd>
%%       <dt>`{skip, Tail, State}'</dt>
%%          <dd>disregard input and continue parsing `Tail' without calling
%%              `Consumer' callback</dd>
%%       </dl>
%%   </dd>
%% <dt>pstate</dt>
%%   <dd>Initial value of the parser state or a functor `fun((File::string()
%%       Consumer::consumer(), Options::options()) -> PState::any())'</dd>
%% </dl>

-export_type([consumer/0, options/0]).

-record(state, {
      consumer      :: consumer()
    , tref          :: reference()
    , fd            :: port()
    , file          :: string()
    , pos = 0       :: integer() | eof
    , end_pos       :: integer() | eof | undefined  % Read until this pos (inclusive)
    , max_size      :: integer()
    , timeout       :: integer()
    , parser        :: {atom(), atom()} |
                       fun((binary(), any()) -> {any(), binary(), any()})
    , pstate        :: any()
    , part_size = 0 :: integer()         % Sz of incomple unparsed chunk at eof
    , done = false  :: false | ok |      % Processing completion indicator
                       {error|exception|exit, Reason::any(), Stacktrace::any()}
    , incompl_count :: integer()         % Counter of incomplete message reads
}).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns `{error,Reason}'.
%%      If init/1 returns `{stop,Reason}' or ignore, the process is
%%      terminated and the function returns `{error,Reason}' or ignore,
%%      respectively.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(atom(), string(), consumer(), options()) ->
    {ok, pid()} | ignore | {error, any()}.
start_link(RegName, File, Consumer, Options)
    when is_atom(RegName), is_list(File), is_function(Consumer,3), is_list(Options) ->
    gen_server:start_link({local, RegName}, ?MODULE, [File, Consumer, Options], []).

-spec start_link(string(), consumer(), options()) ->
    {ok, pid()} | ignore | {error, any()}.
start_link(File, Consumer, Options)
    when is_list(File), is_function(Consumer,3), is_list(Options) ->
    gen_server:start_link(?MODULE, [File, Consumer, Options], []).

%%-----------------------------------------------------------------------------
%% @doc Start the server outside of supervision tree.
%% @end
%%-----------------------------------------------------------------------------
-spec start(atom(), string(), consumer(), options()) ->
    {ok, pid()} | {error, any()}.
start(RegName, File, Consumer, Options)
    when is_atom(RegName), is_list(File), is_function(Consumer,3), is_list(Options) ->
    gen_server:start({local, RegName}, ?MODULE, [File, Consumer, Options], []).

-spec start(string(), consumer(), options()) ->
    {ok, pid()} | {error, any()}.
start(File, Consumer, Options)
    when is_list(File), is_function(Consumer,3), is_list(Options) ->
    gen_server:start(?MODULE, [File, Consumer, Options], []).

%%-----------------------------------------------------------------------------
%% @doc Report last processed file position/size.
%% @end
%%-----------------------------------------------------------------------------
-spec position(pid() | atom()) -> {ok, Position::integer()}.
position(Pid) ->
    gen_server:call(Pid, position).

%%-----------------------------------------------------------------------------
%% @doc Return current parser state (`{pstate, any()}' initialization option).
%% @end
%%-----------------------------------------------------------------------------
-spec pstate(pid() | atom()) -> {ok, any()}.
pstate(Pid) ->
    gen_server:call(Pid, pstate).

%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid() | atom()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

%%-----------------------------------------------------------------------------
%% @doc When using file processor without gen_server, use this function to
%%      initialize the state, and then call run/1.
%% @end
%%-----------------------------------------------------------------------------
-spec init(string(), consumer(), options()) -> {ok, #state{}}.
init(File, Consumer, Options) when is_list(File), is_list(Options) ->
    try
        Offset  = proplists:get_value(pos, Options, 0),
        EndPos  = case proplists:get_value(end_pos,  Options) of
                  M when is_integer(M) -> M;
                  eof                  -> eof;
                  undefined            -> undefined;
                  Other2               -> throw({invalid_option, {end_pos, Other2}})
                  end,
        MaxSize = proplists:get_value(max_size, Options, ?MAX_READ_SIZE),
        Parser  = proplists:get_value(parser,   Options),
        RetryS  = proplists:get_value(retry_sec,Options, 15),
        Timeout = proplists:get_value(timeout,  Options, 1000),
        State   = #state{file=File,  consumer=Consumer, parser=Parser,
                         pos=Offset, end_pos=EndPos, max_size=MaxSize,
                         timeout=Timeout, incompl_count=0},
        {ok, S} = try_open_file(1, RetryS, File, State),
        PState  = case proplists:get_value(pstate, Options) of
                  F when is_function(F, 3) ->
                      F(File, Consumer, Options);
                  Other ->
                      Other
                  end,
        {ok, S#state{pstate=PState}}
    catch
        throw:ignore ->
            ignore;
        T:E ->
            {stop, {T, E, erlang:get_stacktrace()}}
    end.

%%-----------------------------------------------------------------------------
%% @doc Process file from given position `Pos' to `EndPos' (or `eof').
%% @end
%%-----------------------------------------------------------------------------
-spec run(#state{}) -> #state{}.
run(#state{fd=FD, pos=Pos, end_pos=EndPos, max_size=MaxChunkSz, done=false} = S) ->
    MaxPos  = Pos+MaxChunkSz,
    NextPos = if is_integer(EndPos) -> min(MaxPos, EndPos); true -> MaxPos end,
    Size    = NextPos - Pos,
    if Size > 0 ->
        case file:pread(FD, Pos, Size) of
        {ok, Data} when byte_size(Data) > S#state.part_size ->
            try
                S1 = process_chunk(Data, S),
                run(S1)
            catch E:W ->
                end_of_file({E, W, erlang:get_stacktrace()}, S)
            end;
        {ok, _Data} ->
            schedule_timer(S);
        eof when EndPos =:= eof ->
            % Reached the requested end of input - notify consumer:
            end_of_file(ok, S#state{end_pos=Pos});
        eof ->
            % No more data is currently available - wait until there's more.
            schedule_timer(S)
        end;
    true ->
        % Reached the requested end of input - notify consumer:
        end_of_file(ok, S#state{end_pos=Pos})
    end;
run(#state{} = S) ->
    S.
            
end_of_file(Result, #state{pos=Pos, consumer=Callback, file=F, pstate=PState} = S) ->
    PS1 = Callback({'$end_of_file', F, Result}, Pos, PState),
    S#state{pstate = PS1, part_size=0, done=Result}.

schedule_timer(#state{timeout=Timeout, fd=FD} = S) ->
    TRef = erlang:send_after(Timeout, self(), {check_md_files_timer, FD}),
    S#state{tref=TRef}.

%%-----------------------------------------------------------------------------
%% @doc Close file processor (use this method when not using gen_server)
%% @end
%%-----------------------------------------------------------------------------
close(#state{fd=FD} = State) ->
    file:close(FD),
    State#state{fd=undefined}.

%%%----------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initiates the server
%% @end
%%-----------------------------------------------------------------------------
-spec init(list()) ->
    {ok, #state{}} |
    {ok, #state{}, Timeout :: integer() | hibernate} | ignore | {stop, any()}.
init([File, Consumer, Options]) ->
    try
        init(File, Consumer, Options)
    catch _:What ->
        {stop, What}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(any(), From::tuple(), #state{}) ->
    {reply, Reply::any(), #state{}} |
    {reply, Reply::any(), #state{}, Timeout::integer() | hibernate} |
    {noreply, #state{}} |
    {noreply, #state{}, Timeout::integer() | hibernate} |
    {stop, Reason::any(), Reply::any(), #state{}} |
    {stop, Reason::any(), #state{}}.
handle_call(position, _From, #state{pos = Pos} = State) ->
    {reply, {ok, Pos}, State};
handle_call(pstate, _From, #state{pstate = PState} = State) ->
    {reply, {ok, PState}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(any(), #state{}) ->
    {noreply, #state{}} | {noreply, #state{}, Timeout::integer() | hibernate} |
    {stop, Reason::any(), #state{}}.
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
%% @end
%%-----------------------------------------------------------------------------
-spec handle_info(any(), #state{}) ->
    {noreply, #state{}} | {noreply, #state{}, Timeout::integer() | hibernate} |
    {stop, Reason::any(), #state{}}.
handle_info({check_md_files_timer, _FD}, State) ->
    S = #state{done=Done} = run(State),
    case Done of
    false ->
        {noreply, S, hibernate};
    ok ->
        {stop, normal, S};
    {E, Reason, Stacktrace} when E=:=error; E=:=exception; E=:=exit ->
        erlang:raise(E, Reason, Stacktrace)
    end;
handle_info({open_file_timer, Attempt, RetrySec, File}, State) ->
    {ok, State1} = try_open_file(Attempt, RetrySec, File, State),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, #state{}) ->
    ok.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%% @end
%%-----------------------------------------------------------------------------
-spec code_change(any(), #state{}, any()) -> #state{}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------

try_open_file(Attempt, RetrySec, File, #state{} = State) when is_list(File) ->
    case file:open(File, [read,raw,binary,read_ahead]) of
    {ok,FD} ->
        {ok,End} = file:position(FD, eof),
        Pos      = case State#state.pos of
                      N when is_integer(N) -> N;
                      eof                  -> End;
                      Other1               -> throw({invalid_option, {pos, Other1}})
                   end,
        case Attempt of
        1 -> ok;
        _ -> error_logger:info_msg
                ("~w: file ~s was opened successfully\n", [?MODULE, File])
        end,
        {ok, schedule_timer(State#state{fd=FD, pos=Pos})};
    {error, Reason} when Attempt =:= 1, RetrySec > 0 ->
        error_logger:warning_msg(
            "~w: failed to open file ~s (will retry in ~ws): ~s",
            [?MODULE, File, RetrySec, file:format_error(Reason)]),
        Msg = {open_file_timer, Attempt+1, RetrySec, File},
        erlang:send_after(RetrySec*1000, self(), Msg),
        {ok, State};
    {error, _Reason} when RetrySec > 0 ->
        Msg = {open_file_timer, Attempt+1, RetrySec, File},
        erlang:send_after(RetrySec*1000, self(), Msg),
        {ok, State};
    {error, Reason} ->
        throw({cannot_open_file, File, Reason})
    end.

process_chunk(<<>>, S) ->
    S;
process_chunk(Data, #state{part_size=N} = S) when N > 0, byte_size(Data) =< N ->
    %% Last message in the file was found to be incomplete on the
    %% previous processing iteration.  Current read attempt didn't read any more
    %% of new bytes, so do nothing and wait until there's more data available
    S#state{incompl_count=S#state.incompl_count+1};
process_chunk(Data, #state{pos=Pos, consumer=OnMsg, parser=P, pstate=PS} = S) ->
    case parse(P, Data, PS) of
    {ok, Msg, <<>>, PS1} ->
        %% The Data was fully parsed, invoke the callback and try to see if
        %% more data was appended to the file
        NPos = Pos + byte_size(Data),
        PS2  = OnMsg(Msg, NPos, PS1),
        run(S#state{pos=NPos, pstate=PS2, part_size=0});
    {ok, Msg, Tail, PS1} when is_binary(Tail) ->
        %% A chunk of Data up to the Tail was fully parsed, invoke the callback
        %% and continue parsing the Tail
        NPos = Pos + (byte_size(Data) - byte_size(Tail)),
        PS2  = OnMsg(Msg, NPos, PS1),
        process_chunk(Tail, S#state{pos=NPos, pstate=PS2, part_size=0});
    {incomplete, PS1} ->
        I = S#state.incompl_count,
        S#state{pstate=PS1, part_size=byte_size(Data), incompl_count=I+1};
    {skip, Tail, PS1} when is_binary(Tail) ->
        N = byte_size(Data) - byte_size(Tail),
        process_chunk(Tail, S#state{pos=Pos + N, pstate=PS1, part_size=0})
    end.

parse(Fun, Data, State) when is_function(Fun, 2) ->
    Fun(Data, State);
parse({M,F}, Data, State) ->
    M:F(Data, State);
parse(undefined, Data, State) ->
    {ok, Data, <<>>, State}.

%%%----------------------------------------------------------------------------
%%% Test functions
%%%----------------------------------------------------------------------------

-ifdef(EUNIT).

async_read_file_test() ->
    File  = "/tmp/test-file-reader.log",
    Self  = self(),
    N     = 10,

    %% Producer of data
    begin_produce(File, N, false),

    %% Consumer of data
    User = spawn_link(fun() -> ?assertEqual(ok, wait(N, 1)), Self ! done end),

    Prsr = fun(Bin, PState) ->
        case binary:split(Bin, <<"\n">>) of
        [<<"abc5">>, Tail] ->
            {skip, Tail, PState+1};
        [<<"abc1">>=_M,_Tail] when PState =:= 9 ->
            {incomplete, PState+1};
        [Msg, Tail] ->
            {ok, Msg, Tail, PState+1}
        end
    end,
    Opts      = [{timeout, 10}, {parser, Prsr}, {pstate, 0}],
    Consume   = fun(Msg, _Pos, State) -> User ! Msg, State end,
    {ok, Pid} = start_link(File, Consume, Opts),

    ?assertEqual(done, receive done -> done after 1000 -> timeout end),
    {ok, PState} = pstate(Pid),
    ?assertEqual(9,  PState),
    ?assertEqual(ok, stop(Pid)),
    file:delete(File).

begin_produce(File, N, Wait) ->
    file:delete(File),
    Self = self(),

    %% Produce data
    spawn_link(fun() ->
        Res = file:open(File, [write,raw,binary]),
        Self ! continue,
        ?assertMatch({ok,_}, Res),
        {ok, F} = Res,

        (fun
            Loop(0,_FF) -> ok;
            Loop(I, FF) ->
                Bin = <<"abc", (integer_to_binary(I))/binary, $\n>>,
                ok  = file:write(FF, Bin),
                timer:sleep(50),
                Loop(I-1, FF)
         end)(N, F),

        if Wait -> Self ! end_producer; true -> ok end
    end),

    receive continue -> ok end,
    if
        Wait -> receive end_producer -> ok end;
        true -> ok
    end.

wait(I, I) ->
    % Got all messages
    ok;
wait(5, I) ->
    % <<"abc5">> is skipped by the parser
    wait(4, I);
wait(I, End) ->
    Expect = <<"abc", (integer_to_binary(I))/binary>>,
    receive
        Expect ->
            wait(I-1, End);
        Other ->
            ?debugFmt("Unexpected message: ~p", [Other]),
            %{unexpected_msg, Other}
            wait(I-1, End)
    %after 1000 ->
    %    timeout
    end.

consume({'$end_of_file', _F, ok}, Pos, PState) when is_integer(Pos), is_integer(PState) ->
    put(count_end, get(count_end)+1),
    put(end_pos, Pos),
    PState;
consume(Msg, _Pos, PState) when is_binary(Msg), is_integer(PState) ->
    Bin = <<"abc", (integer_to_binary(PState))/binary>>,
    ?assertEqual(Msg, Bin),
    PState-1.

sync_read_file_test() ->
    File  = "/tmp/test-file-reader.log",
    N     = 10,
    file:delete(File),

    %% Produce data
    begin_produce(File, N, true),

    %% Consume data
    Prsr = fun(Bin, PState) ->
        [Msg, Tail] = binary:split(Bin, <<"\n">>),
        {ok, Msg, Tail, PState}
    end,

    put(count_end, 0),

    %% (1) Initialize file reader
    (fun() ->
        {ok, State} = init(File, fun consume/3, [{parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(0, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    ?assertEqual(0,  get(count_end)),

    %% (2) Now test reading from the end of file
    (fun() ->
        {ok, State} = init(File, fun consume/3, [{pos, eof}, {parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(10, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    ?assertEqual(0,  get(count_end)),

    %% (3) Now test reading from the 6th position
    (fun() ->
        {ok, State} = init(File, fun consume/3, [{pos, 6}, {parser, Prsr}, {pstate, N-1}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(0, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    ?assertEqual(0,  get(count_end)),

    %% (4) Now test reading only 11 bytes: <<"abc10\nabc9\n">>
    (fun() ->
        {ok, State} = init(File, fun consume/3, [{end_pos, 11}, {parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(8, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    ?assertEqual(1,  get(count_end)),
    ?assertEqual(11, get(end_pos)),

    %% (5) Now test reading from the end of file till the end of file
    (fun() ->
        {ok, State} = init(File, fun consume/3, [{pos, eof}, {end_pos, eof}, {parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(51, State1#state.pos),
        ?assertEqual(51, State1#state.end_pos),
        ?assertEqual(N,  State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    ?assertEqual(2,  get(count_end)),
    ?assertEqual(51, get(end_pos)),

    erase(count_end),
    erase(end_pos),
    file:delete(File).
       
-endif.

%%%.
%%% vim: set filetype=erlang tabstop=4 foldmarker=%%%',%%%. foldmethod=marker:
