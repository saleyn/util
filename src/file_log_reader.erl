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
-export([start_link/3, start_link/4, start/3, start/4, stop/1, position/1]).
-export([init/3, run/1, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(TEST, 1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(MAX_READ_SIZE, 32*1024*1024).

-type consumer() :: fun((Msg :: {'$end_of_file', pid(), integer()} | any(), State::any()) ->
    NewState::any()).
-type options()  :: [
    {pos,       StartPos::integer()} |
    {end_pos,   ReadUntilPos::integer() | eof} |
    {max_size,  MaxReadSize::integer()  | eof} |
    {timeout,   MSec::integer()} |
    {parser, fun((Data::binary(), ParserState::any()) ->
                {more|done, Msg::any(), Tail::binary(), NewParserState::any()} |
                {skip, Tail::binary(), NewParserState::any()} |
                false) |
             {Mod::atom(), Fun::atom()}} |
    {pstate, any()}].
%% Details:
%% <dl>
%% <dt>pos</dt>
%%   <dd>Start reading from this position (default: 0)</dd>
%% <dt>end_pos</dt>
%%   <dd>Read until this position and stop. If provided and file position reaches
%%       `end_pos', the consumer() callback given to the reader will be called as:
%%       ``Consumer({'$end_of_file', Reader::pid(), Offset::integer()}, State)''
%%   </dd>
%% <dt>max_size</dt>
%%   <dd>Maximum chunk size to read from file in a single pass (default: 32M).</dd>
%% <dt>timeout</dt>
%%   <dd>Number of milliseconds between successive file scanning (default: 1000)</dd>
%% <dt>parser</dt>
%%   <dd>Is the function to be called when the next chunk is read from file. The
%%       function must return `{more, Msg, Tail, State}' if there is more data to
%%       be parsed in the `Tail'; `{done, Msg, Tail, State}' if the `Tail' contains
%%       incomplete message; `{skip, Tail, State}' to continue parsing `Tail'
%%       without calling `Consumer' callback on the last parsed message; `false' if
%%       the `Data' should be ignored without calling `Consumer' callback.
%%   </dd>
%% <dt>pstate</dt>
%%   <dd>Initial value of the parser state.</dd>
%% </dl>

-export_type([consumer/0, options/0]).

-record(state, {
      consumer      :: consumer()
    , tref          :: reference()
    , fd            :: port()
    , pos = 0       :: integer() | eof
    , end_pos       :: integer() | eof | undefined  % Read until this pos (inclusive)
    , max_size      :: integer()
    , timeout       :: integer()
    , chunk  = <<>> :: binary()
    , parser        :: {atom(), atom()} |
                       fun((binary(), any()) -> {any(), binary(), any()})
    , pstate        :: any()
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
    when is_atom(RegName), is_list(File), is_function(Consumer,2), is_list(Options) ->
    gen_server:start_link({local, RegName}, ?MODULE, [File, Consumer, Options], []).

-spec start_link(string(), consumer(), options()) ->
    {ok, pid()} | ignore | {error, any()}.
start_link(File, Consumer, Options)
    when is_list(File), is_function(Consumer,2), is_list(Options) ->
    gen_server:start_link(?MODULE, [File, Consumer, Options], []).

%%-----------------------------------------------------------------------------
%% @doc Start the server outside of supervision tree.
%% @end
%%-----------------------------------------------------------------------------
-spec start(atom(), string(), consumer(), options()) ->
    {ok, pid()} | {error, any()}.
start(RegName, File, Consumer, Options)
    when is_atom(RegName), is_list(File), is_function(Consumer,2), is_list(Options) ->
    gen_server:start({local, RegName}, ?MODULE, [File, Consumer, Options], []).

-spec start(string(), consumer(), options()) ->
    {ok, pid()} | {error, any()}.
start(File, Consumer, Options)
    when is_list(File), is_function(Consumer,2), is_list(Options) ->
    gen_server:start(?MODULE, [File, Consumer, Options], []).

%%-----------------------------------------------------------------------------
%% @doc Report last processed file position/size.
%% @end
%%-----------------------------------------------------------------------------
-spec position(pid() | atom()) -> {ok, Position::integer()}.
position(Pid) ->
    gen_server:call(Pid, position).

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
    % If Parser is defined it can be {M,F} or fun/2. That function fill be
    % called on each incoming packet:
    %   M:F(Packet, ParserState) -> {Data, Pos, NewParserState}
    {ok,FD} = file:open(File, [read,raw,binary,read_ahead]),
    {ok,End}= file:position(FD, eof),
    Offset  = case proplists:get_value(pos, Options, 0) of
              N when is_integer(N) -> N;
              eof                  -> End;
              Other1               -> throw({invalid_option, {pos, Other1}})
              end,
    EndPos  = case proplists:get_value(end_pos,  Options) of
              M when is_integer(M) -> M;
              eof                  -> eof;
              undefined            -> undefined;
              Other2               -> throw({invalid_option, {end_pos, Other2}})
              end,
    MaxSize = proplists:get_value(max_size, Options, ?MAX_READ_SIZE),
    Parser  = proplists:get_value(parser,   Options),
    PState  = proplists:get_value(pstate,   Options),
    State   = #state{consumer=Consumer, fd=FD, parser=Parser, pstate=PState,
                     pos=Offset, end_pos=EndPos, max_size=MaxSize},
    {ok, State}.

%%-----------------------------------------------------------------------------
%% @doc Process file from given position `Pos' to `EndPos' (or `eof').
%% @end
%%-----------------------------------------------------------------------------
-spec run(#state{}) -> #state{}.
run(#state{fd=FD, pos=Pos, end_pos=EndPos} = S) ->
    case file:position(FD, eof) of
    {ok, CurEnd} when Pos < CurEnd ->
        End = if is_integer(EndPos) -> min(CurEnd, EndPos); true -> CurEnd end,
        %% Did we reach the end?
        case min(End - Pos, S#state.max_size) of
        Size when Size > 0 ->
            {ok, Data} = file:pread(FD, Pos, Size),
            process_chunk(Data, Size, S);
        _ when is_integer(EndPos), Pos >= EndPos ->
            % Reached the requested and of input - notify consumer:
            PS1 = (S#state.consumer)({'$end_of_file', self(), CurEnd}, S#state.pstate),
            S#state{pstate = PS1};
        _ ->
            S
        end;
    {ok, CurEnd} when % Pos >= CurEnd andalso
                      ((is_integer(EndPos) andalso Pos >= EndPos) orelse
                       (EndPos =:= eof)) ->
        % Reached the requested end of input - notify consumer:
        PS1 = (S#state.consumer)({'$end_of_file', self(), CurEnd}, S#state.pstate),
        S#state{pstate = PS1};
    {ok, _CurEnd} ->
        S
    end.

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
        {ok, State} = init(File, Consumer, Options),
        Timeout     = proplists:get_value(timeout,Options, 1000),
        TRef        = erlang:send_after(Timeout, self(), check_md_files_timer),

        {ok, State#state{tref=TRef, timeout=Timeout}}
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
    {reply, {ok,Pos}, State};
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
handle_info(check_md_files_timer, #state{end_pos=End} = State) ->
    State1 = run(State),
    if is_integer(End) andalso State1#state.pos >= End ->
        {stop, normal, State1};
    true ->
        Ref = erlang:send_after(State#state.timeout, self(), check_md_files_timer),
        {noreply, State1#state{tref=Ref}}
    end;
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

process_chunk(<<>>, _Size, S) ->
    run(S);
process_chunk(Data, Size, #state{pos=Pos, parser=P, chunk=C, pstate=PS} = S) ->
    case parse(P, join(C, Data), PS) of
    {more, Msg, <<>>, PS1} ->
        PS2 = (S#state.consumer)(Msg, PS1),
        N = byte_size(Data),
        run(S#state{pos=Pos + N, pstate=PS2});
    {more, Msg, Tail, PS1} ->
        PS2 = (S#state.consumer)(Msg, PS1),
        N = byte_size(Data) - byte_size(Tail),
        process_chunk(Tail, Size, S#state{pos=Pos + N, pstate=PS2});
    {done, Msg, Tail, PS1} ->
        PS2 = (S#state.consumer)(Msg, PS1),
        N = byte_size(Data) - byte_size(Tail),
        run(S#state{pos=Pos + N, pstate=PS2});
    {skip, Tail, PS1} ->
        N = byte_size(Data) - byte_size(Tail),
        process_chunk(Tail, Size, S#state{pos=Pos + N, pstate=PS1});
    false ->
        S
    end.

parse({M,F}, Data, State) ->
    M:F(Data, State);
parse(Fun, Data, State) when is_function(Fun, 2) ->
    Fun(Data, State);
parse(undefined, Data, State) ->
    {done, Data, <<>>, State}.

join(<<>>, Bin) -> Bin;
join(Head, Bin) -> <<Head/binary, Bin/binary>>.

%%%----------------------------------------------------------------------------
%%% Test functions
%%%----------------------------------------------------------------------------

-ifdef(EUNIT).

async_read_file_test() ->
    File  = "/tmp/test-file-reader.log",
    Self  = self(),
    N     = 10,
    file:delete(File),

    %% Producer of data
    Loop  = fun
        Writer(0,_F) -> ok;
        Writer(I, F) ->
            Bin = <<"abc", (integer_to_binary(I))/binary, $\n>>,
            ok  = file:write(F, Bin),
            timer:sleep(50),
            Writer(I-1, F)
    end,
    spawn_link(fun() ->
        Res  = file:open(File, [write,raw,binary]),
        Self ! continue,
        ?assertMatch({ok,_}, Res),
        {ok, F} = Res,
        Loop(N, F)
    end),

    receive continue -> ok end,

    %% Consumer of data
    User = spawn_link(fun() -> ?assertEqual(ok, wait(N)), Self ! done end),

    Prsr = fun(Bin, State) ->
        [Msg, Tail] = binary:split(Bin, <<"\n">>),
        {more, Msg, Tail, State}
    end,
    Opts      = [{timeout, 100}, {parser, Prsr}],
    Consume   = fun(Msg, State) -> User ! Msg, State end,
    {ok, Pid} = start_link(File, Consume, Opts),

    ?assertEqual(done, receive done -> done after 1000 -> timeout end),
    ?assertEqual(ok, stop(Pid)),
    file:delete(File).

wait(0) ->
    ok;
wait(I) ->
    Expect = <<"abc", (integer_to_binary(I))/binary>>,
    receive
    Expect ->
        wait(I-1);
    Other ->
        {unexpected_msg, Other}
    after 1000 ->
        timeout
    end.

consume({'$end_of_file', _Reader, Pos}, PState) when is_integer(PState), is_integer(Pos) ->
    put(count_end, get(count_end)+1),
    put(end_pos, Pos),
    PState;
consume(Msg, PState) when is_integer(PState) ->
    Bin = <<"abc", (integer_to_binary(PState))/binary>>,
    ?assertEqual(Msg, Bin),
    PState-1.

sync_read_file_test() ->
    File  = "/tmp/test-file-reader.log",
    Self  = self(),
    N     = 10,
    file:delete(File),

    %% Produce data
    Loop  = fun
        Writer(0,_F) -> ok;
        Writer(I, F) ->
            Bin = <<"abc", (integer_to_binary(I))/binary, $\n>>,
            ok  = file:write(F, Bin),
            Writer(I-1, F)
    end,
    spawn_link(fun() ->
        Res  = file:open(File, [write,raw,binary]),
        ?assertMatch({ok,_}, Res),
        {ok, F} = Res,
        Loop(N, F),
        Self ! continue
    end),

    %% Wait until the file is ready
    receive continue -> ok end,

    %% Consume data

    Prsr = fun(Bin, PState) ->
        [Msg, Tail] = binary:split(Bin, <<"\n">>),
        {more, Msg, Tail, PState}
    end,

    put(count_end, 0),

    %% (1) Initialize file reader
    (fun() ->
        {ok, State} = init(File, fun consume/2, [{parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(0, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    %% (2) Now test reading from the end of file
    (fun() ->
        {ok, State} = init(File, fun consume/2, [{pos, eof}, {parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(10, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    %% (3) Now test reading from the 6th position
    (fun() ->
        {ok, State} = init(File, fun consume/2, [{pos, 6}, {parser, Prsr}, {pstate, N-1}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(0, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    %% (4) Now test reading only 11 bytes: <<"abc10\nabc9\n">>
    (fun() ->
        {ok, State} = init(File, fun consume/2, [{end_pos, 11}, {parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(8, State1#state.pstate),
        %% Close file reader
        close(State1)
    end)(),

    ?assertEqual(1,  get(count_end)),
    ?assertEqual(51, get(end_pos)),

    %% (5) Now test reading from the end of file till the end of file
    (fun() ->
        {ok, State} = init(File, fun consume/2, [{pos, eof}, {end_pos, eof}, {parser, Prsr}, {pstate, N}]),
        %% Execute file reader synchronously
        State1 = run(State),
        ?assertEqual(51, State1#state.pos),
        ?assertEqual(eof,State1#state.end_pos),
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
