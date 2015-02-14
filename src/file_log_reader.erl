%%% vim:ts=4:sw=4:et
%%%----------------------------------------------------------------------------
%%% @doc Periodically read a constantly growing file and parse newly added data
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2015 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2015-02-12
%%%----------------------------------------------------------------------------
-module(file_log_reader).
-author('saleyn@gmail.com').

-behaviour(gen_server).

%% API
-export([start_link/3, start/3, stop/1, position/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type consumer() :: fun((Msg::any()) -> ok).
-type options()  :: [
    {timeout, integer()} |
    {parser, fun((binary(), ParserState::any()) ->
                {more|done, Data::any(), Tail::binary(), NewParserState::any()} |
                {skip, Tail::binary(), NewParserState::any()} |
                false) |
             {Mod::atom(), Fun::atom()}} |
    {pstate, any()}
].

-export_type([consumer/0, options/0]).

-record(state, {
      consumer      :: consumer()
    , tref          :: reference()
    , fd            :: port()
    , offset        :: integer()
    , timeout       :: integer()
    , chunk = <<>>  :: binary()
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
-spec start_link(string(), consumer(), options()) ->
    {ok, pid()} | ignore | {error, any()}.
start_link(File, Consumer, Options) when is_function(Consumer, 1) ->
    gen_server:start_link(?MODULE, [File, Consumer, Options], []).

%%-----------------------------------------------------------------------------
%% @doc Start the server outside of supervision tree.
%% @end
%%-----------------------------------------------------------------------------
-spec start(string(), consumer(), options()) ->
    {ok, pid()} | {error, any()}.
start(File, Consumer, Options) when is_function(Consumer, 1) ->
    gen_server:start(?MODULE, [File, Consumer, Options], []).

%%-----------------------------------------------------------------------------
%% @doc Report last processed file position/size.
%% @end
%%-----------------------------------------------------------------------------
-spec position(pid()) -> {ok, Position::integer()}.
position(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, position).

%%-----------------------------------------------------------------------------
%% @doc Stop the server.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).

%%%----------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @spec (Args) -> {ok, State} | {ok, State, Timeout} |
%%                 ignore | {stop, Reason}
%% @doc Initiates the server
%% @end
%%-----------------------------------------------------------------------------
-spec init(list()) ->
    {ok, #state{}} |
    {ok, #state{}, Timeout :: integer() | hibernate} | ignore | {stop, any()}.
init([File, Consumer, Options]) ->
    %process_flag(trap_exit, true),
    try
        % If Parser is defined it can be {M,F} or fun/2. That function fill be
        % called on each incoming packet:
        %   M:F(Packet, ParserState) -> {Data, Offset, NewParserState}
        Timeout = proplists:get_value(timeout,Options, 1000),
        Parser  = proplists:get_value(parser, Options),
        PState  = proplists:get_value(pstate, Options),
        %Symbols = [atom_to_binary(S, latin1)
        %            || S <- proplists:get_value(symbols, Options, [])],
        %Symbols =:= [] andalso throw({undefined_required_option, symbols}),
        {ok,FD} = file:open(File, [read,raw,binary,read_ahead]),
        TRef    = erlang:send_after(Timeout, self(), check_md_files_timer),
        {ok, #state{consumer=Consumer, tref=TRef, fd=FD,
                    offset=0, parser=Parser, pstate=PState, timeout=Timeout}}
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
handle_call(position, _From, #state{offset = Offset} = State) ->
    {reply, {ok,Offset}, State};
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
handle_info(check_md_files_timer, #state{timeout=Timeout} = State) ->
    State1 = process_file(State),
    TRef   = erlang:send_after(Timeout, self(), check_md_files_timer),
    {noreply, State1#state{tref=TRef}};
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

process_file(#state{fd=FD, offset=Offset} = S) ->
    case file:position(FD, eof) of
    {ok, Offset} ->
        S;
    {ok, Pos} when Pos > Offset ->
        Size = Pos - Offset,
        {ok, Data} = file:pread(FD, Offset, Size),
        process_chunk(Data, Size, S)
    end.

process_chunk(Data, Size, #state{offset=Offset, parser=P, chunk=C, pstate=PS} = S) ->
    case parse(P, join(C, Data), PS) of
    {more, Msg, <<>>, PS1} ->
        (S#state.consumer)(Msg),
        N = byte_size(Data),
        process_file(S#state{offset=Offset + N, pstate=PS1});
    {more, Msg, Tail, PS1} ->
        (S#state.consumer)(Msg),
        N = byte_size(Data) - byte_size(Tail),
        process_chunk(Tail, Size, S#state{offset=Offset + N, pstate=PS1});
    {done, Msg, Tail, PS1} ->
        (S#state.consumer)(Msg),
        N = byte_size(Data) - byte_size(Tail),
        process_file(S#state{offset=Offset + N, pstate=PS1});
    {skip, Tail, PS1} ->
        N = byte_size(Data) - byte_size(Tail),
        process_chunk(Tail, Size, S#state{offset=Offset + N, pstate=PS1});
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

read_file_test() ->
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
    OnMsg     = fun(Msg) -> User ! Msg end,
    {ok, Pid} = start_link(File, OnMsg, Opts),

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
        
-endif.

%%%.
%%% vim: set filetype=erlang tabstop=4 foldmarker=%%%',%%%. foldmethod=marker:
