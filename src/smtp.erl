%%%------------------------------------------------------------------------
%%% @doc SMTP mail client.  This module can sent emails to one or more
%%%      recipients, using primary/backup SMTP servers.  Messages can
%%%      contain attachments.
%%%
%%% ```
%%% Example:
%%%     % Send a message to two recipients with a file attachment using
%%%     % SSL protocol at mail server "mail.bevemyr.com":
%%%     smtp:send(ssl, "Alex <jb@bevemyr.com>",
%%%               ["katrin@bevemyr.com","jb@bevemyr.com"],
%%%               "Test Subject", "My Message",
%%%               [{server, "mail.bevemyr.com"},
%%%                {username, "alex"}, {password, "secret"},
%%%                {attachments, ["file1.txt"]}]).
%%%
%%%     % Send a message to a recipient with a file attachment given custom
%%%     % MIME type using localhost mail server
%%%     smtp:send(tcp, "jb@bevemyr.com",
%%%               ["katrin@bevemyr.com"], "Test Subject", "My Message",
%%%               [{server, "mail.bevemyr.com"},
%%%                {username, "alex"}, {password, "secret"},
%%%                {attachments, [{"file1.bin","application/custom_MIME"}]}]).
%%%
%%%     % Send a message to two recipients with an attachment given as list
%%%     smtp:send(tcp, "jb@bevemyr.com",
%%%               ["katrin@bevemyr.com","jb@bevemyr.com"],
%%%               "Test Subject", "My Message",
%%%               [{"file1.txt","text/plain","Attachment past as list"}]).
%%% '''
%%%
%%% @author  Johan Bevemyr, Serge Aleynikov <saleyn@gmail.com>
%%% @end
%%%------------------------------------------------------------------------
%%% Created 02/24/2004 Johan Bevemyr
%%%------------------------------------------------------------------------
-module(smtp).
-author('jb@son.bevemyr.com').
-author('saleyn@gmail.com').

-export([send/5, send/6, domain/0]).

-include_lib("kernel/include/inet.hrl").

-type proto() :: tcp | ssl.
%% Protocol type.

-type smtp_options() :: [
          {server, Server::string()}
        | {relay, Relay::string()}
        | {port, Port::integer()}
        | {auth, Auth :: always | never}
        | {username, Username::string()}
        | {password, Password::string()}
        | {tls, Tls :: always | if_available}
        | {domain, Domain::string()}
        | {timeout, Millisec::integer()}
        | {verbose, debug}
        | {attachments, [
             Filename::string() |
             {Filename::string(), ContentType::string()} |
             {Filename::string(), ContentType::string(), Data::list()}]}].
%% SNMP Options
%%     <ul>
%%      <li>Server - server to connect to (no MX lookup)</li>
%%      <li>Relay  - domain to do MX lookup of list of servers</li>
%%      <li>Port   - optional port number (ssl def: 465; tcp def: 25)</li>
%%      <li>Auth   - controls mandatory / optional authentication</li>
%%      <li>Tls    - controls enabling of TLS protocol</li>
%%      <li>Domain - name of the domain to include in the HELO handshake</li>
%%      <li>Timeout - timeout to use (default 10000)</li>
%%      <li>Verbose - controls debugging printout</li>
%%      <li>Attachments - list of files to attach</li>
%%     </ul>

%%-------------------------------------------------------------------------
%% @doc Send a message to a list of `To' receipients using `localhost'.
%%      Error is thrown if unable to send a message.
%%      Use inet:format_error/1 to decode the Reason if it is an atom.
%% @end
%%-------------------------------------------------------------------------
-spec send(Proto :: proto(), From :: string() | binary(),
            To :: string() | binary(), Subj :: string() | binary(),
            Msg :: string() | binary()) -> ok.
send(Proto, From, To, Subject, Message) ->
    send(Proto, From, To, Subject, Message, []).

%%-------------------------------------------------------------------------
%% @doc Send a message to a list of recipients by connecting to an SMTP
%%      server Server.  The message can contain attachments in the
%%      Attachments list.  See examples on the top of this page.
%%      Error is thrown if unable to send a message.
%% @end
%%-------------------------------------------------------------------------
-spec send(Proto :: proto(), From :: string() | binary(),
            To :: string() | binary(), Subj :: string() | binary(),
            Msg :: string() | binary(), Opts :: smtp_options()) -> ok.
send(Proto, From, To, Subj, Msg, Opts)
  when Proto =:= tcp; Proto =:= ssl ->
    Module = proto_module(Proto),
    case proplists:get_value(server, Opts) of
    undefined ->
        case proplists:get_value(relay, Opts) of
        undefined ->
            try_send(Module, From, To, Subj, Msg, "localhost", Opts);
        Domain ->
            Servers = mxlookup(Domain),
            send_mail(Module, Servers, {From, To, Subj, Msg},
                no_servers_provided, Opts)
        end;
    [I | _] = Server when is_integer(I) ->
        try_send(Module, From, To, Subj, Msg, Server, Opts);
    Servers when is_list(Servers) ->
        send_mail(Module, Servers, {From, To, Subj, Msg},
            no_servers_provided, Opts)
    end.


%%-------------------------------------------------------------------------
%% @doc Get domain that this host belongs to.
%% @end
%%-------------------------------------------------------------------------
-spec domain() -> binary().
domain() ->
    case lists:keyfind(domain, 1, inet:get_rc()) of
    {domain, D} when is_binary(D) -> D;
    {domain, D} when is_list(D)   -> list_to_binary(D);
    false -> 
        {ok, Hostname} = inet:gethostname(),
        {ok, #hostent{h_name = FQDN}} = inet:gethostbyname(Hostname),
        list_to_binary(FQDN)
    end.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

proto_module(tcp) -> gen_tcp;
proto_module(ssl) -> ssl.

mxlookup(Domain) ->
    case whereis(inet_db) of
    P when is_pid(P)    -> ok;
    _                   -> inet_db:start()
    end,
    case lists:keyfind(nameserver, 1, inet_db:get_rc()) of
    false ->
        % we got no nameservers configured, suck in resolv.conf
        inet_config:do_load_resolv(os:type(), longnames);
    _ ->
        ok
    end,
    case inet_res:lookup(Domain, in, mx) of
    [] -> [];
    L  -> [H || {_, H} <- lists:sort(L)]
    end.

try_send(Module, From, To, Subj, Msg, Server, Opts) ->
    Verbose = proplists:get_value(verbose, Opts),
    Attachments = proplists:get_value(attachments, Opts, []),
    Port = smtp_init(Module, Server, From, To, Verbose, Opts),
    Boundary=boundary_bin(Attachments),
    smtp_send_headers(Module, Port, From, To, Subj, Boundary),
    smtp_send_message(Module, Port, Msg, Boundary),
    smtp_send_attachments(Module, Port, Attachments, Boundary),
    smtp_close(Module, Port).

send_mail(_Mod, [], _What, LastReason, _Options) ->
    throw(LastReason);
send_mail(Mod, [S | Rest], {From, To, Subj, Msg} = What, _LastReason, Options) ->
    try
        ok = try_send(Mod, From, To, Subj, Msg, S, Options)
    catch
        _:Reason when is_atom(Reason) ->
            % This is likely a connection error
            send_mail(Mod, Rest, What, Reason, Options);
        _:_ ->
            % This is the case when a server couldn't send the message due to
            % other than networking reasons.  Don't retry.
            throw
    end.

smtp_send_headers(Mod, Port, From, To, Subject, Boundary) ->
    CommonHeaders = [mail_headers(<<"To: ">>,      [list_to_binary(T) || T <- To]),
                     mail_header (<<"From: ">>,    list_to_binary(From)),
                     mail_header (<<"Subject: ">>, list_to_binary(Subject))],
    Headers =
        case Boundary of
        undefined ->
            [mail_header(<<"Content-Type: ">>, <<"text/plain">>),
             mail_header(<<"Content-Transfer-Encoding: ">>, <<"8bit">>)];
        _ ->
            [mail_header(<<"Mime-Version: ">>, <<"1.0">>),
             mail_header(<<"Content-Type: ">>, [<<"Multipart/Mixed; boundary=\"">>,
                                                Boundary, <<"\"">>]),
             mail_header(<<"Content-Transfer-Encoding: ">>, <<"8bit">>)]
        end,
    Mod:send(Port, [CommonHeaders, Headers, <<"\r\n">>]).

smtp_send_message(Mod, Port, Data, Boundary) ->
    case Boundary of
    undefined ->
        ok;
    _ ->
        Mod:send(Port,
                 [<<"--">>,Boundary,<<"\r\n">>,
                  mail_header(<<"Content-Type: ">>, <<"Text/Plain; charset=us-ascii">>),
                  mail_header(<<"Content-Transfer-Encoding: ">>, <<"8bit">>),
                  <<"\r\n">>])
    end,
    {_LastNL, Escaped} = dot_escape(Data, true),
    Mod:send(Port, Escaped).

smtp_send_attachments(Mod, Port, [], _Boundary) ->
    Mod:send(Port, <<"\r\n.\r\n">>);
smtp_send_attachments(Mod, Port, Attachments, Boundary) ->
    send_attachments(Mod, Port, Boundary, Attachments),
    Mod:send(Port, <<"\r\n.\r\n">>).

send_attachments(Mod, Port, Boundary, []) ->
    Mod:send(Port, <<"\r\n--",(list_to_binary(Boundary))/binary,"--\r\n">>);

send_attachments(Mod, Port, Boundary, [{FileName,ContentType}|Rest]) ->
    Data =
        case file:read_file(FileName) of
        {ok, Bin} ->
            binary_to_list(Bin);
        {error, Reason} ->
            throw(lists:flatten(
                io_lib:format("File ~s: ~s", [FileName, file:format_error(Reason)])))
        end,
    send_attachment(Mod, Port, Boundary, FileName, ContentType, Data),
    send_attachments(Mod, Port, Boundary, Rest);

send_attachments(Mod, Port, Boundary, [{FileName,ContentType,Data}|Rest]) ->
    send_attachment(Mod, Port, Boundary, FileName, ContentType, Data),
    send_attachments(Mod, Port, Boundary, Rest);

send_attachments(Mod, Port, Boundary, [FileName | Rest]) ->
    send_attachments(Mod, Port, Boundary, [{FileName, undefined} | Rest]).

send_attachment(Mod, Port, Boundary, FileName, ContentType, Data) ->
    File = filename:basename(FileName,""),
    CT = case {ContentType, io_lib:printable_list(Data)} of
         {undefined, true}  -> "plain/text";
         {undefined, false} -> "application/octet-stream; name=\"" ++ File ++"\"";
         {_, _}             -> ContentType
         end,
    Mod:send(Port,
             [<<"\r\n--">>,Boundary,<<"\r\n">>,
              mail_header(<<"Content-Type: ">>, CT),
              mail_header(<<"Content-Transfer-Encoding: ">>, <<"base64">>),
              mail_header(<<"Content-Disposition: ">>,
                          [<<"attachment; filename=\"">>, list_to_binary(File), <<"\"">>]),
              <<"\r\n">>
             ]),
    B64 = sting2base64(Data),
    Mod:send(Port, B64).

def_port_and_opts(gen_tcp) ->
    {25, [{active, false}, {reuseaddr,true}, {packet, line}, binary]};
def_port_and_opts(ssl) ->
    {465,[{active, false}, {depth, 0}, {packet, line}, {ssl_imp, new}, binary]}.

connect(Mod, Server, Verbose, Options) ->
    {DefPort, SockOpts} = def_port_and_opts(Mod),
    % For ssl make sure applications crypto, public_key and ssl are started
    if is_port(Server) ->
        case Mod:connect(Server, SockOpts) of
        {ok, Sock}   -> Sock;
        {error, Why} -> throw(Why)
        end;
    true ->
        Port = proplists:get_value(port, Options, DefPort),
        Timeout = proplists:get_value(timeout, Options, 10000),
        print(Verbose, "Connecting to: ~w://~s:~w\n", [Mod, Server, Port]),
        case Mod:connect(Server, Port, SockOpts, Timeout) of
        {ok, Sock}   -> Sock;
        {error, Why} -> throw(Why)
        end
    end.

domain(Options) ->
    case proplists:get_value(domain, Options) of
    undefined ->
        domain();
    Domain when is_binary(Domain) ->
        Domain;
    Domain when is_list(Domain) ->
        list_to_binary(Domain)
    end.

smtp_STARTTLS(Mod, Port, Options, Extensions, Domain, Verbose) ->
    case {proplists:get_value(tls, Options), proplists:get_value(<<"STARTTLS">>, Extensions)} of
    {always, true} ->
        do_STARTTLS(Mod, Port, Extensions, Verbose, Options, Domain);
    {if_available, true} ->
        do_STARTTLS(Mod, Port, Extensions, Verbose, Options, Domain);
    {always, _} ->
        smtp_close(Mod, Port),
        throw({missing_requirement, tls});
    _ ->
        {Port, Extensions}
    end.

do_STARTTLS(Mod, Port, Extensions, Verbose, Options, Domain) ->
    smtp_put(Mod, <<"STARTTLS">>, Port),
    smtp_expect(Mod, <<"220">>, Port, undefined),
    case Mod of
    ssl ->
        {Port, Extensions};
    gen_tcp ->
        Sock = connect(Mod, Port, Verbose, Options),
        {ok, Extensions2} = try_HELO(Mod, Port, Domain),
        {Sock, Extensions2}
    end.

try_AUTH(Mod, Port, Options, Ext, _Verbose) when Ext =:= undefined; Ext =:= [] ->
    case proplists:get_value(auth, Options) of
    always ->
        smtp_close(Mod, Port),
        throw({missing_requirement, auth});
    _ ->
        {false, proplists:get_value(username, Options)}
    end;
try_AUTH(Mod, Port, Options, AuthTypes, Verbose) ->
    Username = proplists:get_value(username, Options),
    Password = proplists:get_value(password, Options),
    Auth     = proplists:get_value(auth,     Options),
    case Auth of
    never ->
        {false, Username};
    _ when Username =:= undefined ->
        throw({missing_auth, username});
    _ when Password =:= undefined ->
        throw({missing_auth, password});
    _ ->
        Types = [decode_auth(X) || X <- re:split(AuthTypes, " ", [{return, binary}, trim])],
        AllowedTypes = [X || X <- Types, is_atom(X)],
        case do_AUTH_each(Mod, Port, Username, Password, AllowedTypes, Verbose) of
        false when Auth =:= always ->
            smtp_close(Mod, Port),
            erlang:throw({permanent_failure, auth_failed});
        false ->
            {false, Username};
        true ->
            {true, Username}
        end
    end.

decode_auth(<<"CRAM-MD5">>) -> cram_md5;
decode_auth(<<"cram-md5">>) -> cram_md5;
decode_auth(<<"LOGIN">>)    -> login;
decode_auth(<<"login">>)    -> login;
decode_auth(<<"PLAIN">>)    -> plain;
decode_auth(<<"plain">>)    -> plain;
decode_auth(Other)          -> Other.

to_hex(Prefix, Bin) -> list_to_binary(Prefix ++ [to_hex_int(I) || <<I>> <= Bin]).
to_hex_int(I) when I < 10  -> $0 + I;
to_hex_int(I) when I < 16  -> $A + (I - 10);
to_hex_int(I) when I < 256 -> J = I div 256, [to_hex_int(J), to_hex_int(I - 256*J)].

do_AUTH_each(_Mod, _Port, _Username, _Password, [], _Verbose) ->
    false;
do_AUTH_each(Mod, Port, Username, Password, [cram_md5 | Tail], Verbose) ->
    smtp_put(Mod, <<"AUTH CRAM-MD5">>, Port),
    try
        {ok, Seed64} = smtp_expect(Mod, <<"334">>, Port, undefined),
        Seed = base64:decode_to_string(Seed64),
        Bin  = crypto:md5_mac(Password, Seed),
        Digest = to_hex([Username, " "], Bin),
        smtp_put(Mod, base64:encode(Digest), Port),
        smtp_expect(Mod, <<"235">>, Port, undefined),
        print(Verbose, "Authenticated using crom_md5\n", []),
        true
    catch _:_ ->
        do_AUTH_each(Mod, Port, Username, Password, Tail, Verbose)
    end;
do_AUTH_each(Mod, Port, Username, Password, [login | Tail], Verbose) ->
    smtp_put(Mod, <<"AUTH LOGIN">>, Port),
    try
        {ok, <<"VXNlcm5hbWU6", _/binary>>} = smtp_expect(Mod, <<"334">>, Port, undefined),
        U = base64:encode(Username),
        smtp_put(Mod, U, Port),
        {ok, <<"UGFzc3dvcmQ6", _/binary>>} = smtp_expect(Mod, <<"334">>, Port, undefined),
        P = base64:encode(Password),
        smtp_put(Mod, P, Port),
        smtp_expect(Mod, <<"235">>, Port, undefined),
        print(Verbose, "Authenticated using login\n", []),
        true
    catch _:_ ->
        do_AUTH_each(Mod, Port, Username, Password, Tail, Verbose)
    end;
do_AUTH_each(Mod, Port, Username, Password, [plain | Tail], Verbose) ->
    AuthString = base64:encode("\0"++Username++"\0"++Password),
    smtp_put(Mod, [<<"AUTH PLAIN ">>, AuthString], Port),
    try
        smtp_expect(Mod, <<"235">>, Port, undefined),
        print(Verbose, "Authenticated using plain\n", []),
        true
    catch _:_ ->
        do_AUTH_each(Mod, Port, Username, Password, Tail, Verbose)
    end.

try_HELO(Mod, Port, Domain) ->
    smtp_put(Mod, [<<"EHLO ">>, Domain], Port),
    try
        smtp_expect(Mod, <<"250">>, Port, undefined)
    catch throw:<<"500", _/binary>> ->
        smtp_put(Mod, [<<"HELO ">>, Domain], Port),
        smtp_expect(Mod, <<"250">>, Port, undefined)
    end.

smtp_init(Mod, Server, From, Recipients, Verbose, Options) ->
    Port = connect(Mod, Server, Verbose, Options),
    smtp_expect(Mod, <<"220">>, Port, "SMTP server does not respond"),
    Domain = domain(Options),
    {ok, Extensions} = try_HELO(Mod, Port, Domain),
    print(debug, "Extensions: ~p\n", [Extensions]),
    {Port2, Extensions2} = smtp_STARTTLS(Mod, Port, Options, Extensions, Domain, Verbose),
    {_Auth, Username} =
        try_AUTH(Mod, Port, Options, proplists:get_value(<<"AUTH">>, Extensions2), Verbose),
    FromEmail = format_email(Username, From),
    print(Verbose, "From email: ~p\n", [FromEmail]),
    smtp_put(Mod, [<<"MAIL FROM: ">>, FromEmail], Port2),
    smtp_expect(Mod, <<"250">>, Port2, undefined),
    send_recipients(Mod, Recipients, Port2),
    smtp_put(Mod, <<"DATA">>, Port2),
    smtp_expect(Mod, <<"354">>, Port2, "Message not accepted by mail server."),
    Port2.

smtp_close(Mod, Port) ->
    smtp_put(Mod, <<".">>, Port),
    smtp_expect(Mod, <<"250">>, Port, "Message not accepted by mail server."),
    Mod:close(Port),
    ok.

format_email(undefined, Default) -> format_email(Default);
format_email(Other, _Default)    -> format_email(Other).

format_email(Addr) when is_list(Addr) ->
    case lists:splitwith(fun(I) -> I =/= $< end, Addr) of
    {_, []} -> [list_to_binary([$< | Addr]), $>];
    {_, A}  -> list_to_binary(A)
    end;
format_email(Addr) when is_binary(Addr) ->
    case binary:match(Addr, <<"<">>) of
    {0, _}  -> Addr;
    {I, _}  -> binary:part(Addr, {I, byte_size(Addr) - I});
    nomatch -> [$<, Addr, $>]
    end.

send_recipients(Mod, To, Port) when is_binary(To) ->
    send_recipients2(Mod, [<<"RCPT TO: ">>, format_email(To)], Port);
send_recipients(Mod, [R|_] = Addr, Port) when is_integer(R) ->
    send_recipients2(Mod, [<<"RCPT TO: ">>, format_email(Addr)], Port);
send_recipients(Mod, List, Port) when is_list(List) ->
    [send_recipients2(Mod, [<<"RCPT TO: ">>, format_email(A)], Port) || A <- List],
    ok.

send_recipients2(Mod, Data, Port) ->
    smtp_put(Mod, Data, Port),
    smtp_expect(Mod, <<"250">>, Port, undefined).

smtp_put(Mod, Message, Port) ->
    Mod:send(Port, [Message,<<"\r\n">>]).

smtp_expect(Mod, Code, Port, ErrorMsg) ->
    smtp_expect(Mod, Code, Port, ErrorMsg, 0, []).
smtp_expect(Mod, Code, Port, ErrorMsg, N, Acc) when is_binary(Code) ->
    case Mod:recv(Port, 0, 15000) of
    {ok, <<RespCode:3/binary, C, Rest/binary>> = Bin} when RespCode =:= Code ->
        case C of
        $  when Acc =:= [] ->
            {ok, trim_nl(Rest)};
        $  ->
            {ok, Acc};
        $- when N =:= 0 ->
            smtp_expect(Mod, Code, Port, ErrorMsg, N+1, Acc);
        $- ->
            ExtensionAcc = parse_extension(Rest, Acc),
            smtp_expect(Mod, Code, Port, ErrorMsg, N+1, ExtensionAcc);
        _ when ErrorMsg =:= undefined ->
            throw(Bin);
        _ ->
            throw(ErrorMsg)
        end;
    {ok, Other} when ErrorMsg =:= undefined ->
        throw(Other);
    {ok, _Other} ->
        throw(ErrorMsg);
    {error, Reason} ->
        throw(lists:flatten(inet:format_error(Reason)))
    end.

parse_extension(Bin, Acc) ->
    case binary:match(Bin, <<" ">>) of
    {I, _} ->
        <<E:I/binary, $ , Args/binary>> = Bin,
        [{to_upper(E), trim_nl(Args)} | Acc];
    nomatch ->
        case binary:match(Bin, <<"=">>) of
        nomatch ->
            [{to_upper(trim_nl(Bin)), true} | Acc];
        _ ->
            Acc
        end
    end.

to_upper(Bin)    -> to_upper(byte_size(Bin)-1, Bin).
to_upper(-1, Bin) -> Bin;
to_upper(I, Bin) ->
    case binary:at(Bin, I) of
    C when C >= $a, $z =< C ->
        list_to_binary(string:to_upper(binary_to_list(Bin)));
    _ ->
        to_upper(I-1, Bin)
    end.

trim_nl(Bin) ->
    case binary:match(Bin, [<<"\r">>, <<"\n">>]) of
    {I, _} ->
        binary:part(Bin, {0, I});
    nomatch ->
        Bin
    end.

print(debug, Fmt, Args) ->
    io:format(Fmt, Args);
print(_, _Fmt, _Args) ->
    ok.

%% Add an . at all lines starting with a dot.

dot_escape(Data, NL) ->
    dot_escape(Data, NL, []).

dot_escape([], NL, Acc) ->
    {NL, lists:reverse(Acc)};
dot_escape([$.|Rest], true, Acc) ->
    dot_escape(Rest, false, [$.,$.|Acc]);
dot_escape([$\n|Rest], _, Acc) ->
    dot_escape(Rest, true, [$\n|Acc]);
dot_escape([C|Rest], _, Acc) ->
    dot_escape(Rest, false, [C|Acc]).

%%

sting2base64(String) ->
    sting2base64(String, []).

sting2base64([], Acc) ->
    lists:reverse(Acc);
sting2base64(String, Acc) ->
    case str2b64_line(String, []) of
    {ok, Line, Rest} ->
        sting2base64(Rest, ["\n",Line|Acc]);
    {more, Cont} ->
        lists:reverse(["\n",str2b64_end(Cont)|Acc])
    end.

%%

str2b64_line(S, [])           -> str2b64_line(S, [], 0);
str2b64_line(S, {Rest,Acc,N}) -> str2b64_line(Rest ++ S, Acc, N).

str2b64_line(S, Out, 76) -> {ok,lists:reverse(Out),S};
str2b64_line([C1,C2,C3|S], Out, N) ->
    O1 = e(C1 bsr 2),
    O2 = e(((C1 band 16#03) bsl 4) bor (C2 bsr 4)),
    O3 = e(((C2 band 16#0f) bsl 2) bor (C3 bsr 6)),
    O4 = e(C3 band 16#3f),
    str2b64_line(S, [O4,O3,O2,O1|Out], N+4);
str2b64_line(S, Out, N) ->
    {more,{S,Out,N}}.

%%

str2b64_end({[C1,C2],Out,_N}) ->
    O1 = e(C1 bsr 2),
    O2 = e(((C1 band 16#03) bsl 4) bor (C2 bsr 4)),
    O3 = e( (C2 band 16#0f) bsl 2 ),
    lists:reverse(Out, [O1,O2,O3,$=]);
str2b64_end({[C1],Out,_N}) ->
    O1 = e(C1 bsr 2),
    O2 = e((C1 band 16#03) bsl 4),
    lists:reverse(Out, [O1,O2,$=,$=]);
str2b64_end({[],Out,_N}) -> lists:reverse(Out);
str2b64_end([])          -> [].

%%

boundary_bin([]) ->
    undefined;
boundary_bin(_) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    <<"Boundary_(", (list_to_binary(random_list(10)))/binary, ")">>.

random_list(0) -> [];
random_list(N) -> [64+random:uniform(25), 96+random:uniform(25) | random_list(N-1)].

%%

e(X) when X >= 0,  X < 26 -> X + $A;
e(X) when X >= 26, X < 52 -> X + $a - 26;
e(X) when X >= 52, X < 62 -> X + $0 - 52;
e(62) -> $+;
e(63) -> $/;
e(X)  -> erlang:error({badchar,X}).

%%
mail_headers(_Key, [])   -> [];
mail_headers(Key, [H|T]) -> [mail_header(Key, H) | mail_headers(Key, T)].

mail_header(_Key, [])    -> [];
mail_header(Key, Val)    -> [Key, Val, <<"\r\n">>].
