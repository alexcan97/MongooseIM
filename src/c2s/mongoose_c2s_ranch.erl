-module(mongoose_c2s_ranch).
-behaviour(mongoose_c2s_socket).

-export([new/2,
         peername/1,
         tcp_to_tls/2,
         handle_socket_data/2,
         activate_socket/1,
         close/1,
         send_text/2,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         is_ssl/1]).

-record(state, {
          transport :: transport(),
          ranch_ref :: ranch:ref(),
          socket :: ranch_transport:socket(),
          ip :: {inet:ip_address(), inet:port_number()}
         }).

-type state() :: #state{}.
-type transport() :: ranch_tcp | ranch_ssl | fast_ssl.

-spec new(term(), mongoose_listener:options()) -> state().
new({ranch_tcp, RanchRef}, #{proxy_protocol := true}) ->
    {ok, #{src_address := PeerIp, src_port := PeerPort}} = ranch:recv_proxy_header(RanchRef, 1000),
    {ok, TcpSocket} = ranch:handshake(RanchRef),
    #state{
        transport = ranch_tcp,
        ranch_ref = RanchRef,
        socket = TcpSocket,
        ip = {PeerIp, PeerPort}};
new({ranch_tcp, RanchRef}, #{proxy_protocol := false}) ->
    {ok, TcpSocket} = ranch:handshake(RanchRef),
    {ok, Ip} = ranch_tcp:peername(TcpSocket),
    #state{
        transport = ranch_tcp,
        ranch_ref = RanchRef,
        socket = TcpSocket,
        ip = Ip}.

-spec peername(state()) -> {inet:ip_address(), inet:port_number()}.
peername(#state{ip = Ip}) ->
    Ip.

-spec tcp_to_tls(state(), mongoose_listener:options()) ->
  {ok, state()} | {error, term()}.
tcp_to_tls(#state{socket = TcpSocket} = State, #{tls := #{module := TlsMod, opts := TlsOpts}}) ->
    ranch_tcp:setopts(TcpSocket, [{active, false}]),
    case tcp_to_tls(TlsMod, TcpSocket, TlsOpts) of
        {ok, TlsSocket} ->
            {ok, State#state{transport = TlsMod, socket = TlsSocket}};
        {error, Reason} ->
            {error, Reason}
    end.

tcp_to_tls(fast_tls, TcpSocket, TlsOpts) ->
    case fast_tls:tcp_to_tls(TcpSocket, TlsOpts) of
        {ok, TlsSocket} ->
            fast_tls:recv_data(TlsSocket, <<>>),
            {ok, TlsSocket};
        Other -> Other
    end;
tcp_to_tls(just_tls, TcpSocket, TlsOpts) ->
    case ranch_ssl:handshake(TcpSocket, TlsOpts, 1000) of
        {ok, TlsSocket, _} -> {ok, TlsSocket};
        Other -> Other
    end.

-spec handle_socket_data(state(), {tcp | ssl, term(), iodata()}) ->
  iodata() | {raw, [term()]} | {error, term()}.
handle_socket_data(#state{transport = fast_tls, socket = TlsSocket}, {tcp, _Socket, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, encrypted_size], iolist_size(Data)),
    case fast_tls:recv_data(TlsSocket, Data) of
        {ok, DecryptedData} ->
            DecryptedData;
        {error, Reason} ->
            {error, Reason}
    end;
handle_socket_data(#state{transport = ranch_ssl, socket = Socket}, {ssl, Socket, Data}) ->
    mongoose_metrics:update(global, [data, xmpp, received, encrypted_size], iolist_size(Data)),
    Data;
handle_socket_data(#state{transport = ranch_tcp, socket = Socket}, {tcp, Socket, Data}) ->
    Data.

-spec activate_socket(state()) -> ok.
activate_socket(#state{transport = fast_tls, socket = Socket}) ->
    fast_tls:setopts(Socket, [{active, once}]);
activate_socket(#state{transport = ranch_ssl, socket = Socket}) ->
    ranch_ssl:setopts(Socket, [{active, once}]);
activate_socket(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]).

-spec close(state()) -> ok.
close(#state{transport = fast_tls, socket = Socket}) ->
    fast_tls:close(Socket);
close(#state{transport = ranch_ssl, socket = Socket}) ->
    ranch_ssl:close(Socket);
close(#state{transport = ranch_tcp, socket = Socket}) ->
    ranch_tcp:close(Socket).

-spec send_text(state(), iodata()) -> ok | {error, term()}.
send_text(#state{transport = fast_tls, socket = Socket}, Text) ->
    fast_tls:send(Socket, Text);
send_text(#state{transport = ranch_ssl, socket = Socket}, Text) ->
    ranch_ssl:send(Socket, Text);
send_text(#state{transport = ranch_tcp, socket = Socket}, Text) ->
    ranch_tcp:send(Socket, Text).

-spec has_peer_cert(mongoose_c2s_socket:state(), mongoose_listener:options()) -> boolean().
has_peer_cert(#state{transport = fast_tls, socket = Socket}, #{tls := TlsOpts}) ->
    case {fast_tls:get_verify_result(Socket), fast_tls:get_peer_certificate(Socket), TlsOpts} of
        {0, {ok, _}, _} -> true;
        {18, {ok, _}, #{verify_mode := selfsigned_peer}} -> true;
        {_, {ok, _}, _} -> false;
        {_, error, _} -> false
    end;
has_peer_cert(#state{transport = ranch_ssl, socket = Socket}, _) ->
    case ssl:peercert(Socket) of
        {ok, _PeerCert} -> true;
        _ -> false
    end;
has_peer_cert(#state{transport = ranch_tcp}, _) ->
    false.

-spec is_channel_binding_supported(mongoose_c2s_socket:state()) -> boolean().
is_channel_binding_supported(#state{transport = Transport}) ->
    fast_tls == Transport.

-spec is_ssl(mongoose_c2s_socket:state()) -> boolean().
is_ssl(#state{transport = Transport}) ->
    ranch_tcp /= Transport.
