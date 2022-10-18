-module(mongoose_c2s_socket).

-include("mongoose_logger.hrl").

-export([new/3,
         tcp_to_tls/2,
         handle_socket_data/2,
         activate_socket/1,
         close/1,
         send_xml/2,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         is_ssl/1]).

-export([get_transport/1,
         get_conn_type/1,
         get_ip/1,
         get_ranch_ref/1]).

-callback new(term(), mongoose_listener:options()) -> state().
-callback peername(state()) -> {inet:ip_address(), inet:port_number()}.
-callback tcp_to_tls(state(), mongoose_listener:options()) ->
  {ok, state()} | {error, term()}.
-callback handle_socket_data(state(), {tcp | ssl, term(), iodata()}) ->
  iodata() | {raw, [term()]} | {error, term()}.
-callback activate_socket(state()) -> ok.
-callback close(state()) -> ok.
-callback mode() -> iodata | xml.
-callback socket_send(state(), exml:element() | iodata()) -> ok | {error, term()}.
-callback has_peer_cert(state(), mongoose_listener:options()) -> boolean().
-callback is_channel_binding_supported(state()) -> boolean().
-callback is_ssl(state()) -> boolean().

-record(c2s_socket, {
          module :: module(),
          state :: state()
         }).
-type socket() :: #c2s_socket{}.
-type state() :: term().
-export_type([socket/0, state/0]).

%%%----------------------------------------------------------------------
%%% socket helpers
%%%----------------------------------------------------------------------

-spec new(module(), term(), mongoose_listener:options()) -> socket().
new(Module, SocketOpts, LOpts) ->
    State = Module:new(SocketOpts, LOpts),
    PeerIp = Module:peername(State),
    verify_ip_is_not_blacklisted(PeerIp),
    C2SSocket = #c2s_socket{
        module = Module,
        state = State},
    handle_socket_and_ssl_config(C2SSocket, LOpts).

handle_socket_and_ssl_config(
  C2SSocket, #{tls := #{mode := tls}} = LOpts) ->
    case tcp_to_tls(C2SSocket, LOpts) of
        {ok, TlsC2SSocket} ->
            activate_socket(TlsC2SSocket),
            TlsC2SSocket;
        {error, closed} ->
            throw({stop, {shutdown, tls_closed}});
        {error, timeout} ->
            throw({stop, {shutdown, tls_timeout}});
        {error, {tls_alert, TlsAlert}} ->
            throw({stop, TlsAlert})
    end;
handle_socket_and_ssl_config(C2SSocket, _Opts) ->
    activate_socket(C2SSocket),
    C2SSocket.

-spec tcp_to_tls(socket(), mongoose_listener:options()) -> {ok, socket()} | {error, term()}.
tcp_to_tls(#c2s_socket{module = Module, state = State} = C2SSocket, LOpts) ->
    case Module:tcp_to_tls(State, LOpts) of
        {ok, NewState} ->
            {ok, C2SSocket#c2s_socket{state = NewState}};
        Error ->
            Error
        end.

verify_ip_is_not_blacklisted(PeerIp) ->
    case mongoose_hooks:check_bl_c2s(PeerIp) of
        true ->
            ?LOG_INFO(#{what => c2s_blacklisted_ip, ip => PeerIp,
                        text => <<"Connection attempt from blacklisted IP">>}),
            throw({stop, {shutdown, ip_blacklisted}});
        false ->
            ok
    end.

-spec handle_socket_data(socket(), {tcp | ssl, term(), iodata()}) ->
    iodata() | {raw, [term()]} | {error, term()}.
handle_socket_data(#c2s_socket{module = Module, state = State},
                   {Transport, _Socket, _Data} = Payload) when Transport == tcp; Transport == ssl ->
    Module:handle_socket_data(State, Payload);
handle_socket_data(_, _) ->
    {error, bad_packet}.

-spec activate_socket(socket()) -> ok | {error, term()}.
activate_socket(#c2s_socket{module = Module, state = State}) ->
    Module:activate_socket(State).

-spec close(socket()) -> ok.
close(#c2s_socket{module = Module, state = State}) ->
    Module:close(State).

-spec send_xml(socket(), exml:element()) -> ok | {error, term()}.
send_xml(#c2s_socket{module = Module, state = State}, Xml) ->
    case Module:mode() of
        iodata ->
            Text = exml:to_iolist(Xml),
            mongoose_metrics:update(global, [data, xmpp, sent, xml_stanza_size], iolist_size(Text)),
            Module:socket_send(State, Text);
        xml ->
            Module:socket_send(State, Xml)
    end.

%% 18 is OpenSSL's and fast_tls's error code for self-signed certs
-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(#c2s_socket{module = Module, state = State}, LOpts) ->
    Module:has_pert_cert(State, LOpts).

-spec is_channel_binding_supported(socket()) -> boolean().
is_channel_binding_supported(#c2s_socket{module = Module, state = State}) ->
    Module:is_channel_binding_supported(State).

-spec is_ssl(socket()) -> boolean().
is_ssl(#c2s_socket{module = Module, state = State}) ->
    Module:is_ssl(State).

-spec get_transport(socket()) -> term().
get_transport(_C2SSocket) ->
    ?MODULE.

-spec get_conn_type(socket()) -> term().
% get_conn_type(#c2s_socket{transport = ranch_tcp}) -> c2s;
% get_conn_type(#c2s_socket{transport = ranch_ssl}) -> c2s_tls;
% get_conn_type(#c2s_socket{transport = fast_tls}) -> c2s_tls.
get_conn_type(_C2SSocket) -> ?MODULE.

-spec get_ip(socket()) -> term().
get_ip(_C2SState) ->
    undefined.

-spec get_ranch_ref(socket()) -> term().
get_ranch_ref(_C2SState) ->
    undefined.
