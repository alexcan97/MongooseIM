-module(mongoose_stanza_helper).
-export([build_message/3]).
-export([build_message_with_headline/4]).
-export([ensure_id/1]).
-export([get_last_messages/4]).
-export([route/5]).

-include("jlib.hrl").
-include("mongoose_logger.hrl").

-spec build_message(From :: binary(), To :: binary(), Body :: binary()) -> exml:element().
build_message(From, To, Body) ->
    #xmlel{name = <<"message">>,
           attrs = add_id([{<<"type">>, <<"chat">>}, {<<"from">>, From}, {<<"to">>, To}]),
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Body}]}]
          }.

build_message_with_headline(From, To, Body, Subject) ->
    Children = maybe_cdata_elem(<<"subject">>, Subject) ++
               maybe_cdata_elem(<<"body">>, Body),
    Attrs = add_id([{<<"type">>, <<"headline">>}, {<<"from">>, From}, {<<"to">>, To}]),
    #xmlel{name = <<"message">>, attrs = Attrs, children = Children}.

ensure_id(Stanza) ->
    case get_id(Stanza) of
        null -> Stanza#xmlel{attrs = add_id(Stanza#xmlel.attrs)};
        _ -> Stanza
    end.

maybe_cdata_elem(_, null) -> [];
maybe_cdata_elem(_, <<>>) -> [];
maybe_cdata_elem(Name, Text) when is_binary(Text) ->
    [cdata_elem(Name, Text)].

cdata_elem(Name, Text) when is_binary(Name), is_binary(Text) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Text}]}.

-spec get_last_messages(Caller :: jid:jid(),
                        Limit :: non_neg_integer(),
                        With :: null | jid:jid(),
                        Before :: null | mod_mam:unix_timestamp()) -> {ok, map()}.
get_last_messages(Caller, Limit, With, Before) ->
    With2 = null_as_undefined(With),
    Before2 = null_as_undefined(Before), %% Before is in microseconds
    Limit2 = min(500, Limit),
    Rows = mongoose_stanza_api:lookup_recent_messages(Caller, With2, Before2, Limit2),
    Maps = lists:map(fun row_to_map/1, Rows),
    {ok, #{<<"stanzas">> => Maps, <<"limit">> => Limit2}}.

null_as_undefined(null) -> undefined;
null_as_undefined(Value) -> Value.

-spec row_to_map(mod_mam:message_row()) -> {ok, map()}.
row_to_map(#{id := MAMId, jid := From, packet := Msg}) ->
    {Microseconds, _} = mod_mam_utils:decode_compact_uuid(MAMId),
    Map = #{<<"sender">> => From, <<"timestamp">> => Microseconds,
            <<"stanza_id">> => get_id(Msg), <<"stanza">> => Msg},
    {ok, Map}.

-spec route(mongoosim:host_type(), jid:lserver(), From :: jid:jid(), To :: jid:jid(),
            Packet :: exml:element()) ->
    {ok, map()} | {error, term()}.
route(HostType, LServer, From, To, Packet) ->
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             host_type => HostType,
                             lserver => LServer,
                             element => Packet}),
    Acc1 = mongoose_hooks:user_send_packet(Acc, From, To, Packet),
    ejabberd_router:route(From, To, Acc1),
    {ok, #{<<"id">> => get_id(Packet)}}.

add_id(Attrs) ->
    [{<<"id">>, mongoose_bin:gen_from_crypto()} | Attrs].

-spec get_id(exml:element()) -> binary() | null.
get_id(Stanza) ->
    exml_query:attr(Stanza, <<"id">>, null).
