-module(mongoose_stanza_api).
-export([send_chat_message/4, send_headline_message/5, send_stanza/2, lookup_recent_messages/5]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").
-include("mongoose_logger.hrl").

%% API

-spec send_chat_message(User :: jid:jid() | undefined, From :: jid:jid() | undefined,
                        To :: jid:jid(), Body :: binary()) ->
          {unknown_sender | invalid_sender, iodata()} | {ok, map()}.
send_chat_message(User, From, To, Body) ->
    M = #{user => User, from => From, to => To, body => Body},
    fold(M, [fun get_sender_jid/1, fun prepare_chat_message/1, fun send/1]).

-spec send_headline_message(User :: jid:jid() | undefined, From :: jid:jid() | undefined,
                            To :: jid:jid(), Body :: binary() | undefined,
                            Subject :: binary() | undefined) ->
          {unknown_sender | invalid_sender | no_sender |
           invalid_recipient | no_recipient, iodata()} | {ok, map()}.
send_headline_message(User, From, To, Body, Subject) ->
    M = #{user => User, from => From, to => To, body => Body, subject => Subject},
    fold(M, [fun get_sender_jid/1, fun prepare_headline_message/1, fun send/1]).

-spec send_stanza(User :: jid:jid() | undefined, exml:element()) ->
          {unknown_sender | invalid_sender, iodata()} | {ok, map()}.
send_stanza(User, Stanza) ->
    M = #{user => User, stanza => Stanza},
    fold(M, [fun get_from_jid/1, fun get_to_jid/1, fun get_sender_jid/1,
             fun prepare_stanza/1, fun send/1]).

-spec lookup_recent_messages(User :: jid:jid(), With :: jid:jid() | undefined,
                             Before :: mod_mam:unix_timestamp() | undefined, % microseconds
                             Limit :: non_neg_integer(), CheckUser :: boolean()) ->
          {unknown_user, iodata()} | {ok, [mod_mam:message_row()]}.
lookup_recent_messages(User, With, Before, Limit, CheckUser) ->
    M = #{user => User, with => With, before => Before, limit => Limit, check_user => CheckUser},
    fold(M, [fun get_host_type/1, fun check_user/1, fun check_before/1, fun lookup_messages/1]).

%% Internal functions

get_sender_jid(M = #{user := User = #jid{}, from := From = #jid{}}) ->
    case jid:are_bare_equal(User, From) of
        true ->
            M;
        false ->
            {invalid_sender, <<"Sender's JID is different from the user's JID">>}
    end;
get_sender_jid(M = #{from := From = #jid{}}) ->
    case ejabberd_auth:does_user_exist(From) of
        true ->
            M;
        false ->
            {unknown_sender, <<"Sender's account does not exist">>}
    end;
get_sender_jid(M = #{user := User = #jid{}}) ->
    M#{from => User}.

get_from_jid(M = #{stanza := Stanza}) ->
    case exml_query:attr(Stanza, <<"from">>) of
        undefined ->
            {no_sender, <<"Missing sender JID">>};
        JidBin ->
            case jid:from_binary(JidBin) of
                error -> {invalid_sender, <<"Invalid sender JID">>};
                Jid -> M#{from => Jid}
            end
    end.

get_to_jid(M = #{stanza := Stanza}) ->
    case exml_query:attr(Stanza, <<"to">>) of
        undefined ->
            {no_recipient, <<"Missing recipient JID">>};
        JidBin ->
            case jid:from_binary(JidBin) of
                error -> {invalid_recipient, <<"Invalid recipient JID">>};
                Jid -> M#{to => Jid}
            end
    end.

prepare_chat_message(M = #{from := From, to := To, body := Body}) ->
    FromBin = jid:to_binary(From),
    ToBin = jid:to_binary(To),
    M#{stanza => build_chat_message(FromBin, ToBin, Body)}.

prepare_headline_message(M = #{from := From, to := To, body := Body, subject := Subject}) ->
    FromBin = jid:to_binary(From),
    ToBin = jid:to_binary(To),
    M#{stanza => build_headline_message(FromBin, ToBin, Body, Subject)}.

prepare_stanza(M = #{stanza := Stanza}) ->
    M#{stanza := ensure_id(Stanza)}.

get_host_type(M = #{user := #jid{lserver = LServer}}) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            M#{host_type => HostType};
        {error, not_found} ->
            {unknown_user, <<"User's domain does not exist">>}
    end.

check_user(M = #{check_user := false}) ->
    M;
check_user(M = #{check_user := true, user := UserJid, host_type := HostType}) ->
    case ejabberd_auth:does_user_exist(HostType, UserJid, stored) of
        true ->
            M;
        false ->
            {unknown_user, <<"User does not exist">>}
    end.

check_before(M = #{before := 0}) ->
    M#{before => undefined};
check_before(M) ->
    M.

lookup_messages(#{user := UserJid, with := WithJid, before := Before, limit := Limit,
                 host_type := HostType}) ->
    #jid{luser = LUser, lserver = LServer} = UserJid,
    Params = #{archive_id => mod_mam_pm:archive_id(LServer, LUser),
               owner_jid => UserJid,
               borders => undefined,
               rsm => #rsm_in{direction = before, id = undefined}, % last msgs
               start_ts => undefined,
               end_ts => Before,
               now => os:system_time(microsecond),
               with_jid => WithJid,
               search_text => undefined,
               page_size => Limit,
               limit_passed => false,
               max_result_limit => 1,
               is_simple => true},
    R = mod_mam_pm:lookup_messages(HostType, Params),
    {ok, {_, _, L}} = R,
    L.

fold({_, _} = Result, _) ->
    Result;
fold(M, [Step | Rest]) when is_map(M) ->
    fold(Step(M), Rest).

-spec build_chat_message(jid:literal_jid(), jid:literal_jid(), binary()) -> exml:element().
build_chat_message(From, To, Body) ->
    #xmlel{name = <<"message">>,
           attrs = add_id([{<<"type">>, <<"chat">>}, {<<"from">>, From}, {<<"to">>, To}]),
           children = [#xmlel{name = <<"body">>,
                              children = [#xmlcdata{content = Body}]}]
          }.

build_headline_message(From, To, Body, Subject) ->
    Children = maybe_cdata_elem(<<"subject">>, Subject) ++
               maybe_cdata_elem(<<"body">>, Body),
    Attrs = add_id([{<<"type">>, <<"headline">>}, {<<"from">>, From}, {<<"to">>, To}]),
    #xmlel{name = <<"message">>, attrs = Attrs, children = Children}.

ensure_id(Stanza) ->
    case get_id(Stanza) of
        undefined -> Stanza#xmlel{attrs = add_id(Stanza#xmlel.attrs)};
        _ -> Stanza
    end.

maybe_cdata_elem(_, undefined) -> [];
maybe_cdata_elem(_, <<>>) -> [];
maybe_cdata_elem(Name, Text) when is_binary(Text) ->
    [cdata_elem(Name, Text)].

cdata_elem(Name, Text) when is_binary(Name), is_binary(Text) ->
    #xmlel{name = Name, children = [#xmlcdata{content = Text}]}.

send(#{host_type := HostType, from := From, to := To, stanza := Stanza}) ->
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             host_type => HostType,
                             lserver => From#jid.lserver,
                             element => Stanza}),
    Acc1 = mongoose_hooks:user_send_packet(Acc, From, To, Stanza),
    ejabberd_router:route(From, To, Acc1),
    {ok, #{<<"id">> => get_id(Stanza)}}.

add_id(Attrs) ->
    [{<<"id">>, mongoose_bin:gen_from_crypto()} | Attrs].

-spec get_id(exml:element()) -> binary() | undefined.
get_id(Stanza) ->
    exml_query:attr(Stanza, <<"id">>, undefined).
