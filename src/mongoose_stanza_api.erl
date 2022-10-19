-module(mongoose_stanza_api).
-export([send_chat_message/4, send_headline_message/5, send_stanza/2, lookup_recent_messages/4]).

-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-spec send_chat_message(jid:jid() | null, jid:jid() | null, jid:jid(), binary()) ->
          {unknown_sender | invalid_sender, iodata()} | {ok, map()}.
send_chat_message(User, From, To, Body) ->
    M = #{user => User, from => From, to => To, body => Body},
    fold(M, [fun get_sender_jid/1, fun prepare_chat_message/1, fun send/1]).

-spec send_headline_message(jid:jid() | null, jid:jid() | null, jid:jid(),
                            binary() | null, binary() | null) ->
          {unknown_sender | invalid_sender | no_sender |
           invalid_recipient | no_recipient, iodata()} | {ok, map()}.
send_headline_message(User, From, To, Body, Subject) ->
    M = #{user => User, from => From, to => To, body => Body, subject => Subject},
    fold(M, [fun get_sender_jid/1, fun prepare_headline_message/1, fun send/1]).

-spec send_stanza(jid:jid() | null, exml:element()) ->
          {unknown_sender | invalid_sender, iodata()} | {ok, map()}.
send_stanza(User, Stanza) ->
    M = #{user => User, stanza => Stanza},
    fold(M, [fun get_from_jid/1, fun get_to_jid/1, fun get_sender_jid/1,
             fun prepare_stanza/1, fun send/1]).

get_sender_jid(M = #{user := null, from := From = #jid{}}) ->
    case ejabberd_auth:does_user_exist(From) of
        true ->
            M;
        false ->
            {unknown_sender, <<"Sender's account does not exist">>}
    end;
get_sender_jid(M = #{user := User = #jid{}, from := null}) ->
    M#{from => User};
get_sender_jid(M = #{user := User = #jid{}, from := From = #jid{}}) ->
    case jid:are_bare_equal(User, From) of
        true ->
            M;
        false ->
            {invalid_sender, <<"Sender's JID is different from the user's JID">>}
    end.

send(#{from := #jid{lserver = SenderDomain} = From, to := To, stanza := Stanza}) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(SenderDomain),
    mongoose_stanza_helper:route(HostType, SenderDomain, From, To, Stanza).

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
    M#{stanza => mongoose_stanza_helper:build_message(FromBin, ToBin, Body)}.

prepare_headline_message(M = #{from := From, to := To, body := Body, subject := Subject}) ->
    FromBin = jid:to_binary(From),
    ToBin = jid:to_binary(To),
    M#{stanza => mongoose_stanza_helper:build_message_with_headline(FromBin, ToBin, Body, Subject)}.

prepare_stanza(M = #{stanza := Stanza}) ->
    M#{stanza := mongoose_stanza_helper:ensure_id(Stanza)}.

%% TODO fix error handling, do not crash for non-existing users
%% Before is in microseconds
-spec lookup_recent_messages(
        ArcJID :: jid:jid(),
        With :: jid:jid() | undefined,
        Before :: mod_mam:unix_timestamp() | undefined, % microseconds
        Limit :: non_neg_integer()) ->
    [mod_mam:message_row()].
lookup_recent_messages(_, _, _, Limit) when Limit > 500 ->
    throw({error, message_limit_too_high});
lookup_recent_messages(ArcJID, WithJID, Before, Limit) ->
    #jid{luser = LUser, lserver = LServer} = ArcJID,
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(LServer),
    EndTS = case Before of
                0 -> undefined;
                _ -> Before
            end,
    Params = #{archive_id => mod_mam_pm:archive_id(LServer, LUser),
               owner_jid => ArcJID,
               borders => undefined,
               rsm => #rsm_in{direction = before, id = undefined}, % last msgs
               start_ts => undefined,
               end_ts => EndTS,
               now => os:system_time(microsecond),
               with_jid => WithJID,
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
