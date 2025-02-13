-module(mongoose_admin_api_muc).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         content_types_accepted/2,
         allowed_methods/2,
         from_json/2,
         delete_resource/2]).

-ignore_xref([to_json/2, from_json/2]).

-import(mongoose_admin_api, [try_handle_request/3, throw_error/2, parse_body/1, resource_created/4]).

-include("jlib.hrl").

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/mucs/:domain", ?MODULE, State},
     {"/mucs/:domain/:name/:arg", ?MODULE, State}].

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_admin_api:init(Req, State).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_admin_api:is_authorized(Req, State).

-spec content_types_accepted(req(), state()) ->
          {[{{binary(), binary(), '*'}, atom()}], req(), state()}.
content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, from_json}
     ], Req, State}.

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"POST">>, <<"DELETE">>], Req, State}.

%% @doc Called for a method of type "POST"
-spec from_json(req(), state()) -> {true | stop, req(), state()}.
from_json(Req, State) ->
    try_handle_request(Req, State, fun handle_post/2).

%% @doc Called for a method of type "DELETE"
-spec delete_resource(req(), state()) -> {true | stop, req(), state()}.
delete_resource(Req, State) ->
    try_handle_request(Req, State, fun handle_delete/2).

%% Internal functions

handle_post(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    handle_post(Req, State, Bindings).

handle_post(Req, State, #{arg := <<"participants">>} = Bindings) ->
    RoomJid = get_room_jid(Bindings),
    Args = parse_body(Req),
    SenderJid = get_sender_jid(Args),
    RecipientJid = get_recipient_jid(Args),
    Reason = get_invite_reason(Args),
    case mod_muc_api:invite_to_room(RoomJid, SenderJid, RecipientJid, Reason) of
        {ok, _} ->
            {true, Req, State};
        {room_not_found, Msg} ->
            throw_error(not_found, Msg)
    end;
handle_post(Req, State, #{arg := <<"messages">>} = Bindings) ->
    RoomJid = get_room_jid(Bindings),
    Args = parse_body(Req),
    SenderJid = get_from_jid(Args),
    Body = get_message_body(Args),
    {ok, _} = mod_muc_api:send_message_to_room(RoomJid, SenderJid, Body),
    {true, Req, State};
handle_post(Req, State, #{domain := Domain}) ->
    Args = parse_body(Req),
    RoomName = get_room_name(Args),
    OwnerJid = get_owner_jid(Args),
    Nick = get_nick(Args),
    %% TODO This check should be done in the API module to work for GraphQL as well
    #jid{lserver = MUCDomain} = make_room_jid(RoomName, get_muc_domain(Domain)),
    case mod_muc_api:create_instant_room(MUCDomain, RoomName, OwnerJid, Nick) of
        {ok, #{title := R}} ->
            Path = [cowboy_req:uri(Req), "/", R],
            resource_created(Req, State, Path, R);
        {user_not_found, Msg} ->
            throw_error(not_found, Msg)
    end.

handle_delete(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = get_room_jid(Bindings),
    #{arg := Nick} = Bindings, % 'name' was present, so 'arg' is present as well (see Cowboy paths)
    Reason = <<"User kicked from the admin REST API">>,
    case mod_muc_api:kick_user_from_room(RoomJid, Nick, Reason) of
        {ok, _} ->
            {true, Req, State};
        {moderator_not_found, Msg} ->
            throw_error(not_found, Msg);
        {room_not_found, Msg} ->
            throw_error(not_found, Msg)
    end.

get_owner_jid(#{owner := Owner}) ->
    case jid:binary_to_bare(Owner) of
        error -> throw_error(bad_request, <<"Invalid owner JID">>);
        OwnerJid -> OwnerJid
    end;
get_owner_jid(#{}) -> throw_error(bad_request, <<"Missing owner JID">>).

get_room_jid(#{domain := Domain} = Bindings) ->
    MUCDomain = get_muc_domain(Domain),
    RoomName = get_room_name(Bindings),
    make_room_jid(RoomName, MUCDomain).

make_room_jid(RoomName, MUCDomain) ->
    try #jid{} = jid:make_bare(RoomName, MUCDomain)
    catch _:_ -> throw_error(bad_request, <<"Invalid room name">>)
    end.

get_nick(#{nick := Nick}) -> Nick;
get_nick(#{}) -> throw_error(bad_request, <<"Missing nickname">>).

get_room_name(#{name := Name}) -> Name;
get_room_name(#{}) -> throw_error(bad_request, <<"Missing room name">>).

get_message_body(#{body := Body}) -> Body;
get_message_body(#{}) -> throw_error(bad_request, <<"Missing message body">>).

get_invite_reason(#{reason := Reason}) -> Reason;
get_invite_reason(#{}) -> throw_error(bad_request, <<"Missing invite reason">>).

get_from_jid(#{from := Sender}) ->
    case jid:from_binary(Sender) of
        error -> throw_error(bad_request, <<"Invalid sender JID">>);
        SenderJid -> SenderJid
    end;
get_from_jid(#{}) -> throw_error(bad_request, <<"Missing sender JID">>).

get_sender_jid(#{sender := Sender}) ->
    case jid:from_binary(Sender) of
        error -> throw_error(bad_request, <<"Invalid sender JID">>);
        SenderJid -> SenderJid
    end;
get_sender_jid(#{}) -> throw_error(bad_request, <<"Missing sender JID">>).

get_recipient_jid(#{recipient := Recipient}) ->
    case jid:from_binary(Recipient) of
        error -> throw_error(bad_request, <<"Invalid recipient JID">>);
        RecipientJid -> RecipientJid
    end;
get_recipient_jid(#{}) -> throw_error(bad_request, <<"Missing recipient JID">>).

get_muc_domain(Domain) ->
    try
        {ok, HostType} = mongoose_domain_api:get_domain_host_type(Domain),
        mod_muc:server_host_to_muc_host(HostType, Domain)
    catch _:_ ->
            throw_error(not_found, <<"MUC domain not found">>)
    end.
