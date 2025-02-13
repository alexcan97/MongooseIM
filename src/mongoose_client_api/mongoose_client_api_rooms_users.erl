-module(mongoose_client_api_rooms_users).

-behaviour(mongoose_client_api).
-export([routes/0]).

-behaviour(cowboy_rest).
-export([trails/0,
         init/2,
         is_authorized/2,
         content_types_accepted/2,
         allowed_methods/2,
         from_json/2,
         delete_resource/2]).

-ignore_xref([from_json/2, trails/0]).

-import(mongoose_client_api, [parse_body/1, try_handle_request/3, throw_error/2]).
-import(mongoose_client_api_rooms, [get_room_jid/3, get_user_aff/2]).

-type req() :: cowboy_req:req().
-type state() :: map().

-spec routes() -> mongoose_http_handler:routes().
routes() ->
    [{"/rooms/:id/users/[:user]", ?MODULE, #{}}].

trails() ->
    mongoose_client_api_rooms_users_doc:trails().

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, Opts) ->
    mongoose_client_api:init(Req, Opts).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_client_api:is_authorized(Req, State).

content_types_accepted(Req, State) ->
    mongoose_client_api_rooms:content_types_accepted(Req, State).

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

handle_post(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = get_room_jid(Bindings, State, required),
    Args = parse_body(Req),
    TargetJid = get_user_jid(Args),
    assert_permissions(get_user_aff(State, RoomJid), add, UserJid, TargetJid),
    mod_muc_light_api:change_affiliation(RoomJid, UserJid, TargetJid, <<"member">>),
    {true, Req, State}.

handle_delete(Req, State = #{jid := UserJid}) ->
    Bindings = cowboy_req:bindings(Req),
    RoomJid = get_room_jid(Bindings, State, required),
    TargetJid = get_user_jid(Bindings),
    assert_permissions(get_user_aff(State, RoomJid), remove, UserJid, TargetJid),
    mod_muc_light_api:change_affiliation(RoomJid, UserJid, TargetJid, <<"none">>),
    {true, Req, State}.

-spec assert_permissions(mod_muc_light_api:aff(), add | remove, jid:jid(), jid:jid()) -> ok.
assert_permissions(owner, _Op, _UserJid, _TargetJid) -> ok;
assert_permissions(member, remove, UserJid, UserJid) -> ok;
assert_permissions(_Aff, _Op, _UserJid, _TargetJid) ->
    throw_error(denied, <<"Operation not permitted for this user">>).

get_user_jid(#{user := JidBin}) ->
    case jid:from_binary(JidBin) of
        error -> throw_error(bad_request, <<"Invalid user JID: ", JidBin/binary>>);
        Jid -> Jid
    end;
get_user_jid(#{}) ->
    throw_error(bad_request, <<"Missing JID">>).
