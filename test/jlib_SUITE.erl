-module(jlib_SUITE).
-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all, nowarn_export_all]).

all() -> [
          error_reply_check
         ].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(jid),
    C.

end_per_suite(C) ->
    C.

error_reply_check(_) ->
    BaseIQReply = base_iq(),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              host_type => <<"localhost">>,
                              element => BaseIQReply,
                              from_jid => jid:make_noprep(<<"a">>, <<"localhost">>, <<>>),
                              to_jid => jid:make_noprep(<<>>, <<"localhost">>, <<>>) }),
    {Acc1, ErrorReply1} = jlib:make_error_reply(Acc, BaseIQReply, #xmlel{name = <<"testerror">>}),
    ?assertMatch(#xmlel{}, ErrorReply1),
    {_Acc2, ErrorReply2} = jlib:make_error_reply(Acc1, ErrorReply1, #xmlel{name = <<"testerror">>}),
    ?assertMatch({error, {already_an_error, #xmlel{}, #xmlel{}}}, ErrorReply2),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


base_iq() ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, base64:encode(crypto:strong_rand_bytes(4))},
                    {<<"xmlns">>, <<"jabber:client">>},
                    {<<"type">>, <<"result">>}],
           children = [#xmlel{name = <<"session">>,
                              attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-session">>}]}
                      ]}.

element_length_is_too_big(Els) ->
    lists:any(fun(El) -> size(El) >= 1024 end, Els).

run_property(Prop, NumTest, StartSize, StopSize) ->
    ?assert(proper:quickcheck(Prop, [verbose, long_result,
                                     {numtests, NumTest},
                                     {start_size, StartSize},
                                     {max_size, StopSize}])).
