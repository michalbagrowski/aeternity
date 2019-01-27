-module(aestratum_server_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_server_session).
-define(JSONRPC_MODULE, aestratum_jsonrpc).
-define(NONCE_MODULE, aestratum_nonce).
-define(TARGET_MODULE, aestratum_target).
-define(EXTRA_NONCE_CACHE_MODULE, aestratum_extra_nonce_cache).
-define(JOB_MODULE, aestratum_job).

-define(TEST_USER, <<"ak_123o45ABCiANzqxxxxUUrrrJuDuxU61zCGr9LJCwtTUg34567">>).

session_test_() ->
    {setup,
     fun() ->
             ok = application:ensure_started(jsx)
     end,
     fun(_) ->
             ok = application:stop(jsx)
     end,
     [{generator, fun server_session/0}]}.

server_session() ->
    {foreach,
     fun() ->
             meck:new(?EXTRA_NONCE_CACHE_MODULE, [passthrough]),
             meck:new(?TARGET_MODULE, [passthrough]),
             meck:new(?TEST_MODULE, [passthrough]),
             {ok, Pid} = aestratum_dummy_handler:start_link(?TEST_MODULE),
             Pid
     end,
     fun(Pid) ->
             meck:unload(?EXTRA_NONCE_CACHE_MODULE),
             meck:unload(?TARGET_MODULE),
             meck:unload(?TEST_MODULE),
             aestratum_dummy_handler:stop(Pid)
     end,
     [fun(Pid) -> t(Pid, init()) end,
      %% connected - error
      fun(Pid) -> t(Pid, when_connected(timeout)) end,
      fun(Pid) -> t(Pid, when_connected(authorize)) end,
      fun(Pid) -> t(Pid, when_connected(submit)) end,
      fun(Pid) -> t(Pid, when_connected(not_req)) end,
      fun(Pid) -> t(Pid, when_connected(jsonrpc_errors)) end,
      %% connected - success
      fun(Pid) -> t(Pid, when_connected(configure)) end,
      fun(Pid) -> t(Pid, when_connected(subscribe)) end,

      %% configured - error
      fun(Pid) -> t(Pid, when_configured(timeout)) end,
      fun(Pid) -> t(Pid, when_configured(configure)) end,
      fun(Pid) -> t(Pid, when_configured(authorize)) end,
      fun(Pid) -> t(Pid, when_configured(submit)) end,
      fun(Pid) -> t(Pid, when_configured(not_req)) end,
      fun(Pid) -> t(Pid, when_configured(jsonrpc_errors)) end,
      %% configured - success
      fun(Pid) -> t(Pid, when_configured(subscribe)) end,

      %% subscribed - error
      fun(Pid) -> t(Pid, when_subscribed(timeout)) end,
      %% TODO: fun(Pid) -> t(Pid, when_subscribed(configure)) end,
      fun(Pid) -> t(Pid, when_subscribed(subscribe)) end,
      fun(Pid) -> t(Pid, when_subscribed(submit)) end,
      fun(Pid) -> t(Pid, when_subscribed(not_req)) end,
      fun(Pid) -> t(Pid, when_subscribed(jsonrpc_errors)) end,
      %% subscribed - success
      fun(Pid) -> t(Pid, when_subscribed(authorize_failure)) end,
      fun(Pid) -> t(Pid, when_subscribed(authorize_success)) end,

      %% authorized - error
      fun(Pid) -> t(Pid, when_authorized(timeout)) end,
      %% TODO: fun(Pid) -> t(Pid, when_authorized(configure)) end,
      %% TODO: fun(Pid) -> t(Pid, when_authorized(subscribe)) end,
      fun(Pid) -> t(Pid, when_authorized(authorize)) end,
      fun(Pid) -> t(Pid, when_authorized(not_req)) end,
      fun(Pid) -> t(Pid, when_authorized(jsonrpc_errors)) end,
      %% authorize - success
      fun(Pid) -> t(Pid, when_authorized(set_initial_share_target)) end,

      %% set_initial_share_target - error
      fun(Pid) -> t(Pid, when_set_initial_share_target(timeout)) end,
      %% TODO: ...
      %% set_initial_share_target - success
      fun(Pid) -> t(Pid, when_set_initial_share_target(new_block_no_target_change)) end,
      fun(Pid) -> t(Pid, when_set_initial_share_target(new_block_target_change)) end

      %% TODO: fun(Pid) -> t(Pid, when_notified(submit)) end,


     ]}.

%% T - title
%% G - test/no_test - generate test/not generate
%% E - event
%% A - action
%% S - session state
%% R - result
t(Pid, Data) ->
    Asserts =
        [begin
            R1 = result(Pid, R, aestratum_dummy_handler:handle_event(Pid, event(E))),
            case G of
                test -> {T, ?_assertEqual(R, R1)};
                no_test -> no_test
            end
         end || {T, G, E, R} <- Data],
    lists:filter(fun({_T, _Assert}) -> true;
                    (no_test) -> false end, Asserts).

event({conn, D}) when is_map(D) ->
    {ok, D1} = ?JSONRPC_MODULE:encode(D),
    {conn, D1};
event(Other) ->
    Other.

result(Pid, {_A, S}, {A1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {A1, S1M};
result(Pid, {_A, S}, {A1, D1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {ok, D1M} = ?JSONRPC_MODULE:decode(D1),
    {A1, D1M, S1M};
result(Pid, {_A, _D, S}, {A1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {A1, S1M};
result(Pid, {_A, D, S}, {A1, D1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {ok, D1M0} = ?JSONRPC_MODULE:decode(D1),
    D1M = maps:with(maps:keys(D), maybe_rsp_result(D, D1M0)),
    {A1, D1M, S1M}.

%% If type is rsp, we need to validate the result.
maybe_rsp_result(#{type := rsp, method := M}, #{type := rsp} = D1M0) ->
    {ok, D1M} = ?JSONRPC_MODULE:validate_rsp(M, D1M0),
    D1M;
maybe_rsp_result(_D, D1M0) ->
    D1M0.

init() ->
    T = <<"init - server">>,
    L = [{{conn, init},
          {no_send,
           #{phase => connected, timer_phase => connected}}
         }],
    [{T, test, E, R} || {E, R} <- L].

when_connected(timeout) ->
    T = <<"when connected - timeout">>,
    L = [{{conn, timeout},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(authorize) ->
    T = <<"when connected - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 0,
                   user => ?TEST_USER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 0, reason => not_subscribed},
           #{phase => connected, timer_phase => connected,
            extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(submit) ->
    T = <<"when connected - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 0,
                   user => ?TEST_USER, job_id => <<"0123456789abcdef">>,
                   miner_nonce => <<"0123456789">>, pow => lists:seq(1, 42)}},
          {send,
           #{type => rsp, method => submit, id => 0, reason => not_subscribed},
           #{phase => connected, timer_phase => connected,
            extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(not_req) ->
    T = <<"when connected - not_req">>,
    %% Server receives unexpected message - response.
    L = [{{conn, #{type => rsp, method => configure, id => null,
                   reason => parse_error, data => <<"foo">>}},
          {send,
           #{type => rsp, method => configure, id => null,
             reason => unknown_error, data => <<"unexpected_msg">>},
           #{phase => connected, timer_phase => connected,
             extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(jsonrpc_errors) ->
    T = <<"when connected - jsonrpc_errors">>,
    prep_connected(T) ++ jsonrpc_errors(T, connected, connected);
when_connected(configure) ->
    T = <<"when connected - configure">>,
    L = [{{conn, #{type => req, method => configure, id => 0, params => []}},
          {send,
           #{type => rsp, method => configure, id => 0, result => []},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L];
when_connected(subscribe) ->
    Host = <<"ae.testpool.com">>,
    Port = 10000,
    ExtraNonce = aestratum_nonce:new(extra, 100, 4),
    mock(subscribe, #{host => Host, port => Port, extra_nonce => ExtraNonce}),
    T = <<"when connected - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 0,
                   user_agent => <<"aeminer/1.0.0">>, session_id => null,
                   host => Host, port => Port}},
          {send,
           #{type => rsp, method => subscribe, id => 0,
             result => [null, ?NONCE_MODULE:to_hex(ExtraNonce)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ExtraNonce}}
         }],
    prep_connected(T) ++ [{T, test, E, R} || {E, R} <- L].

when_configured(timeout) ->
    T = <<"when configured - timeout">>,
    L = [{{conn, timeout},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
            extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(configure) ->
    T = <<"when configured - configure">>,
    L = [{{conn, #{type => req, method => configure, id => 1, params => []}},
          {send,
           #{type => rsp, method => configure, id => 1, reason => unknown_error},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(authorize) ->
    T = <<"when configured - authorize">>,
    L = [{{conn, #{type => req, method => authorize, id => 1,
                   user => ?TEST_USER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(submit) ->
    T = <<"when configured - submit">>,
    L = [{{conn, #{type => req, method => submit, id => 1,
                   user => ?TEST_USER, job_id => <<"0123456789abcdef">>,
                   miner_nonce => <<"0123456789">>, pow => lists:seq(1, 42)}},
          {send,
           #{type => rsp, method => submit, id => 1, reason => not_subscribed},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(not_req) ->
    T = <<"when configured - not_req">>,
    L = [{{conn, #{type => rsp, method => configure, id => 1, result => []}},
          {send,
           #{type => rsp, method => configure, id => 1, reason => unknown_error},
           #{phase => configured, timer_phase => configured,
             extra_nonce => undefined}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L];
when_configured(jsonrpc_errors) ->
    T = <<"when configured - jsonrpc_errors">>,
    prep_configured(T) ++ jsonrpc_errors(T, configured, configured);
when_configured(subscribe) ->
    Host = <<"ae.testpool.com">>,
    Port = 10000,
    ExtraNonce = aestratum_nonce:new(extra, 222, 3),
    mock(subscribe, #{host => Host, port => Port, extra_nonce => ExtraNonce}),
    T = <<"when configured - subscribe">>,
    L = [{{conn, #{type => req, method => subscribe, id => 1,
                   user_agent => <<"aeminer/1.0.0">>, session_id => null,
                   host => Host, port => Port}},
          {send,
           #{type => rsp, method => subscribe, id => 1,
             result => [null, ?NONCE_MODULE:to_hex(ExtraNonce)]},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ExtraNonce}}
         }],
    prep_configured(T) ++ [{T, test, E, R} || {E, R} <- L].

when_subscribed(timeout) ->
    T = <<"when subscribed - timeout">>,
    Opts = #{host => <<"aepool.com">>, port => 9999,
             extra_nonce => aestratum_nonce:new(extra, 123, 1)},
    L = [{{conn, timeout},
          {stop,
           #{phase => disconnected, timer_phase => undefined,
             extra_nonce => undefined}}
         }],
    mock(subscribe, Opts),
    prep_subscribed(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(subscribe) ->
    T = <<"when subscribed - subscribe">>,
    Host = <<"test.aepool.com">>,
    Port = 12345,
    ExtraNonce = aestratum_nonce:new(extra, 987, 4),
    Opts = #{host => Host, port => Port, extra_nonce => ExtraNonce},
    L = [{{conn, #{type => req, method => subscribe, id => 2,
                   user_agent => <<"aeminer/1.0.0">>, session_id => null,
                   host => Host, port => Port}},
          {send,
           #{type => rsp, method => subscribe, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ExtraNonce}}
         }],
    mock(subscribe, Opts),
    prep_subscribed(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(submit) ->
    T = <<"when subscribed - submit">>,
    ExtraNonce = aestratum_nonce:new(extra, 999, 3),
    Opts = #{host => <<"test.aepool.com">>, port => 12345,
             extra_nonce => ExtraNonce},
    L = [{{conn, #{type => req, method => submit, id => 2,
                   user => ?TEST_USER, job_id => <<"0123456789abcdef">>,
                   miner_nonce => <<"0123456789">>, pow => lists:seq(1, 42)}},
          {send,
           #{type => rsp, method => submit, id => 2, reason => unauthorized_worker},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ExtraNonce}}
         }],
    mock(subscribe, Opts),
    prep_subscribed(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(not_req) ->
    T = <<"when subscribed - not_req">>,
    ExtraNonce = aestratum_nonce:new(extra, 1, 2),
    Opts = #{host => <<"mypool.net">>, port => 3214, extra_nonce => ExtraNonce},
    L = [{{conn, #{type => rsp, method => configure, id => 2, result => []}},
          {send,
           #{type => rsp, method => configure, id => 2, reason => unknown_error},
           #{phase => subscribed, timer_phase => subscribed,
             extra_nonce => ExtraNonce}}
         }],
    mock(subscribe, Opts),
    prep_subscribed(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(jsonrpc_errors) ->
    T = <<"when subscribed - jsonrpc_errors">>,
    ExtraNonce = aestratum_nonce:new(extra, 100, 5),
    Opts = #{host => <<"mypool.net">>, port => 8877, extra_nonce => ExtraNonce},
    mock(subscribe, Opts),
    prep_subscribed(T, Opts) ++ jsonrpc_errors(T, subscribed, subscribed);
when_subscribed(authorize_failure) ->
    T = <<"when subscribed - authorize_failure">>,
    ExtraNonce = aestratum_nonce:new(extra, 4254, 4),
    Opts = #{host => <<"aepool.org">>, port => 5429, extra_nonce => ExtraNonce,
             user_and_password => invalid},
    L = [{{conn, #{type => req, method => authorize, id => 2,
                   user => ?TEST_USER, password => null}},
           {send,
            #{type => rsp, method => authorize, id => 2, result => false},
            #{phase => subscribed, timer_phase => subscribed,
              extra_nonce => ExtraNonce}}
         }],
    mock(authorize, Opts),
    prep_subscribed(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_subscribed(authorize_success) ->
    T = <<"when subscribed - authorize_success">>,
    ExtraNonce = aestratum_nonce:new(extra, 4254, 4),
    Opts = #{host => <<"aepool.org">>, port => 5429, extra_nonce => ExtraNonce,
             user_and_password => valid},
    L = [{{conn, #{type => req, method => authorize, id => 2,
                   user => ?TEST_USER, password => null}},
           {send,
            #{type => rsp, method => authorize, id => 2, result => true},
            #{phase => authorized, timer_phase => undefined,
              extra_nonce => ExtraNonce}}
         }],
    mock(authorize, Opts),
    prep_subscribed(T, Opts) ++ [{T, test, E, R} || {E, R} <- L].

when_authorized(timeout) ->
    T = <<"when authorized - timeout">>,
    ExtraNonce = aestratum_nonce:new(extra, 999, 3),
    Opts = #{host => <<"test.aepool.com">>, port => 12345,
             extra_nonce => ExtraNonce, user_and_password => valid},
    L = [{{conn, timeout},
          {no_send,
           #{phase => authorized, timer_phase => undefined,
             extra_nonce => ExtraNonce}}
         }],
    mock(authorize, Opts),
    prep_authorized(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(authorize) ->
    T = <<"when authorized - authorize">>,
    ExtraNonce = aestratum_nonce:new(extra, 999, 3),
    Opts = #{host => <<"test.aepool.com">>, port => 12345,
             extra_nonce => ExtraNonce, user_and_password => valid},
    L = [{{conn, #{type => req, method => authorize, id => 3,
                   user => ?TEST_USER, password => null}},
          {send,
           #{type => rsp, method => authorize, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock(authorize, Opts),
    prep_authorized(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(not_req) ->
    T = <<"when authorized - not_req">>,
    ExtraNonce = aestratum_nonce:new(extra, 1000, 5),
    Opts = #{host => <<"test.aepool.com">>, port => 2222,
             extra_nonce => ExtraNonce, user_and_password => valid},
    L = [{{conn, #{type => rsp, method => subscribe, id => 3,
                   reason => parse_error, data => null}},
          {send,
           #{type => rsp, method => subscribe, id => 3, reason => unknown_error},
           #{phase => authorized, timer_phase => undefined}}
         }],
    mock(authorize, Opts),
    prep_authorized(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_authorized(jsonrpc_errors) ->
    T = <<"when authorized - jsonrpc_errors">>,
    Opts = #{host => <<"test.aepool.com">>, port => 6535,
             extra_nonce => aestratum_nonce:new(extra, 31155, 4),
             user_and_password => valid},
    mock(authorize, Opts),
    prep_authorized(T, Opts) ++ jsonrpc_errors(T, authorized, undefined);
when_authorized(set_initial_share_target) ->
    T = <<"when authorized - set_initial_share_target">>,
    InitialShareTarget = 100000000,
    ExtraNonce = aestratum_nonce:new(extra, 43215, 5),
    Opts = #{host => <<"pool.net">>, port => 13245,
             extra_nonce => ExtraNonce, user_and_password => valid,
             initial_share_target => InitialShareTarget},
    L = [{{chain, set_initial_share_target},
          {send,
           #{type => ntf, method => set_target,
             target => ?TARGET_MODULE:to_hex(InitialShareTarget)},
           #{phase => authorized, timer_phase => undefined,
             extra_nonce => ExtraNonce}}
         }],
    mock(set_initial_share_target, Opts),
    prep_authorized(T, Opts) ++ [{T, test, E, R} || {E, R} <- L].

when_set_initial_share_target(timeout) ->
    T = <<"when set initial target - timeout">>,
    ExtraNonce = aestratum_nonce:new(extra, 3129, 5),
    Opts = #{host => <<"ae.pool.com">>, port => 2532,
             extra_nonce => ExtraNonce, user_and_password => valid,
             initial_share_target => 1234567890},
    L = [{{conn, timeout},
          {no_send,
           #{phase => authorized, timer_phase => undefined,
             extra_nonce => ExtraNonce}}
         }],
    mock(set_initial_share_target, Opts),
    prep_set_initial_share_target(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_set_initial_share_target(new_block_no_target_change) ->
    T = <<"when set initial target - new_block_no_target_change">>,
    ExtraNonce = aestratum_nonce:new(extra, 3129, 5),
    BlockHash = binary:copy(<<"1">>, 64),
    BlockTarget = 1000,
    BlockVersion = 1,
    JobId = ?JOB_MODULE:make_id(BlockHash, BlockTarget, BlockVersion),
    Opts = #{host => <<"ae.pool.com">>, port => 2532,
             extra_nonce => ExtraNonce, user_and_password => valid,
             initial_share_target => 1000000, new_share_target => no_change},
    L = [{{chain, {new_block, #{hash => BlockHash, target => BlockTarget,
                                version => BlockVersion}}},
          {send,
           #{type => ntf, method => notify, job_id => JobId,
             block_hash => BlockHash, block_version => BlockVersion,
             empty_queue => true},
           #{phase => authorized, timer_phase => undefined,
             extra_nonce => ExtraNonce}}
         }],
    mock(set_initial_share_target, Opts),
    prep_set_initial_share_target(T, Opts) ++ [{T, test, E, R} || {E, R} <- L];
when_set_initial_share_target(new_block_target_change) ->
    T = <<"when set initial target - new_block_target_change">>,
    ExtraNonce = aestratum_nonce:new(extra, 54302, 4),
    BlockHash = binary:copy(<<"1">>, 64),
    BlockTarget = 11111,
    BlockVersion = 1,
    ShareTarget = 11111000,
    DesiredSolveTime = 50000,
    MaxSolveTime = 70000,
    JobId = ?JOB_MODULE:make_id(BlockHash, BlockTarget, BlockVersion),
    Opts = #{host => <<"aepool.com">>, port => 8432,
             extra_nonce => ExtraNonce, user_and_password => valid,
             initial_share_target => 1111111,
             new_share_target => {increase, 10.0}, share_target_diff_threshold => 5.0},
    L = [{{chain, {new_block, #{hash => BlockHash, target => BlockTarget,
                                version => BlockVersion}}},
          {send,
           %% NOTE: we don't test the target value here, there is a dedicated
           %% test module for that (or will be!).
           #{type => ntf, method => set_target},
           #{phase => authorized, timer_phase => undefined,
             extra_nonce => ExtraNonce, accept_blocks => false}}
         },
         %% We test that the next new block doesn't cause any new notification.
         %% First, the session expects the {chain, {send_notify, Job}} event.
         %% That job caused set_target notification, so it's supposed to be
         %% first after set_target, other new blocks in between are just
         %% skipped.
         {{chain, {new_block, #{hash => binary:copy(<<"2">>, 64), target => 22222,
                                version => 1}}},
          {no_send,
           #{phase => authorized, timer_phase => undefined,
             extra_nonce => ExtraNonce, accept_blocks => false}}
         },
         %% The previuos new block was skipped, the send notify event will
         %% cause sending noftify notification and will restore processing of
         %% new blocks.
         {{chain, {send_notify, #{job_id => JobId, hash => BlockHash,
                                  target => BlockTarget, version => BlockVersion,
                                  share_targe => ShareTarget,
                                  desired_solve_time => DesiredSolveTime,
                                  max_solve_time => MaxSolveTime}}},
          #{type => ntf, method => notify, job_id => JobId,
            block_hash => BlockHash, block_version => BlockVersion,
            empty_queue => true},
          #{phase => authorized, timer_phase => undefined,
            extra_nonce => ExtraNonce, accept_blocks => true}
         }],
    mock(set_initial_share_target, Opts),
    prep_set_initial_share_target(T, Opts) ++ [{T, test, E, R} || {E, R} <- L].

mock(subscribe, #{host := Host, port := Port, extra_nonce := ExtraNonce}) ->
    meck:expect(?TEST_MODULE, get_host, fun() -> Host end),
    meck:expect(?TEST_MODULE, get_port, fun() -> Port end),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, get, fun(_) -> {ok, ExtraNonce} end),
    meck:expect(?EXTRA_NONCE_CACHE_MODULE, free, fun(_) -> ok end),
    ok;
mock(authorize, #{user_and_password := valid} = Opts) ->
    mock(subscribe, Opts),
    meck:expect(?TEST_MODULE, validate_authorize_req, fun(_) -> ok end),
    ok;
mock(authorize, #{user_and_password := invalid} = Opts) ->
    mock(subscribe, Opts),
    meck:expect(?TEST_MODULE, validate_authorize_req, fun(_) -> {error, user_and_password} end),
    ok;
mock(set_initial_share_target, #{initial_share_target := InitialShareTarget} = Opts) ->
    mock(authorize, Opts),
    application:set_env(aestratum, initial_share_target, InitialShareTarget),
    case maps:get(new_share_target, Opts, undefined) of
        NewShareTarget when NewShareTarget =/= undefined ->
            meck:expect(?TARGET_MODULE, diff, fun(_, _) -> NewShareTarget end);
        undefined ->
            ok
    end,
    case maps:get(share_target_diff_threshold, Opts, undefined) of
        ShareTargetDiffThreshold when ShareTargetDiffThreshold =/= undefined ->
            application:set_env(aestratum, share_target_diff_threshold,
                                ShareTargetDiffThreshold);
        undefined ->
            ok
    end,
    ok.

prep_connected(T) ->
    L = [conn_init()],
    [{T, no_test, E, R} || {E, R} <- L].

prep_configured(T) ->
    L = [conn_init(),
         conn_configure(0)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_subscribed(T, Opts) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1, Opts)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_authorized(T, Opts) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1, Opts),
         conn_authorize(2, Opts)],
    [{T, no_test, E, R} || {E, R} <- L].

prep_set_initial_share_target(T, Opts) ->
    L = [conn_init(),
         conn_configure(0),
         conn_subscribe(1, Opts),
         conn_authorize(2, Opts),
         chain_set_initial_share_target(Opts)],
    [{T, no_test, E, R} || {E, R} <- L].

jsonrpc_errors(T, Phase, TimerPhase) ->
    L = [conn_make_parse_error(Phase, TimerPhase),
         conn_make_invalid_msg(no_id, Phase, TimerPhase),
         conn_make_invalid_msg(id, Phase, TimerPhase),
         conn_make_invalid_method(no_id, Phase, TimerPhase),
         conn_make_invalid_method(id, Phase, TimerPhase),
         conn_make_invalid_param(Phase, TimerPhase)
        ],
    [{T, test, E, R} || {E, R} <- L].

conn_init() ->
    {{conn, init},
     {no_send, #{phase => connected, timer_phase => connected}}
    }.

conn_configure(Id) ->
    {{conn, #{type => req, method => configure, id => Id, params => []}},
     {send,
      #{type => rsp, method => configure, id => Id, result => []},
      #{phase => configured, timer_phase => configured}}
    }.

conn_subscribe(Id, #{host := Host, port := Port, extra_nonce := ExtraNonce}) ->
    {{conn, #{type => req, method => subscribe, id => Id,
              user_agent => <<"aeminer/1.0.0">>, session_id => null,
              host => Host, port => Port}},
     {send,
      #{type => rsp, method => subscribe, id => Id,
        result => [null, ?NONCE_MODULE:to_hex(ExtraNonce)]},
      #{phase => subscribed, timer_phase => subscribed,
        extra_nonce => ExtraNonce}}
    }.

conn_authorize(Id, _Opts) ->
    {{conn, #{type => req, method => authorize, id => Id,
              user => ?TEST_USER, password => null}},
     {send,
      #{type => rsp, method => authorize, id => Id, result => true},
      #{phase => authorized, timer_phase => undefined}}
    }.

chain_set_initial_share_target(#{initial_share_target := InitialShareTarget}) ->
    {{chain, set_initial_share_target},
     {send,
      #{type => ntf, method => set_target,
        target => ?TARGET_MODULE:to_hex(InitialShareTarget)},
      #{phase => authorized, timer_phase => undefined}}
    }.

conn_make_parse_error(Phase, TimerPhase) ->
    {{conn, <<"some random binary">>},
     {send,
      #{type => rsp, method => undefined, id => null, reason => parse_error},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_msg(no_id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":\"none\"}">>},
     {send,
      #{type => rsp, method => undefined, id => null, reason => invalid_msg},
      #{phase => Phase, timer_phase => TimerPhase}}
    };
conn_make_invalid_msg(id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":100}">>},
     {send,
      #{type => rsp, method => undefined, id => 100, reason => invalid_msg},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_method(no_id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":-10,\"method\":\"foo\",\"params\":[]}">>},
     {send,
      #{type => rsp, method => undefined, id => null, reason => invalid_method},
      #{phase => Phase, timer_phase => TimerPhase}}
    };
conn_make_invalid_method(id, Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":200,\"method\":\"foo\",\"params\":[]}">>},
     {send,
      #{type => rsp, method => undefined, id => 200, reason => invalid_method},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

conn_make_invalid_param(Phase, TimerPhase) ->
    {{conn, <<"{\"jsonrpc\":\"2.0\",\"id\":300,\"method\":\"mining.subscribe\","
              "\"params\":[\"aeminer/1.0\",\"invalid session_id\",\"aepool.com\",9876]}">>},
     {send,
      #{type => rsp, method => subscribe, id => 300, reason => invalid_param},
      #{phase => Phase, timer_phase => TimerPhase}}
    }.

%% TODO: howto create internal error?
%%conn_make_internal_error()
