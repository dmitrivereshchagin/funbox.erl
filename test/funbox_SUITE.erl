-module(funbox_SUITE).
-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include("funbox_t.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [test_filterer_polls_queue,
     test_specified_number_of_filterers_are_started,
     test_producer_pushes_to_queue].

init_per_suite(Config) ->
    _  = application:unload(funbox),
    ok = application:load(funbox),
    [{funbox_config, funbox_config:from_env()} | Config].

end_per_suite(_Config) ->
    ok = application:unload(funbox).

init_per_testcase(_TestCase, Config) ->
    FunboxConfig = proplists:get_value(funbox_config, Config),
    RedisOptions = [{name, {local, ?MODULE}} |
                    funbox_config:redis_options(FunboxConfig)],
    {ok, _} = eredis:start_link(RedisOptions),
    Config.

end_per_testcase(_TestCase, _Config) ->
    _ = q(["FLUSHDB"]),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

test_filterer_polls_queue(Config) ->
    FunboxConfig = proplists:get_value(funbox_config, Config),
    QueueKey = funbox_config:queue_key(FunboxConfig),
    ResultSetKey = funbox_config:result_set_key(FunboxConfig),
    {ok, _} = funbox_filterer:start_link(FunboxConfig),
    ?assertEqual(0, q(["SCARD", ResultSetKey])),
    _ = q(["LPUSH", QueueKey, 1, 2, 4, "x", 5]),
    ?WAIT_UNTIL(q(["SISMEMBER", ResultSetKey, 2]) =:= 1),
    ?WAIT_UNTIL(q(["SISMEMBER", ResultSetKey, 5]) =:= 1),
    ?assertEqual(2, q(["SCARD", ResultSetKey])).

test_specified_number_of_filterers_are_started(Config) ->
    FunboxConfig = proplists:get_value(funbox_config, Config),
    FunboxConfig1 = FunboxConfig#{num_filterers := 2},
    {ok, Pid1} = funbox_filterer_sup:start_link(FunboxConfig1),
    Counts1 = supervisor:count_children(Pid1),
    ?assertEqual(2, proplists:get_value(active, Counts1)),
    FunboxConfig2 = FunboxConfig#{num_filterers := 4},
    {ok, Pid2} = funbox_filterer_sup:start_link(FunboxConfig2),
    Counts2 = supervisor:count_children(Pid2),
    ?assertEqual(4, proplists:get_value(active, Counts2)).

test_producer_pushes_to_queue(Config) ->
    FunboxConfig = proplists:get_value(funbox_config, Config),
    QueueKey = funbox_config:queue_key(FunboxConfig),
    ?assertEqual(0, q(["LLEN", QueueKey])),
    {ok, Pid} = funbox_producer:start_link(FunboxConfig),
    ct:sleep(timer:seconds(3)),
    await_killed(Pid),
    QueueLen = q(["LLEN", QueueKey]),
    ?assert(QueueLen > 2 * 3000, {queue_len, QueueLen}).

%%%===================================================================
%%% Helper functions
%%%===================================================================

q(Command) ->
    case eredis:q(?MODULE, Command) of
        {error, Reason} ->
            error(Reason, [Command]);
        {ok, Value} ->
            maybe_integer(Value)
    end.

maybe_integer(Value) when is_binary(Value) ->
    maybe_integer_from_binary(Value);
maybe_integer(Value) ->
    Value.

maybe_integer_from_binary(Binary) ->
    try
        binary_to_integer(Binary)
    catch
        error:badarg -> Binary
    end.

await_killed(Pid) ->
    MonitorRef = monitor(process, Pid),
    unlink(Pid), exit(Pid, kill),
    receive
        {'DOWN', MonitorRef, process, Pid, killed} ->
            ok
    end.
