-module(funbox_config_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup and cleanup
%%%===================================================================

load_application() ->
    ok = application:load(Application = funbox),
    Application.

unload_application(Application) ->
    ok = application:unload(Application).

with_loaded_application_test_() ->
    {foreach,
     fun load_application/0,
     fun unload_application/1,
     [{with, [T]}
      || T <- [fun default_max_number/1,
               fun custom_max_number/1,
               fun default_redis_host/1,
               fun custom_redis_host/1,
               fun default_redis_port/1,
               fun custom_redis_port/1,
               fun default_redis_database/1,
               fun custom_redis_database/1,
               fun default_queue_key/1,
               fun custom_queue_key/1,
               fun default_result_set_key/1,
               fun custom_result_set_key/1,
               fun default_producer_rate/1,
               fun custom_producer_rate/1,
               fun default_num_filterers/1,
               fun custom_num_filterers/1]]}.

%%%===================================================================
%%% Test cases
%%%===================================================================

default_max_number(_Application) ->
    ?assertEqual(1_000_000_000, max_number()).

custom_max_number(Application) ->
    application:set_env(Application, max_number, 1000),
    ?assertEqual(1000, max_number()).

default_redis_host(_Application) ->
    ?assertEqual("localhost", proplists:get_value(host, redis_options())).

custom_redis_host(Application) ->
    application:set_env(Application, redis_host, "redis"),
    ?assertEqual("redis", proplists:get_value(host, redis_options())).

default_redis_port(_Application) ->
    ?assertEqual(6379, proplists:get_value(port, redis_options())).

custom_redis_port(Application) ->
    application:set_env(Application, redis_port, 16379),
    ?assertEqual(16379, proplists:get_value(port, redis_options())).

default_redis_database(_Application) ->
    ?assertEqual(0, proplists:get_value(database, redis_options())).

custom_redis_database(Application) ->
    application:set_env(Application, redis_database, 1),
    ?assertEqual(1, proplists:get_value(database, redis_options())).

default_queue_key(_Application) ->
    ?assertEqual(<<"funbox:numbers">>, queue_key()).

custom_queue_key(Application) ->
    application:set_env(Application, queue_key, "queue"),
    ?assertEqual(<<"queue">>, queue_key()).

default_result_set_key(_Application) ->
    ?assertEqual(<<"funbox:primes">>, result_set_key()).

custom_result_set_key(Application) ->
    application:set_env(Application, result_set_key, "result_set"),
    ?assertEqual(<<"result_set">>, result_set_key()).

default_producer_rate(_Application) ->
    ?assertEqual(3000, producer_rate()).

custom_producer_rate(Application) ->
    application:set_env(Application, producer_rate, 30),
    ?assertEqual(30, producer_rate()).

default_num_filterers(Application) ->
    ?assertEqual({ok, auto}, application:get_env(Application, num_filterers)),
    ?assertEqual(erlang:system_info(schedulers_online), num_filterers()).

custom_num_filterers(Application) ->
    application:set_env(Application, num_filterers,
                        2 * erlang:system_info(schedulers_online)),
    ?assertEqual(2 * erlang:system_info(schedulers_online),
                 num_filterers()).

%%%===================================================================
%%% Helper functions
%%%===================================================================

max_number() ->
    funbox_config:max_number(funbox_config:from_env()).

redis_options() ->
    funbox_config:redis_options(funbox_config:from_env()).

queue_key() ->
    funbox_config:queue_key(funbox_config:from_env()).

result_set_key() ->
    funbox_config:result_set_key(funbox_config:from_env()).

producer_rate() ->
    funbox_config:producer_rate(funbox_config:from_env()).

num_filterers() ->
    funbox_config:num_filterers(funbox_config:from_env()).
