-module(funbox_config).

%% API
-export([from_env/0]).
-export([max_number/1]).
-export([redis_options/1]).
-export([queue_key/1]).
-export([result_set_key/1]).
-export([producer_rate/1]).
-export([num_filterers/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type config() :: #{max_number     := pos_integer(),
                    redis_options  := eredis:options(),
                    queue_key      := binary(),
                    result_set_key := binary(),
                    producer_rate  := pos_integer(),
                    num_filterers  := pos_integer()}.
-export_type([config/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec from_env() -> config().
from_env() ->
    MaxNumber = env(max_number),
    RedisOptions = [{host, env(redis_host)},
                    {port, env(redis_port)},
                    {database, env(redis_database)}],
    QueueKey = list_to_binary(env(queue_key)),
    ResultSetKey = list_to_binary(env(result_set_key)),
    ProducerRate = env(producer_rate),
    NumFilterers = num_procs(env(num_filterers)),
    #{max_number     => MaxNumber,
      redis_options  => RedisOptions,
      queue_key      => QueueKey,
      result_set_key => ResultSetKey,
      producer_rate  => ProducerRate,
      num_filterers  => NumFilterers}.

-spec max_number(config()) -> pos_integer().
max_number(#{max_number := MaxNumber}) ->
    MaxNumber.

-spec redis_options(config()) -> eredis:options().
redis_options(#{redis_options := RedisOptions}) ->
    RedisOptions.

-spec queue_key(config()) -> binary().
queue_key(#{queue_key := QueueKey}) ->
    QueueKey.

-spec result_set_key(config()) -> binary().
result_set_key(#{result_set_key := ResultSetKey}) ->
    ResultSetKey.

-spec producer_rate(config()) -> pos_integer().
producer_rate(#{producer_rate := ProducerRate}) ->
    ProducerRate.

num_filterers(#{num_filterers := NumFilterers}) ->
    NumFilterers.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec env(atom()) -> term().
env(Param) ->
    {ok, Value} = application:get_env(funbox, Param),
    Value.

-spec num_procs(auto | pos_integer()) -> pos_integer().
num_procs(auto) ->
    erlang:system_info(schedulers_online);
num_procs(Num) ->
    Num.
