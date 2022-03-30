-module(funbox_producer).

%% API
-export([start_link/1]).

%% proc_lib callbacks
-export([init/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(state, {max_number   :: pos_integer(),
                queue_key    :: binary(),
                tick_time    :: pos_integer(),
                redis_client :: pid()}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(funbox_config:config()) -> {ok, pid()}.
start_link(Config) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [Config])}.

%%%===================================================================
%%% proc_lib callbacks
%%%===================================================================

-spec init(funbox_config:config()) -> no_return().
init(Config) ->
    RedisOptions = [{reconnect_sleep, no_reconnect} |
                    funbox_config:redis_options(Config)],
    case eredis:start_link(RedisOptions) of
        {ok, Client} ->
            MaxNumber = funbox_config:max_number(Config),
            QueueKey  = funbox_config:queue_key(Config),
            TickTime  = 1000 * 1000 div funbox_config:producer_rate(Config),
            loop(#state{max_number   = MaxNumber,
                        queue_key    = QueueKey,
                        tick_time    = TickTime,
                        redis_client = Client});
        {error, Reason} ->
            exit(Reason)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec loop(state()) -> no_return().
loop(State) ->
    case timer:tc(fun push_number/1, [State]) of
        {ElapsedTime, {ok, _}} ->
            maybe_sleep(State#state.tick_time - ElapsedTime),
            loop(State);
        {_ElapsedTime, {error, Reason}} ->
            exit(Reason)
    end.

-spec push_number(state()) -> {ok, binary()} | {error, term()}.
push_number(State) ->
    Client = State#state.redis_client,
    QueueKey = State#state.queue_key,
    N = funbox_number:random(2, State#state.max_number),
    eredis:q(Client, ["LPUSH", QueueKey, N]).

-spec maybe_sleep(integer()) -> ok.
maybe_sleep(Time) when Time > 0 ->
    funbox_timer:sleep(Time);
maybe_sleep(_Time) ->
    ok.
