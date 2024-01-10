-module(funbox_producer).

%% API
-export([start_link/1]).
-ignore_xref([start_link/1]).

%% proc_lib callbacks
-export([init/1]).
-ignore_xref([init/1]).

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
    Pid = proc_lib:spawn_link(?MODULE, init, [Config]),
    {ok, Pid}.

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
            Rate      = funbox_config:producer_rate(Config),
            TickTime  = funbox_timer:seconds(1) div Rate,
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
        {ElapsedTime, ok} ->
            maybe_sleep(State#state.tick_time - ElapsedTime),
            loop(State);
        {_ElapsedTime, {error, Reason}} ->
            exit(Reason)
    end.

-spec push_number(state()) -> ok | {error, term()}.
push_number(State) ->
    Number = funbox_number:random(2, State#state.max_number),
    Command = ["LPUSH", State#state.queue_key, Number],
    novalue(eredis:q(State#state.redis_client, Command)).

novalue({ok, _}) -> ok;
novalue({error, _} = Error) -> Error.

-spec maybe_sleep(integer()) -> ok.
maybe_sleep(Time) when Time =< 0 -> ok;
maybe_sleep(Time) -> funbox_timer:sleep(Time).
