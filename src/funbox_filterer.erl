-module(funbox_filterer).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/1]).
-ignore_xref([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_continue/2, handle_call/3,
         handle_cast/2, handle_info/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(state, {queue_key      :: binary(),
                result_set_key :: binary(),
                redis_client   :: pid() | undefined}).
-type state() :: #state{}.

-type continue() :: {connect, eredis:options()} |
                    poll_queue |
                    {convert_value, binary()} |
                    {check_primality, integer()} |
                    {save_prime, integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(funbox_config:config()) -> {ok, pid()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(funbox_config:config()) ->
          {ok, state(), {continue, continue()}}.
init(Config) ->
    QueueKey = funbox_config:queue_key(Config),
    ResultSetKey = funbox_config:result_set_key(Config),
    RedisOptions = funbox_config:redis_options(Config),
    State = #state{queue_key = QueueKey,
                   result_set_key = ResultSetKey},
    {ok, State, {continue, {connect, RedisOptions}}}.

-spec handle_continue(continue(), state()) ->
          {noreply, state()} |
          {noreply, state(), {continue, continue()}} |
          {stop, term(), state()}.
handle_continue({connect, Options}, State) ->
    case eredis:start_link(
           [{reconnect_sleep, no_reconnect} | Options]) of
        {ok, Client} ->
            NewState = State#state{redis_client = Client},
            {noreply, NewState, {continue, poll_queue}};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_continue(poll_queue, State) ->
    Command = ["BRPOP", State#state.queue_key, 0],
    eredis:q_async(State#state.redis_client, Command),
    {noreply, State};
handle_continue({convert_value, Value}, State) ->
    case funbox_number:from_binary(Value) of
        {ok, Number} ->
            {noreply, State, {continue, {check_primality, Number}}};
        error ->
            ?LOG_WARNING("Bad value: ~tp", [Value]),
            {noreply, State, {continue, poll_queue}}
    end;
handle_continue({check_primality, Number}, State) ->
    case funbox_number:is_prime(Number) of
        true ->
            ?LOG_INFO("Prime number: ~w", [Number]),
            {noreply, State, {continue, {save_prime, Number}}};
        false ->
            {noreply, State, {continue, poll_queue}}
    end;
handle_continue({save_prime, Number}, State) ->
    Command = ["SADD", State#state.result_set_key, Number],
    case eredis:q(State#state.redis_client, Command) of
        {ok, _} ->
            {noreply, State, {continue, poll_queue}};
        {error, Reason} ->
            {stop, Reason, State}
    end.

-spec handle_call(_, _, state()) -> {noreply, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) ->
          {noreply, state()} |
          {noreply, state(), {continue, continue()}} |
          {stop, term(), state()}.
handle_info({response, {ok, [Key, Value]}},
            #state{queue_key = Key} = State) ->
    {noreply, State, {continue, {convert_value, Value}}};
handle_info({response, {error, Reason}}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ?LOG_WARNING("Unhandled info: ~tp", [Info]),
    {noreply, State}.
