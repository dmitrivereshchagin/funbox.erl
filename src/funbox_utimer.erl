-module(funbox_utimer).

%% API
-export([sleep/1]).
-export([seconds/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec sleep(non_neg_integer()) -> ok.
sleep(Time) when is_integer(Time), Time >= 2000 ->
    {ElapsedTime, _} = timer:tc(timer, sleep, [Time div 1000 - 1]),
    sleep(Time - ElapsedTime, monotonic_time());
sleep(Time) when is_integer(Time), Time >= 0 ->
    sleep(Time, monotonic_time()).

-spec seconds(Seconds) -> MicroSeconds when
      Seconds :: non_neg_integer(),
      MicroSeconds :: non_neg_integer().
seconds(Seconds) ->
    1000 * 1000 * Seconds.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec sleep(non_neg_integer(), integer()) -> ok.
sleep(Time, Start) ->
    case monotonic_time() of
        Now when Now - Start < Time ->
            sleep(Time, Start);
        _ ->
            ok
    end.

-spec monotonic_time() -> integer().
monotonic_time() ->
    erlang:monotonic_time(microsecond).
