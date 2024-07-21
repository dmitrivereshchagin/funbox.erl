-module(funbox_utimer).

%% API
-export([seconds/1]).
-export([sleep/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec seconds(Seconds) -> MicroSeconds when
      Seconds :: non_neg_integer(),
      MicroSeconds :: non_neg_integer().
seconds(Seconds) ->
    1000 * 1000 * Seconds.

-spec sleep(non_neg_integer()) -> ok.
sleep(Time) when is_integer(Time), Time >= 2000 ->
    Deadline = monotonic_time() + Time,
    timer:sleep(Time div 1000 - 1),
    sleep_until(Deadline);
sleep(Time) when is_integer(Time), Time >= 0 ->
    sleep_until(monotonic_time() + Time).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec sleep_until(integer()) -> ok.
sleep_until(Deadline) ->
    case monotonic_time() < Deadline of
        true ->
            sleep_until(Deadline);
        false ->
            ok
    end.

-compile({inline, [monotonic_time/0]}).
-spec monotonic_time() -> integer().
monotonic_time() ->
    erlang:monotonic_time(microsecond).
