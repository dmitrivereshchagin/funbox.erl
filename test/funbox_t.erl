-module(funbox_t).

%% API
-export([wait_until/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type wait_until_option() :: {timeout, timeout()} |
                             {delay, non_neg_integer()}.
-export_type([wait_until_option/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec wait_until(Pred, Opts) -> ok | timeout when
      Pred :: fun(() -> boolean()),
      Opts :: [wait_until_option()].
wait_until(Pred, Opts) ->
    Timeout = proplists:get_value(timeout, Opts, 5000),
    Deadline = monotonic_time() + Timeout,
    Delay = proplists:get_value(delay, Opts, 100),
    wait_until(Pred, Deadline, Delay).

%%%===================================================================
%%% Internal functions
%%%===================================================================

wait_until(Pred, Deadline, Delay) ->
    case {Pred(), monotonic_time()} of
        {true, _Now} ->
            ok;
        {false, Now} when Now < Deadline ->
            timer:sleep(Delay),
            wait_until(Pred, Deadline, Delay);
        {false, _Now} ->
            timeout
    end.

monotonic_time() ->
    erlang:monotonic_time(millisecond).
