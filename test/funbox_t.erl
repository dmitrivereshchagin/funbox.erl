-module(funbox_t).

%% API
-export([wait_until/2]).

%%%===================================================================
%%% Macro definitions
%%%===================================================================

-define(NOW(), erlang:monotonic_time(millisecond)).

%%%===================================================================
%%% API
%%%===================================================================

-spec wait_until(fun(() -> boolean()), [Option]) ->
          ok | timeout when
      Option :: {timeout, timeout()} |
                {delay, non_neg_integer()}.
wait_until(Pred, Opts) ->
    Time  = proplists:get_value(timeout, Opts, 5000),
    Delay = proplists:get_value(delay, Opts, 100),
    wait_until(Pred, ?NOW() + Time, Delay).

%%%===================================================================
%%% Internal functions
%%%===================================================================

wait_until(Pred, Deadline, Delay) ->
    case {Pred(), ?NOW()} of
        {true, _Now} ->
            ok;
        {false, Now} when Now < Deadline ->
            timer:sleep(Delay),
            wait_until(Pred, Deadline, Delay);
        {false, _Now} ->
            timeout
    end.
