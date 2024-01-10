-module(funbox_number).

%% API
-export([from_binary/1]).
-export([is_prime/1]).
-export([random/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec from_binary(binary()) -> {ok, integer()} | error.
from_binary(Binary) ->
    try
        {ok, binary_to_integer(Binary)}
    catch
        error:badarg -> error
    end.

-spec is_prime(integer()) -> boolean().
is_prime(N) when is_integer(N), N >= 2 ->
    is_prime(N, 2, trunc(math:sqrt(N)) + 1);
is_prime(N) when is_integer(N) ->
    false.

-spec random(integer(), integer()) -> integer().
random(From, To) when is_integer(From), is_integer(To), From =< To ->
    rand:uniform(To - From + 1) + From - 1.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_prime(_, M, M) -> true;
is_prime(N, I, _) when N rem I =:= 0 -> false;
is_prime(N, I, M) -> is_prime(N, I + 1, M).
