-module(funbox_number_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

from_binary_test_() ->
    [?_assertEqual({ok, 2}, funbox_number:from_binary(<<"2">>)),
     ?_assertEqual({ok, 4}, funbox_number:from_binary(<<"4">>)),
     ?_assertEqual(error, funbox_number:from_binary(<<"x">>))].

is_prime_test_() ->
    [{integer_to_list(N), ?_assertEqual(R, funbox_number:is_prime(N))}
     || {N, R} <- [{-1, false},
                   {0, false},
                   {1, false},
                   {2, true},
                   {3, true},
                   {2 * 2, false},
                   {2 * 3, false},
                   {3 * 3, false}]].

primes_test_() ->
    %% https://en.wikipedia.org/wiki/List_of_prime_numbers
    [{integer_to_list(N), ?_assert(funbox_number:is_prime(N))}
     || N <- [31636373, 39916801, 193877777, 433494437, 479001599,
              2971215073, 8589935681, 8589935681, 16148168401,
              16148168401, 22815088913, 77777677777, 87178291199,
              99999199999, 200560490131, 228204732751]].

pseudoprimes_test_() ->
    %% https://en.wikipedia.org/wiki/Pseudoprime
    [{integer_to_list(N), ?_assertNot(funbox_number:is_prime(N))}
     || N <- [7957, 8321, 8481, 8911, 272611, 283361, 302101, 303101,
              12327121, 443372888629441]].

random_test_() ->
    [times(50, ?_assert(funbox_number:random(0, 5) >= 0)),
     times(50, ?_assert(funbox_number:random(0, 5) =< 5)),
     times(10, ?_assert(funbox_number:random(0, 0) == 0)),
     times(50, ?_assert(funbox_number:random(-5, 0) >= -5)),
     times(50, ?_assert(funbox_number:random(-5, 0) =< 0))].

%%%===================================================================
%%% Helper functions
%%%===================================================================

times(N, Test) ->
    {generator,
     fun() when N > 0 ->
             [Test, times(N - 1, Test)];
        () ->
             []
     end}.
