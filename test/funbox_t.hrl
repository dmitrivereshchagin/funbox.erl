-ifndef(FUNBOX_T_HRL).
-define(FUNBOX_T_HRL, true).

-define(WAIT_UNTIL(Expr), ?WAIT_UNTIL((Expr), [])).

-define(WAIT_UNTIL(Expr, Opts),
        case funbox_t:wait_until(fun() -> (Expr) end, (Opts)) of
            timeout ->
                erlang:error({wait_timeout, [{expression, (??Expr)}]});
            ok ->
                ok
        end).

-endif.
