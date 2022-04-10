-module(funbox_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(application:start_type(), _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    funbox_sup:start_link(funbox_config:from_env()).

-spec stop(_) -> ok.
stop(_State) ->
    ok.
