-module(funbox_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    funbox_sup:start_link(funbox_config:from_env()).

stop(_State) ->
    ok.
