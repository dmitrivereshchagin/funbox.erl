-module(funbox_filterer_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(funbox_config:config()) -> {ok, pid()}.
start_link(Config) ->
    supervisor:start_link(?MODULE, Config).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(funbox_config:config()) ->
          {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Config) ->
    NumFilterers = funbox_config:num_filterers(Config),
    ChildSpecs =
        [#{id => {filterer, N},
           start => {funbox_filterer, start_link, [Config]}} ||
            N <- lists:seq(1, NumFilterers)],
    {ok, {#{strategy => one_for_one}, ChildSpecs}}.
