-module(funbox_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(funbox_config:config()) -> {ok, pid()}.
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(funbox_config:config()) ->
          {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Config) ->
    ChildSpecs =
        [#{id    => filterer,
           start => {funbox_filterer_sup, start_link, [Config]},
           type  => supervisor},
         #{id    => producer,
           start => {funbox_producer, start_link, [Config]},
           type  => worker}],
    {ok, {#{strategy => one_for_one}, ChildSpecs}}.
