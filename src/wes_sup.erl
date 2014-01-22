-module(wes_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Actors, Channels) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Actors, Channels]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Actors, Channels]) ->
    Config = {wes_config, {wes_config, start_link, [Actors, Channels]},
              permanent, 2000, worker, [wes_config]},
    {ok, { {one_for_one, 5, 10}, [Config]} }.
