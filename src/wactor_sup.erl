-module(wactor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% FIXME: Move to uses app skeleton.
    Locker = {wactor_locker, {locker, start_link, [1, 1000, 1000, 250]},
              permanent, 2000, worker, [locker]},
    {ok, { {one_for_one, 5, 10}, [Locker]} }.
