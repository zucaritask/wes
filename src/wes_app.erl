-module(wes_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    {ok, Actors} = application:get_env(wes, actors),
    {ok, Channels} = application:get_env(wes, channels),
    wes_sup:start_link(Actors, Channels).

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
