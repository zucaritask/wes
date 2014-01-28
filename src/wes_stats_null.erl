-module(wes_stats_null).

-behaviour(wes_stats).

%% Callback
-export([get/2,
         stat/2]).

get(_Type, _Name) ->
    0.

stat(_Type, _Name) ->
    ok.
