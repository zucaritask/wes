-module(wactor_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Res = wactor_sup:start_link(),
    ok = locker:set_nodes([node()], [node()], []),
    ok = locker:set_w([node()], 1),
    Res.

stop(_State) ->
    ok.
