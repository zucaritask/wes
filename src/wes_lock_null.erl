-module(wes_lock_null).

-behaviour(wes_lock).

%% {via, , }  api.
-export([send/2,
         whereis_name/1,
         unregister_name/1,
         register_name/2]).

%% Actor stuff
-export([register_actor/3,
         actor_timeout/3,
         unregister_actor/3,
         channel_for_actor/1]).

%% Channel stuff
-export([channel_timeout/1]).

send(Id, Event) ->
    exit({badarg, {Id, Event}}).

whereis_name(_Id) ->
    undefined.

unregister_name(_Id) ->
    ok.

register_name(_Id, _Pid) ->
    ok.

register_actor(_Id, _ChannelType, _ChannelName) ->
    {ok, 1000}.

unregister_actor(_Id, _ChannelType, _ChannelName) ->
    ok.

channel_for_actor(_Id) ->
    undefined.

actor_timeout(_Name, _ChannelType, _ChannelName) ->
    ok.

channel_timeout(_Channel) ->
    ok.
