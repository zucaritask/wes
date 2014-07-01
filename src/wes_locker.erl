-module(wes_locker).

-behaviour(wes_lock).

%% API
-export([start/6]).

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
-export([channel_timeout/1,
         lock_renew_duration/0]).

%% ---------------------------------------------------------------------------
%% User API

start(PrimaryNodes, Replicas, W, LeaseExpireInterval, LockExpireInterval,
      PushTransInterval) ->
    Locker = {wes_locker,
              {locker, start_link,
               [W, LeaseExpireInterval, LockExpireInterval,
                PushTransInterval]},
              permanent, 2000, worker, [locker]},
    {ok, _} = supervisor:start_child(wes_sup, Locker),
    ok = locker:set_nodes(PrimaryNodes, PrimaryNodes, Replicas).

%% ---------------------------------------------------------------------------
%% Lib callback

send(Id, Event) ->
    case locker:dirty_read({channel, Id}) of
        {ok, Pid} ->
            Pid ! Event;
        {error, not_found} ->
            exit({badarg, {Id, Event}})
    end.

whereis_name(Id) ->
    case locker:dirty_read({channel, Id}) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            undefined
    end.

unregister_name(Id) ->
    %% Assumed called from user process.
    {ok, _, _, _} = locker:release({channel, Id}, self()),
    ok.

register_name(Id, Pid) ->
    case locker:lock({channel, Id}, Pid, lock_lease_duration()) of
        {ok, _, _, _} ->
            yes;
        {error, no_quorum} ->
            no
    end.

lock_lease_duration() ->
    application:get_env(wes, locker_lease_duration, 1000 * 60 * 5).

lock_renew_duration() ->
    application:get_env(wes, locker_renew_duration, 1000 * 60 * 2).

register_actor(Id, ChannelType, ChannelName) ->
    case locker:lock({actor, Id}, {ChannelType, ChannelName},
                     lock_lease_duration()) of
        {ok, _, _, _} ->
            {ok, [{{lock, ChannelType, ChannelName}, lock_renew_duration()}]};
        {error, no_quorum} ->
            {error, no_quorum}
    end.

unregister_actor(Id, ChannelType, ChannelName) ->
    {ok, _, _, _} = locker:release({actor, Id}, {ChannelType, ChannelName}),
    ok.

channel_for_actor(Id) ->
    case locker:dirty_read({actor, Id}) of
        {ok, {ChannelType, ChannelName}} ->
            {ChannelType, ChannelName};
        {error, not_found} ->
            undefined
    end.

actor_timeout(Name, ChannelType, ChannelName) ->
    ok = locker:extend_lease({actor, Name}, {ChannelType, ChannelName},
                             lock_lease_duration()).

channel_timeout(Channel) ->
    ok = locker:extend_lease({channel, Channel}, self(), lock_lease_duration()).
