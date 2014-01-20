-module(wes_locker).

%% API
-export([start/6,
         start_channel/2, start_channel/3,
         stop/1,
         command/2,
         event/2,
         read/2,
         channel_timeout/1,
         register_actor/5]).

%% {via, , }  api.
-export([send/2,
         whereis_name/1,
         unregister_name/1,
         register_name/2]).

%% Actor stuff
-export([register_actor/2,
         actor_timeout/2,
         unregister_actor/2,
         channel_for_actor/1]).

%% ---------------------------------------------------------------------------
%% User API

start(PrimaryNodes, Replicas, W, LeaseExpireInterval, LockExpireInterval,
      PushTransInterval) ->
    Locker = {wes_locker,
              {locker, start_link,
               [W, LeaseExpireInterval, LockExpireInterval,
                PushTransInterval]},
              permanent, 2000, worker, [locker]},
    supervisor:start_child(wes_sup, Locker),
    ok = locker:set_nodes(PrimaryNodes, PrimaryNodes, Replicas).

stop(ChannelName) ->
    wes_channel:stop(ChannelName, ?MODULE).

start_channel(ChannelName, Timeout) ->
    wes_channel:start(ChannelName, ?MODULE, Timeout).

start_channel(ChannelName, StartActors, Timeout) ->
    wes_channel:start(ChannelName, StartActors, ?MODULE, Timeout).

command(ChannelName, Message) ->
    wes_channel:command(ChannelName, Message, ?MODULE).

event(ChannelName, Message) ->
    wes_channel:event(ChannelName, Message, ?MODULE).

read(ActorName, Message) ->
    wes_channel:read(ActorName, Message, ?MODULE).

register_actor(ChannelName, ActorName, CbMod, DbMod, InitArgs) ->
    wes_channel:register_actor(
      ChannelName, ActorName, CbMod, DbMod, InitArgs, ?MODULE).


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
    {ok, _, _, _} = locker:release({channel, Id}, self()).

register_name(Id, Pid) ->
    case locker:lock({channel, Id}, Pid, locker_lease_duration()) of
        {ok, _, _, _} ->
            yes;
        {error, no_quorum} ->
            no
    end.

locker_lease_duration() ->
    1000 * 60 * 5. %% fixme config.

register_actor(Id, Channel) ->
    error_logger:info_msg("registring actor ~p ~p", [Id, Channel]),
    case locker:lock({actor, Id}, Channel, locker_lease_duration()) of
        {ok, _, _, _} ->
            ok;
        {error, no_quorum} ->
            {error, no_quorum}
    end.

unregister_actor(Id, Channel) ->
    error_logger:info_msg("unregistring actor ~p ~p", [Id, Channel]),
    {ok, _, _, _} = locker:release({actor, Id}, Channel).

channel_for_actor(Id) ->
    case locker:dirty_read({actor, Id}) of
        {ok, Channel} ->
            Channel;
        {error, not_found} ->
            undefined
    end.

actor_timeout(Name, Channel) ->
    locker:extend_lease(Name, Channel, locker_lease_duration()).

channel_timeout(Channel) ->
    locker:extend_lease(Channel, self(), locker_lease_duration()).
