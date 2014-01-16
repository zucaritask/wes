-module(wactor_lock_ets).

%% API
-export([start/1,
         start_link/1,
         start_channel/2, start_channel/3,
         stop/1,
         status/1,
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

start(Timeout) ->
    Locker = {wactor_lock_ets,
              {wactor_lock_ets_srv, start_link, [Timeout]},
              permanent, 2000, worker, [wactor_lock_ets_srv]},
    supervisor:start_child(wactor_sup, Locker).

start_link(Timeout) ->
    wactor_lock_ets_srv:start_link(Timeout).

stop(ChannelName) ->
    wactor_channel:stop(ChannelName, ?MODULE).

status(ChannelName) ->
    wactor_channel:status(ChannelName, ?MODULE).

start_channel(ChannelName, Timeout) ->
    wactor_channel:start(ChannelName, ?MODULE, Timeout).

start_channel(ChannelName, StartActors, Timeout) ->
    wactor_channel:start(ChannelName, StartActors, ?MODULE, Timeout).

command(ChannelName, Message) ->
    wactor_channel:command(ChannelName, Message, ?MODULE).

event(ChannelName, Message) ->
    wactor_channel:event(ChannelName, Message, ?MODULE).

read(ActorName, Message) ->
    wactor_channel:read(ActorName, Message, ?MODULE).

register_actor(ChannelName, ActorName, CbMod, DbMod, InitArgs) ->
    wactor_channel:register_actor(
      ChannelName, ActorName, CbMod, DbMod, InitArgs, ?MODULE).


%% ---------------------------------------------------------------------------
%% Lib callback

send(Id, Event) ->
    case wactor_lock_ets_srv:read({channel, Id}) of
        {ok, Pid} ->
            Pid ! Event;
        {error, not_found} ->
            exit({badarg, {Id, Event}})
    end.

whereis_name(Id) ->
    case wactor_lock_ets_srv:read({channel, Id}) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            undefined
    end.

unregister_name(Id) ->
    %% Assumed called from user process.
    ok = wactor_lock_ets_srv:release({channel, Id}, self()).

register_name(Id, Pid) ->
    ok = wactor_lock_ets_srv:lock({channel, Id}, Pid),
    yes.

register_actor(Id, Channel) ->
    error_logger:info_msg("registring actor ~p ~p", [Id, Channel]),
    ok = wactor_lock_ets_srv:lock({actor, Id}, Channel).

unregister_actor(Id, Channel) ->
    error_logger:info_msg("unregistring actor ~p ~p", [Id, Channel]),
    ok = wactor_lock_ets_srv:release({actor, Id}, Channel).

channel_for_actor(Id) ->
    case wactor_lock_ets_srv:read({actor, Id}) of
        {ok, Channel} ->
            Channel;
        {error, not_found} ->
            undefined
    end.

actor_timeout(Name, Channel) ->
    wactor_lock_ets_srv:extend_lease(Name, Channel).

channel_timeout(Channel) ->
    wactor_lock_ets_srv:extend_lease(Channel, self()).
