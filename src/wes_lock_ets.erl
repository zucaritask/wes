-module(wes_lock_ets).

%% API
-export([start/1,
         stop/0,
         start_link/1,
         start_channel/3, start_channel/4,
         stop_channel/1,
         status/1,
         command/3,
         event/3,
         read/3,
         channel_timeout/1,
         register_actor/7]).

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
    Locker = {wes_lock_ets,
              {wes_lock_ets_srv, start_link, [Timeout]},
              permanent, 2000, worker, [wes_lock_ets_srv]},
    supervisor:start_child(wes_sup, Locker).

start_link(Timeout) ->
    wes_lock_ets_srv:start_link(Timeout).

stop() ->
    supervisor:terminate_child(wes_sup, wes_lock_ets),
    supervisor:delete_child(wes_sup, wes_lock_ets).

stop_channel(ChannelName) ->
    wes_channel:stop(ChannelName, ?MODULE).

status(ChannelName) ->
    wes_channel:status(ChannelName, ?MODULE).

start_channel(ChannelName, Timeout, StatsMod) ->
    wes_channel:start(ChannelName, ?MODULE, Timeout, StatsMod).

start_channel(ChannelName, StartActors, Timeout, StatsMod) ->
    wes_channel:start(ChannelName, StartActors, ?MODULE, Timeout, StatsMod).

command(ChannelName, CmdName, CmdMessage) ->
    wes_channel:command(ChannelName, CmdName, CmdMessage, ?MODULE).

event(ChannelName, CmdName, CmdMessage) ->
    wes_channel:event(ChannelName, CmdName, CmdMessage, ?MODULE).

read(ActorName, Message, ActorLockMod) ->
    wes_channel:read(ActorName, Message, ActorLockMod, ?MODULE).

register_actor(ChannelName, ActorName, CbMod, DbMod, DbConf, ActorLockMod,
               InitArgs) ->
    wes_channel:register_actor(
      ChannelName, ActorName, CbMod, DbMod, DbConf, ActorLockMod, InitArgs,
      ?MODULE).


%% ---------------------------------------------------------------------------
%% Lib callback

send(Id, Event) ->
    case wes_lock_ets_srv:read({channel, Id}) of
        {ok, Pid} ->
            Pid ! Event;
        {error, not_found} ->
            exit({badarg, {Id, Event}})
    end.

whereis_name(Id) ->
    case wes_lock_ets_srv:read({channel, Id}) of
        {ok, Pid} ->
            Pid;
        {error, not_found} ->
            undefined
    end.

unregister_name(Id) ->
    %% Assumed called from user process.
    ok = wes_lock_ets_srv:release({channel, Id}, self()).

register_name(Id, Pid) ->
    ok = wes_lock_ets_srv:lock({channel, Id}, Pid),
    yes.

register_actor(Id, Channel) ->
    error_logger:info_msg("registring actor ~p ~p", [Id, Channel]),
    ok = wes_lock_ets_srv:lock({actor, Id}, Channel),
    {ok, 1000}.

unregister_actor(Id, Channel) ->
    error_logger:info_msg("unregistring actor ~p ~p", [Id, Channel]),
    ok = wes_lock_ets_srv:release({actor, Id}, Channel).

channel_for_actor(Id) ->
    case wes_lock_ets_srv:read({actor, Id}) of
        {ok, Channel} ->
            Channel;
        {error, not_found} ->
            undefined
    end.

actor_timeout(Name, Channel) ->
    wes_lock_ets_srv:extend_lease({actor, Name}, Channel).

channel_timeout(Channel) ->
    wes_lock_ets_srv:extend_lease(Channel, self()).
