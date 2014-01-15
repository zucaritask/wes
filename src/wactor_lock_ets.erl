-module(wactor_lock_ets).


%% API
-export([start/0,
         start_link/0,
         start_channel/1, start_channel/2,
         stop/1,
         command/2,
         event/2,
         read/2,
         register_actor/5]).

%% {via, , }  api.
-export([send/2,
         whereis_name/1,
         unregister_name/1,
         register_name/2]).

%% Actor stuff
-export([register_actor/2,
         unregister_actor/2,
         channel_for_actor/1]).

%% Channel callbacks
-export([timeout/1]).

%% ---------------------------------------------------------------------------
%% User API

start() ->
    Locker = {wactor_lock_ets,
              {wactor_lock_ets_srv, start_link, []},
              permanent, 2000, worker, [wactor_lock_ets_srv]},
    supervisor:start_child(wactor_sup, Locker).

%% for testing
start_link() ->
    wactor_lock_ets_srv:start_link().

stop(ChannelName) ->
    wactor_channel:stop(ChannelName, ?MODULE).

start_channel(ChannelName) ->
    wactor_channel:start(ChannelName, ?MODULE).

start_channel(ChannelName, StartActors) ->
    wactor_channel:start(ChannelName, StartActors, ?MODULE).

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

timeout(_) ->
    ok.
