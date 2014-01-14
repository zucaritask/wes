-module(wactor_locker).

%% API
-export([start/1, start/2,
         command/2,
         event/2,
         read/2,
         register_actor/4]).

%% {via, , }  api.
-export([send/2,
         whereis_name/1,
         unregister_name/1,
         register_name/2]).

%% Actor stuff
-export([register_actor/2,
         unregister_actor/2,
         channel_for_actor/1]).

%% ---------------------------------------------------------------------------
%% User API

start(ChannelName) ->
    wactor_channel:start(ChannelName, ?MODULE).

start(ChannelName, StartActors) ->
    wactor_channel:start(ChannelName, StartActors, ?MODULE).

command(ChannelName, Message) ->
    wactor_channel:command(ChannelName, Message, ?MODULE).

event(ChannelName, Message) ->
    wactor_channel:event(ChannelName, Message, ?MODULE).

read(ActorName, Message) ->
    wactor_channel:read(ActorName, Message, ?MODULE).

register_actor(ChannelName, ActorName, CbMod, InitArgs) ->
    wactor_channel:register_actor(
      ChannelName, ActorName, CbMod, InitArgs, ?MODULE).


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
            io:format("{error, no_quorum} ->"),
            no
    end.

locker_lease_duration() ->
    10000. %% fixme config.

register_actor(Id, Channel) ->
    case locker:lock({actor, Id}, Channel, locker_lease_duration()) of
        {ok, _, _, _} ->
            ok;
        {error, no_quorum} ->
            {error, no_quorum}
    end.

unregister_actor(Id, Channel) ->
    {ok, _, _, _} = locker:release({actor, Id}, Channel).

channel_for_actor(Id) ->
    case locker:dirty_read({actor, Id}) of
        {ok, Channel} ->
            Channel;
        {error, not_found} ->
            undefined
    end.
