-module(wes_channel).

-behaviour(gen_server).

-include("wes.hrl").

%% API
-export([start/3, start/4,
         stop/2,
         command/3,
         event/3,
         read/4,
         status/2,
         register_actor/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {name,
         locker_mod,
         timeouts,
         lock_timeout_interval,
         actors = []}).

%%%===================================================================
%%% API
%%%===================================================================

start(ChannelName, LockerMod, LockTimeout) ->
    start(ChannelName, [], LockerMod, LockTimeout).

start(ChannelName, StartActors, LockerMod, LockTimeout) ->
    gen_server:start_link(channel_name(ChannelName, LockerMod), ?MODULE,
                     [ChannelName, StartActors, LockerMod, LockTimeout], []).

stop(ChannelName, LockerMod) ->
    gen_server:call(channel_name(ChannelName, LockerMod), stop).

status(ChannelName, LockerMod) ->
    case LockerMod:whereis_name(ChannelName) of
        undefined -> {error, not_found};
        Pid when is_pid(Pid) -> {ok, Pid}
    end.

command(ChannelName, Message, LockerMod) ->
    gen_server:call(channel_name(ChannelName, LockerMod), {command, Message}).

event(ChannelName, Message, LockerMod) ->
    gen_server:cast(channel_name(ChannelName, LockerMod), {event, Message}).

read(ActorName, Message, ActorLockerMod, ChannelLockerMod) ->
    gen_server:call(
      channel_name(actor_name_to_channel(ActorName, ActorLockerMod),
                   ChannelLockerMod),
      {read, ActorName, Message}).

register_actor(ChannelName, ActorName, CbMod, DbMod, ActorLockerMod, InitArgs,
               ChannelLockerMod) ->
    gen_server:call(channel_name(ChannelName, ChannelLockerMod),
                    {register_actor, ActorName, CbMod, DbMod, ActorLockerMod,
                     InitArgs}).

%% ---------------------------------------------------------------------------
%% API Helpers

channel_name(ChannelName, LockerMod) ->
    {via, LockerMod, ChannelName}.

actor_name_to_channel(ActorName, LockerMod) ->
    LockerMod:channel_for_actor(ActorName).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ChannelName, ActorArgs, LockerMod, LockTimeout]) ->
    Now = wes_timeout:now(),
    {Actors, ActorTimeouts} =
        actor_inits(ChannelName, ActorArgs, Now, wes_timeout:new()),
    Timeouts = wes_timeout:add(channel_lock_timeout, LockTimeout,
                               wes_timeout:now(), ActorTimeouts),
    timeout_reply(
      {ok, #state{name = ChannelName,
                  timeouts = Timeouts,
                  locker_mod = LockerMod,
                  actors = Actors}}).

handle_call({command, Message}, _From,
            #state{name = Name, actors = []} = State) ->
    error_logger:error_msg("Channel has no actors ~p got command ~p",
                           [Name, Message]),
    timeout_reply({reply, ok, State});
handle_call({command, Message}, _From,
            #state{actors = Actors} = State) ->
    try
        {NewActors, ShouldStop} =
            lists:mapfoldl(
              fun(A0, Stop) ->
                      {Actor, AStop} = wes_actor:act(A0, Message),
                      {Actor, Stop or AStop}
              end,
              false,
              Actors),
        if ShouldStop ->
                error_logger:info_msg("stop command~p", [Message]),
                {stop, normal, ok, State#state{actors = NewActors}};
           true ->
                timeout_reply({reply, ok, State#state{actors = NewActors}})
        end
    catch throw:Reason ->
            {stop, normal, {error, Reason}, State}
    end;
handle_call({read, ActorName, Message}, _From,
            #state{actors = Actors} = State) ->
    Actor = wes_actor:list_get(ActorName, Actors),
    Reply = wes_actor:read(Actor, Message),
    timeout_reply({reply, Reply, State});
handle_call({register_actor, ActorName, CbMod, DbMod, LockerMod, InitArgs},
            _From, #state{actors = Actors, name = ChannelName,
                          timeouts = Timeouts} = State) ->
    Now = wes_timeout:now(),
    try
        {[Actor], NewTimeouts} =
            actor_inits(ChannelName,
                        [{ActorName, CbMod, DbMod, LockerMod, InitArgs}],
                        Now, Timeouts),
        NewActors = wes_actor:list_add(ActorName, Actor, Actors),
        NewState = State#state{actors = NewActors, timeouts = NewTimeouts},
        timeout_reply({reply, ok, NewState})
    catch throw:Reason ->
            error_logger:info_msg("Error ~p", [Reason]),
            {stop, normal, {error, Reason}, State}
    end;
handle_call(stop, _From, State) ->
    error_logger:info_msg("Stop command", []),
    {stop, normal, ok, State}.

handle_cast({event, Event}, #state{actors = Actors} = State) ->
    try
        {NewActors, ShouldStop} =
            lists:mapfoldl(
              fun(A0, Stop) ->
                      {Actor, AStop} = wes_actor:act(A0, Event),
                      {Actor, Stop or AStop}
              end,
              false,
              Actors),
        if ShouldStop ->
                timeout_reply({noreply, State#state{actors = NewActors}});
           true ->
                {stop, normal, State#state{actors = NewActors}}
        end
    catch throw:_Reason ->
            error_logger:info_msg("Error ~p", [_Reason]),
            {stop, normal, State}
    end;
handle_cast(_Msg, State) ->
    timeout_reply({noreply, State}).

handle_info(timeout, #state{timeouts = Timeouts} = State) ->
    {Name, _} = wes_timeout:next(Timeouts),
    %% FIXME: Check if the times match.
    handle_timeout(Name, State);
handle_info(_Info, State) ->
    timeout_reply({noreply, State}).

terminate(normal, #state{name = Name, actors = Actors,
                         locker_mod = LockerMod}) ->
    lists:foreach(fun(A0) -> wes_actor:save(A0) end, Actors),
    lists:foreach(
      fun(A0) -> wes_actor:deregister_name(A0, Name) end,
      Actors),
    channel_deregister_name(Name, LockerMod),
    ok;
terminate(Reason, #state{name = Name, actors = Actors,
                         locker_mod = LockerMod}) ->
    error_logger:error_msg("Reason ~p", [Reason]),
    lists:foreach(
      fun(A0) -> wes_actor:deregister_name(A0, Name) end,
      Actors),
    channel_deregister_name(Name, LockerMod),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_timeout(channel_lock_timeout = TimeoutName,
               #state{timeouts = Timeouts,
                      name = ChannelName, locker_mod = LockerMod} = State) ->
    channel_lock_timeout(ChannelName, LockerMod),
    wes_timeout:reset(TimeoutName, wes_timeout:now(), Timeouts),
    timeout_reply({noreply, State});
handle_timeout({actor_timeout, ActorName, TimeoutData} = TimeoutName,
               #state{actors = Actors, timeouts = Timeouts} = State) ->
    wes_actor:timeout(wes_actor:list_get(ActorName, Actors), TimeoutData),
    wes_timeout:reset(TimeoutName, wes_timeout:now(), Timeouts),
    timeout_reply({noreply, State}).

timeout_reply({noreply, State}) ->
    {noreply, State, do_timeout_reply(State)};
timeout_reply({ok, State}) ->
    {ok, State, do_timeout_reply(State)};
timeout_reply({reply, Reply, State}) ->
    {reply, Reply, State, do_timeout_reply(State)}.

do_timeout_reply(#state{timeouts = Timeouts}) ->
    {_Name, Time} = wes_timeout:next(Timeouts),
    wes_timeout:time_diff(Time, wes_timeout:now()).

actor_inits(ChannelName, ActorArgs, Now, Timeouts) ->
    lists:mapfoldl(
      fun(Act, TAcc0) ->
              {Actor, TimeOuts} = wes_actor:init(ChannelName, Act),
              NewTacc =
                  lists:foldl(
                    fun({TimeoutName, Interval}, TAcc1) ->
                            wes_timeout:add(
                              {actor_timeout,
                               wes_actor:name(Actor), TimeoutName},
                              Interval, Now, TAcc1)
                    end,
                    TAcc0,
                    TimeOuts),
              {Actor, NewTacc}
      end,
      Timeouts,
      ActorArgs).




%% ---------------------------------------------------------------------------
%% Naming of channels

channel_deregister_name(ChannelName, LockerMod) ->
    LockerMod:unregister_name(ChannelName).

channel_lock_timeout(Channel, LockerMod) ->
    LockerMod:channel_timeout(Channel).
