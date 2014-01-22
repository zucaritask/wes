-module(wes_channel).

-behaviour(gen_server).

-include("wes.hrl").

%% API
-export([start/3,
         stop/2,
         command/4,
         event/4,
         read/4,
         status/2,
         register_actor/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {type,
         name,
         timeouts,
         actors = []}).

%%%===================================================================
%%% API
%%%===================================================================

start(ChannelType, ChannelName, StartActors) ->
    #channel_config{locker_mod = LockerMod} = Config =
        wes_config:channel(ChannelType),
    gen_server:start_link(channel_name(ChannelName, LockerMod), ?MODULE,
                     [ChannelType, ChannelName, StartActors, Config], []).

stop(ChannelType, ChannelName) ->
    #channel_config{locker_mod = LockerMod} = wes_config:channel(ChannelType),
    gen_server:call(channel_name(ChannelName, LockerMod), stop).

status(ChannelType, ChannelName) ->
    #channel_config{locker_mod = LockerMod} = wes_config:channel(ChannelType),
    case LockerMod:whereis_name(ChannelName) of
        undefined -> {error, not_found};
        Pid when is_pid(Pid) -> {ok, Pid}
    end.

command(ChannelType, ChannelName, CmdName, CmdPayload) ->
    #channel_config{locker_mod = LockerMod} = Config =
        wes_config:channel(ChannelType),
    gen_server:call(channel_name(ChannelName, LockerMod),
                    {command, CmdName, CmdPayload, Config}).

event(ChannelType, ChannelName, EvName, EvPayload) ->
    #channel_config{locker_mod = LockerMod} = ChannelConfig =
        wes_config:channel(ChannelType),
    gen_server:cast(channel_name(ChannelName, LockerMod),
                    {event, EvName, EvPayload, ChannelConfig}).

read(ChannelType, ActorType, ActorName, Message) ->
    #channel_config{locker_mod = ChannelLockerMod} = ChannelConfig =
        wes_config:channel(ChannelType),
    #actor_config{locker_mod = ActorLockerMod} = ActorConfig =
        wes_config:actor(ActorType),
    gen_server:call(
      channel_name(actor_name_to_channel(ActorName, ActorLockerMod),
                   ChannelLockerMod),
      {read, ActorName, Message, ChannelConfig, ActorConfig}).

register_actor(ChannelType, ChannelName, ActorType, ActorName, InitArgs) ->
    #channel_config{locker_mod = ChannelLockerMod} = ChannelConfig =
        wes_config:channel(ChannelType),
    gen_server:call(channel_name(ChannelName, ChannelLockerMod),
                    {register_actor, ActorName, ActorType, InitArgs,
                     ChannelConfig}).

%% ---------------------------------------------------------------------------
%% API Helpers

channel_name(ChannelName, LockerMod) ->
    {via, LockerMod, ChannelName}.

actor_name_to_channel(ActorName, LockerMod) ->
    LockerMod:channel_for_actor(ActorName).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ChannelType, ChannelName, ActorArgs, _Config]) ->
    Now = wes_timeout:now(),
    #channel_config{stats_mod = StatsMod,
                    lock_timeout_interval = LockTimeout} =
        wes_config:channel(ChannelType),
    {Actors, ActorTimeouts} =
        actor_inits(ChannelName, ActorArgs, Now, wes_timeout:new(), StatsMod),
    Timeouts = wes_timeout:add(channel_lock_timeout, LockTimeout,
                               wes_timeout:now(), ActorTimeouts),
    StatsMod:stat(start, channel),
    timeout_reply(
      {ok, #state{name = ChannelName,
                  type = ChannelType,
                  timeouts = Timeouts,
                  actors = Actors}}).

handle_call({command, CmdName, _CmdPayload, Config}, _From,
            #state{name = ChannelName,
                   actors = []} = State) ->
    #channel_config{stats_mod = StatsMod} = Config,
    error_logger:error_msg("Channel has no actors ~p got command ~p",
                           [ChannelName, CmdName]),
    StatsMod:stat(command, CmdName),
    timeout_reply({reply, ok, State});
handle_call({command, CmdName, CmdPayload, Config}, _From,
            #state{name = ChannelName,
                   actors = Actors} = State) ->
    #channel_config{stats_mod = StatsMod} = Config,
    try
        {NewActors, ShouldStop} =
            lists:mapfoldl(
              fun(A0, Stop) ->
                      {Actor, AStop} = wes_actor:act(A0, CmdName, CmdPayload),
                      {Actor, Stop or AStop}
              end,
              false,
              Actors),
        if ShouldStop ->
                error_logger:info_msg("stop command ~p:~p",
                                      [ChannelName, CmdName]),
                {stop, normal, ok, State#state{actors = NewActors}};
           true ->
                timeout_reply({reply, ok, State#state{actors = NewActors}})
        end
    catch throw:Reason ->
            {stop, normal, {error, Reason}, State}
    after
        StatsMod:stat(command, CmdName)
    end;
handle_call({read, ActorName, Name, ChannelConfig, ActorConfig}, _From,
            #state{actors = Actors} = State) ->
    #channel_config{stats_mod = StatsMod} = ChannelConfig,
    Actor = wes_actor:list_get(ActorName, Actors),
    Reply = wes_actor:read(Actor, Name, ActorConfig),
    StatsMod:stat(read, Name),
    timeout_reply({reply, Reply, State});
handle_call({register_actor, ActorName, ActorType, InitArgs, Config},
            _From, #state{actors = Actors, name = ChannelName,
                          timeouts = Timeouts} = State) ->
    #channel_config{stats_mod = StatsMod} = Config,
    Now = wes_timeout:now(),
    try
        {[Actor], NewTimeouts} =
            actor_inits(ChannelName, [{ActorName, ActorType, InitArgs}],
                        Now, Timeouts, StatsMod),
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

handle_cast({event, EvName, EvPayload, ChannelConfig},
            #state{actors = Actors} = State) ->
    #channel_config{stats_mod = StatsMod} = ChannelConfig,
    try
        {NewActors, ShouldStop} =
            lists:mapfoldl(
              fun(A0, Stop) ->
                      {Actor, AStop} = wes_actor:act(A0, EvName, EvPayload),
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
    after
        StatsMod:stat(event, EvName)
    end;
handle_cast(_Msg, State) ->
    timeout_reply({noreply, State}).

handle_info(timeout, #state{timeouts = Timeouts} = State) ->
    {Name, _} = wes_timeout:next(Timeouts),
    %% FIXME: Check if the times match.
    handle_timeout(Name, State);
handle_info(_Info, State) ->
    timeout_reply({noreply, State}).

terminate(normal, #state{type = ChannelType, name = Name, actors = Actors}) ->
    #channel_config{
       locker_mod = LockerMod,
       stats_mod = StatsMod} = wes_config:channel(ChannelType),
    StatsMod:stat(stop, normal),
    lists:foreach(fun(A0) -> wes_actor:save(A0) end, Actors),
    lists:foreach(
      fun(A0) -> wes_actor:deregister_name(A0, Name) end,
      Actors),
    channel_deregister_name(Name, LockerMod),
    ok;
terminate(Reason, #state{type = ChannelType, name = Name, actors = Actors}) ->
    #channel_config{
       locker_mod = LockerMod,
       stats_mod = StatsMod} = wes_config:channel(ChannelType),
    StatsMod:stat(stop, error),
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
                      type = ChannelType,
                      name = ChannelName} = State) ->
    #channel_config{
       locker_mod = LockerMod,
       stats_mod = StatsMod} = wes_config:channel(ChannelType),

    StatsMod:stat(timeout, channel_lock),
    channel_lock_timeout(ChannelName, LockerMod),
    wes_timeout:reset(TimeoutName, wes_timeout:now(), Timeouts),
    timeout_reply({noreply, State});
handle_timeout({actor_timeout, ActorName, TimeoutData} = TimeoutName,
               #state{actors = Actors,
                      type = ChannelType,
                      timeouts = Timeouts} = State) ->
    #channel_config{stats_mod = StatsMod} = wes_config:channel(ChannelType),
    StatsMod:stat(timeout, actor_timeout),
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

actor_inits(ChannelName, ActorArgs, Now, Timeouts, StatsMod) ->
    lists:mapfoldl(
      fun(Act, TAcc0) ->
              {Actor, TimeOuts} = wes_actor:init(ChannelName, Act),
              StatsMod:stat(start, actor),
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
