-module(wes_channel).

-behaviour(gen_server).

-include("wes_internal.hrl").

%% API
-export([start/3,
         stop/2,
         command/4,
         event/4,
         read/3,
         status/2,
         register_actor/5,
         ensure_actor/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(channel,
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
    gen_server:start(channel_name(ChannelName, LockerMod), ?MODULE,
                     [ChannelType, ChannelName, StartActors, Config], []).

stop(ChannelType, ChannelName) ->
    #channel_config{locker_mod = ChannelLockerMod} =
        wes_config:channel(ChannelType),
    call(ChannelName, ChannelLockerMod, stop).

status(ChannelType, ChannelName) ->
    #channel_config{locker_mod = LockerMod} = wes_config:channel(ChannelType),
    case LockerMod:whereis_name(ChannelName) of
        undefined -> {error, not_found};
        Pid when is_pid(Pid) -> {ok, Pid}
    end.

command(ChannelType, ChannelName, CmdName, CmdPayload) ->
    #channel_config{locker_mod = ChannelLockerMod} = Config =
        wes_config:channel(ChannelType),
    Payload = {command, CmdName, CmdPayload, Config},
    call(ChannelName, ChannelLockerMod, Payload).

event(ChannelType, ChannelName, EvName, EvPayload) ->
    #channel_config{locker_mod = LockerMod} = ChannelConfig =
        wes_config:channel(ChannelType),
    gen_server:cast(channel_name(ChannelName, LockerMod),
                    {event, EvName, EvPayload, ChannelConfig}).

read(ActorType, ActorName, Message) ->
    #actor_config{locker_mod = ActorLockerMod} = ActorConfig =
        wes_config:actor(ActorType),
    case actor_name_to_channel(ActorName, ActorLockerMod) of
        {ChannelType, ChannelName} ->
            #channel_config{locker_mod = ChannelLockerMod} = ChannelConfig =
                wes_config:channel(ChannelType),
            Payload = {read, ActorName, Message, ChannelConfig, ActorConfig},
            call(ChannelName, ChannelLockerMod, Payload);
        undefined ->
            erlang:error(actor_not_active)
    end.

register_actor(ChannelType, ChannelName, ActorType, ActorName, InitArgs) ->
    #channel_config{locker_mod = ChannelLockerMod} = ChannelConfig =
        wes_config:channel(ChannelType),
    Payload = {register_actor, ActorName, ActorType, InitArgs,
               ChannelConfig},
    call(ChannelName, ChannelLockerMod, Payload).

ensure_actor(ChannelType, ChannelName, ActorType, ActorName, InitArgs) ->
    #channel_config{locker_mod = ChannelLockerMod} = ChannelConfig =
        wes_config:channel(ChannelType),
    Payload = {ensure_actor, ActorName, ActorType, InitArgs, ChannelConfig},
    call(ChannelName, ChannelLockerMod, Payload).


call(ChannelName, ChannelLockerMod, Payload) ->
    try
        gen_server:call(channel_name(ChannelName, ChannelLockerMod), Payload)
    catch exit:{noproc, _} ->
            erlang:error(channel_not_started)
    end.

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
    Now = wes_timeout:now_milli(),
    ChannelConfig = wes_config:channel(ChannelType),
    try
        State = channel__init(ChannelType, ChannelName, ActorArgs, Now,
                              ChannelConfig),
        timeout_reply({ok, State})
    catch throw:Reason ->
            {stop, Reason}
    end.

handle_call({command, CmdName, _CmdPayload, ChannelConfig}, _From,
            #channel{actors = []} = State) ->
    channel__dead_letter_handle(CmdName, ChannelConfig, State),
    Now = wes_timeout:now_milli(),
    State2 = update_message_timeout(State, Now),
    timeout_reply({reply, ok, State2});
handle_call({command, CmdName, CmdPayload, ChannelConfig}, _From, State) ->
    #channel_config{stats_mod = StatsMod} = ChannelConfig,
    Now = wes_timeout:now_milli(),
    try
        {NewActors, ShouldStop} = channel__command(CmdName, CmdPayload, State),
        if ShouldStop ->
                error_logger:info_msg("stop command ~p:~p",
                                      [State#channel.name, CmdName]),
                {stop, normal, ok, State#channel{actors = NewActors}};
           true ->
                State2 = update_message_timeout(State, Now),
                timeout_reply({reply, ok, State2#channel{actors = NewActors}})
        end
    catch throw:Reason ->
            {stop, normal, {error, Reason}, State}
    after
        StatsMod:stat(command, CmdName)
    end;
handle_call({read, ActorName, Name, ChannelConfig, ActorConfig}, _From,
            State) ->
    Now = wes_timeout:now_milli(),
    Reply = channel__read(ActorName, Name, ChannelConfig, ActorConfig, State),
    State2 = update_message_timeout(State, Now),
    timeout_reply({reply, Reply, State2});
handle_call({register_actor, ActorName, ActorType, InitArgs, Config},
            _From, State) ->
    do_add_actor(ActorName, ActorType, InitArgs, Config, State);
handle_call({ensure_actor, ActorName, ActorType, InitArgs, Config},
            _From, #channel{actors = Actors} = State) ->
    case wes_actor:list_find(ActorName, Actors) of
        {ok, _} ->
            timeout_reply({reply, ok, State});
        false ->
            do_add_actor(ActorName, ActorType, InitArgs, Config, State)
    end;
handle_call(stop, _From, State) ->
    error_logger:info_msg("Stop command", []),
    {stop, normal, ok, State}.

handle_cast({event, EvName, EvPayload, ChannelConfig}, State) ->
    #channel_config{stats_mod = StatsMod} = ChannelConfig,
    Now = wes_timeout:now_milli(),
    try
        {NewActors, ShouldStop} = channel__command(EvName, EvPayload, State),
        if ShouldStop ->
                error_logger:info_msg("stop event ~p:~p",
                                      [State#channel.name, EvName]),
                {stop, normal, State#channel{actors = NewActors}};
           true ->
                State2 = update_message_timeout(State, Now),
                timeout_reply({noreply, State2#channel{actors = NewActors}})
        end
    catch throw:_Reason ->
            error_logger:info_msg("Error ~p", [_Reason]),
            {stop, normal, State}
    after
        StatsMod:stat(event, EvName)
    end;
handle_cast(_Msg, State) ->
    timeout_reply({noreply, State}).

handle_info(timeout, #channel{type = ChannelType,
                              timeouts = Timeouts} = State) ->
    {Name, _} = wes_timeout:next(Timeouts),
    error_logger:info_msg("Timeout ~p", [Name]),
    Now = wes_timeout:now_milli(),
    %% FIXME: Check if the times match.
    ChannelConfig = wes_config:channel(ChannelType),
    case channel__timeout(Name, Now, ChannelConfig, State) of
        {stop, NewState} ->
            {stop, normal, NewState};
        NewState ->
            timeout_reply({noreply, NewState})
    end;
handle_info(Info, State) ->
    error_logger:info_msg("FIXME not handled info ~p", [Info]),
    %% FIXME: Broadcast info to actors that have exported some info
    %% handling function.
    timeout_reply({noreply, State}).

terminate(Reason, #channel{name = ChannelName, type = ChannelType} = State) ->
    error_logger:info_msg("Terminating ~p ~p", [ChannelName, Reason]),
    ChannelConfig = wes_config:channel(ChannelType),
    channel__stop(Reason, ChannelConfig, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_add_actor(ActorName, ActorType, InitArgs, Config, State) ->
    Now = wes_timeout:now_milli(),
    try
        State2 = channel__register_actor(ActorName, ActorType, InitArgs,
                                         Config, Now, State),
        State3 = update_message_timeout(State2, Now),
        timeout_reply({reply, ok, State3})
    catch throw:Reason ->
            error_logger:info_msg("Error ~p", [Reason]),
            {stop, normal, {error, Reason}, State}
    end.

timeout_reply({noreply, State}) ->
    {noreply, State, do_timeout_reply(State)};
timeout_reply({ok, State}) ->
    {ok, State, do_timeout_reply(State)};
timeout_reply({reply, Reply, State}) ->
    {reply, Reply, State, do_timeout_reply(State)}.

do_timeout_reply(#channel{timeouts = Timeouts}) ->
    {_Name, Time} = wes_timeout:next(Timeouts),
    wes_timeout:time_diff(Time, wes_timeout:now_milli()).

update_message_timeout(#channel{timeouts = Timeouts} = State,  Now) ->
    NewTimeouts = wes_timeout:reset(message_timeout, Now, Timeouts),
    State#channel{timeouts = NewTimeouts}.

%% ---------------------------------------------------------------------------
%% Naming of channels

channel_deregister_name(ChannelName, LockerMod) ->
    LockerMod:unregister_name(ChannelName).

channel_lock_timeout(Channel, LockerMod) ->
    LockerMod:channel_timeout(Channel).

%% ---------------------------------------------------------------------------
%% Channel logic

channel__init(ChannelType, ChannelName, ActorArgs, Now, ChannelConfig) ->
    #channel_config{stats_mod = StatsMod,
                    message_timeout = MessageTimeout,
                    lock_timeout_interval = LockTimeout} = ChannelConfig,
    {Actors, ActorTimeouts} =
        actor_inits(ChannelType, ChannelName, ActorArgs, Now, wes_timeout:new(),
                    StatsMod),
    Timeouts =
        wes_timeout:add(message_timeout, MessageTimeout, Now,
                        wes_timeout:add(channel_lock_timeout, LockTimeout,
                                        Now, ActorTimeouts)),
    StatsMod:stat(start, channel),
    #channel{name = ChannelName,
             type = ChannelType,
             timeouts = Timeouts,
             actors = Actors}.

actor_inits(ChannelType, ChannelName, ActorArgs, Now, Timeouts, StatsMod) ->
    lists:mapfoldl(
      fun(Act, TAcc0) ->
              {Actor, TimeOuts} = wes_actor:init(ChannelType, ChannelName, Act),
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

channel__dead_letter_handle(CmdName, Config, State) ->
    #channel{name = ChannelName} = State,
    #channel_config{stats_mod = StatsMod} = Config,
    error_logger:error_msg("Channel has no actors ~p got command ~p",
                           [ChannelName, CmdName]),
    StatsMod:stat(command, CmdName).

channel__command(CmdName, CmdPayload, State) ->
    #channel{actors = Actors, type = ChannelType, name = ChannelName} = State,
    wes_util:zffoldl(
      fun(A0, Stop) ->
              case wes_actor:act(A0, CmdName, CmdPayload) of
                  {Actor, AStop, true} ->
                      wes_actor:save(Actor),
                      wes_actor:deregister_name(Actor, ChannelType,ChannelName),
                      {false, Stop or AStop};
                  {Actor, AStop, false} ->
                      {ok, Actor, Stop or AStop}
              end
      end,
      false,
      Actors).

channel__read(ActorName, Name, ChannelConfig, ActorConfig, State) ->
    #channel{actors = Actors} = State,
    #channel_config{stats_mod = StatsMod} = ChannelConfig,
    Actor = wes_actor:list_get(ActorName, Actors),
    StatsMod:stat(read, Name),
    wes_actor:read(Actor, Name, ActorConfig).

channel__register_actor(ActorName, ActorType, InitArgs,
                        Config, Now, State) ->
    #channel{actors = Actors, name = ChannelName,
             timeouts = Timeouts, type = ChannelType} = State,
    #channel_config{stats_mod = StatsMod} = Config,
    {[Actor], NewTimeouts} =
        actor_inits(ChannelType, ChannelName,
                    [{ActorName, ActorType, InitArgs}],
                    Now, Timeouts, StatsMod),
    NewActors = wes_actor:list_add(ActorName, Actor, Actors),
    State#channel{actors = NewActors, timeouts = NewTimeouts}.

channel__stop(Reason, ChannelConfig, State) ->
    #channel{type = ChannelType, name = Name, actors = Actors} = State,
    #channel_config{
       locker_mod = LockerMod,
       stats_mod = StatsMod} = ChannelConfig,
    if Reason =:= normal ->
            StatsMod:stat(stop, normal),
            lists:foreach(fun(A0) -> wes_actor:save(A0) end, Actors);
       true ->
            StatsMod:stat(stop, error)
    end,
    lists:foreach(
      fun(A0) -> wes_actor:deregister_name(A0, ChannelType, Name) end,
      Actors),
    channel_deregister_name(Name, LockerMod).

channel__timeout(message_timeout, _Now, ChannelConfig, State) ->
    #channel_config{stats_mod = StatsMod} = ChannelConfig,
    StatsMod:stat(timeout, messages),
    {stop, State};
channel__timeout(channel_lock_timeout = TimeoutName,
                 Now,
                 ChannelConfig,
                 #channel{timeouts = Timeouts,
                          name = ChannelName} = State) ->
    #channel_config{stats_mod = StatsMod,
                    locker_mod = LockerMod} = ChannelConfig,
    StatsMod:stat(timeout, channel),
    channel_lock_timeout(ChannelName, LockerMod),
    NewTimeouts = wes_timeout:reset(TimeoutName, Now, Timeouts),
    State#channel{timeouts = NewTimeouts};
channel__timeout({actor_timeout, ActorName, TimeoutData} = TimeoutName,
                 Now, ChannelConfig,
                 #channel{actors = Actors,
                          timeouts = Timeouts} = State) ->
    #channel_config{stats_mod = StatsMod} = ChannelConfig,
    StatsMod:stat(timeout, actor),
    wes_actor:timeout(wes_actor:list_get(ActorName, Actors), TimeoutData),
    NewTimeouts = wes_timeout:reset(TimeoutName, Now, Timeouts),
    State#channel{timeouts = NewTimeouts}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("wes_actor.hrl").

channel_timeout_test() ->
    ChannelName = channel1,
    ChannelType = ch_type1,
    ActorName = act1,
    ActorType = ac_type1,
    Interval = 60,
    InitNow = 1,
    Timeouts = wes_timeout:add(channel_lock_timeout, Interval, InitNow,
                               wes_timeout:new()),
    State0 = #channel{type = ChannelType,
                      name = ChannelName,
                      timeouts = Timeouts,
                      actors = [#actor{name = ActorName,
                                       type = ActorType,
                                       state = []}]},
    ?assertEqual([{channel_lock_timeout, {InitNow + Interval, Interval}}],
                 wes_timeout:to_list(State0#channel.timeouts)),

    ChannelConfig = #channel_config{locker_mod = wes_lock_null,
                                    lock_timeout_interval = Interval,
                                    message_timeout = 300,
                                    stats_mod = wes_stats_null},
    TimeoutNow = 59,
    TimeoutMsg = channel_lock_timeout,
    State1 = channel__timeout(TimeoutMsg, TimeoutNow, ChannelConfig, State0),
    ?assertEqual([{channel_lock_timeout, {TimeoutNow + Interval, Interval}}],
                 wes_timeout:to_list(State1#channel.timeouts)),
    ok.

-endif. %% TEST
