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

init([ChannelName, Actors, LockerMod, LockTimeout]) ->
    timer:send_after(LockTimeout, lock_timeout),
    ActorData = [wes_actor:init(ChannelName, Act) || Act <- Actors],
    {ok, #state{name = ChannelName,
                lock_timeout_interval = LockTimeout,
                locker_mod = LockerMod,
                actors = ActorData}}.

handle_call({command, Message}, _From,
            #state{name = Name, actors = []} = State) ->
    error_logger:error_msg("Channel has no actors ~p got command ~p",
                           [Name, Message]),
    {reply, ok, State};
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
                {reply, ok, State#state{actors = NewActors}}
        end
    catch throw:Reason ->
            {stop, normal, {error, Reason}, State}
    end;
handle_call({read, ActorName, Message}, _From,
            #state{actors = Actors} = State) ->
    Actor = wes_actor:list_get(ActorName, Actors),
    Reply = wes_actor:read(Actor, Message),
    {reply, Reply, State};
handle_call({register_actor, ActorName, CbMod, DbMod, LockerMod, InitArgs}, _From,
            #state{actors = Actors, name = ChannelName} = State) ->
    try
        case wes_actor:init(ChannelName, {ActorName, CbMod, DbMod, LockerMod,
                                      InitArgs}) of
            {ok, Actor} ->
                NewActors = wes_actor:list_add(ActorName, Actor, Actors),
                {reply, ok, State#state{actors = NewActors}};
            {stop, Actor} ->
                NewActors = wes_actor:list_add(ActorName, Actor, Actors),
                {stop, normal, ok, State#state{actors = NewActors}}
        end
    catch throw:Reason ->
            error_logger:info_msg("Error ~p", [Reason]),
            {stop, normal, {error, Reason}, State}
    end;
handle_call(stop, _From, State) ->
    error_logger:info_msg("Stopp command", []),
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
                {noreply, State#state{actors = NewActors}};
           true ->
                {stop, normal, State#state{actors = NewActors}}
        end
    catch throw:_Reason ->
            error_logger:info_msg("Error ~p", [_Reason]),
            {stop, normal, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(lock_timeout, #state{name = Name, actors = Actors,
                                 lock_timeout_interval = Timeout,
                                 locker_mod = LockerMod} = State) ->
    channel_lock_timeout(Name, LockerMod),
    lists:foreach(
      fun(A0) -> wes_actor:lock_timeout(A0, Name) end,
      Actors),
    timer:send_after(Timeout, lock_timeout),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

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


%% ---------------------------------------------------------------------------
%% Naming of channels

channel_deregister_name(ChannelName, LockerMod) ->
    LockerMod:unregister_name(ChannelName).

channel_lock_timeout(Channel, LockerMod) ->
    LockerMod:channel_timeout(Channel).
