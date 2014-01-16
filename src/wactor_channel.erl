-module(wactor_channel).

-behaviour(gen_server).

-include("wactor.hrl").

%% API
-export([start/3, start/4,
         stop/2,
         command/3,
         event/3,
         read/3,
         status/2,
         register_actor/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {name,
         locker_mod,
         lock_timeout_interval,
         actors = []}).

-record(actor,
        {name,
         cb_mod,
         db_mod,
         state,
         state_name = event}).

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

read(ActorName, Message, LockerMod) ->
    gen_server:call(
      channel_name(actor_name_to_channel(ActorName, LockerMod), LockerMod),
      {read, ActorName, Message}).

register_actor(ChannelName, ActorName, CbMod, DbMod, InitArgs, LockerMod) ->
    gen_server:call(channel_name(ChannelName, LockerMod),
                    {register_actor, ActorName, CbMod, DbMod, InitArgs}).

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
    ActorData = [actor_init(ChannelName, Act, LockerMod) || Act <- Actors],
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
                      {Actor, AStop} = actor_act(A0, Message),
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
    Actor = actor_get(ActorName, Actors),
    Reply = actor_read(Actor, Message),
    {reply, Reply, State};
handle_call({register_actor, ActorName, CbMod, DbMod, InitArgs}, _From,
            #state{actors = Actors, name = ChannelName,
                   locker_mod = LockerMod} = State) ->
    try
        case actor_init(ChannelName, {ActorName, CbMod, DbMod, InitArgs},
                        LockerMod) of
            {ok, Actor} ->
                NewActors = actor_add(ActorName, Actor, Actors),
                {reply, ok, State#state{actors = NewActors}};
            {stop, Actor} ->
                NewActors = actor_add(ActorName, Actor, Actors),
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
                      {Actor, AStop} = actor_act(A0, Event),
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
      fun(A0) -> actor_lock_timeout(A0#actor.name, Name, LockerMod) end,
      Actors),
    timer:send_after(Timeout, lock_timeout),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, #state{name = Name, actors = Actors,
                         locker_mod = LockerMod}) ->
    lists:foreach(fun(A0) -> actor_save(A0) end, Actors),
    lists:foreach(
      fun(A0) -> actor_deregister_name(A0#actor.name, Name, LockerMod) end,
      Actors),
    channel_deregister_name(Name, LockerMod),
    ok;
terminate(Reason, #state{name = Name, actors = Actors,
                         locker_mod = LockerMod}) ->
    error_logger:error_msg("Reason ~p", [Reason]),
    lists:foreach(
      fun(A0) -> actor_deregister_name(A0#actor.name, Name, LockerMod) end,
      Actors),
    channel_deregister_name(Name, LockerMod),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% ---------------------------------------------------------------------------
%% Naming of channels / actors

channel_deregister_name(ChannelName, LockerMod) ->
    LockerMod:unregister_name(ChannelName).

channel_lock_timeout(Channel, LockerMod) ->
    LockerMod:channel_timeout(Channel).

actor_register_name(Name, Channel, LockerMod) ->
    ok = LockerMod:register_actor(Name, Channel).

actor_deregister_name(Name, Channel, LockerMod) ->
    LockerMod:unregister_actor(Name, Channel).

actor_lock_timeout(Name, Channel, LockerMod) ->
    LockerMod:actor_timeout(Name, Channel).

%% ---------------------------------------------------------------------------
%% Actor

actor_init(ChannelName, {ActorName, ActorCb, DbMod, InitArgs}, LockerMod) ->
    Response =
        case DbMod:read(ActorCb:key(ActorName)) of
            {ok, {_Key, _Value} = Data} ->
                response(ActorCb:from_struct(Data));
            not_found ->
                response(ActorCb:init(InitArgs))
        end,
    actor_register_name(ActorName, ChannelName, LockerMod),
    #actor{name = ActorName,
           cb_mod = ActorCb,
           db_mod = DbMod,
           state_name = Response#actor_response.state_name,
           state = Response#actor_response.state}.

actor_save(#actor{state_name = StateName, cb_mod = CbMod,
                  name = ActorName,
                  state = ActorState, db_mod = DbMod}) ->
    DbMod:write(CbMod:key(ActorName),
                CbMod:to_struct(StateName, ActorState)).

actor_add(ActorName, Actor, Actors) ->
    lists:keystore(ActorName, #actor.name, Actors, Actor).

actor_get(ActorName, Actors) ->
    {value, Actor} = lists:keysearch(ActorName, #actor.name, Actors),
    Actor.

actor_read(#actor{cb_mod = CbMod, state = ActorState}, Message) ->
    CbMod:read(Message, ActorState).

actor_act(#actor{state_name = StateName, cb_mod = CbMod,
                 state = ActorState} = Actor,
          Message) ->
    Response = response(CbMod:command(StateName, Message, ActorState)),
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     Response#actor_response.stop_after}.

response(#actor_response{} = Response) -> Response;
response({stop, NewState}) ->
    #actor_response{state = NewState, stop_after = true};
response({ok, NewState}) -> #actor_response{state = NewState}.
