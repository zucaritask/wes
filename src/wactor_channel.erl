-module(wactor_channel).

-behaviour(gen_server).

-include("wactor.hrl").

%% API
-export([start/2, start/3,
         command/3,
         event/3,
         read/3,
         register_actor/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {name,
         locker_mod,
         actors = []}).

-record(actor,
        {name,
         cb_mod,
         db_mod,
         state,
         state_name = event}).

-define(TIMEOUT, 30000).

%%%===================================================================
%%% API
%%%===================================================================

start(ChannelName, LockerMod) ->
    start(ChannelName, [], LockerMod).

start(ChannelName, StartActors, LockerMod) ->
    gen_server:start(channel_name(ChannelName, LockerMod), ?MODULE,
                     [ChannelName, StartActors, LockerMod], []).

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

init([ChannelName, Actors, LockerMod]) ->
    timer:send_after(10000, timeout),
    ActorData =
        lists:map(
          fun(Act) -> actor_init(ChannelName, Act, LockerMod) end,
          Actors),
    {ok, #state{name = ChannelName,
                locker_mod = LockerMod,
                actors = ActorData}}.

handle_call({command, Message}, _From,
            #state{actors = Actors} = State) ->
    NewActors = lists:map(fun(A0) -> actor_act(A0, Message) end, Actors),
    {reply, ok, State#state{actors = NewActors}};
handle_call({read, ActorName, Message}, _From,
            #state{actors = Actors} = State) ->
    Actor = actor_get(ActorName, Actors),
    Reply = actor_read(Actor, Message),
    {reply, Reply, State};
handle_call({register_actor, ActorName, CbMod, DbMod, InitArgs}, _From,
            #state{actors = Actors, name = ChannelName,
                   locker_mod = LockerMod} = State) ->
    Actor = actor_init(ChannelName, {ActorName, CbMod, DbMod, InitArgs},
                       LockerMod),
    NewActors = actor_add(ActorName, Actor, Actors),
    {reply, ok, State#state{actors = NewActors}}.

handle_cast({event, Event}, #state{actors = Actors} = State) ->
    NewActors = lists:map(fun(A0) -> actor_act(A0, Event) end, Actors),
    {noreply, State#state{actors = NewActors}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, #state{name = Name, actors = Actors,
                         locker_mod = LockerMod}) ->
    lists:foreach(fun(A0) -> actor_save(A0) end, Actors),
    lists:foreach(
      fun(A0) -> actor_deregister_name(A0, Name, LockerMod) end, Actors),
    channel_deregister_name(Name, LockerMod),
    ok;
terminate(Reason, #state{name = Name, actors = Actors,
                         locker_mod = LockerMod}) ->
    io:format("Reason ~p", [Reason]),
    lists:foreach(
      fun(A0) -> actor_deregister_name(A0, Name, LockerMod) end, Actors),
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

actor_register_name(Name, Channel, LockerMod) ->
    ok = LockerMod:register_actor(Name, Channel).

actor_deregister_name(Name, Channel, LockerMod) ->
    LockerMod:unregister_actor(Name, Channel).

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
    Actor#actor{state_name = Response#actor_response.state_name,
                state = Response#actor_response.state}.

response(#actor_response{} = Response) -> Response;
response(NewState0) -> #actor_response{state = NewState0}.
