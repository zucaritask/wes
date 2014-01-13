-module(wactor_channel).

-behaviour(gen_server).

-include("wactor.hrl").

%% API
-export([start/1, start/2,
         command/2,
         event/2,
         read/2,
         register_actor/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {name,
         actors = []}).

-record(actor,
        {name,
         cbmod,
         state,
         state_name = event}).

-define(TIMEOUT, 30000).

%%%===================================================================
%%% API
%%%===================================================================

start(ChannelName) ->
    start(ChannelName, []).

start(ChannelName, StartActors) ->
    gen_server:start(channel_name(ChannelName), ?MODULE,
                     [ChannelName, StartActors], []).

command(ChannelName, Message) ->
    gen_server:call(channel_name(ChannelName), {command, Message}).

event(ChannelName, Message) ->
    gen_server:cast(channel_name(ChannelName), {event, Message}).

read(ActorName, Message) ->
    gen_server:call(channel_name(actor_name_to_channel(ActorName)),
                    {read, ActorName, Message}).

register_actor(ChannelName, ActorName, CbMod) ->
    gen_server:call(channel_name(ChannelName),
                    {register_actor, ActorName, CbMod}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ChannelName, Actors]) ->
    io:format("actors ~p~n", [Actors]),
    {ok, #state{name = ChannelName,
                actors = lists:map(fun(Act) -> init_actor(ChannelName, Act) end,
                                   Actors)}}.

init_actor(ChannelName, {ActorName, CbMod}) ->
    Response = CbMod:init(),
    actor_register_name(ActorName, ChannelName),
    #actor{name = ActorName,
           cbmod = CbMod,
           state_name = Response#actor_response.state_name,
           state = Response#actor_response.state}.

handle_call({command, Message}, _From,
            #state{actors = Actors} = State) ->
    NewActors = lists:map(fun(A0) -> actor_act(A0, Message) end, Actors),
    {reply, ok, State#state{actors = NewActors}};
handle_call({read, ActorName, Message}, _From,
            #state{actors = Actors} = State) ->
    Actor = actor_get(ActorName, Actors),
    Reply = actor_read(Actor, Message),
    {reply, Reply, State};
handle_call({register_actor, ActorName, CbMod}, _From,
            #state{actors = Actors, name = ChannelName} = State) ->
    Actor = init_actor(ChannelName, {ActorName, CbMod}),
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

terminate(normal, #state{name = Name, actors = Actors}) ->
    lists:foreach(fun(A0) -> actor_save(A0) end, Actors),
    lists:foreach(fun(A0) -> actor_deregister_name(A0) end, Actors),
    channel_deregister_name(Name),
    ok;
terminate(Reason, #state{name = Name, actors = Actors}) ->
    io:format("Reason ~p", [Reason]),
    lists:foreach(fun(A0) -> actor_deregister_name(A0) end, Actors),
    channel_deregister_name(Name),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

channel_name(ChannelName) ->
    {via, wactor_locker, ChannelName}.

channel_deregister_name(ChannelName) ->
    wactor_locker:unregister_name(ChannelName).

actor_name_to_channel(ActorName) ->
    wactor_locker:channel_for_actor(ActorName).

actor_register_name(Name, Channel) ->
    ok = wactor_locker:register_actor(Name, Channel).

actor_deregister_name(Name) ->
    wactor_locker:deregister_actor(Name).


conf(Var) ->
    conf(Var, undefined).

conf(Var, undefined) ->
    proplists:get_value(Var, application:get_env(wactor, channel), undefined).

%% ---------------------------------------------------------------------------
%% Actor

actor_save(#actor{state_name = StateName, cbmod = CbMod,
                  state = ActorState}) ->
    CbMod:save(StateName, ActorState),
    ok.

actor_add(ActorName, Actor, Actors) ->
    lists:keystore(ActorName, #actor.name, Actors, Actor).

actor_get(ActorName, Actors) ->
    {value, Actor} = lists:keysearch(ActorName, #actor.name, Actors),
    Actor.

actor_read(#actor{cbmod = CbMod, state = ActorState}, Message) ->
    CbMod:read(Message, ActorState).

actor_act(#actor{state_name = StateName, cbmod = CbMod,
                 state = ActorState} = Actor,
          Message) ->
    Response = CbMod:command(StateName, Message, ActorState),
    Actor#actor{state_name = Response#actor_response.state_name,
                state = Response#actor_response.state}.
