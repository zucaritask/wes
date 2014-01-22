-module(wes_actor).

-include("wes.hrl").

-record(actor,
        {name,
         type,
         state,
         state_name = event
         }).

-export([init/2,
         save/1,
         read/2, read/3,
         act/3,
         name/1
         ]).

-export([list_add/3,
         list_get/2]).

-export([register_name/3,
         deregister_name/2,
         timeout/2]).

-type state_name()  :: any().

-export_type([state_name/0]).

-callback init(InitArgs::any()) ->  Response::wes:actor_response().
-callback read(Name::any(), ActorState::wes:actor_state()) -> View::any().
-callback command(StateName::state_name(), CommandName::any(), CommandArgs::any(),
                  ActorState::wes:actor_state()) ->
    Response::wes:actor_response().
-callback key(ActorName::wes:actor_name()) -> wes_db:key().
-callback to_struct(ActorName::wes:actor_name(),
                    ActorState::wes:actor_state()) ->
    wes:serialized_actor().
-callback from_struct({Key::wes_db:key(), wes:serialized_actor()}) ->
    {ok, ActorState::wes:actor_state()}.

init(ChannelName, {ActorName, Type, InitArgs}) ->
    #actor_config{db_mod = DbMod, cb_mod = ActorCb, db_conf= DbConf,
                  locker_mod = LockerMod} = wes_config:actor(Type),
    Response =
        case DbMod:read(ActorCb:key(ActorName), DbConf) of
            {ok, {_Key, _Value} = Data} ->
                response(ActorCb:from_struct(Data));
            not_found ->
                response(ActorCb:init(InitArgs))
        end,
    {ok, LockDuration} = register_name(ActorName, ChannelName, LockerMod),
    Timeouts = [{{lock, ChannelName}, LockDuration} |
                Response#actor_response.new_timeouts],
    {#actor{name = ActorName,
            type = Type,
            state_name = Response#actor_response.state_name,
            state = Response#actor_response.state},
     Timeouts}.

name(#actor{name = Name}) -> Name.

save(#actor{state_name = StateName,
            name = ActorName, type = Type,
            state = ActorState}) ->
    #actor_config{db_mod = DbMod, cb_mod = CbMod, db_conf = DbConf} =
        wes_config:actor(Type),
    DbMod:write(CbMod:key(ActorName),
                CbMod:to_struct(StateName, ActorState),
                DbConf).

list_add(ActorName, Actor, Actors) ->
    lists:keystore(ActorName, #actor.name, Actors, Actor).

list_get(ActorName, Actors) ->
    {value, Actor} = lists:keysearch(ActorName, #actor.name, Actors),
    Actor.

read(#actor{type = Type} = Actor, Message) ->
    read(Actor, Message, wes_config:actor(Type)).

read(#actor{state = ActorState}, Message,
     #actor_config{cb_mod = CbMod}) ->
    CbMod:read(Message, ActorState).

act(#actor{state_name = StateName, type = Type, state = ActorState} = Actor,
    CmdName, CmdPayload) ->
    #actor_config{cb_mod = CbMod} = wes_config:actor(Type),
    Response = response(CbMod:command(StateName, CmdName, CmdPayload,
                                      ActorState)),
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     Response#actor_response.stop_after}.

register_name(Name, Channel, LockerMod) ->
    LockerMod:register_actor(Name, Channel).

deregister_name(#actor{name = Name, type = Type} = _Actor,
                      Channel) ->
    #actor_config{locker_mod = LockerMod} = wes_config:actor(Type),
    LockerMod:unregister_actor(Name, Channel).


%% FIXME: Cleanup the response from this function.
timeout(#actor{name = Name, type = Type, state = State} = Actor,
        {lock, Channel}) ->
    #actor_config{locker_mod = LockerMod} = wes_config:actor(Type),
    ok = LockerMod:actor_timeout(Name, Channel),
    Response = response({ok, State}),
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     Response#actor_response.stop_after};
timeout(#actor{state_name = StateName, type = Type,
               state = ActorState} = Actor, Name) ->
    #actor_config{cb_mod = CbMod} = wes_config:actor(Type),
    Response = response(CbMod:timeout(StateName, Name, ActorState)),
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     Response#actor_response.stop_after}.

response(#actor_response{} = Response) -> Response;
response({stop, NewState}) ->
    %% FIXME: Looses trace of state_name.
    #actor_response{state = NewState, stop_after = true};
response({ok, NewState}) -> #actor_response{state = NewState}.
