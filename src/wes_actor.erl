-module(wes_actor).

-include("wes.hrl").
-include("wes_actor.hrl").

-export([init/3,
         save/1,
         read/2, read/3,
         act/3,
         name/1
         ]).

-export([list_add/3,
         list_get/2,
         list_find/2]).

-export([register_name/4,
         deregister_name/3,
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

init(ChannelType, ChannelName, {ActorName, Type, InitArgs}) ->
    #actor_config{db_mod = DbMod, cb_mod = ActorCb, db_conf= DbConf,
                  locker_mod = LockerMod} = wes_config:actor(Type),
    Response =
        case DbMod:read(ActorCb:key(ActorName), DbConf) of
            {ok, {_Key, _Value} = Data} ->
                response(ActorCb:from_struct(Data));
            not_found ->
                response(ActorCb:init(InitArgs))
        end,
    {ok, LockDuration} = register_name(ActorName, ChannelType, ChannelName,
                                       LockerMod),
    Timeouts = [{{lock, ChannelType, ChannelName}, LockDuration} |
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

list_find(ActorName, Actors) ->
    case lists:keysearch(ActorName, #actor.name, Actors) of
        {value, Actor} ->
            {ok, Actor};
        false ->
            false
    end.

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

register_name(Name, ChannelType, ChannelName, LockerMod) ->
    LockerMod:register_actor(Name, ChannelType, ChannelName).

deregister_name(#actor{name = Name, type = Type} = _Actor,
                ChannelType, ChannelName) ->
    #actor_config{locker_mod = LockerMod} = wes_config:actor(Type),
    LockerMod:unregister_actor(Name, ChannelType, ChannelName).

%% FIXME: Cleanup the response from this function.
timeout(#actor{name = Name, type = Type, state = State} = Actor,
        {lock, ChannelType, ChannelName}) ->
    #actor_config{locker_mod = LockerMod} = wes_config:actor(Type),
    ok = LockerMod:actor_timeout(Name, ChannelType, ChannelName),
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
