-module(wes_actor).

-include("wes.hrl").

-record(actor,
        {name,
         locker_mod,
         cb_mod,
         db_mod,
         state,
         state_name = event}).

-export([init/2,
         save/1,
         read/2,
         act/3,
         name/1
         ]).

-export([list_add/3,
         list_get/2]).

-export([register_name/3,
         deregister_name/2,
         timeout/2]).

init(ChannelName, {ActorName, ActorCb, DbMod, LockerMod, InitArgs}) ->
    Response =
        case DbMod:read(ActorCb:key(ActorName)) of
            {ok, {_Key, _Value} = Data} ->
                response(ActorCb:from_struct(Data));
            not_found ->
                response(ActorCb:init(InitArgs))
        end,
    {ok, LockDuration} = register_name(ActorName, ChannelName, LockerMod),
    Timeouts = [{{lock, ChannelName}, LockDuration} |
                Response#actor_response.new_timeouts],
    {#actor{name = ActorName,
            locker_mod = LockerMod,
            cb_mod = ActorCb,
            db_mod = DbMod,
            state_name = Response#actor_response.state_name,
            state = Response#actor_response.state},
     Timeouts}.

name(#actor{name = Name}) -> Name.

save(#actor{state_name = StateName, cb_mod = CbMod,
                  name = ActorName,
                  state = ActorState, db_mod = DbMod}) ->
    DbMod:write(CbMod:key(ActorName),
                CbMod:to_struct(StateName, ActorState)).

list_add(ActorName, Actor, Actors) ->
    lists:keystore(ActorName, #actor.name, Actors, Actor).

list_get(ActorName, Actors) ->
    {value, Actor} = lists:keysearch(ActorName, #actor.name, Actors),
    Actor.

read(#actor{cb_mod = CbMod, state = ActorState}, Message) ->
    CbMod:read(Message, ActorState).

act(#actor{state_name = StateName, cb_mod = CbMod,
           state = ActorState} = Actor,
    CmdName, CmdPayload) ->
    Response = response(CbMod:command(StateName, CmdName, CmdPayload,
                                      ActorState)),
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     Response#actor_response.stop_after}.

register_name(Name, Channel, LockerMod) ->
    LockerMod:register_actor(Name, Channel).

deregister_name(#actor{name = Name, locker_mod = LockerMod} = _Actor,
                      Channel) ->
    LockerMod:unregister_actor(Name, Channel).


%% FIXME: Cleanup the response from this function.
timeout(#actor{name = Name, locker_mod = LockerMod, state = State} = Actor,
        {lock, Channel}) ->
    ok = LockerMod:actor_timeout(Name, Channel),
    Response = response({ok, State}),
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     Response#actor_response.stop_after};
timeout(#actor{state_name = StateName, cb_mod = CbMod,
               state = ActorState} = Actor, Name) ->
    Response = response(CbMod:timeout(StateName, Name, ActorState)),
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     Response#actor_response.stop_after}.

response(#actor_response{} = Response) -> Response;
response({stop, NewState}) ->
    %% FIXME: Looses trace of state_name.
    #actor_response{state = NewState, stop_after = true};
response({ok, NewState}) -> #actor_response{state = NewState}.
