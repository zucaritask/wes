-module(wes_actor).

-include("wes_internal.hrl").
-include("../include/wes.hrl").
-include("wes_actor.hrl").

-export([init/3,
         save/1,
         act/2,
         name/1,
         code_change/3
         ]).

-export([list_add/3,
         list_get/2,
         list_find/2]).

-export([register_name/4,
         deregister_name/3,
         timeout/2,
         spec/2]).

-type state_name()  :: any().

-export_type([state_name/0]).

-callback init(InitArgs::any()) ->  Response::wes:actor_response().
-callback command(StateName::state_name(), CommandName::any(),
                  ActorState::wes:actor_state()) ->
    Response::wes:actor_response().
-callback command(StateName::state_name(), CommandName::any(), CommandArgs::any(),
                  ActorState::wes:actor_state()) ->
    Response::wes:actor_response().
-callback key(ActorName::wes:name()) -> wes_db:key().
-callback to_struct(ActorName::wes:name(),
                    ActorState::wes:actor_state()) ->
    wes:serialized_actor().
-callback from_struct(Key::wes_db:key(), wes:serialized_actor()) ->
    {ok, ActorState::wes:actor_state()}.

init(ChannelType, ChannelName, Spec) ->
    #actor_config{db_mod = DbMod, cb_mod = ActorCb, db_conf = DbConf}
        = Config = wes_config:actor(spec(type, Spec)),
    Key = ActorCb:key(spec(name, Spec)),

    StartType = spec(start_type, Spec),

    case {StartType, DbMod:read(Key, DbConf)} of
        {create, {ok, _Value}} ->
            {error, actor_already_exists};
        {create, not_found} ->
            Data = ActorCb:init(spec(init_args, Spec)),
            create_actor(ChannelType, ChannelName, Spec, Data, Config);
        {load, {ok, Value}} ->
            Data = ActorCb:from_struct(Key, Value),
            create_actor(ChannelType, ChannelName, Spec, Data, Config);
        {load, not_found} ->
            {error, actor_not_found};
        {load_or_create, {ok, Value}} ->
            Data = ActorCb:from_struct(Key, Value),
            create_actor(ChannelType, ChannelName, Spec, Data, Config);
        {load_or_create, not_found} ->
            Data = ActorCb:init(spec(init_args, Spec)),
            create_actor(ChannelType, ChannelName, Spec, Data, Config)
    end.

name(#actor{name = Name}) -> Name.

save(#actor{state_name = StateName,
            name = ActorName, type = Type,
            state = ActorState}) ->
    #actor_config{db_mod = DbMod, cb_mod = CbMod, db_conf = DbConf} =
        wes_config:actor(Type),
    case erlang:function_exported(CbMod, should_save, 2) andalso
        not CbMod:should_save(StateName, ActorState) of
        true -> %% Should write exported and it didn't return true
            ok;
        false ->
            DbMod:write(CbMod:key(ActorName),
                        CbMod:to_struct(StateName, ActorState),
                        DbConf)
    end.

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

act(#actor{state_name = StateName, type = Type, state = ActorState} = Actor,
    Command) ->
    #actor_config{cb_mod = CbMod} = wes_config:actor(Type),
    Response = case Command of
        {cmd, Cmd, CmdPayload} ->
            response(CbMod:command(StateName, Cmd, CmdPayload, ActorState));
        {cmd, Cmd} ->
            response(CbMod:command(StateName, Cmd, ActorState))
    end,
    {Actor#actor{state_name = Response#actor_response.state_name,
                 state = Response#actor_response.state},
     {actor_id(Actor), Response#actor_response.value},
     Response#actor_response.stop_channel,
     Response#actor_response.stop_actor}.

register_name(Name, ChannelType, ChannelName, Lockmod) ->
    Lockmod:register_actor(Name, ChannelType, ChannelName).

deregister_name(#actor{name = Name, type = Type} = _Actor,
                ChannelType, ChannelName) ->
    #actor_config{lock_mod = Lockmod} = wes_config:actor(Type),
    Lockmod:unregister_actor(Name, ChannelType, ChannelName).

%% FIXME: Cleanup the response from this function.
timeout(#actor{name = Name, type = Type, state = State} = Actor,
        {lock, ChannelType, ChannelName}) ->
    #actor_config{lock_mod = Lockmod} = wes_config:actor(Type),
    ok = Lockmod:actor_timeout(Name, ChannelType, ChannelName),
    Response = response({ok, State}),
    {Actor#actor{state_name =
                     maybe_overwrite_state_name(
                       Actor, Response#actor_response.state_name),
                 state = Response#actor_response.state},
     Response#actor_response.stop_channel};
timeout(#actor{state_name = StateName, type = Type,
               state = ActorState} = Actor, Name) ->
    #actor_config{cb_mod = CbMod} = wes_config:actor(Type),
    Response = response(CbMod:timeout(StateName, Name, ActorState)), % FIXME: Check if callback exists!
    {Actor#actor{state_name =
                     maybe_overwrite_state_name(
                       Actor, Response#actor_response.state_name),
                 state = Response#actor_response.state},
     Response#actor_response.stop_channel}.

code_change(#actor{state_name = StateName, type = Type, state = State} = Actor,
            OldVsn, Extra) ->
    #actor_config{cb_mod = CbMod} = wes_config:actor(Type),
    case erlang:function_exported(CbMod, code_change, 4) of
        true ->
            Response = response(CbMod:code_change(StateName, State, OldVsn,
                                                  Extra)),
            Actor#actor{state_name =
                            maybe_overwrite_state_name(
                              Actor, Response#actor_response.state_name),
                        state = Response#actor_response.state};
        false ->
            Actor
    end.

spec(name,       {_, {_, Name}, _})      -> Name;
spec(type,       {_, {Type, _}, _})      -> Type;
spec(init_args,  {_, _, InitArgs})       -> InitArgs;
spec(start_type, {ActorStartType, _, _}) -> ActorStartType.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_actor(ChannelType, ChannelName, Spec, Data, Config) ->
    #actor_config{lock_mod = Lockmod} = Config,
    Response = response(Data),
    Name = spec(name, Spec),
    {ok, LockTimeouts} = register_name(Name, ChannelType, ChannelName,
                                       Lockmod),
    Timeouts = LockTimeouts ++ Response#actor_response.new_timeouts,
    ActorState = #actor{name = Name,
                        type = spec(type, Spec),
                        state_name = Response#actor_response.state_name,
                        state = Response#actor_response.state},
    {ok, ActorState, Timeouts}.

response(#actor_response{} = Response) ->
    Response;
response({stop, NewState}) ->
    #actor_response{state = NewState, stop_channel = true};
response({ok, NewState}) ->
    #actor_response{state = NewState};
response({reply, Value, NewState}) ->
    #actor_response{state = NewState, value = Value}.

maybe_overwrite_state_name(#actor{state_name = Name}, undefined) -> Name;
maybe_overwrite_state_name(_, StateName) -> StateName.

actor_id(#actor{type = Type, name = Name}) -> {Type, Name}.
