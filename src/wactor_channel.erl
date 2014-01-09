-module(wactor_channel).

-behaviour(gen_server).

%% API
-export([start/1,
         send/2, send/3,
         register_cast_actor/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {actor,
         cast_actors = []}).

-record(actor,
        {name,
         cbmod,
         state,
         state_name = event}).

-record(actor_response,
        {state_name = event,
         state,
         reply = ok}).

%%%===================================================================
%%% API
%%%===================================================================

%% FIXME: don't register locally. Use callback for channel register
start(ChannelName) ->
    start(ChannelName, []).

start(ChannelName, StartActor) ->
    gen_server:start(reg_name(ChannelName), ?MODULE, [StartActor], []).

send(ChannelName, Message) ->
    send(ChannelName, Message, conf(timeout)).

send(ChannelName, Message, Timeout) ->
    gen_server:call(reg_name(ChannelName), {send, Message}, Timeout).

register_cast_actor(ChannelName, ActorName, CbMod) ->
    gen_server:call(
      reg_name(ChannelName), {register_actor, ActorName, CbMod},
      conf(timeout)).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Actor]) ->
    {ok, #state{actor = init_actor(Actor)}}.

init_actor({Name, CbMod}) ->
    Response = CbMod:init(),
    #actor{name = Name,
           cbmod = CbMod,
           state_name = Response#actor_response.state_name,
           state = Response#actor_response.state}.

handle_call({send, Message}, _From, #state{actor = Actor,
                                           cast_actors = CastActors} = State) ->
    ActorResp = {_, Response} = actor_send(Actor, Message),
    CastActorsResp = lists:map(fun(A0) -> actor_send(A0, Message) end, CastActors),
    NewActor = handle_response(ActorResp),
    NewCastActors = lists:map(fun handle_response/1, CastActorsResp),
    {reply, Response#actor_response.reply,
     State#state{actor = NewActor, cast_actors = NewCastActors}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reg_name(ChannelName) ->
    {local, ChannelName}.

conf(Var) ->
    conf(Var, undefined).

conf(Var, undefined) ->
    proplists:get_value(Var, application:get_env(wactor, channel), undefined).

actor_send(#actor{state_name = StateName, cbmod = CbMod,
                  state = ActorState} = Actor,
           Message) ->
    {Actor, CbMod:StateName(Message, ActorState)}.

handle_response({Actor, Response}) ->
    Actor#actor{state_name = Response#actor_response.state_name,
                state = Response#actor_response.state}.
