-module(wes_example_count).

-behaviour(wes_actor).

-include("../include/wes.hrl").

-export([init/1,
         command/3,
         command/4,
         key/1,
         to_struct/2,
         code_change/4,
         from_struct/2]).

init([]) ->
    {ok, 0}.

command(_StateName, read, ActorState) ->
    {reply, ActorState, ActorState};
command(_StateName, incr, ActorState) ->
    {ok, ActorState + 1};
command(_StateName, stop, ActorState) ->
    #actor_response{state = ActorState, stop_actor = true}.

command(_StateName, incr, 0, ActorState) ->
    {stop, ActorState};
command(_StateName, incr, Nr, ActorState) when Nr > 0 ->
    {ok, ActorState + Nr};
command(_StateName, incr, Nr, _ActorState) ->
    throw({negative_increment, Nr}).

key(Actorname) ->
    <<"example_counter", (atom_to_binary(Actorname, utf8))/binary>>.

to_struct(_Actorname, ActorState) ->
    list_to_binary(integer_to_list(ActorState)).

from_struct(_Key, Value) ->
    {ok, list_to_integer(binary_to_list(Value))}.

code_change(_StateName, State, 1, []) ->
    {ok, State}.
