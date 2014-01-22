-module(wes_example_count).

-behaviour(wes_actor).

-include("../src/wes.hrl").

-export([init/1,
         read/2,
         command/4,
         key/1,
         to_struct/2,
         from_struct/1]).

init([]) ->
    {ok, 0}.

read(counter, ActorState) ->
    ActorState.

command(_StateName, incr, [0], ActorState) ->
    {stop, ActorState};
command(_StateName, incr, [], ActorState) ->
    command(_StateName, incr, [1], ActorState);
command(_StateName, incr, [Nr], ActorState) when Nr > 0 ->
    {ok, ActorState+Nr};
command(_StateName, incr, [Nr], _ActorState) ->
    throw({negative_increment, Nr}).

key(Actorname) ->
    <<"example_counter", (atom_to_binary(Actorname, utf8))/binary>>.

to_struct(_Actorname, ActorState) ->
    list_to_binary(integer_to_list(ActorState)).

from_struct({_Key, Value}) ->
    {ok, list_to_integer(binary_to_list(Value))}.
