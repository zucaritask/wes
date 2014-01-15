-module(wactor_example_count).

-include("../src/wactor.hrl").

-export([init/1,
         read/2,
         command/3,
         key/1,
         to_struct/2,
         from_struct/1]).

init([]) ->
    0.

read(counter, ActorState) ->
    ActorState.

command(_StateName, incr, ActorState) ->
    ActorState+1.

key(Actorname) ->
    atom_to_binary(Actorname, utf8).

to_struct(_Actorname, ActorState) ->
    integer_to_binary(ActorState).

from_struct({_Key, Value}) ->
    binary_to_integer(Value).
