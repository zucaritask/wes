-module(wactor_example_count).

-include("wactor.hrl").

-export([init/1,
         read/2,
         command/3]).

init([]) ->
    0.

read(counter, ActorState) ->
    ActorState.

command(_StateName, incr, ActorState) ->
    ActorState+1.
