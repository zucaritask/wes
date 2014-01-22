-module(wes).

-include("wes.hrl").

-export_type([serialized_actor/0,
              actor_state/0,
              actor_name/0,
              actor_response/0]).

-type actor_name()  :: any().
-type actor_state()  :: any().
-type serialized_actor()  :: binary().
-type actor_response() :: #actor_response{} | {stop | ok, actor_state()}.
