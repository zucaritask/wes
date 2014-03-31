-record(actor_response,
        {state_name = event :: wes_actor:state_name(),
         state :: wes:actor_state(),
         stop_channel = false :: boolean(),
         stop_actor = false :: boolean(),
         new_timeouts = [] :: list(), %% FIXME: more precise type
         events = [] :: list(), %% FIXME: more precise type
         value = ok :: term()}).
