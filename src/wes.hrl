-record(actor_response,
        {state_name = event :: wes_actor:state_name(),
         state :: wes:actor_state(),
         stop_channel = false :: boolean(),
         stop_actor = false :: boolean(),
         new_timeouts = [] :: list(), %% FIXME: more precise type
         events = [] :: list()}). %% FIXME: more precise type

%% See wes_config for default values.
-record(actor_config,
        {locker_mod,
         cb_mod,
         db_mod,
         db_conf}).

-record(channel_config,
        {locker_mod,
         stats_mod,
         message_timeout,
         lock_timeout_interval}).
