-record(actor_response,
        {state_name = event,
         state,
         stop_after = false,
         new_timeouts = [],
         events = []}).

%% See wes_config for default values.
-record(actor_config,
        {locker_mod,
         cb_mod,
         db_mod,
         db_conf}).

-record(channel_config,
        {locker_mod,
         stats_mod,
         lock_timeout_interval}).
