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
