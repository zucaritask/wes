%% See wes_config for default values.
-record(actor_config,
        {lock_mod,
         cb_mod,
         db_mod,
         db_conf}).

-record(channel_config,
        {lock_mod,
         stats_mod,
         message_timeout,
         save_timeout,
         lock_timeout_interval}).
