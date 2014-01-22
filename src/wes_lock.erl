-module(wes_lock).

%% The first part is the {via, _, _} callback.
-callback send(ChannelName::any(), Event::any()) -> any().

-callback whereis_name(ChannelName::any()) -> pid() | undefined.

-callback unregister_name(ChannelName::any()) -> ok.

-callback register_name(ChannelName::any(), Pid::pid()) -> yes.

%% Actor stuff
-callback register_actor(ActorName::any(), ChannelName::any()) ->
    {ok, Timeout::integer()}. %% what?

-callback actor_timeout(ActorName::any(), ChannelName::any()) ->
    ok | {error, Reason::atom()}.

-callback unregister_actor(ActorName::any(), ChannelName::any()) -> ok.

-callback channel_for_actor(ActorName::any()) ->
    ChannelName::any() | undefined.
%% fixme: bad api.

-callback channel_timeout(ChannelName::any()) ->
    ok | {error, Reason::atom()}.
