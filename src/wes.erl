-module(wes).

-include("wes.hrl").

-export([
    create_channel/1,
    create_channel/2,
    ensure_channel/1,
    ensure_channel/2,
    create_actor/2,
    ensure_actor/2,
    status/1,
    stop_channel/1,
    command/3,
    read/2
]).

-export_type([
    type/0,
    name/0,
    channel/0,
    actor/0,
    actor_args/0,
    actor_spec/0,
    actor_specs/0,
    actor_state/0,
    serialized_actor/0,
    actor_response/0,
    ref/0
]).

-type type() :: atom().
-type name() :: any().
-type channel() :: {type(), name()}.
-type actor() :: {type(), name()}.
-type actor_args() :: [any()].
-type actor_spec() :: {create | load | load_or_create, actor(), actor_args()}.
-type actor_specs() :: actor_spec() | [actor_spec()].

-type actor_state()  :: any().
-type serialized_actor()  :: binary().
-type actor_response() :: #actor_response{} | {stop | ok, actor_state()}.

-opaque ref() :: pid().

-spec create_channel(channel()) -> {ok, ref()} | {error, _}.
create_channel(Channel)        -> wes_channel:start(Channel, []).

-spec create_channel(channel(), actor_specs()) -> {ok, ref()} | {error, _}.
create_channel(Channel, Specs) -> wes_channel:start(Channel, Specs).

-spec ensure_channel(channel()) -> {ok, ref()} | {error, _}.
ensure_channel(Channel) -> ensure_channel(Channel, []).

-spec ensure_channel(channel(), actor_specs()) ->  {ok, ref()} | {error, _}.
ensure_channel(Channel, Specs) ->
    case wes_channel:start(Channel, Specs) of
        {error, {already_started, ChannelRef}} -> {ok, ChannelRef};
        Else                                   -> Else
    end.

-spec create_actor(channel(), actor_specs()) -> any().
create_actor(Channel, Specs) -> wes_channel:add_actors(Channel, Specs).

-spec ensure_actor(channel(), actor_specs()) -> any().
ensure_actor(Channel, Specs) -> wes_channel:ensure_actors(Channel, Specs).

-spec status(channel()) -> {ok, ref()} | {error, not_found}.
status(Channel) -> wes_channel:status(Channel).

-spec stop_channel(channel()) -> any().
stop_channel(Channel) -> wes_channel:stop(Channel).

-spec command(channel(), _, _) -> any().
command(Channel, Command, Payload) ->
    wes_channel:command(Channel, Command, Payload).

-spec read(channel(), _) -> any().
read(Actor, Key) -> wes_channel:read(Actor, Key).
