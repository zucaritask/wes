-module(wes_actor_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/wes_internal.hrl").
-include("../src/wes_actor.hrl").

test_setup() ->
    Config = #actor_config{
                lock_mod = wes_lock_null,
                cb_mod = wes_example_count,
                db_mod = wes_db_ets,
                db_conf = []},
    {ok, _} = wes_db_ets:start_link(),
    meck:new([wes_config], [passthrough]),
    meck:expect(wes_config, actor, fun(actortype1) -> Config end).

test_teardown(_) ->
    meck:validate(wes_config),
    meck:unload(wes_config).

actor_test_() ->
    [{setup, local,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_init_load/0,
       fun test_init_create/0,
       fun test_act/0,
       fun test_timeout/0,
       fun test_code_change/0
      ]}].

test_init_load() ->
    Actor = {actortype1, actor1},
    Spec = {load, Actor, []},
    ?assertMatch({error, actor_not_found},
                 wes_actor:init(channeltype1, channelname1, Spec)).

test_init_create() ->
    Actor = {actortype1, actor1},
    Spec = {create, Actor, []},
    {ok, State, _Timeouts} = wes_actor:init(channeltype1, channelname1, Spec),
    ok = wes_actor:save(State),
    ?assertMatch({error, actor_already_exists},
                 wes_actor:init(channeltype1, channelname1,
                                {create, Actor, []})).

test_act() ->
    InitArgs = [],
    ActorName = actor1,
    ActorType = actortype1,
    {ok, State0} = wes_example_count:init(InitArgs),
    ActState0 = #actor{name = ActorName,
                       type = ActorType,
                       state = State0},
    ?assertMatch({#actor{name = ActorName,
                         type = ActorType},
                  false, false},
                 wes_actor:act(ActState0, incr, [99])).

test_timeout() ->
    InitArgs = [],
    ActorName = actor1,
    ActorType = actortype1,
    {ok, State0} = wes_example_count:init(InitArgs),
    ActState0 = #actor{name = ActorName,
                       type = ActorType,
                       state = State0},
    ?assertMatch({#actor{name = ActorName,
                         type = ActorType,
                         state = State0},
                  false},
                 wes_actor:timeout(ActState0,
                                   {lock, channeltype1, channelname1})).


test_code_change() ->
    InitArgs = [],
    ActorName = actor1,
    ActorType = actortype1,
    {ok, State0} = wes_example_count:init(InitArgs),
    ActState0 = #actor{name = ActorName,
                       type = ActorType,
                       state = State0},
    ?assertMatch(
       #actor{name = ActorName,
              type = ActorType,
              state = State0},
       wes_actor:code_change(ActState0, 1, [])).
