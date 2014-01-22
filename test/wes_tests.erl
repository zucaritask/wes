-module(wes_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

db_test_() ->
    [{foreach, spawn,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_counters/0,
       fun test_ets/0,
       fun test_lock_restart/0,
       fun test_stop/0,
       fun test_bad_command/0,
       fun test_add_actor/0
      ]}].

test_setup() ->
    ActorTypes =
        [
         [{id, counter},
          {locker_mod, wes_lock_ets},
          {locker_conf, []},
          {cb_mod, wes_example_count},
          {db_mod, wes_db_ets},
          {db_conf, []}],
         [{id, null_counter},
          {locker_mod, wes_lock_ets},
          {locker_conf, []},
          {cb_mod, wes_example_count},
          {db_mod, wes_db_null},
          {db_conf, []}]
        ],
    ChannelTypes =
        [
         [{id, session},
          {locker_mod, wes_lock_ets},
          {locker_conf, []},
          {lock_timeout_interval, 1000},
          {stats_mod, wes_stats_ets}]
        ],
    wes_sup:start_link(ActorTypes, ChannelTypes),
    wes_db_ets:start([]),
    wes_stats_ets:start_link(),
    {ok, _} = wes_lock_ets:start(1000).

test_teardown(_) ->
    ok = wes_lock_ets:stop(),
    wes_stats_ets:stop(),
    wes_db_ets:stop([]),
    ok.

test_counters() ->
    Channel = session1,
    ChannelType = session,
    Actor = act1,
    ActorType = null_counter,
    Actors = [{Actor, ActorType, []}],
    ?assertEqual([], wes_stats_ets:all_stats()),
    ?assertMatch({ok, _}, wes_channel:start(ChannelType, Channel, Actors)),
    ?assertEqual([{{start, actor}, 1},
                  {{start, channel}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(ok, wes_channel:command(ChannelType, Channel, incr, [])),
    ?assertEqual([{{command, incr},1},
                  {{start, actor}, 1},
                  {{start, channel}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(1, wes_channel:read(ChannelType, ActorType, Actor, counter)),
    ?assertEqual([{{command, incr},1},
                  {{read, counter}, 1},
                  {{start, actor}, 1},
                  {{start, channel}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(ok, wes_channel:stop(ChannelType, Channel)),
    ?assertMatch({ok, _}, wes_channel:start(ChannelType, Channel, Actors)),
    ?assertEqual(0, wes_channel:read(ChannelType, ActorType, Actor, counter)),
    %% Test stats.
    ?assertEqual([{{command, incr},1},
                  {{read, counter}, 2},
                  {{start, actor}, 2},
                  {{start, channel}, 2},
                  {{stop, normal}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(ok, wes_channel:stop(ChannelType, Channel)).

test_lock_restart() ->
    Channel = hej2,
    ChannelType = session,
    Actor = act2,
    ActorType = counter,
    Actors = [{Actor, ActorType, []}],
    {ok, _Pid} = wes_channel:start(ChannelType, Channel, Actors),
    ?assertEqual(ok, wes_channel:command(ChannelType, Channel, incr, [])),
    ?assertEqual(1, wes_channel:read(ChannelType, ActorType, Actor, counter)),
    wes_channel:stop(ChannelType, Channel),
    {ok, _OtherPid} = wes_channel:start(ChannelType, Channel, Actors),
    ?assertEqual(1, wes_channel:read(ChannelType, ActorType, Actor, counter)).

test_ets() ->
    Channel = hej3,
    ChannelType = session,
    Actor = act3,
    ActorType = counter,
    Actors = [{Actor, ActorType, []}],
    {ok, _Pid} = wes_channel:start(ChannelType, Channel, Actors),
    ok = wes_channel:command(ChannelType, Channel, incr, []),
    error_logger:error_msg("before sleep tab ~p",
                           [ets:tab2list(wes_lock_ets_srv)]),
    timer:sleep(1000),
    ?assertEqual(1, wes_channel:read(ChannelType, ActorType, Actor, counter)),
    ok = wes_channel:stop(ChannelType, Channel),
    {ok, _Pid2} = wes_channel:start(ChannelType, Channel, Actors),
    io:format("tab ~p", [ets:tab2list(wes_lock_ets_srv)]),
    ?assertEqual(1, wes_channel:read(ChannelType, ActorType, Actor, counter)).

test_stop() ->
    Channel = hej4,
    ChannelType = session,
    Actor = act4,
    ActorType = counter,
    Actors = [{Actor, ActorType, []}],
    {ok, _Pid} = wes_channel:start(ChannelType, Channel, Actors),
    ok = wes_channel:command(ChannelType, Channel, incr, []),
    ?assertEqual(1, wes_channel:read(ChannelType, ActorType, Actor, counter)),
    io:format("tab ~p", [ets:tab2list(wes_lock_ets_srv)]),
    ?assertMatch({ok, _Pid}, wes_channel:status(ChannelType, Channel)),
    %% This should generate a stop by the actor.
    ok = wes_channel:command(ChannelType, Channel, incr, [0]),
    ?assertMatch({error, not_found}, wes_channel:status(ChannelType, Channel)).

test_bad_command() ->
    Channel = hej4,
    ChannelType = session,
    Actor = act4,
    ActorType = counter,
    Actors = [{Actor, ActorType, []}],
    {ok, _Pid} = wes_channel:start(ChannelType, Channel, Actors),
    ok = wes_channel:command(ChannelType, Channel, incr, []),
    ?assertEqual({error, {negative_increment, -1}},
                 wes_channel:command(ChannelType, Channel, incr, [-1])).

test_add_actor() ->
    Channel = hej5,
    ChannelType = session,
    Actor = act5,
    ActorType = counter,
    {ok, _Pid} = wes_channel:start(ChannelType, Channel, []),
    wes_channel:register_actor(ChannelType, Channel, ActorType, Actor, []),
    ok = wes_channel:command(ChannelType, Channel, incr, []),
    ?assertEqual(1, wes_channel:read(ChannelType, ActorType, Actor, counter)).
