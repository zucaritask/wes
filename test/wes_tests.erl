-module(wes_tests).

-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    [{foreach, spawn,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_counters/0,
       fun test_ets/0,
       fun test_lock_restart/0,
       fun test_stop/0,
       fun test_bad_command/0,
       fun test_add_actor/0]}].

test_setup() ->
    wes_sup:start_link(),
    wes_db_ets:start_link(),
    wes_stats_ets:start_link(),
    {ok, _} = wes_lock_ets:start(1000).

test_teardown(_) ->
    ok = wes_lock_ets:stop(),
    wes_stats_ets:stop(),
    wes_db_ets:stop(),
    ok.

test_counters() ->
    Channel = hej,
    Actor = act1,
    Actors = [{Actor, wes_example_count, wes_db_null, wes_lock_ets, []}],
    ?assertEqual([], wes_stats_ets:all_stats()),
    ?assertMatch({ok, _}, wes_lock_ets:start_channel(Channel, Actors, 2000,
                                                     wes_stats_ets)),
    ?assertEqual([{{start, actor}, 1},
                  {{start, channel}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(ok, wes_lock_ets:command(Channel, incr, [])),
    ?assertEqual([{{command, incr},1},
                  {{start, actor}, 1},
                  {{start, channel}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    ?assertEqual([{{command, incr},1},
                  {{read, counter}, 1},
                  {{start, actor}, 1},
                  {{start, channel}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(ok, wes_lock_ets:stop_channel(Channel)),
    ?assertMatch({ok, _}, wes_lock_ets:start_channel(Channel, Actors, 2000,
                                                     wes_stats_ets)),
    ?assertEqual(0, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    %% Test stats.
    ?assertEqual([{{command, incr},1},
                  {{read, counter}, 2},
                  {{start, actor}, 2},
                  {{start, channel}, 2},
                  {{stop, normal}, 1}], wes_stats_ets:all_stats()),
    ?assertEqual(ok, wes_lock_ets:stop_channel(Channel)).

test_lock_restart() ->
    Channel = hej2,
    Actor = act2,
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_lock_ets, []}],
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, Actors, 2000, wes_stats_ets),
    ?assertEqual(ok, wes_lock_ets:command(Channel, incr, [])),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    wes_lock_ets:stop_channel(Channel),
    {ok, _OtherPid} = wes_lock_ets:start_channel(Channel, Actors, 2000, wes_stats_ets),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)).

test_ets() ->
    Channel = hej3,
    Actor = act3,
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_lock_ets, []}],
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, Actors, 2000, wes_stats_ets),
    ok = wes_lock_ets:command(Channel, incr, []),
    error_logger:error_msg("before sleep tab ~p",
                           [ets:tab2list(wes_lock_ets_srv)]),
    timer:sleep(1000),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    ok = wes_lock_ets:stop_channel(Channel),
    {ok, _Pid2} = wes_lock_ets:start_channel(Channel, Actors, 2000, wes_stats_ets),
    io:format("tab ~p", [ets:tab2list(wes_lock_ets_srv)]),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)).

test_stop() ->
    Channel = hej4,
    Actor = act4,
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_lock_ets, []}],
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, Actors, 2000, wes_stats_ets),
    ok = wes_lock_ets:command(Channel, incr, []),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    io:format("tab ~p", [ets:tab2list(wes_lock_ets_srv)]),
    ?assertMatch({ok, _Pid}, wes_lock_ets:status(Channel)),
    %% This should generate a stop by the actor.
    ok = wes_lock_ets:command(Channel, incr, [0]),
    ?assertMatch({error, not_found}, wes_lock_ets:status(Channel)).

test_bad_command() ->
    Channel = hej4,
    Actor = act4,
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_lock_ets, []}],
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, Actors, 2000, wes_stats_ets),
    ok = wes_lock_ets:command(Channel, incr, []),
    ?assertEqual({error, {negative_increment, -1}},
                 wes_lock_ets:command(Channel, incr, [-1])).

test_add_actor() ->
    Channel = hej5,
    Actor = act5,
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, [], 2000, wes_stats_ets),
    wes_lock_ets:register_actor(Channel, Actor, wes_example_count,
                                wes_db_null, wes_lock_ets, []),
    ok = wes_lock_ets:command(Channel, incr, []),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)).
