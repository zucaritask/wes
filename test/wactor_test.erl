-module(wactor_test).

-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    [{foreach, spawn,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_locker/0,
       fun test_ets/0,
       fun test_locker_restart/0,
       fun test_stop/0,
       fun test_bad_command/0]}].

test_setup() ->
    wactor_sup:start_link().

test_teardown(_) ->
    ok.

test_locker() ->
    Channel = hej,
    Actor = act1,
    wactor_locker:start([node()], [], 1, 1000, 1000, 100),
    Actors = [{Actor, wactor_example_count, wactor_db_null, []}],
    {ok, _Pid} = wactor_locker:start_channel(Channel, Actors, 2000),
    ok = wactor_locker:command(Channel, incr),
    ?assertEqual(1, wactor_locker:read(Actor, counter)),
    wactor_locker:stop(Channel),
    {ok, _OtherPid} = wactor_locker:start_channel(Channel, Actors, 2000),
    ?assertEqual(0, wactor_locker:read(Actor, counter)),
    wactor_locker:stop(Channel).

test_locker_restart() ->
    Channel = hej2,
    Actor = act2,
    wactor_db_ets:start_link(),
    wactor_db_ets:clear(),
    Actors = [{Actor, wactor_example_count, wactor_db_ets, []}],
    {ok, _Pid} = wactor_locker:start_channel(Channel, Actors, 2000),
    ok = wactor_locker:command(Channel, incr),
    ?assertEqual(1, wactor_locker:read(Actor, counter)),
    wactor_locker:stop(Channel),
    {ok, _OtherPid} = wactor_locker:start_channel(Channel, Actors, 2000),
    ?assertEqual(1, wactor_locker:read(Actor, counter)),
    wactor_locker:stop(Channel),
    wactor_db_ets:clear().

test_ets() ->
    Channel = hej3,
    Actor = act3,
    wactor_db_ets:start_link(),
    wactor_db_ets:clear(),
    {ok, _} = wactor_lock_ets:start(1000),
    Actors = [{Actor, wactor_example_count, wactor_db_ets, []}],
    {ok, _Pid} = wactor_lock_ets:start_channel(Channel, Actors, 2000),
    ok = wactor_lock_ets:command(Channel, incr),
    timer:sleep(1000),
    ?assertEqual(1, wactor_lock_ets:read(Actor, counter)),
    ok = wactor_lock_ets:stop(Channel),
    {ok, _Pid2} = wactor_lock_ets:start_channel(Channel, Actors, 2000),
    io:format("tab ~p", [ets:tab2list(wactor_lock_ets_srv)]),
    ?assertEqual(1, wactor_lock_ets:read(Actor, counter)),
    wactor_db_ets:clear().

test_stop() ->
    Channel = hej4,
    Actor = act4,
    wactor_db_ets:start_link(),
    wactor_db_ets:clear(),
    wactor_lock_ets:start(1000),
    Actors = [{Actor, wactor_example_count, wactor_db_ets, []}],
    {ok, _Pid} = wactor_lock_ets:start_channel(Channel, Actors, 2000),
    ok = wactor_lock_ets:command(Channel, incr),
    ?assertEqual(1, wactor_lock_ets:read(Actor, counter)),
    io:format("tab ~p", [ets:tab2list(wactor_lock_ets_srv)]),
    ?assertMatch({ok, _Pid}, wactor_lock_ets:status(Channel)),
    ok = wactor_lock_ets:command(Channel, {incr, 0}),
    ?assertMatch({error, not_found}, wactor_lock_ets:status(Channel)).

test_bad_command() ->
    Channel = hej4,
    Actor = act4,
    wactor_db_ets:start_link(),
    wactor_db_ets:clear(),
    wactor_lock_ets:start(1000),
    Actors = [{Actor, wactor_example_count, wactor_db_ets, []}],
    {ok, _Pid} = wactor_lock_ets:start_channel(Channel, Actors, 2000),
    ok = wactor_lock_ets:command(Channel, incr),
    ok = wactor_lock_ets:command(Channel, {incr, -0}).
