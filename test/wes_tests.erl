-module(wes_tests).

-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    [{foreach, spawn,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_locker/0,
       fun test_ets/0,
       fun test_locker_restart/0,
       fun test_stop/0,
       fun test_bad_command/0,
       fun test_add_actor/0]}].

test_setup() ->
    wes_sup:start_link().

test_teardown(_) ->
    ok.

test_locker() ->
    Channel = hej,
    Actor = act1,
    wes_locker:start([node()], [], 1, 1000, 1000, 100),
    Actors = [{Actor, wes_example_count, wes_db_null, wes_locker, []}],
    {ok, _Pid} = wes_locker:start_channel(Channel, Actors, 2000),
    ok = wes_locker:command(Channel, incr),
    ?assertEqual(1, wes_locker:read(Actor, counter, wes_locker)),
    wes_locker:stop(Channel),
    {ok, _OtherPid} = wes_locker:start_channel(Channel, Actors, 2000),
    ?assertEqual(0, wes_locker:read(Actor, counter, wes_locker)),
    wes_locker:stop(Channel).

test_locker_restart() ->
    Channel = hej2,
    Actor = act2,
    wes_db_ets:start_link(),
    wes_db_ets:clear(),
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_locker, []}],
    {ok, _Pid} = wes_locker:start_channel(Channel, Actors, 2000),
    ok = wes_locker:command(Channel, incr),
    ?assertEqual(1, wes_locker:read(Actor, counter, wes_locker)),
    wes_locker:stop(Channel),
    {ok, _OtherPid} = wes_locker:start_channel(Channel, Actors, 2000),
    ?assertEqual(1, wes_locker:read(Actor, counter, wes_locker)),
    wes_locker:stop(Channel),
    wes_db_ets:clear().

test_ets() ->
    Channel = hej3,
    Actor = act3,
    wes_db_ets:start_link(),
    wes_db_ets:clear(),
    {ok, _} = wes_lock_ets:start(1000),
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_lock_ets, []}],
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, Actors, 2000),
    ok = wes_lock_ets:command(Channel, incr),
    error_logger:error_msg("before sleep tab ~p",
                           [ets:tab2list(wes_lock_ets_srv)]),
    timer:sleep(1000),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    ok = wes_lock_ets:stop(Channel),
    {ok, _Pid2} = wes_lock_ets:start_channel(Channel, Actors, 2000),
    io:format("tab ~p", [ets:tab2list(wes_lock_ets_srv)]),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    wes_db_ets:clear().

test_stop() ->
    Channel = hej4,
    Actor = act4,
    wes_db_ets:start_link(),
    wes_db_ets:clear(),
    wes_lock_ets:start(1000),
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_lock_ets, []}],
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, Actors, 2000),
    ok = wes_lock_ets:command(Channel, incr),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)),
    io:format("tab ~p", [ets:tab2list(wes_lock_ets_srv)]),
    ?assertMatch({ok, _Pid}, wes_lock_ets:status(Channel)),
    ok = wes_lock_ets:command(Channel, {incr, 0}),
    ?assertMatch({error, not_found}, wes_lock_ets:status(Channel)).

test_bad_command() ->
    Channel = hej4,
    Actor = act4,
    wes_db_ets:start_link(),
    wes_db_ets:clear(),
    wes_lock_ets:start(1000),
    Actors = [{Actor, wes_example_count, wes_db_ets, wes_lock_ets, []}],
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, Actors, 2000),
    ok = wes_lock_ets:command(Channel, incr),
    ok = wes_lock_ets:command(Channel, {incr, -0}).

test_add_actor() ->
    Channel = hej5,
    Actor = act5,
    wes_db_ets:start_link(),
    wes_db_ets:clear(),
    wes_lock_ets:start(1000),
    {ok, _Pid} = wes_lock_ets:start_channel(Channel, [], 2000),
    wes_lock_ets:register_actor(Channel, Actor, wes_example_count,
                                wes_db_null, wes_lock_ets, []),
    ok = wes_lock_ets:command(Channel, incr),
    ?assertEqual(1, wes_lock_ets:read(Actor, counter, wes_lock_ets)).
