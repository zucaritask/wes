-module(wactor_test).

-include_lib("eunit/include/eunit.hrl").

db_test_() ->
    [{foreach, local,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_locker/0,
       fun test_ets/0,
       fun test_locker_restart/0]}].

test_setup() ->
    wactor_sup:start_link().

test_teardown(_) ->
    ok.

test_locker() ->
    wactor_locker:start([node()], [], 1, 1000, 1000, 100),
    Actors = [{act1, wactor_example_count, wactor_db_null, []}],
    {ok, _Pid} = wactor_locker:start_channel(hej, Actors),
    wactor_locker:command(hej, incr),
    ?assertEqual(1, wactor_locker:read(act1, counter)),
    wactor_locker:stop(hej),
    {ok, _OtherPid} = wactor_locker:start_channel(hej, Actors),
    ?assertEqual(0, wactor_locker:read(act1, counter)),
    wactor_locker:stop(hej).

test_locker_restart() ->
    wactor_db_ets:start_link(),
    Actors = [{act1, wactor_example_count, wactor_db_ets, []}],
    {ok, _Pid} = wactor_locker:start_channel(hej, Actors),
    wactor_locker:command(hej, incr),
    ?assertEqual(1, wactor_locker:read(act1, counter)),
    wactor_locker:stop(hej),
    {ok, _OtherPid} = wactor_locker:start_channel(hej, Actors),
    ?assertEqual(1, wactor_locker:read(act1, counter)),
    wactor_locker:stop(hej),
    wactor_db_ets:clear().

test_ets() ->
    wactor_db_ets:start_link(),
    wactor_lock_ets:start(),
    Actors = [{act1, wactor_example_count, wactor_db_ets, []}],
    {ok, _Pid} = wactor_lock_ets:start_channel(hej, Actors),
    wactor_lock_ets:command(hej, incr),
    ?assertEqual(1, wactor_lock_ets:read(act1, counter)),
    ok = wactor_lock_ets:stop(hej),
    {ok, _Pid2} = wactor_lock_ets:start_channel(hej, Actors),
    ?assertEqual(1, wactor_lock_ets:read(act1, counter)),
    wactor_db_ets:clear().
