-module(wactor_test).

-include_lib("eunit/include/eunit.hrl").

session_test_() ->
    [{setup, local,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_locker/0]}].

test_setup() ->
    wactor_sup:start_link(),
    wactor_locker:start([node()], [], 1, 1000, 1000, 100).

test_teardown(_) ->
    ok.

test_locker() ->
    Actors = [{act1, wactor_example_count, wactor_db_null, []}],
    {ok, _Pid} = wactor_locker:start_channel(hej, Actors),
    wactor_locker:command(hej, incr),
    wactor_locker:read(act1, counter).
