-module(wactor_test).

-include_lib("eunit/include/eunit.hrl").

session_test_() ->
    [{setup, local,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_locker/0]}].

test_setup() ->
    application:start(wactor),
    wactor_locker:start([node()]).

test_teardown(_) ->
    application:stop(wactor).


test_locker() ->
    {ok, _Pid} = wactor_locker:start_channel(hej, [{act1, wactor_example_count, []}]),
    wactor_locker:command(hej, incr),
    wactor_locker:read(act1, counter).
