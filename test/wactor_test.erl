-module(wactor_test).

-include_lib("eunit/include/eunit.hrl").

session_test_() ->
    [{setup, local,
      fun test_setup/0,
      fun test_teardown/1,
      [fun test_simple/0]}].

test_setup() ->
    application:start(wactor).

test_teardown(_) ->
    application:stop(wactor).


test_simple() ->
    {ok, _Pid} = wactor_channel:start(hej, [{act1, wactor_example_count, []}]),
    wactor_channel:command(hej, incr),
    wactor_channel:read(act1, counter).
