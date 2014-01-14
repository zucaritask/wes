-module(wactor_test).

-export([test/0]).

test() ->
    {ok, _Pid} = wactor_channel:start(hej, [{act1, wactor_example_count, []}]),
    wactor_channel:command(hej, incr),
    wactor_channel:read(act1, counter).
