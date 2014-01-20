-module(wes_timeout_tests).

-include_lib("eunit/include/eunit.hrl").

add_2_test() ->
    Timeouts = wes_timeout:add(b, 8, 2,
                               wes_timeout:add(a, 12, 1, wes_timeout:new())),
    io:format("Timeouts ~p", [Timeouts]),
    ?assertEqual({b, 10}, wes_timeout:next(Timeouts)),
    Timeouts2 = wes_timeout:reset(b, 3, Timeouts),
    ?assertEqual({b, 11}, wes_timeout:next(Timeouts2)),
    Timeouts3 = wes_timeout:reset(b, 6, Timeouts2),
    ?assertEqual({a, 13}, wes_timeout:next(Timeouts3)).
