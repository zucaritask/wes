-module(wes_bench).

-export([bench/3]).

bench(N, M, Sleep) ->
    timer:tc(
      fun() ->
              {ok, _} = wes_sup:start_link([], []),
              wes_locker:start([node()], [], 1, 1000, 1000, 100),
              do(N, M, Sleep)
      end).

do(0, _, _) -> ok;
do(N, M, Sleep) ->
    do_do(N, M),
    timer:sleep(Sleep),
    do(N-1, M, Sleep).

do_do(_, 0) -> ok;
do_do(N, M) ->
    {ok, _} = wes_channel:start({session, {N, M}}, []),
    do_do(N, M-1).
