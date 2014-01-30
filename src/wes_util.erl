-module(wes_util).

-export([zffoldl/3]).

zffoldl(F, Accu0, [Hd|Tail]) ->
    case F(Hd, Accu0) of
        {ok, R, Accu1} ->
            {Rs,Accu2} = zffoldl(F, Accu1, Tail),
            {[R|Rs],Accu2};
        {false, Accu1} ->
            {Rs,Accu2} = zffoldl(F, Accu1, Tail),
            {Rs,Accu2}
    end;
zffoldl(F, Accu, []) when is_function(F, 2) -> {[],Accu}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

zffoldl_test() ->
    F = fun(A, Acc) ->
                if A rem 2 =:= 0 -> {ok, A+1,A+Acc};
                   true -> {false, Acc}
                end
        end,
    ?assertEqual({[3,5], 6}, zffoldl(F, 0, [1,2,3,4])),
    ?assertEqual({[], 1}, zffoldl(F, 1, [1,3,5])),
    ?assertEqual({[], 2}, zffoldl(F, 2, [])),
    ?assertEqual({[3,5], 9}, zffoldl(F, 3, [2,4])).

-endif.
