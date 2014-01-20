-module(wes_timeout).

%% Make this into some kind of heap if this implementation has
%% performance impact.

-export([next/1,
         reset/3,
         new/0,
         add/4,
         now/0,
         time_diff/2]).

next(Timeouts) ->
    dict:fold(fun(Name, {NextTimeout, _}, {NameAcc, TimeoutAcc}) ->
                      if TimeoutAcc == infinite -> {Name, NextTimeout};
                         TimeoutAcc =< NextTimeout -> {NameAcc, TimeoutAcc};
                         true -> {Name, NextTimeout}
                      end
              end,
              {bogus, infinite},
              Timeouts).

reset(Name, Now, Timeouts) ->
    dict:update(Name, fun({_OldTimeout, Timeout}) ->
                              {Now+Timeout, Timeout}
                      end,
                Timeouts).

add(Name, infinite, _Now, Timeouts) ->
    dict:erase(Name, Timeouts);
add(Name, Timeout, Now, Timeouts) ->
    dict:store(Name, {Timeout+Now, Timeout}, Timeouts).

new() ->
    dict:new().

now() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    MegaSecs * 1000000 + Secs.

time_diff(infinite, _) -> infinite;
time_diff(A, B) -> A - B.
