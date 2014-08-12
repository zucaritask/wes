-module(wes_db_s3).

-behaviour(wes_db).

-export([start/1,
         stop/1]).

-export([read/2,
         multi_read/2,
         write/3]).

start(Conf) ->
    Args = proplists:get_value(start_args, Conf, []),
    Name = proplists:get_value(sup_name, Conf, s3_server),
    Spec = {Name, {s3_server, start_link, [Args]},
            permanent, 2000, worker, [s3_server]},
    {ok, _} = supervisor:start_child(wes_sup, Spec),
    ok.

stop(Conf) ->
    Name = proplists:get_value(sup_name, Conf, s3_server),
    ok = supervisor:terminate_child(wes_sup, Name),
    ok = supervisor:delete_child(wes_sup, Name).

read(Key, Config) ->
    Bucket = bucket(Config),
    case s3:get(Bucket, Key) of
        {ok, not_found} ->
            not_found;
        {'EXIT', {timeout, _}} ->
            {error, timeout};
        {ok, _Headers, Result} ->
            {ok, Result};
        {ok, Result} ->
            {ok, Result}
    end.

multi_read(Keys, Config) ->
    [ read(Key, Config) || Key <- Keys ].

write(Key, Value, Config) ->
    Bucket = bucket(Config),
    case s3:put(Bucket, Key, Value, "application/json") of
        {'EXIT', {timeout, _}} ->
            {error, timeout};
        Result ->
            Result
    end.

bucket(Config) ->
    case lists:keyfind(bucket, 1, Config) of
        {bucket, Bucket} -> Bucket;
        _ -> erlang:error(bucket_not_defined)
    end.
