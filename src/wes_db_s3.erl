-module(wes_db_s3).

-export([start/1,
         stop/1]).

-export([read/2,
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
    supervisor:terminate_child(wes_sup, Name),
    supervisor:delete_child(wes_sup, Name).

read(Key, Config) ->
    Bucket = proplists:get_value(bucket, Config, ""),
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

write(Key, Value, Config) ->
    Bucket = proplists:get_value(bucket, Config, ""),
    case s3:put(Bucket, Key, Value, "application/json") of
        {'EXIT', {timeout, _}} ->
            {error, timeout};
        Result ->
            Result
    end.
