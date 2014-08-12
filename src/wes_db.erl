-module(wes_db).

-export_type([key/0]).

-type key() :: any().

-callback read(Key::key(), Config::proplists:proplist()) ->
    not_found |
    {ok, Result::wes:serialized_actor()} |
    {ok, MetaData::proplists:proplist(), Result::wes:serialized_actor()}.

-callback multi_read([Key::key()], Config::proplists:proplist()) ->
    [not_found |
     {ok, Result::wes:serialized_actor()} |
     {ok, MetaData::proplists:proplist(), Result::wes:serialized_actor()}].

-callback write(Key::term(), Value::wes:serialized_actor(),
                Config::proplists:proplist()) ->
    {error, Reason::atom()} | ok.
