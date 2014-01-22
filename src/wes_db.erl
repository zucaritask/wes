-module(wes_db).

-callback read(Key::term(), Config::proplists:proplist()) ->
    not_found |
    {ok, Result::wes:serialized_actor()} |
    {ok, MetaData::proplists:proplits(), Result::wes:serial_actor()}.

-callback write(Key::term(), Value::wes:serialized_actor(),
                Config::proplists:proplist()) ->
    {error, Reason::atom()} | ok.
