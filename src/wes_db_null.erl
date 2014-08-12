-module(wes_db_null).

-behaviour(wes_db).

-export([read/2,
         multi_read/2,
         write/3]).

read(_Key, _) -> not_found.

multi_read(Keys, Config) ->
    [ read(Key, Config) || Key <- Keys ].

write(_Key, _Value, _) -> ok.
