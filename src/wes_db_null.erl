-module(wes_db_null).

-behaviour(wes_db).

-export([read/2,
         write/3]).

read(_Key, _) -> not_found.

write(_Key, _Value, _) -> ok.
