-module(wes_db_null).

-behaviour(wes_db).

-export([read/2,
         write/3]).

read(_Key, []) -> not_found.

write(_Key, _Value, []) -> ok.
