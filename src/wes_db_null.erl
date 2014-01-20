-module(wes_db_null).

-export([read/1,
         write/2]).

read(_Key) -> not_found.

write(_Key, _Value) -> ok.
