-module(wes_stats).

%% Callback
-callback get(Type::any(), Name::any()) -> Value::any().

-callback stat(Type::any(), Name::any()) -> ok.
