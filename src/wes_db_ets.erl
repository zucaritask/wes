-module(wes_db_ets).

-behaviour(wes_db).
-behaviour(gen_server).

%% API
-export([start/1,
         stop/1,
         start_link/0,
         stop/0]).

%% wes db callback interface.
-export([read/2,
         write/3,
         clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab}).

%%%===================================================================
%%% API
%%%===================================================================

start(Conf) ->
    Name = proplists:get_value(sup_name, Conf, ?MODULE),
    Spec = {Name, {?MODULE, start_link, []},
            permanent, 2000, worker, [?MODULE]},
    {ok, _} = supervisor:start_child(wes_sup, Spec),
    ok.

stop(Conf) ->
    Name = proplists:get_value(sup_name, Conf, ?MODULE),
    supervisor:terminate_child(wes_sup, Name),
    supervisor:delete_child(wes_sup, Name).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

read(Key, []) ->
    case ets:lookup(?MODULE, Key) of
        [{_Key, _Value} = Data] ->
            {ok, Data};
        [] ->
            not_found
    end.

write(Key, Value, []) ->
    ets:insert(?MODULE, {Key, Value}),
    ok.

clear() ->
    ets:delete_all_objects(?MODULE).

stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Tab = ets:new(?MODULE, [named_table,
                            {write_concurrency, true},
                            {read_concurrency, true},
                            public]),
    {ok, #state{tab = Tab}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
