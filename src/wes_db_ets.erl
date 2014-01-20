-module(wes_db_ets).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% wes db callback interface.
-export([read/1,
         write/2,
         clear/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

read(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{_Key, _Value} = Data] ->
            {ok, Data};
        [] ->
            not_found
    end.

write(Key, Value) ->
    ets:insert(?MODULE, {Key, Value}).

clear() ->
    ets:delete_all_objects(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Tab = ets:new(?MODULE, [named_table,
                            {write_concurrency, true},
                            {read_concurrency, true},
                            public]),
    {ok, #state{tab = Tab}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
