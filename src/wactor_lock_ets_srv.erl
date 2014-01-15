-module(wactor_lock_ets_srv).

-behaviour(gen_server).

%% API
-export([start_link/0,
         read/1,
         lock/2,
         release/2]).

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

read(Id) ->
    gen_server:call(?SERVER, {read, Id}).

lock(Id, Value) ->
    gen_server:call(?SERVER, {lock, Id, Value}).

release(Id, Value) ->
    gen_server:call(?SERVER, {release, Id, Value}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{tab = ets:new(?MODULE, [])}}.

handle_call({read, Id}, _From, #state{tab = Tab} = State) ->
    Reply =
        case ets:lookup(Tab, Id) of
            [{Id, Value}] ->
                {ok, Value};
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({lock, Id, Value}, _From, #state{tab = Tab} = State) ->
    Reply =
        case ets:lookup(Tab, Id) of
            [{Id, _}] ->
                {error, already_locked};
            [] ->
                ets:insert(Tab, [{Id, Value}]),
                ok
        end,
    {reply, Reply, State};
handle_call({release, Id, Value}, _From, #state{tab = Tab} = State) ->
    Reply =
        case ets:lookup(Tab, Id) of
            [{Id, Value}] ->
                ets:delete(Tab, Id),
                ok;
            [{Id, OtherValue}] ->
                {error, {locked_with_other_value, OtherValue}};
            [] ->
                {error, no_such_lock}
        end,
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
