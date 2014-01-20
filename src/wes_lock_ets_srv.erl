-module(wes_lock_ets_srv).

-behaviour(gen_server).

%% API
-export([start_link/1,
         read/1,
         lock/2,
         release/2,
         extend_lease/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab, timeout}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Timeout) ->
    error_logger:info_msg("Starting ~p", [Timeout]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Timeout], []).

read(Id) ->
    gen_server:call(?SERVER, {read, Id}).

lock(Id, Value) ->
    gen_server:call(?SERVER, {lock, Id, Value}).

release(Id, Value) ->
    gen_server:call(?SERVER, {release, Id, Value}).

extend_lease(Id, Value) ->
    gen_server:call(?SERVER, {extend_lease, Id, Value}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Timeout]) ->
    {ok, #state{
            timeout = Timeout,
            tab = ets:new(?MODULE, [named_table])}}.

handle_call({read, Id}, _From, #state{tab = Tab} = State) ->
    Now = now_seconds(),
    Reply =
        case ets:lookup(Tab, Id) of
            [{Id, Value, Timeout}] when Now =< Timeout ->
                {ok, Value};
            [{Id, Value, _}] ->
                {ok, Value};
            [] ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call({lock, Id, Value}, _From,
            #state{tab = Tab, timeout = Timeout} = State) ->
    Reply =
        case ets:lookup(Tab, Id) of
            [{Id, _, _}] ->
                {error, already_locked};
            [] ->
                ets:insert(Tab, [{Id, Value, Timeout + now_seconds()}]),
                ok
        end,
    {reply, Reply, State};
handle_call({release, Id, Value}, _From, #state{tab = Tab} = State) ->
    Now = now_seconds(),
    Reply =
        case ets:lookup(Tab, Id) of
            [{Id, Value, Timeout}] when Now =< Timeout ->
                ets:delete(Tab, Id),
                ok;
            [{Id, Value, Timeout}] ->
                {error, {locked_timeout, Now, Timeout}};
            [{Id, OtherValue, _Timeout}] ->
                {error, {locked_with_other_value, OtherValue}};
            [] ->
                {error, {no_such_lock, Id}}
        end,
    {reply, Reply, State};
handle_call({extend_lease, Id, Value}, _From, #state{tab = Tab} = State) ->
    Now = now_seconds(),
    Reply =
        case ets:lookup(Tab, Id) of
            [{Id, Value, Timeout}] when Now =< Timeout ->
                ets:insert(Tab, [{Id, Value, Timeout + now_seconds()}]),
                ok;
            [{Id, Value, _Timeout}] ->
                {error, prev_lock_timeout};
            [{Id, OtherValue, _Timeout}] ->
                {error, {locked_with_other_value, OtherValue}};
            [] ->
                {error, {no_such_lock, Id}}
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

now_seconds() ->
    {Mega, Secs, _} = now(),
    Mega * 1000000 + Secs.
