-module(wes_stats_ets).

-behaviour(gen_server).

%% API
-export([start_link/0,
         stop/0,
         all_stats/0]).

%% Callback
-export([get/2,
         stat/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(state, {tab}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

all_stats() ->
    lists:sort(ets:tab2list(?TABLE)).

stat(Type, Name) ->
    Key = {Type, Name},
    case catch ets:update_counter(?TABLE, Key, 1) of
        {'EXIT', {badarg, _}} ->
            ets:insert(?TABLE, {Key, 1}),
            ok;
        _ ->
            ok
    end.

get(Type, Name) ->
    Key = {Type, Name},
    case ets:lookup(?TABLE, Key) of
        [] -> 0;
        [{Key, Val}] -> Val
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{tab = ets:new(?TABLE, [named_table, public])}}.

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
