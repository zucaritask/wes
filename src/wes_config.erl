-module(wes_config).

-include("wes_internal.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2,
         actor/1,
         channel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(state, {tab}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Actors, Channels) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Actors, Channels], []).

actor(Type) ->
    case ets:lookup(?TABLE, {actor, Type}) of
        [{_, ActorConfig}] -> ActorConfig;
        [] -> throw({actor_not_configured, Type})
    end.

channel(Type) ->
    case ets:lookup(?TABLE, {channel, Type}) of
        [{_, ChannelConfig}] -> ChannelConfig;
        [] -> throw({channel_not_configured, Type})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Actors, Channels]) ->
    Tab = ets:new(?TABLE, [named_table, protected,
                           {read_concurrency, true}]),
    [init_actor(Actor) || Actor <- Actors],
    [init_channel(Channel) || Channel <- Channels],
    error_logger:error_msg("Config tab ~p", [ets:tab2list(?TABLE)]),
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

init_actor(Actor) ->
    Key = proplists:get_value(id, Actor),
    Config = #actor_config{
                locker_mod = proplists:get_value(locker_mod, Actor),
                cb_mod = proplists:get_value(cb_mod, Actor),
                db_mod = proplists:get_value(db_mod, Actor),
                db_conf = proplists:get_value(db_conf, Actor)
               },
    ets:insert(?TABLE, {{actor, Key}, Config}),
    ok.

init_channel(Channel) ->
    Key = proplists:get_value(id, Channel),
    Config = #channel_config{
                locker_mod = proplists:get_value(locker_mod, Channel),
                stats_mod = proplists:get_value(stats_mod, Channel),
                message_timeout = proplists:get_value(message_timeout, Channel),
                lock_timeout_interval =
                    proplists:get_value(lock_timeout_interval, Channel)
               },
    ets:insert(?TABLE, {{channel, Key}, Config}),
    ok.
