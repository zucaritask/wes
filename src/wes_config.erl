-module(wes_config).

-include("wes_internal.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2,
         update/2,
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

update(Actors, Channels) ->
    gen_server:call(?SERVER, {update, Actors, Channels}).

actor(Type) ->
    case ets:lookup(?TABLE, {actor, Type}) of
        [{_, ActorConfig}] -> ActorConfig;
        [] -> error({actor_not_configured, Type})
    end.

channel(Type) ->
    case ets:lookup(?TABLE, {channel, Type}) of
        [{_, ChannelConfig}] -> ChannelConfig;
        [] -> error({channel_not_configured, Type})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Actors, Channels]) ->
    Tab = ets:new(?TABLE, [named_table, protected,
                           {read_concurrency, true}]),
    set_config(Actors, Channels),
    {ok, #state{tab = Tab}}.

handle_call({update, Actors, Channels}, _From, State) ->
    set_config(Actors, Channels),
    {reply, ok, State}.

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

set_config(Actors, Channels) ->
    _ = [ok = set_actor_config(Actor) || Actor <- Actors],
    _ = [ok = set_channel_config(Channel) || Channel <- Channels],
    ok.

set_actor_config(Actor) ->
    Key = proplists:get_value(id, Actor),
    Config = #actor_config{
                lock_mod = proplists:get_value(lock_mod, Actor),
                cb_mod = proplists:get_value(cb_mod, Actor),
                db_mod = proplists:get_value(db_mod, Actor),
                db_conf = proplists:get_value(db_conf, Actor)
               },
    ets:insert(?TABLE, {{actor, Key}, Config}),
    ok.

set_channel_config(Channel) ->
    Key = proplists:get_value(id, Channel),
    Config = #channel_config{
                lock_mod = proplists:get_value(lock_mod, Channel),
                stats_mod = proplists:get_value(stats_mod, Channel),
                message_timeout = proplists:get_value(message_timeout, Channel),
                save_timeout = proplists:get_value(save_timeout, Channel),
                lock_timeout_interval =
                    proplists:get_value(lock_timeout_interval, Channel)
               },
    ets:insert(?TABLE, {{channel, Key}, Config}),
    ok.
