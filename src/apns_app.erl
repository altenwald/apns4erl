%%%-------------------------------------------------------------------
%%% @hidden
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @doc Apple Push Notification Server for Erlang
%%% @end
%%%-------------------------------------------------------------------
-module(apns_app).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).
-export([init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
%% @hidden
-spec init(_) -> {ok, {{supervisor:strategy(), MaxR::non_neg_integer(), MaxT::pos_integer()}, [supervisor:child_spec()]}}.
init(_) ->
    {ok, {{one_for_one, 5, 10}, [
        {connection_sup, {apns_sup, start_link, []}, permanent, 5000, supervisor, [apns_sup]},
        {queue, {apns_queue, start_link, []}, permanent, 5000, worker, [apns_queue]}
    ]}}.
