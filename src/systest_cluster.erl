%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ----------------------------------------------------------------------------
-module(systest_cluster).

-behaviour(gen_server).

-export([start/1, start/2, start_link/2, stop/1, stop/2]).
-export([check_config/2, status/1, print_status/1, log_status/1]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("systest.hrl").

%%
%% Public API
%%

start(Config) ->
    start(global, Config).

start(ClusterId, Config) ->
    start_it(start, ClusterId, ClusterId, Config).

start_link(ClusterId, Config) ->
    start_it(start_link, ClusterId, ClusterId, Config).

start_it(How, ScopeId, ClusterId, Config) ->
    ct:pal("Processing cluster n", [ClusterId]),
    case apply(gen_server, How, [[ScopeId, ClusterId, Config], []]) of
        {error, noconfig} ->
            Config;
        {ok, Pid} ->
            Config2 = systest_config:ensure_value(ClusterId, Pid, Config),
            systest_config:replace_value(active, Pid, Config2);
        {error, _Other}=Err ->
            throw(Err)
    end.

stop(ClusterRef) ->
    stop(ClusterRef, infinity).

stop(ClusterRef, Timeout) ->
    gen_server:call(ClusterRef, stop, Timeout).

status(ClusterRef) ->
    gen_server:call(ClusterRef, status).

print_status(Cluster) ->
    ct:pal(lists:flatten([print_status_info(N) || N <- status(Cluster)])).

log_status(Cluster) ->
    ct:log(lists:flatten([print_status_info(N) || N <- status(Cluster)])).

check_config(Cluster, Config) ->
    with_cluster(Cluster, fun build_nodes/3, Config).

%%
%% OTP gen_server API
%%

init([Scope, Id, Config]) ->
    process_flag(trap_exit, true),
    case systest_watchdog:cluster_started(Id, self()) of
        ok ->
            case with_cluster({Scope, Id}, fun start_host/3, Config) of
                noconfig ->
                    {stop, noconfig};
                Cluster ->
                    {ok, Cluster}
            end;
        {error, clash} ->
            {stop, name_in_use}
    end.

handle_call(status, _From, State=#'systest.cluster'{nodes=Nodes}) ->
    {reply, [{N, systest_node:status(N)} || N <- Nodes], State};
handle_call({stop, Timeout}, From, State) ->
    shutdown(State, Timeout, From);
handle_call(stop, From, State) ->
    shutdown(State, infinity, From);
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, normal}, State=#'systest.cluster'{name=Cluster}) ->
    systest_watchdog:node_stopped(Cluster, Pid),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    {stop, {nodedown, node_from_pid(Pid, State), Reason}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal API
%%

shutdown(State=#'systest.cluster'{name=Id, nodes=Nodes}, Timeout, ReplyTo) ->
    %% NB: unlike systest_node:shutdown_and_wait/2, this does not have to
    %% block and quite deliberately so - we want 'timed' shutdown when a
    %% common test hook is in effect unless the user prevents this...
    %%
    %% Another thing to note here is that systest_cleaner runs the kill_wait
    %% function in a different process. If we put a selective receive block
    %% here, we might well run into unexpected message ordering that could
    %% leave us in an inconsistent state or even deadlock on the wrong kind
    %% of input message.
    ct:pal("killing nodes: ~p~n", [Nodes]),
    case systest_cleaner:kill_wait(Nodes, fun systest_node:stop/1, Timeout) of
        ok ->
            ct:pal("stopping cluster...~n"),
            [systest_watchdog:node_stopped(Id, N) || N <- Nodes],
            gen_server:reply(ReplyTo, ok),
            {stop, normal, State};
        {error, {killed, StoppedOk}} ->
            Err = {halt_error, orphans, Nodes -- StoppedOk},
            gen_server:reply(ReplyTo, Err),
            {stop, Err, State};
        Other ->
            gen_server:reply(ReplyTo, {error, Other}),
            {stop, {halt_error, Other}, State}
    end.

with_cluster(ClusterId, NodeHandler, Config) ->
    case systest:cluster_config(ClusterId) of
        undefined -> noconfig;
        []        -> noconfig;
        Hosts ->
            ct:log("Configured hosts: ~p~n", [Hosts]),
            Nodes = lists:flatten(
                     [NodeHandler(ClusterId, Host, Config) || Host <- Hosts]),
            #'systest.cluster'{name=ClusterId, nodes=Nodes}
    end.

node_from_pid(Pid, #'systest.cluster'{nodes=Nodes}) ->
    case [N#'systest.node_info'.id || N <- Nodes,
                                       N#'systest.node_info'.owner == Pid] of
        []   -> {unknown_node, Pid};
        [Id] -> Id
    end.

%% TODO: make a Handler:status call to get detailed information back...
print_status_info({Node, Status}) ->
    Lines = [{status, Status}|systest_node:node_data(Node)],
    lists:flatten("Node Info~n" ++ systest_utils:proplist_format(Lines) ++
                  "~n----------------------------------------------------~n").

build_nodes(Cluster, {Host, Nodes}, Config) ->
    [systest_node:make_node(Cluster, N, [{host, Host}, {scope, Cluster},
                                         {name, N}|Config]) || N <- Nodes].

start_host(Cluster, {localhost, Nodes}, Config) ->
    {ok, Hostname} = inet:gethostname(),
    start_host(Cluster, {list_to_atom(Hostname), Nodes}, Config);
start_host(Cluster, {Host, Nodes}=HostConf, Config) when is_atom(Host) andalso
                                                         is_list(Nodes) ->
    case ?CONFIG(verify_hosts, Config, false) of
        true  -> verify_host(Host);
        false -> ok
    end,
    [start_node(Cluster, Node) ||
            Node <- build_nodes(Cluster, HostConf, Config)].

start_node(Cluster, Node) ->
    {ok, NodeRef} = systest_node:start(Node),
    systest_watchdog:node_started(Cluster, NodeRef),
    NodeRef.

verify_host(Host) ->
    case systest_utils:is_epmd_contactable(Host, 5000) of
        true ->
            ok;
        {false, Reason} ->
            ct:pal("Unable to contact ~p: ~p~n", [Host, Reason]),
            throw({host_unavailable, Host})
    end.

