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

-export([start/1, start/2, start_link/2, start/3, start_link/3]).
-export([stop/1, stop/2]).
-export([restart_node/2, restart_node/3]).
-export([list_nodes/1, check_config/2, status/1]).
-export([print_status/1, log_status/1]).
-export([node_names/1, node_pids/1]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("systest.hrl").

-exprecs_prefix([operation]).
-exprecs_fname(["record_", prefix]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records(['systest.cluster']).

%%
%% Public API
%%

start(Config) ->
    start(global, Config).

start(ClusterId, Config) ->
    start(ClusterId, ClusterId, Config).

start(ScopeId, ClusterId, Config) ->
    start_it(start, ScopeId, ClusterId, Config).

start_link(ClusterId, Config) ->
    start(ClusterId, ClusterId, Config).

start_link(ScopeId, ClusterId, Config) ->
    start_it(start_link, ScopeId, ClusterId, Config).

start_it(How, ScopeId, ClusterId, Config) ->
    ct:log("Processing Cluster ~p~n", [ClusterId]),
    case apply(gen_server, How, [{local, ClusterId},
                                 ?MODULE, [ScopeId, ClusterId, Config], []]) of
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

restart_node(ClusterRef, Node) ->
    restart_node(ClusterRef, Node, infinity).

restart_node(ClusterRef, Node, Timeout) ->
    case gen_server:call(ClusterRef, {restart, Node}, Timeout) of
        {restarted, {_OldNode, NewNode}} ->
            {ok, NewNode};
        Other ->
            {error, Other}
    end.

status(ClusterRef) ->
    gen_server:call(ClusterRef, status).

list_nodes(ClusterRef) ->
    gen_server:call(ClusterRef, nodes).

print_status(Cluster) ->
    ct:log(lists:flatten([print_status_info(N) || N <- status(Cluster)])).

log_status(Cluster) ->
    ct:log(lists:flatten([print_status_info(N) || N <- status(Cluster)])).

check_config(Cluster, Config) ->
    with_cluster({Cluster, Cluster}, fun build_nodes/4, Config).

%% doing useful things with cluster records....

node_names(Cluster) when is_record(Cluster, 'systest.cluster') ->
    [element(1, N) || N <- get(nodes, Cluster)].

node_pids(Cluster) when is_record(Cluster, 'systest.cluster') ->
    [element(2, N) || N <- get(nodes, Cluster)].

%%
%% OTP gen_server API
%%

init([Scope, Id, Config]) ->
    process_flag(trap_exit, true),
    %% TODO: now that we're using locally registered
    %% names, perhaps this logic can go away?
    case systest_watchdog:cluster_started(Id, self()) of
        ok ->
            case with_cluster({Scope, Id}, fun start_host/4, Config) of
                noconfig ->
                    {stop, noconfig};
                Cluster=#'systest.cluster'{nodes=Nodes, on_start=Hooks} ->
                    case Hooks of
                        [{on_start, Run}|_] ->
                            ct:log("running cluster on_start hooks ~p~n",
                                   [Run]),
                            [systest_hooks:run(Cluster,
                                               Hook, Cluster) || Hook <- Run];
                        Other ->
                            ct:log("ignoring cluster hooks ~p~n", [Other]),
                            ok
                    end,
                    [begin
                         {_, Ref} = Node,
                         ct:log("~p joined_cluster~n", [Node]),
                         systest_node:joined_cluster(Ref, Cluster,
                                                     Nodes -- [Node])
                     end || Node <- Nodes],
                    {ok, Cluster}
            end;
        {error, clash} ->
            {stop, name_in_use}
    end.

handle_call(nodes, _From, State=#'systest.cluster'{nodes=Nodes}) ->
    {reply, Nodes, State};
handle_call(status, _From, State=#'systest.cluster'{nodes=Nodes}) ->
    {reply, [{N, systest_node:status(N)} || {_, N} <- Nodes], State};
handle_call({stop, Timeout}, From, State) ->
    shutdown(State, Timeout, From);
handle_call(stop, From, State) ->
    shutdown(State, infinity, From);
handle_call({restart, Node}, From,
            State=#'systest.cluster'{nodes=Nodes, pending=P}) ->
    case [N || N <- Nodes, element(1, N) == Node orelse
                           element(2, N) == Node] of
        [] ->
            {reply, {error, Node}, State};
        [{_, Ref}=Found] ->
            systest_node:stop(Ref),
            State2 = State#'systest.cluster'{
                pending=[{restart, Found, From}|P]
            },
            {noreply, State2}
    end;
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, normal}=Ev,
            State=#'systest.cluster'{name=Cluster}) ->
    systest_watchdog:node_stopped(Cluster, Pid),
    {noreply, clear_pending(Ev, State)};
handle_info({'EXIT', Pid, Reason}, State) ->
    {stop, {nodedown, Pid, Reason}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal API
%%

clear_pending({'EXIT', Pid, normal},
              #'systest.cluster'{id = Identity, name = ClusterName,
                                 config = Config, nodes = Nodes,
                                 pending = Pending}=State) ->
    case [P || {_, {_, DyingPid}, _}=P <- Pending, DyingPid == Pid] of
        [{restart, {Id, Pid}=DeadNode, Client}]=Restart ->
            {NodeName, Host} = systest_utils:node_id_and_hostname(Id),
            [Node] = build_nodes(ClusterName, ClusterName,
                                 {Host, [NodeName]}, Config),
            NewNode = start_node(Identity, Node),
            RemainingNodes = Nodes -- [DeadNode],
            systest_node:joined_cluster(element(2, NewNode),
                                        State, RemainingNodes),
            gen_server:reply(Client, {restarted, {DeadNode, NewNode}}),
            NewNodeState = [NewNode|(RemainingNodes)],
            NewPendingState = Pending -- Restart,
            State#'systest.cluster'{nodes   = NewNodeState,
                                    pending = NewPendingState};
        _ ->
            State
    end.


shutdown(State=#'systest.cluster'{name=Id, nodes=Nodes}, Timeout, ReplyTo) ->
    %% NB: unlike systest_node:shutdown_and_wait/2, this does not have to
    %% block and quite deliberately so - we want 'timed' shutdown when a
    %% common test hook is in effect unless the user prevents this...
    %%
    %% Another thing to note here is that systest_cleaner runs the kill_wait
    %% function in a different process. If we put a selective receive block
    %% here, we might well run into unexpected message ordering that could
    %% leave us in an inconsistent state.
    NodeRefs = [NodeRef || {_, NodeRef} <- Nodes],
    case systest_cleaner:kill_wait(NodeRefs,
                                   fun systest_node:stop/1, Timeout) of
        ok ->
            ct:log("Stopping Cluster...~n"),
            [systest_watchdog:node_stopped(Id, N) || N <- NodeRefs],
            gen_server:reply(ReplyTo, ok),
            {stop, normal, State};
        {error, {killed, StoppedOk}} ->
            ct:log("Halt Error: killed~n"),
            Err = {halt_error, orphans, NodeRefs -- StoppedOk},
            gen_server:reply(ReplyTo, Err),
            {stop, Err, State};
        Other ->
            ct:log("Halt Error: ~p~n", [Other]),
            gen_server:reply(ReplyTo, {error, Other}),
            {stop, {halt_error, Other}, State}
    end.

with_cluster({Scope, Identity}, Handler, Config) ->
    case systest_config:cluster_config(Scope, Identity) of
        {_, noconfig} ->
            noconfig;
        {Alias, ClusterConfig} ->
            {Hosts, Hooks} = lists:splitwith(fun(E) ->
                                                 element(1, E) =/= on_start
                                             end, ClusterConfig),
            ct:log("Configured hosts: ~p~n", [Hosts]),
            Nodes = lists:flatten([Handler(Identity, Alias,
                                           Host, Config) || Host <- Hosts]),

            #'systest.cluster'{id = Identity,
                               scope = Scope,
                               name = Alias,
                               nodes = Nodes,
                               config = Config,
                               on_start = Hooks}
    end.

%% TODO: make a Handler:status call to get detailed information back...
print_status_info({Node, Status}) ->
    Lines = [{status, Status}|systest_node:node_data(Node)],
    lists:flatten("Node Info~n" ++ systest_utils:proplist_format(Lines) ++
                  "~n----------------------------------------------------~n").

build_nodes(Identity, Cluster, {Host, Nodes}, Config) ->
    [systest_node:make_node(Cluster, N, [{host, Host}, {scope, Identity},
                                         {name, N}|Config]) || N <- Nodes].

start_host(Identity, Cluster, {localhost, Nodes}, Config) ->
    {ok, Hostname} = inet:gethostname(),
    start_host(Identity, Cluster, {list_to_atom(Hostname), Nodes}, Config);
start_host(Identity, Cluster,
           {Host, Nodes}=HostConf, Config) when is_atom(Host) andalso
                                                is_list(Nodes) ->
    case ?CONFIG(verify_hosts, Config, false) of
        true  -> verify_host(Host);
        false -> ok
    end,
    [start_node(Identity, Node) ||
            Node <- build_nodes(Identity, Cluster, HostConf, Config)].

start_node(Identity, Node) ->
    {ok, NodeRef} = systest_node:start(Node),
    systest_watchdog:node_started(Identity, NodeRef),
    %% NB: the id field of Node will *not* be set (correctly)
    %% until after the gen_server has started, so an API call
    %% is necessary rather than using systest_node:get/2
    {?CONFIG(id, systest_node:node_data(NodeRef)), NodeRef}.

verify_host(Host) ->
    case systest_utils:is_epmd_contactable(Host, 5000) of
        true ->
            ok;
        {false, Reason} ->
            ct:log("Unable to contact ~p: ~p~n", [Host, Reason]),
            throw({host_unavailable, Host})
    end.

