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
-module(systest_watchdog).

-include("systest.hrl").

-define(ETS_OPTS,
        [private, named_table,
         {read_concurrency, true},
         {write_concurrency, false}]).

-record(state, {cluster_table, node_table, exception_table}).

-behaviour(gen_server).

%% API Exports

-export([start/0, cluster_started/2, exceptions/1,
         node_started/2, node_stopped/2, force_stop/1]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%
%% Public API
%%

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cluster_started(Id, Pid) ->
    gen_server:call(?MODULE, {cluster_started, Id, Pid}).

force_stop(Id) ->
    case gen_server:call(?MODULE, {force_stop, Id}) of
        {error, regname, Id} ->
            ct:pal("ignoring stop for deceased cluster ~p~n",
                   [Id]);
        Ok ->
            Ok
    end.

node_started(Cid, Pid) ->
    gen_server:call(?MODULE, {node_started, Cid, Pid}).

node_stopped(Cid, Pid) ->
    gen_server:call(?MODULE, {node_stopped, Cid, Pid}).

exceptions(ClusterId) ->
    gen_server:call(?MODULE, {exceptions, ClusterId}).

%%
%% OTP gen_server API
%%

init([]) ->
    process_flag(trap_exit, true),
    CT = ets:new(cluster_table,   [ordered_set|?ETS_OPTS]),
    NT = ets:new(node_table,      [duplicate_bag|?ETS_OPTS]),
    OT = ets:new(exception_table, [duplicate_bag|?ETS_OPTS]),
    {ok, #state{cluster_table=CT, node_table=NT, exception_table=OT}}.

handle_call({force_stop, ClusterId}, _From,
            State=#state{cluster_table=CT}) ->
    case ets:lookup(CT, ClusterId) of
        [] ->
            {reply, {error, regname, ClusterId}, State};
        [{ClusterId, Pid}] ->
            systest_cluster:stop(Pid),
            {reply, ok, State}
    end;
handle_call({exceptions, ClusterId}, _From,
            State=#state{exception_table=ET}) ->
    {reply, ets:match(ET, {ClusterId, '_', '_'}), State};
handle_call({cluster_started, ClusterId, ClusterPid},
            _From, State=#state{cluster_table=CT}) ->
    case ets:insert_new(CT, {ClusterId, ClusterPid}) of
        true  -> %% NB: we link to cluster pids so that we
                 %% can be sure to know when and why they exit
                 link(ClusterPid),
                 {reply, ok, State};
        false -> {reply, {error, clash}, State}
    end;
handle_call({node_started, ClusterId, NodePid}, _From,
            State=#state{cluster_table=CT,
                         node_table=NT}) ->
    case ets:member(CT, ClusterId) of
        false ->
            {reply, {error, unknown_cluster}, State};
        true  ->
            %% NB: we do *not* link to nodes, as the cluster *should*
            %% in theory be responsible for cleaning up its own nodes
            case ets:insert_new(NT, {{ClusterId, NodePid}}) of
                false -> {reply, {error, duplicate_node}, State};
                true  -> {reply, ok, State}
            end
    end;
handle_call({node_stopped, ClusterId, NodePid}, _From,
            State=#state{node_table=NT}) ->
    ets:delete(NT, {ClusterId, NodePid}),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, normal},
            State=#state{cluster_table=CT, node_table=NT,
                         exception_table=ET}) ->
    case ets:match(CT, {'_', Pid}) of
        [Cluster] ->
            %% the cluster has exited normally - any nodes left
            %% alive are not supposed to be (they're orphans) and
            %% so we report these as errors
            Nodes = find_nodes(NT, Cluster),
            report_orphans(Cluster, Nodes, ET),

            %% else happens....
            handle_down(Cluster, NT);
        [] ->
            ok
    end,
    {noreply, State};
handle_info({'EXIT', Pid, Reason},
            State=#state{cluster_table=CT, node_table=NT,
                         exception_table=ET}) ->
    case ets:match(CT, {'_', Pid}) of
        [{ClusterId, _}=Cluster] ->
            %% the cluster has crashed (reason /= normal)
            ets:insert(ET, [{ClusterId, crashed, Reason}]),

            %% ensure any orphan nodes are cleaned up, but
            %% in this case we don't register them as 'errors'
            %% because the cluster crash is the more immediate cause
            handle_down(Cluster, NT);
        [] ->
            ok
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Private API
%%

report_orphans({ClusterId, _}, Nodes, ET) ->
    ct:pal("watchdog detected orphaned nodes of dead cluster ~p: ~p~n",
           [ClusterId, Nodes]),
    ets:insert(ET, [{ClusterId, orphan, N} || N <- Nodes]).

find_nodes(NodeTable, {ClusterId, _}) ->
    ets:match(NodeTable, {{ClusterId, '_'}}).

handle_down(Cluster, NodeTable) ->
    kill_wait(find_nodes(NodeTable, Cluster)).

kill_wait(Nodes) ->
    systest_cleaner:kill_wait(Nodes, fun systest_node:kill/1).

