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
        [{read_concurrency, true},
         {write_concurrency, false}]).

-record(state, {cluster_table, node_table, exception_table}).

-behaviour(gen_server).

%% API Exports

-export([start/0, cluster_started/2, exceptions/1, reset/0,
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

reset() ->
    gen_server:call(?MODULE, reset).

force_stop(Id) ->
    case gen_server:call(?MODULE, {force_stop, Id}) of
        {error, regname, Id} ->
            ct:pal("ignoring stop for deceased cluster ~p~n", [Id]);
        Ok ->
            Ok
    end.

node_started(Cid, Pid) ->
    %% NB: we do *not* link to nodes, as the cluster *should*
    %% in theory be responsible for cleaning up its own nodes
    %%
    %% NB: if we blocked on accessing the node_table, we would
    %% very likely deadlock the system - just when automated
    %% cleanup is meant to be leaving a clean environment behind
    case ets:insert_new(node_table, {{Cid, Pid}}) of
        false -> {error, duplicate_node};
        true  -> ok
    end.

node_stopped(Cid, Pid) ->
    %% NB: if we blocked on accessing the node_table, we would
    %% very likely deadlock the system - just when automated
    %% cleanup is meant to be leaving a clean environment behind
    ets:delete(node_table, {Cid, Pid}).

exceptions(ClusterId) ->
    gen_server:call(?MODULE, {exceptions, ClusterId}).

%%
%% OTP gen_server API
%%

init([]) ->
    process_flag(trap_exit, true),
    CT = ets:new(cluster_table,   [ordered_set, private|?ETS_OPTS]),
    NT = ets:new(node_table,      [ordered_set, named_table,
                                   public|?ETS_OPTS]),
    OT = ets:new(exception_table, [duplicate_bag, private|?ETS_OPTS]),
    {ok, #state{cluster_table=CT, node_table=NT, exception_table=OT}}.

handle_call(reset, _From, State=#state{cluster_table=CT,
                                       node_table=NT,
                                       exception_table=ET}) ->
    CPids = [CPid || {_, CPid} <- ets:tab2list(CT)],
    systest_cleaner:kill_wait(CPids, fun(P) -> erlang:exit(P, reset) end),
    [ets:delete_all_objects(T) || T <- [ET, NT, CT]],
    {reply, ok, State};
handle_call({force_stop, ClusterId}, _From,
            State=#state{cluster_table=CT}) ->
    case ets:lookup(CT, ClusterId) of
        [] ->
            ct:pal("~p not found in ~p~n", [ClusterId, ets:tab2list(CT)]),
            {reply, {error, regname, ClusterId}, State};
        [{ClusterId, Pid}] ->
            systest_cluster:stop(Pid),
            ct:pal("force stop complete~n"),
            {reply, ok, State}
    end;
handle_call({exceptions, ClusterId}, _From,
            State=#state{exception_table=ET}) ->
    {reply, ets:match_object(ET, {ClusterId, '_', '_'}), State};
handle_call({cluster_started, ClusterId, ClusterPid},
            _From, State=#state{cluster_table=CT}) ->
    ct:pal("cluster ~p has started...~n", [ClusterId]),
    case ets:insert_new(CT, {ClusterId, ClusterPid}) of
        true  -> %% NB: we link to cluster pids so that we
                 %% can be sure to know when and why they exit
                 link(ClusterPid),
                 {reply, ok, State};
        false -> {reply, {error, clash}, State}
    end;
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason},
            State=#state{cluster_table=CT, node_table=NT,
                         exception_table=ET}) ->
    case ets:match_object(CT, {'_', Pid}) of
        [{ClusterId, _}=Cluster] ->
            ct:pal("watchdog handling cluster (process) down"
                   " event for ~p~n", [ClusterId]),

            case Reason of
                normal ->
                    %% the cluster has exited normally - any nodes left
                    %% alive are not supposed to be (they're orphans) and
                    %% so we report these as errors
                    Nodes = find_nodes(NT, Cluster),
                    report_orphans(Cluster, Nodes, ET);
                noconfig ->
                    ok;
                _Other ->
                    %% the cluster has crashed (reason /= normal)
                    ets:insert(ET, [{ClusterId, crashed, Reason}])
            end,

            handle_down(Cluster, NT),
            ets:delete(CT, ClusterId);
        [[]] ->
            ct:pal("cluster down even unhandled: no id for "
                   "~p in ~p~n", [Pid, ets:tab2list(CT)]),
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

report_orphans(_, [], _) ->
    ok;
report_orphans({ClusterId, _}, Nodes, ET) ->
    ct:pal("watchdog detected orphaned nodes of dead cluster ~p: ~p~n",
           [ClusterId, Nodes]),
    ets:insert(ET, [{ClusterId, orphan, N} || N <- Nodes]).

find_nodes(NodeTable, {ClusterId, _}) ->
    [P || {{_, P}} <- ets:match_object(NodeTable, {{ClusterId, '_'}})].

handle_down(Cluster, NodeTable) ->
    kill_wait(find_nodes(NodeTable, Cluster)).

kill_wait([]) ->
    ct:pal("no nodes to kill~n");
kill_wait(Nodes) ->
    systest_cleaner:kill_wait(Nodes, fun systest_node:kill/1).

