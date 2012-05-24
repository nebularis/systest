%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
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
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(systest_cluster).

-export([start/1, start/2, stop/2]).
-export([check_config/2, status/1, print_status/1]).

-include("systest.hrl").

%%
%% Public API
%%

start(Config) ->
    start(global, Config).

start(ClusterId, Config) ->
    ct:pal("Processing cluster info for ~p~n", [ClusterId]),
    case with_cluster(ClusterId, fun start_host/3, Config) of
        noconfig ->
            Config;
        #'systest.cluster'{}=Cluster ->
            Config2 = systest_config:ensure_value(?MODULE, Cluster, Config),
            systest_config:replace_value(active, Cluster, Config2)
    end.

stop(ClusterId, Config) ->
    case lists:keyfind(?MODULE, 1, Config) of
        false ->
            ok;
        {_, Clusters} when is_list(Clusters) ->
            case lists:keyfind(ClusterId, 2, Clusters) of
                false ->
                    ok;
                #'systest.cluster'{nodes=Nodes} ->
                    %% TODO: kill order
                    %% TODO: additional shutdown hooks?
                    [systest_node:stop(NI) || NI <- Nodes]
            end;
        {_, #'systest.cluster'{nodes=Nodes}} ->
            %% TODO: kill order
            %% TODO: additional shutdown hooks?
            [systest_node:stop(NI) || NI <- Nodes]
    end.

status(#'systest.cluster'{nodes=Nodes}) ->
    [{N, systest_node:status(N)} || N <- Nodes].

print_status(Cluster) ->
    ct:pal(lists:flatten([print_status_info(N) || N <- status(Cluster)])).

check_config(Cluster, Config) ->
    with_cluster(Cluster, fun build_nodes/3, Config).

%%
%% Internal API
%%

with_cluster(ClusterId, NodeHandler, Config) ->
    case systest:cluster_config(ClusterId) of
        undefined -> noconfig;
        Hosts -> 
            ct:pal("Configured hosts: ~p~n", [Hosts]),
            Nodes = lists:flatten(
                     [NodeHandler(ClusterId, Host, Config) || Host <- Hosts]),
            ct:pal("Configured nodes: ~p~n",
                    [[N#'systest.node_info'.name || N <- Nodes]]),
            #'systest.cluster'{name=ClusterId, nodes=Nodes}
    end.

print_status_info({#'systest.node_info'{host=Host,
                                        id=Name,
                                        handler=Type,
                                        flags=Flags,
                                        apps=Apps,
                                        os_pid=Proc,
                                        extra=Xtra,
                                        owner=Port}, Status}) ->
     io_lib:format("----------------------------------------------------~n"
                   "Node Info~n"
                   "         name:  ~p (status=~p)~n"
                   "         host:  ~p~n"
                   "         type:  ~p~n"
                   "         args:  ~p~n"
                   "         apps:  ~p~n"
                   "         proc:  ~p~n"
                   "         xtra:  ~p~n"
                   "         port:  ~p~n"
                   "----------------------------------------------------~n",
                   [Host, Status, Name, Type, Flags, Apps, Proc, Xtra, Port]).

build_nodes(Cluster, {Host, Nodes}, Config) ->
    [systest_node:make_node(Cluster, N, [{host, Host},
                                         {name, N}|Config]) || N <- Nodes].

start_host(Cluster, {localhost, Nodes}=HostConf, Config) ->
    {ok, Hostname} = inet:gethostname(),
    start_host(Cluster, {Hostname, Nodes}=HostConf, Config);
start_host(Cluster, {Host, Nodes}=HostConf, Config) when is_atom(Host) andalso
                                                         is_list(Nodes) ->
    case ?CONFIG(verify_hosts, Config) of
        true  -> verify_host(Host);
        false -> ok
    end,
    [start_node(Node) || Node <- build_nodes(Cluster, HostConf, Config)].

start_node(Node) ->
    {ok, NI} = systest_node:start(Node),
    NI.

verify_host(Host) ->
    case systest_net:is_epmd_contactable(Host, 5000) of
        true ->
            ok;
        {false, Reason} ->
            ct:pal("Unable to contact ~p: ~p~n", [Host, Reason]),
            ct:fail("Cluster configuration failed!~n", [])
    end.
