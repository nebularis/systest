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
-export([status/1, print_status/1]).

-include("systest.hrl").

%%
%% Public API
%%

start(Config) ->
    start(global, Config).

start(ClusterId, Config) ->
    ct:pal("Processing cluster info for ~p~n", [ClusterId]),
    case systest:cluster_config(ClusterId) of
        undefined ->
            Config;
        Hosts ->
            ct:pal("Configured hosts: ~p~n", [Hosts]),
            Nodes = lists:flatten(
                    [start_host(ClusterId, Host, Config) || Host <- Hosts]),
            ct:pal("Configured nodes: ~p~n",
                   [[N#'systest.node_info'.id || N <- Nodes]]),
            Cluster = #'systest.cluster'{name=ClusterId, nodes=Nodes},
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

%%
%% Internal API
%%

print_status_info({#'systest.node_info'{host=Host,
                                        id=Name,
                                        handler=Type,
                                        vmflags=Args,
                                        apps=Apps,
                                        os_pid=Proc,
                                        extra=Xtra,
                                        owner=Port}, Status}) ->
     io_lib:format("Node Info~n"
                   "         name: ~p (status=~p)~n"
                   "         host: ~p~n"
                   "         type: ~p~n"
                   "         args: ~s~n"
                   "         apps: ~p~n"
                   "         proc: ~p~n"
                   "         xtra: ~p~n"
                   "         port: ~p~n"
                   "----------------------------------------------------~n",
                   [Host, Status, Name, Type, Args, Apps, Proc, Xtra, Port]).

start_host(Cluster, {Host, Nodes}, Config) when is_atom(Host) andalso
                                                is_list(Nodes) ->
    verify_host(Host),
    [start_node(Host, Cluster, Node, Config) || Node <- Nodes].

start_node(localhost, Cluster, Node, Config) ->
    {ok, Hostname} = inet:gethostname(),
    start_node(list_to_atom(Hostname), Cluster, Node, Config);
start_node(Host, Cluster, Node, Config) ->
    BasicConfig = [{host, Host},
                   {name, Node}],
    Globals = systest:global_config(Cluster, Node),
    AppConfig = systest:static_config(Cluster, Node),
    RuntimeConfig = systest:runtime_config(Cluster, Node, Config),
    Config2 = systest:merge_config(AppConfig, RuntimeConfig),
    %% TODO: if we *die* performing the start, who rolls back pre-existing
    %% nodes we've already set up!???
    {ok, NI} = systest_node:start(BasicConfig ++ Globals ++ Config2),
    NI.

verify_host(Host) ->
    case systest_net:is_epmd_contactable(Host, 5000) of
        true ->
            ok;
        {false, Reason} ->
            ct:pal("Unable to contact ~p: ~p~n", [Host, Reason]),
            ct:fail("Cluster configuration failed!~n", [])
    end.

