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
-module(systest).

-include("systest.hrl").

-export([start/0]).
-export([start_suite/2, stop_suite/2, start/2, stop/2]).
-export([active_cluster/1, clusters/1, cluster_nodes/1]).
-export([cluster_config/1]).
-export([interact/2, write_pid_file/1, write_pid_file/2]).

%%
%% Public APIs
%%

%% application startup

start() ->
    application:start(?MODULE).

%% startup/shutdown

start_suite(Suite, Config) ->
    start(strip_suite_suffix(Suite), Config).

stop_suite(Suite, Config) ->
    stop(strip_suite_suffix(Suite), Config).

start(Scope, Config) ->
    systest_cluster:start(Scope, Config).

stop(Scope, Config) ->
    systest_cluster:stop(Scope, Config).

%% interactions

interact(Node, Inputs) ->
    systest_node:interact(Node, Inputs).

write_pid_file(Dir) ->
    Pid = os:getpid(),
    write_pid_file(Pid ++ ".pid", Dir).

write_pid_file(Name, {dir, Dir}) ->
    Pid = os:getpid(),
    File = filename:join(Dir, Name),
    file:write_file(File, Pid, [write]).

%% config handling

active_cluster(Config) ->
    systest_config:read(active, Config).

clusters(Config) ->
    systest_config:read(?MODULE, Config).

cluster_nodes(#'systest.cluster'{nodes=Nodes}) ->
    Nodes.

cluster_config(Scope) ->
    ct:get_config({Scope, cluster}).

%%
%% Private API
%%

%global_config(Scope, Node#'systest.node_info'{handler=systest_slave}) ->
%    VmFlags = systest_config:get_config(Scope, Node, flags),
%    SysTestPath = filename:absname(filename:dirname(code:which(systest))),
%    [{flags, "-pa " ++ SysTestPath ++ " " ++ VmFlags}];

strip_suite_suffix(Suite) ->
    S = atom_to_list(Suite),
    list_to_atom(re:replace(S, "_SUITE", "", [{return, list}])).
