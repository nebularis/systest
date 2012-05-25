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
-module(systest_cli_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/systest.hrl").
-compile(export_all).

suite() -> [{timetrap, {seconds, 20}}].

all() ->
    systest_suite:export_all(?MODULE).

init_per_testcase(TestCase, Config) ->
    systest:start(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    systest:stop(TestCase, Config).

local_and_global_scope_configuration_handling(Config) ->
    Scope = systest_cli_config_example,
    CheckConf = systest_cluster:check_config(Scope, Config),
    ct:pal("~p~n", [CheckConf]),
    ok.

starting_and_stopping_nodes(Config) ->
    Cluster = systest_cluster:start(systest_cli_config_example, Config),
    systest_cluster:print_status(Cluster),
    [begin
         Node = N#'systest.node_info'.id,
         ?assertEqual(nodeup, systest_node:status(N)),
         ?assertEqual(pong, net_adm:ping(Node)),
         
         ok = systest_node:stop(N),
         ct:pal("~p status: ~p~n", [Node, systest_node:status(N)]),
         ?assertEqual(pang, net_adm:ping(Node))
     end || N <- systest:cluster_nodes(Cluster)],
    ok.
