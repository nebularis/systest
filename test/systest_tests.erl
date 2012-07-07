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
-module(systest_tests).
-include_lib("eunit/include/eunit.hrl").

-include("systest.hrl").

-record(node, {id, name, ip_addr, port}).

%% cli flags

parameterised_eval_test_() ->
    Config = test_config(),
    [
        ?_assertEqual("tag", systest_config:eval("node.tag", Config)),
        ?_assertMatch(systest_cli,
                      systest_config:eval("node.startup.handler", Config)),
        ?_assertMatch(true,
                      systest_config:eval("node.startup.link_to_parent",
                                          Config)),
        ?_assertMatch({rpc_enabled, {true, {init, stop, []}}},
                      systest_config:eval("node.startup.rpc_enabled",
                                          Config, [{return, key}])),
        ?_assertMatch({"node.on_start.local", {local, test_utils, wait, []}},
                      systest_config:eval("node.on_start.local",
                                          Config, [{return, path}])),
        {setup, fun() ->
                    systest:start(),
                    systest_config:set_env("BASE_DIR", "/var/tmp/foobar")
                end,
         ?_assertMatch("/var/tmp/foobar/scripts/start-hopper",
                       systest_config:eval("node.flags.start.program",
                                           Config, [{callback, {node_info,
                                                    fun get_node_info/2}}]))},
        {setup, fun() ->
                    systest:start(),
                    systest_config:set_env("BASE_DIR", "/var/tmp/foobar")
                end,
         begin
            Cfg = systest_config:eval("node.flags.start.environment",
                                      Config, [{callback, {node_info,
                                                    fun get_node_info/2}}]),
            % ?debugFmt("Cfg = ~p~n", [Cfg]),
            ?_assertMatch({"LOG_BASE", "/tmp/priv"},
                          lists:keyfind("LOG_BASE", 1, Cfg))
         end},
         ?_assertMatch({"PLUGINS_DIR", "/tmp/priv/hopper-plugins"},
                      lists:keyfind("PLUGINS_DIR", 1,
                            systest_config:eval("node.flags.start.environment",
                                                Config,
                                                [{callback, {node_info,
                                                 fun get_node_info/2}}])))
    ].

cli_flags_test_() ->
    [begin
         Node = #proc{id=cli},
         Flags = [{start, [{program, "priv/start"},
                           {proc, id},
                           {environment, "LOGDIR"}]}],
         Config = [{"logdir", "/tmp/logs"}],
         {setup, fun() ->
                     systest:start(),
                     systest_config:set_env("LOGDIR", "/tmp/logs")
                 end,
            ?_assertEqual({[{"LOGDIR", "/tmp/logs"}],["cli"],"priv/start"},
                           systest_cli:convert_flags(start, Node,
                                                     Flags, Config))}
     end,
     begin
         GF = [{program,"resources/test/start"},
               {node,id},
               {environment,"LOGDIR"}],
         NF = [{program,"resources/test/start-daemon"}],
         ?_assertEqual([{program,"resources/test/start-daemon"},
                        {node,id},
                        {environment,"LOGDIR"}],
                        systest_config:merge_config(GF, NF))
     end].

%% config handling

overwrite_globals_with_local_value_test() ->
    ?assertEqual([{a, 2}, {b, 3}],
                 systest_config:merge_config([{a, 2}, {b, 1}],
                                             [{b, 3}])).

merge_locals_into_global_test() ->
    Globals = [{flags, [{start, [{program, "priv/start"},
                                 {node, id}, {environment, "LOGDIR"}]},
                        {stop,  ["priv/stop", {node, id}]}]}],
    Locals = [{flags, [{start, ["--dumpfile=red.dump"]}]}],
    ?assertMatch([{flags, [{start, ["--dumpfile=red.dump",
                                    {program, "priv/start"},
                                    {node, id}, {environment, "LOGDIR"}]}|_]}],
                 systest_config:merge_config(Globals, Locals)).

path_merge_test() ->
    ?assertEqual([{file,"tmp/mnesia/running.pid"},
                  {dir,"tmp-db"},
                  {priv_dir,"tmp"}],
        systest_config:merge_config(
                [{priv_dir, "tmp"}],
                [{dir, [scratch, "-db"]},
                 {file, {path, [scratch, "mnesia", "running.pid"]}}])).

%%
%% Utility Functions
%%

test_config() ->
    [
    {ct, [{priv_dir, "/tmp/priv"}]},
    {node, [
        {tag, "tag"},
        {startup, [
            {handler, systest_cli},
            {link_to_parent, true},
            {detached, false},
            {rpc_enabled, {true, {init, stop, []}}}
        ]},
        {flags, [
            {start, [
                {program, "%{BASE_DIR}/scripts/start-${node_info.id}"},
                {environment, [
                    {"LOG_BASE",    "${ct.priv_dir}"},
                    {"PLUGINS_DIR", "${ct.priv_dir}/${node_info.id}-plugins"},
                    {"IP_ADDRESS",  "${node_info.ip_addr}"},
                    {"PORT",        "${node_info.port}"},
                    {"NODENAME",    "${node_info.name}"},
                    {"DB_DIR",      "${ct.priv_dir}/${node_info.name}-db"},
                    {"PID_FILE", "${ct.priv_dir}/${node_info.name}-server.pid"}
                ]}
            ]}
        ]},
        {on_start, [
            {local, test_utils, wait, []}
        ]}
    ]}, {node_info, dummy_node()}].

get_node_info(id, #node{id=Id})           -> Id;
get_node_info(name, #node{name=Name})     -> Name;
get_node_info(ip_addr, #node{ip_addr=Ip}) -> Ip;
get_node_info(port, #node{port=Port})     -> Port;
get_node_info(K, N) -> throw({wtf, K, N}).

dummy_node() ->
    #node{id=hopper, name='hopper@bunny',
          ip_addr="0.0.0.0", port=5642}.

