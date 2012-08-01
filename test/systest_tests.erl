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

settings_merging_test_() ->
    [{setup, fun() ->
                 os:putenv("USER", "ci")
             end,
     ?_assertMatch([{authz_url, "https://localhost:30001/ssos/login"}],
                 systest_settings:load("../sample-config/default.settings"))}].

pre_loaded_config_test_() ->
    systest:start(),
    systest_config:start_link(),
    {ok, Terms} = file:consult("../sample-config/test.config"),
    systest_config:load_config_terms(resources, Terms),
    [?_assertMatch({handling_detached_processes,
                        [{localhost,[yellow,blue]},{on_start,[]}]},
                    systest_config:sut_config(systest_cli_SUITE,
                                              handling_detached_processes)),
     ?_assertMatch([{handler,systest_cli},
                    {link_to_parent,false},
                    {detached,true},
                    {rpc_enabled, {true, {init, stop, []}}}],
                   ?REQUIRE(startup, systest_proc:proc_config(
                                handling_detached_processes, yellow))),
     ?_assertMatch([
            {start, [{program, "${settings.base_dir}/resources/test/start"},
                     {args, ["${proc.id}"]},
                     {environment, [
                         {"LOGDIR", "%{TMPDIR}/logs/${proc.id}.log"},
                         {"DUMPDIR", "${ct.priv_dir}/dump/${proc.id}.log"},
                         {"PORT", "${proc.user.port}"}
                     ]}]},
            {stop,  [{program, "${settings.base_dir}/resources/test/stop"},
                     {args, ["${proc.id}"]}]}],
                   ?REQUIRE(flags, systest_proc:proc_config(
                                handling_detached_processes, yellow))),
     ?_assertMatch([], ?REQUIRE(user,
                systest_proc:proc_config(handling_detached_processes,
                                         yellow))),
     ?_assertMatch([{local, systest, write_os_pid, []}],
                                  ?REQUIRE(on_start,
                systest_proc:proc_config(handling_detached_processes,
                                         yellow))),
     ?_assertMatch([], ?REQUIRE(on_stop,
                systest_proc:proc_config(handling_detached_processes,
                                         yellow))),
     ?_assertMatch([], ?REQUIRE(on_join,
                systest_proc:proc_config(handling_detached_processes,
                                         yellow)))].

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
         GF = [{program,"resources/test/start"},
               {node,id},
               {environment,"LOGDIR"}],
         NF = [{program,"resources/test/start-daemon"}],
         ?_assertEqual([{program,"resources/test/start-daemon"},
                        {node,id},
                        {environment,"LOGDIR"}],
                        systest_config:merge(GF, NF))
     end].

%% config handling

overwrite_globals_with_local_value_test() ->
    ?assertEqual([{a, 2}, {b, 3}],
                 systest_config:merge([{a, 2}, {b, 1}],
                                             [{b, 3}])).

merge_locals_into_global_test() ->
    Globals = [{flags, [{start, [{program, "priv/start"},
                                 {node, id}, {environment, "LOGDIR"}]},
                        {stop,  ["priv/stop", {node, id}]}]}],
    Locals = [{flags, [{start, ["--dumpfile=red.dump"]}]}],
    ?assertMatch([{flags, [{start, ["--dumpfile=red.dump",
                                    {program, "priv/start"},
                                    {node, id}, {environment, "LOGDIR"}]}|_]}],
                 systest_config:merge(Globals, Locals)).

path_merge_test() ->
    ?assertEqual([{file,"tmp/mnesia/running.pid"},
                  {dir,"tmp-db"},
                  {priv_dir,"tmp"}],
        systest_config:merge(
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

