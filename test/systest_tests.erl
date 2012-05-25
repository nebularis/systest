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
-module(systest_tests).
-include_lib("eunit/include/eunit.hrl").

-include("systest.hrl").

%% cli flags

cli_flags_test_() ->
    [begin
        Node = #'systest.node_info'{id=cli},
        Flags = [{start, ["priv/start",
                          {node, id},
                          {environment, "LOGDIR"}]}],
        Config = [{"logdir", "/tmp/logs"}],
        ?_assertEqual({[{"LOGDIR", "/tmp/logs"}], ["priv/start", "cli"]},
                      systest_cli:convert_flags(start, Node, Flags, Config))
     end].

%% config handling

overwrite_globals_with_local_value_test() ->
    ?assertEqual([{a, 2}, {b, 3}],
                 systest_config:merge_config([{a, 2}, {b, 1}],
                                             [{b, 3}])).

merge_locals_into_global_test() ->
    Globals = [{flags, [{start, ["priv/start",
                                 {node, id}, {environment, "LOGDIR"}]},
                        {stop,  ["priv/stop", {node, id}]}]}],
    Locals = [{flags, [{start, [{"--dumpfile", "red.dump"}]}]}],
    ?assertMatch([{flags, [{start, [{"--dumpfile", "red.dump"},
                                    "priv/start",
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



