%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com).
%%
%% Some portions of the code taken from sh (c) 2005 - 2012 Nebularis
%% Some portions of the code taken from rebar (c) 2010 Dave Smith
%% Some portions of the code taken from retest (c) 2010 Dave Smith
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
-module(systest_cli_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/systest.hrl").
-compile(export_all).

suite() -> [{timetrap, {seconds, 60}}].

all() ->
    systest_suite:export_all(?MODULE).

local_and_global_scope_configuration_handling(Config) ->
    Scope = systest_cli_config_example,
    Cfg = systest_sut:check_config(Scope, Config),
    ok.

starting_and_stopping_procs(Config) ->
    process_flag(trap_exit, true),
    Sut = systest:active_sut(Config),
    systest_sut:print_status(Sut),
    [begin
         ?assertEqual(up,   systest_proc:status(Ref)),
         ?assertEqual(pong, net_adm:ping(Id)),

         ok = systest_proc:stop_and_wait(Ref),
         ?assertEqual(pang, net_adm:ping(Id))
     end || {Id, Ref} <- systest:procs(Sut)],
    ok.

killing_procs(Config) ->
    process_flag(trap_exit, true),
    Sut = systest:active_sut(Config),
    systest_sut:print_status(Sut),
    [begin
         ?assertEqual(up, systest_proc:status(Ref)),
         ok = systest_proc:kill_and_wait(Ref),

         ?assertEqual(pang, net_adm:ping(Id))
     end || {Id, Ref} <- systest:procs(Sut)],
    ok.

sigkill_on_procs(Config) ->
    process_flag(trap_exit, true),
    Sut = systest:active_sut(Config),
    systest_sut:print_status(Sut),
    [begin
         ?assertEqual(up, systest_proc:status(Ref)),
         ok = systest_proc:shutdown_and_wait(Ref,
                                             fun systest_proc:'kill -9'/1),
         ?assertEqual(false, erlang:is_process_alive(Ref))
     end || {_, Ref} <- systest:procs(Sut)],
    ok.

handling_detached_processes(Config) ->
    process_flag(trap_exit, true),
    Sut = systest:active_sut(Config),
    systest_sut:print_status(Sut),
    [begin
         ?assertEqual(up, systest_proc:status(Ref)),
         ok = systest_proc:kill_and_wait(Ref),

         ?assertEqual(pang, net_adm:ping(Id))
     end || {Id, Ref} <- systest:procs(Sut)],
    ok.
