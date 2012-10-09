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
-module(systest_proc_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/systest.hrl").
-compile(export_all).

%%
%% Config/Fixture Definitions
%%

all() ->
    systest_suite:export_all(?MODULE).

restarting_procs(Config) ->
    process_flag(trap_exit, true),
    Sut = systest:get_system_under_test(Config),
    [begin
         ?assertEqual(pong, net_adm:ping(Id)),
         {ok, {Id, Pid}} = systest:restart_process(Sut, Ref),
         ?assertEqual(true, erlang:is_process_alive(Pid)),
         ?assertEqual(pong, net_adm:ping(Id))
     end || {Id, Ref} <- systest:list_processes(Sut)],
    ok.

manually_starting_processes(Config) ->
    Sut = systest:get_system_under_test(Config),
    [{Id1, _Ref1}, {Id2, Ref2}] = systest:list_processes(Sut),
    %% process 1 has been started
    ?assertEqual(pong, net_adm:ping(Id1)),

    %% process 2 has not (yet), but we know what its node id *will*
    %% look like once it is activated manually (below)
    ?assertEqual(pang, net_adm:ping(Id2)),
    ?assertEqual(not_started, systest:process_activity_state(Ref2)),

    %% start up the backing module for process 2 (e.g., cli, slave, ssh, etc)
    ok = systest:activate_process(Ref2),
    ?assertEqual(pong, net_adm:ping(Id2)),
    ?assertEqual(running, systest:process_activity_state(Ref2)),
    ok.

restarting_manually_started_processes(Config) ->
    Sut = systest:get_system_under_test(Config),
    [{Id1, _Ref1}, {Id2, Ref2}] = systest:list_processes(Sut),
    %% process 1 has been started
    ?assertEqual(pong, net_adm:ping(Id1)),

    %% process 2 has not (yet), but we know what its node id *will*
    %% look like once it is activated manually (below)
    ?assertEqual(pang, net_adm:ping(Id2)),
    ?assertEqual(not_started, systest:process_activity_state(Ref2)),

    %% start up the backing module for process 2 (e.g., cli, slave, ssh, etc)
    ok = systest:activate_process(Ref2),
    ?assertEqual(pong, net_adm:ping(Id2)),
    ?assertEqual(running, systest:process_activity_state(Ref2)),

    ok = systest:stop_and_wait(Ref2),
    ?assertEqual(pang, net_adm:ping(Id2)),
    ok.
