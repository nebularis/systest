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

starting_and_stopping_procs(Config) ->
    Sut = systest:active_sut(Config),
    systest_sut:log_status(Sut),
    [begin
         ?assertEqual(up,   systest_proc:status(Ref)),
         ?assertEqual(pong, net_adm:ping(Id)),

         ok = systest:stop_and_wait(Ref),
         ?assertEqual(pang, net_adm:ping(Id))
     end || {Id, Ref} <- systest:procs(Sut)],
    ok.

killing_procs(Config) ->
    Sut = systest:active_sut(Config),
    systest_sut:log_status(Sut),
    [begin
         ?assertEqual(up, systest_proc:status(Ref)),
         ok = systest:kill_and_wait(Ref),

         %% NB: the following statement are actually required in this test, as
         %% without them, we get 'pong' back even though the node in question
         %% has physically stopped (because the port that launched it exited)
         rpc:call(Id, io, format, ["~p~n", [nodes()]]),
         timer:sleep(1000),
         ?assertEqual(pang, net_adm:ping(Id))
     end || {Id, Ref} <- systest:procs(Sut)],
    ok.

sigkill_on_procs(Config) ->
    Sut = systest:active_sut(Config),
    systest_sut:log_status(Sut),
    [begin
         ?assertEqual(up, systest_proc:status(Ref)),
         ok = systest_proc:shutdown_and_wait(Ref,
                                             fun systest_proc:'kill -9'/1),
         ?assertEqual(false, erlang:is_process_alive(Ref))
     end || {_, Ref} <- systest:procs(Sut)],
    ok.

handling_attached_processes_with_exec(Config) ->
    Sut = systest:active_sut(Config),
    systest_sut:log_status(Sut),
    [begin
         ?assertEqual(up, systest_proc:status(Ref)),
         ok = systest_proc:kill_and_wait(Ref),

         ?assertEqual(pang, net_adm:ping(Id))
     end || {Id, Ref} <- systest:procs(Sut)],
    ok.

generate_exit_on_eof_wrapper(Config) ->
    Sut = systest:get_system_under_test(Config),
    [{Id, Ref}|_] = systest:list_processes(Sut),
    systest_utils:remote_load(Id, ?MODULE),
    ?assertMatch({module, ?MODULE},
                 rpc:call(Id, code, ensure_loaded,
                          [?MODULE])),
    Pid = rpc:call(Id, ?MODULE, write_to_sout, []),
    ?assertEqual(true, rpc:call(Id, erlang,
                                is_process_alive, [Pid])),
    systest:kill_and_wait(Ref),
    ?assertEqual(pang, net_adm:ping(Id)),
    ok.

handling_normal_exit_status(Config) ->
    Sut = systest:get_system_under_test(Config),
    ?assertEqual(0, length(systest:list_processes(Sut))),
    {ok, Hostname} = inet:gethostname(),
    {ok, Ref} = systest_proc:start(start_explicit, n1,
                              [{scope, handling_normal_exit_status},
                               {name, n1},
                               {host, list_to_atom(Hostname)}|Config]),

    %% oh man, we need an API for reading things like 'start.environment'!!!
    PrivDir = ?config(priv_dir, Config),
    Scope = "handling_normal_exit_status",
    PipeDir = filename:join([PrivDir, Scope, ".PIPEDIR"]),

    filelib:ensure_dir(filename:join(PipeDir, "foo")),
    ?assertEqual(true, filelib:is_dir(PipeDir)),

    process_flag(trap_exit, true),

    %% sends 'quit' to the pipe - we ignore the stop
    catch systest:stop_no_wait(Ref),
    receive
        {'EXIT', Ref, normal} -> ok;
        Other                 -> throw({unexpected_exit, Other})
    end.

handling_non_zero_exit_status(Config) ->
    Sut = systest:get_system_under_test(Config),
    ?assertEqual(0, length(systest:list_processes(Sut))),
    {ok, Hostname} = inet:gethostname(),
    {ok, Ref} = systest_proc:start(start_explicit, n2,
                              [{scope, handling_non_zero_exit_status},
                               {name, n2},
                               {host, list_to_atom(Hostname)}|Config]),

    %% oh man, we need an API for reading things like 'start.environment'!!!
    PrivDir = ?config(priv_dir, Config),
    Scope = "handling_non_zero_exit_status",
    PipeDir = filename:join([PrivDir, Scope, ".PIPEDIR"]),

    filelib:ensure_dir(filename:join(PipeDir, "foo")),
    ?assertEqual(true, filelib:is_dir(PipeDir)),

    process_flag(trap_exit, true),
    %% sends 'bang' to the pipe - the script runs exit ( 1 )
    systest:stop_no_wait(Ref),
    receive
        {'EXIT', Ref, {exit_status, 1}} -> ok;
        Other                           -> throw({unexpected_exit, Other})
    end.

handling_detached_processes(Config) ->
    Sut = systest:active_sut(Config),
    systest_sut:log_status(Sut),
    [begin
         ?assertEqual(up, systest_proc:status(Ref)),
         ok = systest_proc:kill_and_wait(Ref),

         ?assertEqual(pang, net_adm:ping(Id))
     end || {Id, Ref} <- systest:procs(Sut)],
    ok.

write_to_sout() ->
    spawn(fun() -> write_sout() end).

write_sout() ->
    io:format(user, "sdtout here I am!~n", []),
    write_sout().
