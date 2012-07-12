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
%% @hidden
%% ----------------------------------------------------------------------------
-module(systest_ct).

-behaviour(systest_runner).

-export([dryrun/1, run/1]).

dryrun(RunSpec) ->
    run(RunSpec, true).

run(RunSpec) ->
    run(RunSpec, false).

run(RunSpec, DryRun) ->
    Profile = systest_runner:get(profile, RunSpec),
    Targets = systest_runner:get(targets, RunSpec),
    Label   = systest_profile:get(name, Profile),
    LogDir  = systest_profile:get(log_dir, Profile),
    Hooks   = systest_profile:get(hooks, Profile),

    ok = systest_log:start(system, systest_ct_log, common_test),
    ok = systest_log:start(framework, systest_ct_log, common_test),

    TestFun = if DryRun =:= false -> fun run_test/1;
                            true  -> fun print_test/1
              end,

    HooksEntry = case Hooks of
                     [] ->
                         {ct_hooks, [cth_log_redirect,
                                     {systest_cth, [], 100000}]};
                     _ ->
                         [begin
                             M = case Hook of
                                     Mod when is_atom(Mod) ->
                                         Mod;
                                     {ModName, _, _} ->
                                         ModName
                                 end,
                             code:ensure_loaded(M)
                          end || Hook <- Hooks],
                         {ct_hooks, Hooks}
                 end,

    case TestFun([{logdir, LogDir},
                  {label, Label},
                  {auto_compile, false},
                  {allow_user_terms, true},
                  {event_handler, systest_event},
                  {enable_builtin_hooks, true},
                  HooksEntry|Targets]) of
        {error, _}=Error ->
            Error;
        _Other ->
            case application:get_env(systest, failures) of
                undefined    -> ok;
                {ok, 0}      -> ok;
                {ok, Failed} -> {error, {failures, Failed}}
            end
    end.

run_test(Cfg) ->
    print_test_data(Cfg, "Starting Test Run"),
    ct:run_test(Cfg).

print_test(Cfg) ->
    print_test_data(Cfg, "Starting Dry Run"),
    io:format("done.~n").

print_test_data(Config, Border) ->
    io:format("~s~n"
              "~s~n",
              [systest_utils:border(Border, "-"),
               systest_utils:proplist_format([{"framework", "Common Test"},
                                              {"handler", ?MODULE}|Config])]).
