%% -------------------------------------------------------------------
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
-module(systest_ct).

-behaviour(systest_runner).

-export([dryrun/1, run/1]).

dryrun(RunSpec) ->
    run(RunSpec, true).

run(RunSpec) ->
    run(RunSpec, false).

run(RunSpec, DryRun) ->
    Profile = systest_runner:get(profile, RunSpec),
    Label   = systest_profile:get(name, Profile),
    Specs   = systest_profile:get(specifications, Profile),
    LogDir  = systest_profile:get(log_dir, Profile),
    Timeout = systest_profile:get(default_timetrap, Profile),
    Targets = systest_profile:get(targets, Profile),

    ok = systest_log:start(system, systest_ct_log, common_test),
    ok = systest_log:start(framework, systest_ct_log, common_test),

    TestFun = if DryRun =:= true -> fun ct:run_test/1;
                            true -> fun print_test_data/1
              end,

    case TestFun([{'spec', Specs},
                  {logdir, LogDir},
                  {label, Label},
                  {auto_compile, false},
                  {allow_user_terms, true},
                  {event_handler, systest_event},
                  {enable_builtin_hooks, true},
                  {timetrap, Timeout},
                  {ct_hooks, [cth_log_redirect,
                              {systest_cth, [], 100000}]}|Targets]) of
        {error, _}=Error ->
            Error;
        _Other ->
            case application:get_env(systest, failures) of
                {ok, 0}      -> ok;
                {ok, Failed} -> {error, {failures, Failed}}
            end
    end.

print_test_data(Config) ->
    io:format("Dry Run:~n"
              "Framework: Common Test~n"
              "Handler: ~p~n"
              "Test Run Configuration:~n~s~ndone.~n",
              [?MODULE, systest_utils:proplist_format(Config)]).
