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

-include("systest.hrl").
-define(PRIORITY, 100000).

dryrun(RunSpec) ->
    run(RunSpec, true).

run(RunSpec) ->
    run(RunSpec, false).

run(RunSpec, DryRun) ->
    Quiet   = systest_runner:get(quiet, RunSpec),
    Profile = systest_runner:get(profile, RunSpec),
    Targets = systest_runner:get(targets, RunSpec),
    Label   = systest_profile:get(name, Profile),
    LogDir  = systest_profile:get(log_dir, Profile),
    Hooks   = systest_profile:get(hooks, Profile),

    systest_ct_log:start(),

    TestFun = if DryRun =:= false -> fun run_test/2;
                            true  -> fun print_test/2
              end,

    HooksEntry = case Hooks of
                     [] ->
                         {ct_hooks, install_cth(Profile, [cth_log_redirect])};
                     _  ->
                         [begin
                             M = case Hook of
                                     Mod when is_atom(Mod) -> Mod;
                                     {ModName, _, _}       -> ModName
                                 end,
                             code:ensure_loaded(M)
                          end || Hook <- Hooks],
                         Hooks3 = case find_hook(Hooks) of
                                      {true, Hook, Hooks2} ->
                                          update_cth(Hook, Hooks2, Profile);
                                      false ->
                                          install_cth(Profile, Hooks)
                                  end,
                         {ct_hooks, Hooks3}
                 end,

    case TestFun([{logdir, LogDir},
                  {label, Label},
                  {auto_compile, false},
                  {allow_user_terms, true},
                  {event_handler, systest_event},
                  {enable_builtin_hooks, true},
                  HooksEntry|Targets], Quiet) of
        {error, _}=Error ->
            Error;
        _Other ->
            try
                Config = systest_runner:get(base_config, RunSpec),
                #results{failed=Failed, skipped=Skipped} =
                    systest_results:reconciled_test_results(),
                if Failed > 0 -> {error, {failed, Failed}};
                         true -> check_skip_ok(Skipped, Config)
                end
            after
                application:set_env(systest, failures, undefined)
            end
    end.

install_cth(Profile, Hooks) ->
    case lists:member(do_not_install, Hooks) of
        false -> update_cth(systest_cth, Hooks, Profile);
        true  -> lists:delete(do_not_install, Hooks)
    end.

update_cth(systest_cth, Hooks, Profile) ->
    TeardownTimetrap = systest_profile:get(teardown_timetrap, Profile),
    AggressiveTeardown = systest_profile:get(aggressive_teardown, Profile),
    [{systest_cth,
      [{teardown_timetrap, TeardownTimetrap},
       {aggressive_teardown, AggressiveTeardown}], ?PRIORITY}|Hooks];
update_cth({systest_cth, Opts, Priority}, Hooks, Profile) ->
    Opts2 = lists:foldl(
              fun(Opt, Acc) ->
                 case lists:keymember(Opt, 1, Acc) of
                     true  -> Acc;
                     false -> [{Opt, systest_profile:get(Opt, Profile)}|Acc]
                 end
              end, Opts, [teardown_timetrap, aggressive_teardown]),
    [{systest_cth, Opts2, Priority}|Hooks].

find_hook(Hooks) ->
    case lists:member(systest_cth, Hooks) of
        true ->
            {true, systest_cth, lists:delete(systest_cth, Hooks)};
        false ->
            case lists:keytake(systest_cth, 1, Hooks) of
                false ->
                    false;
                {value, Hook, Rest} ->
                    {true, Hook, Rest}
            end
    end.

check_skip_ok(0, _) ->
    ok;
check_skip_ok(SkipCount, Config) when SkipCount > 0 ->
    case ?CONFIG(ignore_skipped, Config, false) of
        true  -> ok;
        false -> {error, {skipped, SkipCount}}
    end.


run_test(Cfg, Quiet) ->
    case Quiet of
        true  -> ok;
        false -> print_test_data(Cfg, "Starting Test Run")
    end,
    ct:run_test(Cfg).

print_test(Cfg, _) ->
    print_test_data(Cfg, "Starting Dry Run"),
    ok.

print_test_data(Config, Border) ->
    io:format("~s~n"
              "~s~n",
              [systest_utils:border(Border, "-"),
               systest_utils:proplist_format([{"framework", "Common Test"},
                                              {"handler", ?MODULE}|Config])]).
