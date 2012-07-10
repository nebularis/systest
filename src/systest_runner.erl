%% Copyright (c) 2012 Nebularis.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(systest_runner).

-include("systest.hrl").

%-export([execute/1]).
-compile(export_all).

-type execution() :: #execution{}.
-export_type([execution/0]).

-define(WORKDIR(A, B, C),
        systest_env:work_directory(A, B, C)).

-exprecs_prefix([operation]).
-exprecs_fname(["record_", prefix]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records([execution]).

behaviour_info(callbacks) ->
    [{run, 1}, {dryrun, 1}];
behaviour_info(_) ->
    undefined.

execute(Config) ->
    maybe_start_net_kernel(Config),
    {ok, BaseDir} = file:get_cwd(),
    Exec = build_exec([{base_dir, BaseDir}|Config]),
    BaseDir = Exec#execution.base_dir,
    Prof = Exec#execution.profile,
    DefaultSettings = systest_profile:get(settings_base, Prof),
    Resources = verify_resources(Prof, BaseDir),

    %% because sometimes, code that is accessed from an escript archive doesn't
    %% get handled in a particularly useful way by the code server.... :/
    % code:ensure_loaded(systest_utils)
    systest:start(),
    print_banner(),
    set_defaults(Prof),
    start_logging(Config),

    preload_resources(Resources),
    ensure_test_directories(Prof),
    systest_config:set_env(base_dir, BaseDir),
    
    Targets = load_test_targets(Prof, Config),
    Settings = systest_settings:load(DefaultSettings),
    Exec2 = set([{targets, Targets}, {settings, Settings}], Exec),
    verify(Exec2).

set_defaults(Profile) ->
    ScratchDir = systest_profile:get(output_dir, Profile),
    systest_config:set_env("SCRATCH_DIR", ScratchDir).

print_banner() ->
    %% Urgh - could there be an uglier way!?
    %% TODO: refactor this...
    AppVsn = element(3, lists:keyfind(systest, 1,
                                      application:loaded_applications())),
    {ok, Banner} = application:get_env(systest, banner),
    io:format("~s~n"
              "Version ~s~n", [Banner, AppVsn]).

start_logging(Config) ->
    Active     = ?CONFIG(logging, Config, []),
    [begin
        io:format(user, "activating logging sub-system ~p~n", [SubSystem]),
        ok = systest_log:start(SubSystem, systest_log, user)
     end || SubSystem <- Active],
    ok.

verify(Exec2=#execution{profile     = Prof,
                        base_dir    = BaseDir,
                        targets     = Targets,
                        base_config = Config}) ->
    Mod = systest_profile:get(framework, Prof),

    io:format(user, "SysTest Task Descriptor:~n", []),
    io:format(user, "~s~n",
              [systest_utils:proplist_format([
                {"Base Directory", BaseDir},
                {"Test Directories", [D || {dir, D} <- Targets]},
                {"Test Suites", [S || {suite, S} <- Targets]}])]),

    Prop = systest_utils:record_to_proplist(Prof, systest_profile),
    Print = systest_utils:proplist_format(Prop),
    io:format(user, "SysTest Profile:~n", []),
    io:format(user, "~s~n", [Print]),

    TestFun = case ?CONFIG(dryrun, Config, false) of
                  true  -> dryrun;
                  false -> run
              end,

    case catch( erlang:apply(Mod, TestFun, [Exec2]) ) of
        {'EXIT', Reason} ->
            handle_errors(Exec2, Reason, Config);
        {failed, Reason} ->
            handle_errors(Exec2, Reason, Config);
        ok ->
            ok
    end.

handle_errors(_Exec, Reason, Config) ->
    ErrorHandler = ?CONFIG(error_handler, Config, fun systest_utils:abort/2),
    ErrorHandler("Execution Failed: ~p~n", [Reason]).

load_test_targets(Prof, Config) ->
    case proplists:get_all_values(testsuite, Config) of
        [] ->
            case ?CONFIG(testcase, Config, undefined) of
                undefined   -> load_test_targets(Prof);
                {Suite, TC} -> [{suite, Suite}, {testcase, TC}]
            end;
        Suites ->
            [{suite, Suites}]
    end.

load_test_targets(Prof) ->
    {Dirs, Suites} = load_targets_from_profile(Prof),
    [{suite, systest_utils:uniq(Suites)},
     {dir, systest_utils:uniq(Dirs)}].

load_targets_from_profile(Prof) ->
    lists:foldl(
        fun(Path, {Dirs, _}=Acc) when is_list(Path) ->
                setelement(1, Acc, [Path|Dirs]);
           (Mod, {Dirs, Suites}) when is_atom(Mod) ->
                Path = test_dir(Mod),
                {[Path|Dirs], [Mod|Suites]}
        end, {[], []}, systest_profile:get(targets, Prof)).

test_dir(Thing) when is_atom(Thing) ->
    case code:ensure_loaded(Thing) of
        {error, Reason} ->
            throw({invalid_target, {Thing, Reason}});
        _ ->
            filename:dirname(code:which(Thing))
    end.

preload_resources(Resources) ->
    [begin
        case file:consult(Resource) of
            {ok, Terms} ->
                systest_config:load_config_terms(Terms);
            Error ->
                throw(Error)
        end
     end || Resource <- Resources].

verify_resources(Profile, BaseDir) ->
    Resources = lists:foldl(fun(P, Acc) ->
                                Glob = filename:join(BaseDir, P),
                                filelib:wildcard(Glob) ++ Acc
                            end, [], systest_profile:get(resources, Profile)),
    [begin
        case filelib:is_regular(Path) of
            false -> throw({invalid_resource, Path});
            true  -> Path
        end
     end || Path <- Resources].

ensure_test_directories(Prof) ->
    filelib:ensure_dir(filename:join(
                       systest_profile:get(output_dir, Prof), "foo")),
    filelib:ensure_dir(filename:join(
                       systest_profile:get(log_dir, Prof), "foo")).

build_exec(Config) ->
    %% TODO: consider adding a time stamp to the scratch
    %%       dir like common test does
    ScratchDir = ?CONFIG(scratch_dir, Config,
                         ?WORKDIR(systest_env:temp_dir(),
                                  systest, "SYSTEST_SCRATCH_DIR")),
    Config2 = ?REPLACE(scratch_dir, ScratchDir, Config),
    Profile = systest_profile:load(Config2),
    BaseDir = ?REQUIRE(base_dir, Config),
    #execution{profile      = Profile,
               base_dir     = BaseDir,
               base_config  = Config2}.

maybe_start_net_kernel(Config) ->
    UseLongNames = ?CONFIG(longnames, Config, false),
    case net_kernel:longnames() of
        ignored ->
            if
                UseLongNames =:= true ->
                    net_kernel:start([?MODULE, longnames]);
                UseLongNames =:= false ->
                    net_kernel:start([?MODULE, shortnames])
            end;
        LongNames ->
            systest_utils:throw_unless(
                LongNames == UseLongNames, runner,
                "The supplied configuration indicates that "
                "longnames should be ~p, "
                "but the current node is running with ~p.~n",
                [use_longnames(UseLongNames),
                long_or_short_names(LongNames)])
    end.

use_longnames(true)  -> enabled;
use_longnames(false) -> disabled.

long_or_short_names(true)  -> longnames;
long_or_short_names(false) -> shortnames.
