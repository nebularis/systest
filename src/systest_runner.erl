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
%% @doc This module handles the execution of <i>test runs</i>, and provides a
%% custom OTP behaviour that callback module can implement in order to provide
%% support for different testing frameworks.
%%
%% The entry point for client code (executing a test run) is execute/1.
%% ----------------------------------------------------------------------------
-module(systest_runner).

-include("systest.hrl").

-export([behaviour_info/1, execute/1]).

-export([timed_abort/2]).

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

%% @doc Execute SysTest using the supplied parameters (in Config)
%% @end
-spec execute(systest_config:config()) -> 'ok'.
execute(Config) ->
    systest:start(),
    start_logging(Config),
    maybe_start_net_kernel(Config),
    {ok, BaseDir} = file:get_cwd(),
    Exec = build_exec([{base_dir, BaseDir}|Config]),
    BaseDir = Exec#execution.base_dir,
    Prof = Exec#execution.profile,
    DefaultSettings = systest_profile:get(settings_base, Prof),
    Resources = verify_resources(Prof, BaseDir),

    preload_resources(Resources, Config),
    print_banner(Config),
    set_defaults(Prof),

    ensure_test_directories(Prof),
    systest_config:set_env(base_dir, BaseDir),

    Targets = load_test_targets(Prof, Config),
    Settings = systest_settings:load(DefaultSettings),
    Exec2 = set([{targets, Targets}, {settings, Settings}], Exec),
    verify(Exec2).

set_defaults(Profile) ->
    ScratchDir = systest_profile:get(output_dir, Profile),
    systest_config:set_env("SCRATCH_DIR", ScratchDir).

print_banner(Config) ->
    %% Urgh - could there be an uglier way!?
    %% TODO: refactor this...
    case quiet(Config) of
        true  -> ok;
        false ->
            {ok, Banner} = application:get_env(systest, banner),
            io:format("~s~n", [Banner])
    end.

start_logging(Config) ->
    SystemLog = case quiet(Config) of
                    true  -> LogName = lists:flatten(
                                io_lib:format("systest.~s.log",
                                              [systest_env:timestamp()])),
                             io:format(user, "quiet: (logging to ~s)~n",
                                       [LogName]),
                             filename:absname(LogName);
                    false -> user
                end,
    systest_log:start(SystemLog),
    Active = proplists:get_all_values(logging, Config),
    [begin
        %% TODO: reinstate logging to different appenders...
        Target = if is_atom(SubSystem) -> SubSystem;
                    is_list(SubSystem) -> list_to_atom(SubSystem);
                                  true -> throw({badarg, SubSystem})
                 end,
        case quiet(Config) of
            true  -> ok;
            false -> io:format(user, "activating logging sub-system ~p~n",
                               [Target])
        end,
        ok = systest_log:start(Target, systest_log, user)
     end || SubSystem <- Active],
    if length(Active) > 0 -> io:nl();
                     true -> ok
    end.

quiet(Config) ->
    ?CONFIG(quiet, Config, false).

verify(Exec2=#execution{profile     = Prof,
                        base_dir    = BaseDir,
                        targets     = Targets,
                        settings    = Settings,
                        base_config = Config}) ->

    %% we want 'settings' to be available as a configuration section,
    %% so we store it in systest_config as a static config element
    systest_config:set_static(settings, [{base_dir, BaseDir}|Settings]),

    Mod = get_framework(Prof, Config),

    case quiet(Config) of
        true ->
            ok;
        false ->
            AppVsn = element(3, lists:keyfind(systest, 1,
                                        application:which_applications())),
            systest_utils:print_section("SysTest Task Descriptor", [
                {"Release Version", AppVsn},
                {"Test Coordinator", node()},
                {"Test Suites", lists:concat([S || {suite, S} <- Targets])},
                {"Test Directories", lists:concat([D || {dir, D} <- Targets])},
                {"Base Directory", BaseDir},
                {"Options", "(user supplied settings....)"}] ++ Config),

            Prop = systest_utils:record_to_proplist(Prof, systest_profile),
            systest_utils:print_section("SysTest Profile", Prop)
    end,

    ScratchDir = systest_profile:get(output_dir, Prof),
    CoverBase = filename:join(ScratchDir, "cover"),
    {ok, Export} = systest_cover:start(ScratchDir, Config),
    io:nl(),

    TestFun = case ?CONFIG(dryrun, Config, false) of
                  true  -> dryrun;
                  false -> run
              end,

    case systest_profile:get(overall_timetrap, Prof) of
        undefined ->
            ok;
        TimeoutNode ->
            Ms = systest_utils:time_to_ms(TimeoutNode),
            timer:apply_after(Ms, ?MODULE, timed_abort, [Config, Ms])
    end,

    Result = case catch( erlang:apply(Mod, TestFun, [Exec2]) ) of
                 R -> R
             end,

    systest_cover:report_cover(CoverBase, Export, Config),

    systest:stop(),
    case Result of
        ok ->
            case ?CONFIG(dryrun, Config, false) of
                true  -> io:format("[dryrun] done~n");
                false -> io:format("[passed] all test cases succeeded~n")
            end;
        {error,{Failure, _}=F} when Failure == skipped orelse
                                    Failure == failed ->
            handle_failures(Prof, F, Config);
        {'EXIT', Reason} ->
            handle_errors(Exec2, Reason, Config);
        Errors ->
            handle_errors(Exec2, Errors, Config)
    end.

timed_abort(Config, Ms) ->
    AbortHandler = ?CONFIG(error_handler, Config, fun systest_utils:abort/2),
    AbortHandler("ABORT: reached max profile execution time: ~p ms~n", [Ms]).

get_framework(Prof, Config) ->
    list_to_atom(case ?CONFIG(stand_alone, Config, false) of
                     true  -> "systest_standalone";
                     false -> case ?CONFIG(shell, Config, false) of
                                  true ->
                                      "systest_shell";
                                  false ->
                                      systest_profile:get(framework, Prof)
                              end
                 end).

handle_failures(Prof, {How, N}, Config) ->
    maybe_dump(Config),
    ProfileName = systest_profile:get(name, Prof),
    ErrorHandler = ?CONFIG(error_handler, Config, fun systest_utils:abort/2),
    ErrorHandler("[failed] test profile ~s "
                 "completed with ~p ~p test cases~n",
                 [ProfileName, N, How]).

handle_errors(_Exec, Reason, Config) ->
    maybe_dump(Config),
    ErrorHandler = ?CONFIG(error_handler, Config, fun systest_utils:abort/2),
    ErrorHandler("[error] framework encountered unhandled errors: ~p~n",
                 [Reason]).

maybe_dump(Config) ->
    case ?CONFIG(dump, Config, false) of
        true  -> systest_stats:dump();
        false -> ok
    end.

load_test_targets(Prof, Config) ->
    %% TODO: this explicit handling of a known framework is pretty
    %% awful - we should have a way to delegate to the framework!
    %% see https://github.com/nebularis/systest/issues/40
    case get_framework(Prof, Config) of
        F when F == systest_standalone orelse
               F == systest_shell ->
            [];
        _F ->
            case proplists:get_all_values(testsuite, Config) of
                [] ->
                    case ?CONFIG(testcase, Config, undefined) of
                        undefined   -> load_test_targets(Prof);
                        {Mod, Func} -> Suite = list_to_atom(Mod),
                                       TC = list_to_atom(Func),
                                       systest_utils:throw_unless(
                                         systest_env:is_exported(Suite, TC, 1),
                                         "~p:~p/1 is not a valid test case "
                                         "- have you checked the code path?~n",
                                         [Suite, TC]),
                                       [{suite, Suite},
                                        {testcase, TC},
                                        {dir, test_dir(Suite)}]
                    end;
                [Configured] ->
                    Suite = list_to_atom(lists:flatten(Configured)),
                    [{suite, Suite},
                     {dir, test_dir(Suite)}]
            end
    end.

load_test_targets(Prof) ->
    {Dirs, Suites} = load_targets_from_profile(Prof),
    Suites2 = case Suites of
                  [] ->
                      [begin
                          list_to_atom(hd(string:tokens(
                                filename:basename(F), ".")))
                       end || Dir <- Dirs,
                                F <- filelib:wildcard(
                                            filename:join(Dir, "*_SUITE.*"))];
                  _ ->
                      Suites
              end,
    Dirs2 = case Dirs of
                [Dir] -> Dir;
                _     -> systest_utils:uniq(Dirs)
            end,
    [{suite, systest_utils:uniq(Suites2)},
     {dir, Dirs2}].

load_targets_from_profile(Prof) ->
    lists:foldl(
        fun(Path, {Dirs, _}=Acc) when is_list(Path) ->
                setelement(1, Acc, [filename:absname(Path)|Dirs]);
           (Mod, {Dirs, Suites}) when is_atom(Mod) ->
                Path = test_dir(Mod),
                {[Path|Dirs], [Mod|Suites]}
        end, {[], []}, systest_profile:get(targets, Prof)).

test_dir(Thing) when is_atom(Thing) ->
    case code:ensure_loaded(Thing) of
        {error, Reason} ->
            throw({invalid_target, {Thing, Reason}});
        _ ->
            filename:absname(filename:dirname(code:which(Thing)))
    end.

preload_resources(Resources, Config) ->
    [begin
        case file:consult(Resource) of
            {ok, Terms} ->
                systest_config:load_config_terms(resources, Terms);
            Error ->
                ErrorHandler = ?CONFIG(error_handler, Config,
                                       fun systest_utils:abort/2),
                ErrorHandler("unable to parse ~s: ~p~n",
                             [Resource, Error])
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
               base_config  = Config2,
               quiet        = ?CONFIG(quiet, Config, false)}.

maybe_start_net_kernel(Config) ->
    UseLongNames = ?CONFIG(longnames, Config, false),
    NodeName = case ?CONFIG(node, Config, undefined) of
                   undefined -> ?MODULE;
                   Other     -> list_to_atom(Other)
               end,
    case net_kernel:longnames() of
        ignored ->
            {ok, Host} = inet:gethostname(),
            case systest_env:is_epmd_contactable(Host, 5000) of
                {false, Reason} ->
                    io:format(user, "epmd not contactable on ~s => ~p~n",
                             [Host, Reason]),
                    os:cmd("epmd -daemon");
                true  ->
                    ok
            end,
            case re:run(os:cmd("epmd -names"), "[^\\n]+",
                        [global, {capture, all, list}]) of
                {match, EpmdData} ->
                    EpmdRunInfo = re:replace(hd(EpmdData),
                                             "with data:",
                                             ""),
                    [begin
                         io:format(user, "[systest.net] ~s~n",
                                   [re:replace(hd(re:split(Line, "\\r")),
                                               "name", "found")])
                     end || Line <- [EpmdRunInfo|tl(EpmdData)]];
                _ ->
                    ok
            end,
            if
                UseLongNames =:= true ->
                    {ok, _} = net_kernel:start([NodeName, longnames]);
                UseLongNames =:= false ->
                    {ok, _} = net_kernel:start([NodeName, shortnames])
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
