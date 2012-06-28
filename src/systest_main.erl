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
-module(systest_main).

-export([run/1, help/0]).

run(["export"]) ->
    Config = init_rebar(parse_args([])),
    export(Config),
    ok;
run(Args) ->
    Options = parse_args(Args),
    BaseConfig = init_rebar(Options),

    MainConfig = rebar_config:get_local(BaseConfig, plugins, []),
    Config = rebar_config:set(BaseConfig, plugins, [systest_rebar|MainConfig]),

    Commands = case proplists:get_all_values(build, Options) of
                   []   -> case proplists:get_value(compile, Options) of
                               true -> [compile, systest];
                               _    -> [systest]
                           end;
                   Cmds -> Cmds ++ [systest]
               end,
    maybe_export(Commands, Config),
    rebar_config:set_global(skip_deps, "true"),
    rebar_core:process_commands(Commands, Config).

export(Config) ->
    [export_entry(F, Config) || F <- get(escript_files)].

export_entry({File, Bin}=Entry, Config) ->
    case filename:split(File) of
        [FN] ->
            case filename:basename(FN, ".hrl") of
                FN       -> ok;
                _HrlFile -> Dest = filename:join(["systest", "include", FN]),
                            do_export({Dest, Bin}, Config)
            end;
        ["systest", "ebin", _] ->
            do_export(Entry, Config);
        _ ->
            ok
    end.

do_export({File, Bin}, Config) ->
    Path = filename:join(deps_dir(Config), File),
    rebar_utils:ensure_dir(Path),
    file:write_file(Path, Bin).

maybe_export(Commands, Config) ->
    case lists:member(compile, Commands) of
        false -> ok;
        true  -> export(Config)
    end.

deps_dir(Config) ->
    rebar_config:get_local(Config, deps_dir,
                    rebar_config:get_global(deps_dir, "deps")).

init_rebar(Options) ->
    %% Initialize rebar *stuff*
    application:load(rebar),
    crypto:start(),
    rebar_log:init(),
    erlang:put(operations, 0),

    %% Initialize vsn cache
    _VsnCacheTab = ets:new(rebar_vsn_cache,[named_table, public]),

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    rebar_config:set_global(escript, filename:absname(escript:script_name())),

    %% Note the top-level directory for reference
    rebar_config:set_global(base_dir, filename:absname(rebar_utils:get_cwd())),

    {ok, Files} = rebar_utils:escript_foldl(
                    fun(Name, _, GetBin, Acc) ->
                            [{Name, GetBin()} | Acc]
                    end,
                    [], rebar_config:get_global(escript, undefined)),
    erlang:put(escript_files, Files),

    base_config().

base_config() ->
    base_config(["systest.config", "rebar.config"]).

base_config([]) ->
    rebar_config:new();
base_config([H|T]) ->
    case filelib:is_regular(H) of
        true  -> rebar_config:new(filename:join(rebar_utils:get_cwd(), H));
        false -> base_config(T)
    end.

parse_args(Args) ->
    %% Parse getopt options
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, _NonOptArgs}} ->
            GlobalDefines = proplists:get_all_values(defines, Options),
            rebar_config:set_global(defines, GlobalDefines),

            %% Set global variables based on getopt options
            set_log_level(Options),
            set_jobs(Options),
            Options;
        {error, Err} ->
            throw(Err)
    end.

help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "rebar",
                 "[var=value,...] <command,...>",
                 [{"var=value", "rebar global variables (e.g. force=1)"},
                  {"command", "Command to run (e.g. compile)"}]).

option_spec_list() ->
    Jobs = rebar_config:get_jobs(),
    JobsHelp = io_lib:format(
                 "Number of concurrent workers a command may use. Default: ~B",
                 [Jobs]),
    VerboseHelp = "Verbosity level (-v, -vv, -vvv, --verbose 3). Default: 0",
    ProfileHelp = "Test profile(s) to run. Default: $USER.~n"
                  "If profiles/$USER.spec is not present,"
                  " falls back to 'default.spec'",
    CompileHelp = "Pre-build sources using (embedded) rebar",
    BuildHelp = "Run custom build commands (using embedded) rebar",
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     %{help,     $h, "help",     undefined, "Show the program options"},

     %% forwarding rebar options
     {verbose,  $v, "verbose",  integer,   VerboseHelp},
     {defines,  $D, undefined,  string,    "Define compiler macro(s)"},
     {jobs,     $j, "jobs",     integer,   JobsHelp},

     %% execution environment
     {profile,  $P, "profile",  string,    ProfileHelp},

     %%

     %% build support
     {compile,  $C, "compile",  undefined, CompileHelp},
     {build,    $b, "build",    string,    BuildHelp}
    ].

config_overrides(Options) ->
    [begin
         %% if there is an environment variable, we prefer it
         {Key, Value} = Option,
         case systest_config:get_env(Key) of
             not_found -> proplists:get_value(Key, Options, Option);
             Setting   -> Setting
         end
     end || Option <- config_overrides()].

config_overrides() ->
    [{systest_scratch_dir, filename:join(temp_dir(), "systest")},
     {systest_profile,     get("USER", "default")}].

temp_dir() ->
    %% TODO: move this into hyperthunk/rebar_plugin_manager?
    case os:type() of
        {win32, _} ->
            %% mirrors the behaviour of the win32 GetTempPath function...
            get("TMP", get("TEMP", element(2, file:get_cwd())));
        _ ->
            case os:getenv("TMPDIR") of
                false -> "/tmp"; %% this is what the JVM does, but honestly...
                Dir   -> Dir
            end
    end.

get(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        Value -> Value
    end.

set_log_level(Options) ->
    LogLevel = case proplists:get_all_values(verbose, Options) of
                   [] ->
                       rebar_log:default_level();
                   Verbosities ->
                       lists:last(Verbosities)
               end,
    rebar_config:set_global(verbose, LogLevel).

set_jobs(Options) ->
    DefJobs = rebar_config:get_jobs(),
    case proplists:get_value(jobs, Options, DefJobs) of
        DefJobs ->
            ok;
        Jobs ->
            rebar_config:set_global(jobs, Jobs)
    end.

