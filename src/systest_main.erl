%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2012 Tim Watson (watson.timothy@gmail.com)
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
-module(systest_main).

-export([run/1]).

run(Args) ->
    Options = parse_args(Args),

    BaseConfig = init_rebar(Options),
    MainConfig = rebar_config:get_local(BaseConfig, plugins, [])),
    Config = rebar_config:set(MainConfig, plugins, [systest_rebar|MainConfig]),



    Commands = case proplists:get_all_values(build, Options) of
                   []   -> case proplists:get_value(compile, Options) of
                               true -> [compile, systest];
                               _    -> [systest]
                           end;
                   Cmds -> Cmds ++ [systest]
               end,

    rebar_core:process_commands(Commands, Config).

init_rebar(Options) ->
    %% Initialize rebar *stuff*
    rebar_log:init(),

    %% Initialize vsn cache
    _VsnCacheTab = ets:new(rebar_vsn_cache,[named_table, public]),

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    rebar_config:set_global(escript, filename:absname(escript:script_name())),

    %% Note the top-level directory for reference
    rebar_config:set_global(base_dir, filename:absname(rebar_utils:get_cwd())),

    rebar_config:base_config(rebar_config:new()).

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
            Options
        {error, Err} ->
            throw(Err)
    end.

option_spec_list() ->
    Jobs = rebar_config:get_jobs(),
    JobsHelp = io_lib:format(
                 "Number of concurrent workers a command may use. Default: ~B",
                 [Jobs]),
    VerboseHelp = "Verbosity level (-v, -vv, -vvv, --verbose 3). Default: 0",
    ProfileHelp = "Test profile(s) to run. Default: $USER.~n"
                  "If $USER.spec is not present, falls back to 'default.spec'",
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
     {systest_profile,     get("USER", "default")}]

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
