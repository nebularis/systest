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

-export([execute/1]).

-define(WORKDIR(A, B, C),
        systest_utils:work_directory(A, B, C)).

-record(exec, {
    profile         :: systest_profile:profile(),
    specification   :: file:filename(),
    scratch_dir     :: file:filename(),
    ct_log_dir      :: file:filename(),
    sys_log_dir     :: file:filename(),
    config_dir      :: file:filename()
}).

execute(Config) ->
    maybe_start_net_kernel(Config),
    run(build_exec(Config)).

run(Exec) ->
    
    %% NB: we want systest_profile to do all the loading/configuring
    %% and then we bootstrap the arguments to the testing framework
    %% and execute the appropriate tests.......

    %% we want to keep the scratch_dir, as this contains the ct logs....
    %% but the config dir should be blown away whenever we copy to it...
    systest_utils:rm_rf(Exec#exec.config_dir),
    % filelib:ensure_dir(filename:join([Ex#exec.ct_log_dir,  "foo"])),
    % filelib:ensure_dir(filename:join([Ex#exec.sys_log_dir, "foo"])),
    % filelib:ensure_dir(filename:join([Ex#exec.config_dir,  "foo"])),

    % rebar_config:set_global(scratch_dir, ScratchDir),

%    InitSpec =  case find_files("profiles", Profile ++ "\\.spec$") of
%                    [SpecFile] -> SpecFile;
%                    _          -> filename:join("profiles", "default.spec")
%                end,
%    Spec =  case filelib:is_regular(Spec) of
%                false ->
%                    case file:list_dir("resources") of
%                        {ok, ResFiles} ->
%                            ok %% generate_default_spec()
%                    end;
%                true ->
%                    ok
%            end,
%    Env = [{scratch_dir, ScratchDir}] ++ rebar_env() ++ os_env(),
%    {ok, SpecOutput} = transform_file(Spec, temp_dir(), Env),
%    {ok, FinalSpec} = process_config_files(ScratchDir, SpecFile, Env),
%    LogDir = "systest-logs",
%    case ct:run_test([{'spec', FinalSpec},
%                      {logdir, filename:join(ScratchDir, LogDir)},
%                      {auto_compile, false},
%                      {allow_user_terms, true}]) of
%        {error, Reason}=Err ->
%            error(Reason);
%        Results ->
%            rebar_log:log(info, "Results: ~p~n", [Results]),
            ok
%    end.
.

build_exec(Config) ->
    %% TODO: consider adding a time stamp to the scratch
    %%       dir like common test does
    ScratchDir = ?CONFIG(scratch_dir, Config,
                         ?WORKDIR(systest_utils:temp_dir(),
                                  systest, "SYSTEST_SCRATCH_DIR")),
    Config2 = ?REPLACE(scratch_dir, ScratchDir, Config),
    Profile = systest_profile:load(Config2),
    LogBase = systest_profile:get(log_base, Profile),
    #exec{profile     = Profile,
          scratch_dir = ScratchDir,
          ct_log_dir  = ?WORKDIR(LogBase, "ct-log",      "SYSTEST_CT_LOG_DIR"),
          sys_log_dir = ?WORKDIR(LogBase, "systest-log", "SYSTEST_LOG_DIR"),
          config_dir  = ?WORKDIR(LogBase, "config",      "SYSTEST_CONF_DIR")}.

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
            systest_assert:throw_unless(
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

process_config_files(SDir, TempSpec, Env) ->
    ScratchDir = filename:join(SDir, "temp"),
    {ok, Terms} = file:consult(TempSpec),
    {[Configs], Rest} = proplists:split(Terms, [config]),
    rebar_log:log(debug, "Processing config sections: ~p~n", [Configs]),
    Replacements = [begin
                        {ok, Path} = transform_file(F, ScratchDir, Env),
                        {config, Path}
                    end || {_, F} <- lists:flatten(Configs)],
    Spec = filename:join(ScratchDir, filename:basename(TempSpec)),
    {ok, Fd} = file:open(Spec, [append]),
    Content = Replacements ++ Rest,
    rebar_log:log(debug, "Write to ~s: ~p~n", [Spec, Content]),
    write_terms(Content, Fd),
    {ok, Spec}.

transform_file(File, ScratchDir, Env) ->
    systest_assert:throw_unless(
        filelib:is_regular(File), runner,
        "File ~s not found.~n", [File]),

    Output = filename:join(ScratchDir, filename:basename(File)),
    Target = filename:absname(Output),
    Origin = filename:absname(File),
    rebar_log:log(info, "transform ~s into ~s~n", [Origin, Target]),

    %% this looks *pointless* but avoids calling dict:to_list/1
    %% unless it is actually going to use the result
    case rebar_log:get_level() of
        debug -> rebar_log:log(debug, "template environment: ~p~n", [Env]);
        _     -> ok
    end,

    Context = rebar_templater:resolve_variables(Env, dict:new()),
    {ok, Bin} = file:read_file(File),
    Rendered = rebar_templater:render(Bin, Context),

    file:write_file(Output, Rendered),
    {ok, Target}.

write_terms(Terms, Fd) ->
    try
        [begin
            Element = lists:flatten(erl_pp:expr(erl_parse:abstract(Item))),
            Term = Element ++ ".\n",
            ok = file:write(Fd, Term)
         end || Item <- Terms]
    after
        file:close(Fd)
    end.

rebar_env() ->
    [{base_dir, rebar_config:get_global(base_dir, rebar_utils:get_cwd())}] ++
    clean_env(application:get_all_env(rebar_global)).

os_env() ->
    systest_config:get_env().

clean_env(Env) when is_list(Env) ->
    [ E || {_, [H|_]}=E <- Env, is_integer(H) ];
clean_env(_) ->
    [].
