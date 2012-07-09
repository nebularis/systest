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
-module(systest_profile).
-include("systest.hrl").

-export([load/1]).

-define(lookup(W, D), systest_utils:lookup_env(W, D)).

-record(profile, {
    name            :: string(),
    source          :: file:filename(),
    root_dir        :: file:filename(),
    log_base        :: file:filename(),
    config_files    :: [file:filename()],
    test_dirs       :: [file:filename()],
    suites          :: [module()],
    cases           :: [{module(), atom()}],
    ct_config       :: [tuple()]
}).

-opaque profile() :: #profile{}.
-export_type([profile/0]).

-exprecs_prefix([operation]).
-exprecs_fname([prefix, "profile"]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records([profile]).

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

log_terms(Type, Data) when is_list(Data) ->
    rebar_log:log(debug, "Loading ~s: ~s~n", [Type, Data]);
log_terms(Type, Data) when is_atom(Data) ->
    rebar_log:log(debug, "Loading ~s: ~p~n", [Type, Data]).

-spec load(systest_config:config()) -> profile().
load(Config) ->
    %% NB: change of plans...
    %% 1. we want to support other testing frameworks besides common_test
    %% 2. we want users to be able to override things on the command line
    %% 3. we want to generate sensible defaults for things where possible
    %% 4. we want to call ct:run_tests([Options]) even when running ct!
    %% 5. so.....
    %% 
    %% we will use the profile(s) to load custom settings and environment vars
    %% but, we will generate the other stuff wherever possible
    %% and we *will* continue to support ct.config files, but re-use them in
    %% other testing environments
    %% AND
    %% we will continue to expand relative paths (in config files) to absolute
    %% AND
    %% we will do all of this as simply as possible....
    
    {ok, Cwd} = file:get_cwd(),
    case load_all_profiles(Cwd, Config) of
        []   -> generate_default_spec(Cwd);
        Spec -> Spec
    end.

%%
%% Private API
%%

generate_default_spec(Root) ->
    LogBase = filename:join([systest_utils:temp_dir(), "systest", "logs"]),
    SuiteModules = systest_utils:find_files(Root, ".*_SUITE.beam"),
    Suites = [list_to_atom(filename:basename(Mod, ".beam")) ||
                                                    Mod <- SuiteModules],
    #profile{name         = generated_default,
             source       = none,
             root_dir     = Root,
             log_base     = LogBase,
             config_files = [],
             test_dirs    = [filename:join(Root, "ebin")],
             suites       = Suites,
             cases        = [],
             ct_config    = []}.

load_all_profiles(Cwd, Config) ->
    ProfileBase = filename:join(Cwd, ?CONFIG(profile_dir, Config, "profiles")),
    Default = filename:join(ProfileBase, "default.profile"),
    Selected = filename:join(ProfileBase, profile_name(Config)),
    if Default == Selected ->
        load_profile(Cwd, Default, Config);
       true ->
        systest_config:merge_config(load_profile(Cwd, Default, Config),
                                    load_profile(Cwd, Selected, Config))
    end.

load_profile(Root, ProfilePath, Config) ->
    Terms = load_terms(ProfilePath, Config),
    BaseDir = ?CONFIG(base_dir, Config, Root),
    ProfileName = list_to_atom(filename:basename(ProfilePath, ".profile")),
    LogBase = ?CONFIG(logdir, Terms, filename:join(Root, "logs")),
    InitProfile = #profile{name         = ProfileName,
                           source       = ProfilePath,
                           root_dir     = Root,
                           log_base     = LogBase,
                           config_files = [],
                           test_dirs    = [],
                           suites       = [],
                           cases        = [],
                           ct_config    = []},
    lists:foldl(fun({config, Path}, P=#profile{config_files=Paths}) ->
                    Path2 = convert_path(Path, BaseDir),
                    P#profile{config_files=[Path2|Paths]};
                   (T, P=#profile{test_dirs=Dirs, ct_config=CtConf,
                                  suites=Suites, cases=Cases})
                            when is_tuple(T) ->
                    case element(1, T) of
                        alias ->
                            Path = element(3, T),
                            T2 = setelement(3, T,
                                    convert_path(Path, BaseDir)),
                            P#profile{test_dirs=[T2|Dirs]};
                        suites ->
                            P#profile{suites=[T|Suites]};
                        cases ->
                            P#profile{cases=[T|Cases]};
                        _ ->
                            P#profile{ct_config=[T|CtConf]}
                    end
                end, InitProfile, Terms).

convert_path(Path, BaseDir) ->
    case filename:pathtype(Path) of
        absolute -> Path;
        relative -> filename:join(BaseDir, Path)
    end.

load_terms(Path, Config) ->
    case file:consult(Path) of
        {ok, Terms} ->
            systest_config:expand(Terms, Config,
                        [{return, value},
                         {callback,
                            {runtime,
                             fun(K, C) ->
                                 proplists:get_value(K, C)
                             end}}]);
        _ ->
            []
    end.

profile_name(Config) ->
    ?CONFIG(profile, Config,
            systest_utils:lookup_env("SYSTEST_PROFILE", os:getenv("USER"))).

