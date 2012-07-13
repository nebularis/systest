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

-export([load/1, load/2]).
-import(systest_utils, [combine/2, as_list/1]).

-define(lookup(W, D), systest_utils:lookup_env(W, D)).

-type testable() :: file:filename() | module().
-type time_unit() :: 'hr' | 'min' | 'sec' | 'ms'.

%% {allow_user_terms, Bool}, {label, ProfileName}

-record(profile, {
    name                        :: string(),        %% {label, Label}
    framework = systest_ct      :: module(),
    source                      :: file:filename(),
    output_dir                  :: file:filename(),
    log_dir                     :: file:filename(),
    settings_base               :: file:filename(),
    resources        = []       :: [file:filename()],
    targets          = ["ebin"] :: [testable()],
    specifications   = []       :: [file:filename()],
    hooks            = []       :: [term()],
    default_timetrap            :: {integer(), time_unit()}
}).

-exprecs_prefix([operation]).
-exprecs_fname([prefix, "profile"]).
-exprecs_vfname([fname, "__", version]).

-compile({parse_transform, exprecs}).
-export_records([profile]).

-opaque profile() :: #profile{}.
-export_type([profile/0]).

%%
%% @doc load a new test profile using the supplied configuration
%% @end
-spec load(systest_config:config()) -> profile().
load(Config) ->
    BaseDir = ?REQUIRE(base_dir, Config),
    case ?CONFIG(profile, Config, undefined) of
        undefined -> load_configured_profile_data(Config, BaseDir);
        Name      -> Profile = load_named_profile_data(Name, BaseDir),
                     Profile#profile{ name=Name }
    end.

%%
%% @doc load a new test profile from the supplied ProfFile
%% @end
-spec load(file:filename(),
           file:filename()) -> profile().
load(ProfFile, BaseDir) ->
    %% NB: set/2 record access is *generated* by the exprecs parse_transform
    Terms = load_terms(ProfFile),
    Profile = lists:foldl(fun(E, P) -> set([E], P) end,
                          default_profile(BaseDir), Terms),
    Profile#profile{ source=ProfFile }.

%%
%% Private/Internal API
%%

load_configured_profile_data(Config, BaseDir) ->
    case ?CONFIG(systest_profile, Config, undefined) of
        undefined -> default_profile(BaseDir);
        Data      -> Data
    end.

load_named_profile_data(Profile, BaseDir) ->
    ProfName = systest_utils:as_string(Profile),
    case systest_utils:find_files(BaseDir, ProfName ++ "\\.profile$") of
        []           -> throw({profile_unavailable, ProfName});
        [_P1, _P2|_] -> throw({ambiguous_profile,   ProfName});
        [ProfFile]   -> load(ProfFile, BaseDir)
    end.

load_terms(ProfFile) ->
    systest_utils:with_termfile(ProfFile, fun read_terms/1).

read_terms(Terms) ->
    lists:foldl(fun({K, V}=E, Acc) ->
                    case lists:keyfind(K, 1, Acc) of
                        false   -> case {K, V} of
                                       {resource, [H|_]} when is_integer(H) ->
                                           [{sanitize_key(K), [V]}|Acc];
                                       {resource, R} when is_list(R) ->
                                           [{sanitize_key(K), R}|Acc];
                                       _ ->
                                           [E|Acc]
                                   end;
                        {K, V2} -> Key = sanitize_key(K),
                                   lists:keyreplace(Key, 1, Acc,
                                                    {Key, combine(V, V2)})
                    end
                end, [], Terms).

sanitize_key(resource) -> resources;
sanitize_key(K)        -> K.

glob(Parts) ->
    filelib:wildcard(filename:join(Parts)).

default_profile(BaseDir) ->
    ScratchDir = systest_env:default_scratch_dir(),
    #profile{ name          = default,
              source        = generated,
              output_dir    = ScratchDir,
              log_dir       = filename:join(ScratchDir, "logs"),
              settings_base = filename:join([BaseDir, "test",
                                            "default.settings"]),
              resources     = glob([BaseDir, "test", "*.resource"]),
              targets       = [filename:join(BaseDir, "ebin")] }.
