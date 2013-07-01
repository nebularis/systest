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
-module(systest_rebar_plugin).

-export([systest/2]).

systest(Config, _) ->
    case is_base_dir(Config) of
        true->
            SystestConfig = rebar_config:get_local(Config, systest, []),
            TestConfig = case rebar_config:is_verbose(Config) of
                             true  -> [{logging, "framework"},
                                       {logging, "process"}|SystestConfig];
                             false -> SystestConfig
                         end,
            systest_runner:execute([{error_handler,
                                     fun handle_errors/2}|TestConfig]);
        false ->
            ok
    end.

handle_errors(Fmt, Args) ->
    rebar_utils:abort(Fmt, Args).

is_base_dir(Config) ->
    case erlang:function_exported(rebar_utils, processing_base_dir, 1) of
        true ->
            rebar_utils:processing_base_dir(Config);
        false ->
            GDir = case erlang:function_exported(rebar_config, get_global, 2) of
                       true ->
                           rebar_config:get_global(base_dir, undefined);
                       false ->
                           rebar_config:get_global(Config, base_dir, undefined)
                   end,
            rebar_utils:get_cwd() == GDir
    end.
