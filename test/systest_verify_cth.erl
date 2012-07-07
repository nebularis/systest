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
%% @doc Module systest_verify_cth
%% This module extends the default common test hook (systest_cth) to allow for
%% expected failing test cases.
%% ----------------------------------------------------------------------------
-module(systest_verify_cth).

-extends(systest_cth).

-include("systest.hrl").
-include_lib("common_test/include/ct.hrl").

-export([pre_init_per_testcase/3,
         post_init_per_suite/4,
         pre_end_per_suite/3]).

pre_init_per_testcase(TC, Config, State) ->
    case systest_cth:pre_init_per_testcase(TC, Config, State) of
        {{fail,{cluster_start,{error,_}}},_}=Err ->
            systest_event:console("ignoring expected cluster start failure~n",[]),
            systest_watchdog:clear_exceptions(),
            {Config, State};
        Other ->
            {{fail, [{expected, fail},{actual, Other}]}, State}
    end.

post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.
