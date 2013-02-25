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
%% @end
%% ----------------------------------------------------------------------------
-module(systest_timetraps_cth).

-extends(systest_cth).

-include("systest.hrl").
-include_lib("common_test/include/ct.hrl").

-export([id/1, init/2]).
-export([pre_init_per_suite/3, post_init_per_suite/4]).
-export([pre_end_per_suite/3, post_end_per_suite/4]).
-export([pre_init_per_group/3, post_init_per_group/4]).
-export([pre_end_per_group/3, post_end_per_group/4]).
-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).
-export([terminate/1]).

id(_) ->
    systest.

init(systest, Opts) ->
    systest_cth:init(systest, Opts).

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,State) ->
    systest_cth:pre_init_per_suite(Suite, Config, State).

post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.

post_end_per_suite(Suite,Config,Return,State) ->
    systest_cth:post_end_per_suite(Suite, Config, Return, State).

pre_init_per_group(Group,Config,State) ->
    systest_cth:pre_init_per_group(Group, Config, State).

post_init_per_group(_Group, _Config, Return, State) ->
    {Return, State}.

pre_end_per_group(_Group, Config, State) ->
    {Config, State}.

post_end_per_group(Group, Config, Return, State) ->
    systest_cth:post_end_per_group(Group, Config, Return, State).

pre_init_per_testcase(TC,Config,State) ->
    systest_cth:pre_init_per_testcase(TC, Config, State).

post_end_per_testcase(TC=simple_test_case_pass_through,
                      Config, Return, State) ->
    case systest_cth:post_end_per_testcase(TC, Config, Return, State) of
        {{fail,{error,timeout,[Pid1, Pid2]}}, State2}
          when is_pid(Pid1) andalso is_pid(Pid2) ->
            systest:log("ignoring expected failure of ~p~n", [TC]),
            {proplists:delete(tc_status, Config), State2};
        {OtherReturn, OtherState2} ->
            {{fail, OtherReturn}, OtherState2}
    end;
post_end_per_testcase(TC=hang_on_startup, Config, Return, State) ->
    case systest_cth:post_end_per_testcase(TC, Config, Return, State) of
        {{fail,{timout,{hang_on_startup, _}}}, State2} ->
            systest:log("ignoring expected failure of ~p~n", [TC]),
                {proplists:delete(tc_status, Config), State2};
        {OtherReturn, OtherState2} ->
            {{fail, OtherReturn}, OtherState2}
    end;
post_end_per_testcase(TC, Config, Return, State) ->
    systest_cth:post_end_per_testcase(TC, Config, Return, State).

terminate(State) ->
    systest_cth:terminate(State).
