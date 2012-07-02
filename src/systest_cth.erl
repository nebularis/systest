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
-module(systest_cth).

-include("systest.hrl").
-include_lib("common_test/include/ct.hrl").

-export([id/1, init/2]).
-export([pre_init_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

% -export([on_tc_fail/3]).
% -export([on_tc_skip/3]).

-export([terminate/1]).

-record(state, {
    auto_start :: boolean(),
    suite      :: atom()
}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
  systest.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(systest, _Opts) ->
    case application:start(systest, permanent) of
        {error, {already_started, systest}} -> systest:reset();
        {error, _Reason}=Err                -> Err;
        ok                                  -> ok
    end,
    AutoStart = case application:get_env(systest, auto_start) of
                    undefined -> true;
                    Value     -> Value
                end,
    {ok, #state{auto_start=AutoStart}}.

%% @doc Called before init_per_suite is called, this code might start a
%% cluster, if one is configured for this suite.
pre_init_per_suite(Suite, Config, State=#state{auto_start=false}) ->
    {Config, State#state{suite=Suite}};
pre_init_per_suite(Suite, Config, State) ->
    ct:log("pre_init_per_suite: maybe start ~p", [Suite]),
    %% TODO: handle init_per_suite use of cluster aliases
    {systest:start_suite(Suite, systest:trace_on(Suite, Config)),
                    State#state{suite=Suite}}.

post_end_per_suite(Suite, Config, Result, State) ->
    %% TODO: check and see whether there *is* actually an active cluster
    case ?CONFIG(systest_utils:strip_suite_suffix(Suite), Config, undefined) of
        undefined ->
            ct:log("no configured suite to stop~n");
        ClusterPid ->
            ct:log("stopping ~p~n", [ClusterPid]),
            ct:log("stopped ~p~n",
                   [systest_cluster:stop(ClusterPid)])
    end,
    systest:trace_off(Config),
    {Result, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group, Config, State=#state{auto_start=false}) ->
    {Config, State};
pre_init_per_group(Group, Config, State=#state{suite=Suite}) ->
    {systest:start(Suite, Group, Config), State}.

post_end_per_group(Group, Config, Result, State) ->
    case ?CONFIG(Group, Config, undefined) of
        undefined ->
            {Result, State};
        ClusterPid ->
            {systest_cluster:stop(ClusterPid), State}
    end.

%% @doc Called before each test case.
pre_init_per_testcase(TC, Config, State=#state{auto_start=false}) ->
    {systest:trace_on(TC, Config), State};
pre_init_per_testcase(TC, Config, State=#state{suite=Suite}) ->
    ct:log("~p handling pre_init_per_testcase [~p]~n", [?MODULE, TC]),
    {systest:start(Suite, TC, Config), State}.

post_end_per_testcase(TC, Config, Return, State) ->
    %% TODO: handle {save_config, Config} return values in st:stop
    ct:log("processing post_end_per_testcase: ~p: ~p~n", [TC, Config]),
    case ?CONFIG(TC, Config, undefined) of
        undefined ->
            systest:trace_off(Config),
            {Return, State};
        ClusterPid ->
            case erlang:is_process_alive(ClusterPid) of
                true ->
                    ct:log("stopping ~p~n", [ClusterPid]),
                    ct:log("stopped ~p~n",
                           [systest_cluster:stop(ClusterPid)]);
                false ->
                    ct:log("cluster ~p is already down~n", [ClusterPid])
            end,
            systest:trace_off(Config),
            {Return, State}
    end.

terminate(_State) ->
    ok.
