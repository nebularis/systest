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
-module(systest_supervision_cth).

-include("systest.hrl").
-include_lib("common_test/include/ct.hrl").

-export([id/1, init/2]).
-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).
-export([terminate/1]).

-record(ctx, {active}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    systest_supervision.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    ct:log("intialising ~p~n", [?MODULE]),
    {ok, #ctx{}}.

pre_init_per_testcase(TC, Config, State) ->
    Active = ?CONFIG(TC, Config, undefined),
    ct:log("active cluster: ~p~n", [Active]),
    {Config, State#ctx{active={TC, Active}}}.

%% NB: this cheecky clause is being used to pass a failing test - there are few
%% other ways to test the behaviour of common test hooks without performing
%% incantations of this ilk
post_end_per_testcase(should_fail, Config,
                      {skip,{failed,
                        {systest_supervision_SUITE,init_per_testcase,
                        {error, {already_started, _Pid}}}}}, State) ->
    {Config, State};
post_end_per_testcase(TC, Config, Return,
                      State=#ctx{active={TC, Active}}) when is_pid(Active) ->
    %% the end_per_testcase implementation in the SUITE *should* have shut down
    %% the cluster, so here we simply assert that this is the case...
    case ?CONFIG(TC, Config, undefined) of
        undefined ->
            %% the end_per_testcase implementation in the SUITE *should* have
            %% shut down the cluster, so here we assert that this is the case..
            case erlang:is_process_alive(Active) of
                false ->
                    ct:log("cluster process is already dead! pass~n"),
                    {Return, State};
                true ->
                    case systest_watchdog:exceptions(TC) of
                        [] ->
                            ct:log("cluster process is still "
                                   "alive, with no exception~n"),
                            {Return, State};
                        Ex ->
                            ErrMsg =
                            io_lib:format("Unexpected shutdown failures: ~p~n",
                                          [Ex]),
                            {{fail, ErrMsg}, State}
                    end
            end;
        ClusterPid ->
            ct:log("ignoring active cluster ~p~n", [ClusterPid]),
            %% we don't actually *care* about this case, because the user
            %% defined end_per_testcase in the SUITE only shuts down the
            %% cluster for a specific subset of the test cases, and all the
            %% others will be handled by the default systest ct-hook.
            {Return, State}
    end;
post_end_per_testcase(TC, _Config, Return, State) ->
    ct:log("ignoring testcase ~p~n", [TC]),
    {Return, State}.

%% TODO: test group shutdown here....


terminate(_State) ->
    ok.

