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
-module(systest_test_run).

-behaviour(gen_server).

-export([start/1, start/2, start_link/2, start/3, start_link/3]).
-export([stop/1, stop/2]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-import(systest_log, [log/2, log/3]).

-include("systest.hrl").

-record(testcase, {
    name    :: atom(),
    %% TODO: do we want to deal with timings here, or let the frameworks do it?
    
}).

-record(state, {
    auto_start = true       :: boolean(),
    suites     = dict:new() :: dict()
}).


%%
%% gen_server callbacks
%%

init([]) ->
    ignore.

handle_call({start_suite, Suite, Data}, _From, State) ->
    log(framework, "pre_init_per_suite: maybe start ~p~n", [Suite]),
    Config2 = systest:start_suite(Suite, systest:trace_on(Suite, Config)),
    {reply, {ok, Config2}, State}.


handle_cast({stop_suite, Suite, Data, Result}, State) ->
    case ?CONFIG(systest_utils:strip_suite_suffix(Suite), Data, undefined) of
        undefined ->
            log(framework, "no configured suite to stop~n");
        SutPid ->
            log(framework, "stopping ~p~n", [SutPid]),
            log(framework, "~p~n", [systest_sut:stop(SutPid)])
    end,
    systest:trace_off(Config),
    {noreply, State}.

handle_info(What, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

