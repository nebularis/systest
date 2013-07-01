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
%% @doc Common Test Hook
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
-export([terminate/1]).

-import(systest_log, [log/2, log/3, framework/2]).

%% TODO: implement systest_sut_lifecycle and move
%% the test outcome/state tracking there instead

-record(state, {
    auto_start :: boolean(),
    aggressive :: boolean() | integer(),
    setup      :: integer(),
    teardown   :: integer(),
    suite      :: atom(),
    failed     :: integer(),
    skipped    :: integer(),
    passed     :: integer()
}).

-define(SOURCE, 'systest-common-test-hooks').
-define(AGGRESSIVE_SHUTDOWN_MAX_WAIT, 20000).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    systest.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(systest, Opts) ->
    case application:start(systest, permanent) of
        {error, {already_started, systest}} -> systest:reset();
        {error, _Reason}=Err                -> Err;
        ok                                  -> ok
    end,
    AutoStart = case application:get_env(systest, auto_start) of
                    undefined -> true;
                    Value     -> Value
                end,
    systest_results:test_run(?SOURCE),
    SetupTimetrap = ?CONFIG(setup_timetrap, Opts, infinity),
    TeardownTimetrap = ?CONFIG(teardown_timetrap, Opts, infinity),
    Timeout = systest_utils:time_to_ms(TeardownTimetrap),
    Aggressive = case ?CONFIG(aggressive_teardown, Opts, false) of
                     {_, _}=T -> systest_utils:time_to_ms(T);
                     Other    -> Other
                 end,
    {ok, #state{auto_start=AutoStart,
                aggressive=Aggressive,
                setup=SetupTimetrap,
                teardown=Timeout,
                failed=0,
                skipped=0,
                passed=0}}.

%% @doc Called before init_per_suite is called, this code might start a
%% SUT, if one is configured for this suite.
pre_init_per_suite(Suite, Config, State=#state{auto_start=false}) ->
    {Config, State#state{suite=Suite}};
pre_init_per_suite(Suite, Config,
                   State=#state{setup=STT}) ->
    log(framework, "pre_init_per_suite: maybe start ~p~n", [Suite]),
    %% TODO: handle init_per_suite use of SUT aliases
    Config2 = [{setup_timetrap, STT}|Config],
    {systest:start_suite(Suite, systest:trace_on(Suite, Config2)),
                    State#state{suite=Suite}}.

post_end_per_suite(Suite, Config, Result,
                   State=#state{failed=Failed,
                                skipped=Skipped,
                                passed=Passed}) ->
    systest_results:add_results(?SOURCE, Passed, Skipped, Failed),
    %% TODO: check and see whether there *is* actually an active SUT
    What = systest_utils:strip_suite_suffix(Suite),
    {stop(What, State, Config, Result), State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group, Config, State=#state{auto_start=false}) ->
    {Config, State};
pre_init_per_group(Group, Config,
                   State=#state{suite=Suite, setup=STT}) ->
    Config2 = [{setup_timetrap, STT}|Config],
    {systest:start(Suite, Group, Config2), State}.

post_end_per_group(Group, Config, Result, State) ->
    stop(Group, State, Config, Result).

%% @doc Called before each test case.
pre_init_per_testcase(TC, Config, State=#state{auto_start=false}) ->
    {systest:trace_on(TC, Config), State};
pre_init_per_testcase(TC, Config,
                      State=#state{suite=Suite, setup=STT}) ->
    log(framework, "handling ~p pre_init_per_testcase~n", [TC]),
    Config2 = [{setup_timetrap, STT}|Config],
    {systest:start(Suite, TC, Config2), State}.

post_end_per_testcase(TC, Config, Result, State) ->
    log(framework, "processing ~p post_end_per_testcase~n", [TC]),
    stop(TC, State, Config, Result).

terminate(_) ->
    ok.

stop(Target, State=#state{aggressive=Aggressive, teardown=Timeout},
     Config, Result) ->
    log(framework, "~p returned ~p. Stopping....~n", [Target, Result]),
    {Result2, State2} = check_exceptions(Target, Result, State),
    possibly_generate_killer(Target, Aggressive),  %% TODO: if this fails?
    try
        %% TODO: check the shutdown succedded, otherwise fail!
        case systest:stop_scope(Target, Timeout) of
            ok ->
                {Result2, State2};
            Stop ->
                Err = case Stop of
                          {error,timeout,Data} -> {timeout, Data};
                          _                    -> Stop
                      end,
                log(framework, "stopped: ~p~n", [Err]),
                {{fail, {shutdown_error, Err}},
                 State2#state{failed=fail_count(State, State2)}}
        end
    catch
        %% a failure in the sut stop procedure should cause the test to fail,
        %% so that the operator has a useful indication that all is not well
        _:Error ->
            log(system, "shutdown error: ~p~n~p~n",
                [Error, erlang:get_stacktrace()]),
            {{fail, {shutdown_error, Error}},
             State2#state{failed=fail_count(State, State2)}}
    after
        systest:trace_off(Config)
    end.

fail_count(State, State2) ->
    erlang:min(State#state.failed, State2#state.failed).

possibly_generate_killer(_, false) ->
    ok;
possibly_generate_killer(Target, true) ->
    possibly_generate_killer(Target, ?AGGRESSIVE_SHUTDOWN_MAX_WAIT);
possibly_generate_killer(Target, Timeout) ->
    timer:kill_after(Timeout, Target).

check_exceptions(SutId, Return,
                 State=#state{failed=F, skipped=S, passed=P}) ->
    log(framework, "checking for out of band exceptions in ~p~n", [SutId]),
    case systest_watchdog:exceptions(SutId) of
        [] ->
            case Return of
                {failed, {_M, _F, {'EXIT', _}=Exit}} ->
                    {Exit, State#state{failed=F + 1}};
                {'EXIT',{{_, _}, _}} ->
                    {Return, State#state{failed=F + 1}};
                {error, _What} ->
                    {Return, State#state{failed=F + 1}};
                {timetrap_timeout, _} ->
                    {Return, State#state{failed=F + 1}};
                {skip, _Reason} ->
                    {Return, State#state{skipped=S + 1}};
                _ ->
                    {Return, State#state{passed=P + 1}}
            end;
        Ex ->
            log(system,
                "ERROR ~p: unexpected process exits "
                "detected - see test log(s) for details~n",
                [SutId]),

            [begin
                 framework("ERROR: ~p saw exit: ~p~n ", [SutId, Reason])
             end || {_, _, Reason} <- Ex],

            Failures = case Ex of
                           [E] -> E;
                           _   -> Ex
                       end,
            {{fail, {Return, Failures}}, State#state{failed=F + 1}}
    end.
