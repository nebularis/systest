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
-module(systest_error_handling_cth).

-extends(systest_cth).

-include("systest.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

pre_init_per_testcase(TC=sut_start_scripts_badly_configured, Config, State) ->
    case systest_cth:pre_init_per_testcase(TC, Config, State) of
        {{fail, {configuration_not_found, bad_cli}}, _} ->
            systest_log:console("ignoring expected sut start failure~n", []),
            systest_watchdog:clear_exceptions(),
            {Config, State};
        Other ->
            {{fail, Other}, State}
    end;
pre_init_per_testcase(TC, Config, State)
  when TC == failing_proc_on_start_hook orelse
       TC == failing_sut_on_start_hook ->
    case systest_cth:pre_init_per_testcase(TC, Config, State) of
        {{fail,{hook_failed, {local, erlang, error, _}=What, _}},_} ->
            systest_log:console("ignoring expected start failure ~p~n",
                                [What]),
            systest_watchdog:clear_exceptions(),
            {Config, State};
        Other ->
            {{fail, Other}, State}
    end;
pre_init_per_testcase(TC=failing_proc_on_joined_hook,
                      Config, State) ->
    case catch(systest_cth:pre_init_per_testcase(TC, Config, State)) of
        {{fail,{{hook_failed,
                {local, M, F, A}, undef}, _}}, State2} ->
            systest_log:console("ignoring expected sut start failure "
                                "(call to undefined mfa ~p:~p/~p)~n",
                                [M, F, length(A)]),
            case systest_watchdog:exceptions(TC) of
                [{_Sut, crashed, Reason}] ->
                    case Reason of
                        {{hook_failed,
                          {local, M, F, _Args}, undef}, _} ->
                            systest_watchdog:clear_exceptions(),
                            {Config, State2};
                        Other ->
                            {{fail, Other}, State2}
                    end;
                [] ->
                    {{fail, no_exceptions_found}, State2}
            end;
        Other ->
            {{fail, Other}, State}
    end;
pre_init_per_testcase(TC, Config, State) ->
    Return = systest_cth:pre_init_per_testcase(TC, Config, State),
    true = erlang:is_process_alive(whereis(TC)),
    Return.

post_end_per_testcase(TC=timetrap_failure, Config, Return, State) ->
    Pid = whereis(TC),
    true = erlang:is_process_alive(Pid),
    systest_cth:post_end_per_testcase(TC, Config, Return, State),
    undefined = whereis(TC),
    false = erlang:is_process_alive(Pid),
    [] = nodes(),
    systest_log:log(system,
                    "Test Case timetrap_failure is expected to fail, "
                    "however the failure cannot be trapped by common-test; "
                    "this message indicates that all the "
                    "required post-conditions have been fulfilled~n", []),
    {proplists:delete(tc_status, Config), State};
post_end_per_testcase(TC, Config, Return, State) ->
    systest_cth:post_end_per_testcase(TC, Config, Return, State).

post_init_per_suite(_Suite, _Config, Return, State) ->
    {Return, State}.

pre_end_per_suite(_Suite, Config, State) ->
    {Config, State}.
