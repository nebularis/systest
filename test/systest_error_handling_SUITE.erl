%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com).
%%
%% Some portions of the code taken from sh (c) 2005 - 2012 Nebularis
%% Some portions of the code taken from rebar (c) 2010 Dave Smith
%% Some portions of the code taken from retest (c) 2010 Dave Smith
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
%% @doc Error Handling Test Suite
%% This common test suite provides test cases that are configured to
%% deliberately fail, allow us to verify the behaviour of our actual common test
%% hooks by extending them and checking for expected error conditions.
%% ----------------------------------------------------------------------------
-module(systest_error_handling_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/systest.hrl").
-compile(export_all).

%%
%% Config/Fixture Definitions
%%

suite() -> [{timetrap, {minutes, 2}}].

all() ->
    %% the timetrap_failure test *must* run separately
    %% from the rest of the SUITE, as the error it generates
    %% cannot be handled even by a ct_hook, and therefore the
    %%
    [sut_start_scripts_badly_configured,
     failing_proc_on_start_hook,
     failing_sut_on_start_hook,
     failing_proc_on_joined_hook].

%%
%% Test Case Definitions
%%

timetrap_failure() ->
    [{timetrap, {seconds, 10}}].

timetrap_failure(_) ->
    receive never_arrives -> ok end,
    ok.

sut_start_scripts_badly_configured() ->
    [{userdata, [{doc, "Attempts to start a cli node for a non-existent script."
                       "This testcase should never run, being simply "
                       "a place-holder for an init_per_testcase that "
                       "we expect to fail.\n"
                       "The validation of this failure is performed in "
                       "the systest_supervision_cth common test hook!"}]}].

sut_start_scripts_badly_configured(_) ->
    ok.

failing_proc_on_start_hook() ->
    [{userdata, [{doc, "Fires off a deliberately failing proc on_start hook."
                       "This testcase should never run, being simply "
                       "a place-holder for an init_per_testcase that "
                       "we expect to fail.\n"
                       "The validation of this failure is performed in "
                       "the systest_supervision_cth common test hook!"}]}].

failing_proc_on_start_hook(_) ->
    ok.

failing_sut_on_start_hook() ->
    [{userdata, [{doc, "Fires off a deliberately failing sut on_start hook."
                       "This testcase should never run, being simply "
                       "a place-holder for an init_per_testcase that "
                       "we expect to fail.\n"
                       "The validation of this failure is performed in "
                       "the systest_supervision_cth common test hook!"}]}].

failing_sut_on_start_hook(_) ->
    ok.

failing_proc_on_joined_hook() ->
    [{userdata, [{doc, "Fires off a deliberately failing proc on_joined hook."
                       "This testcase should never run, being simply "
                       "a place-holder for an init_per_testcase that "
                       "we expect to fail.\n"
                       "The validation of this failure is performed in "
                       "the systest_supervision_cth common test hook!"}]}].

failing_proc_on_joined_hook(_) ->
    ok.
