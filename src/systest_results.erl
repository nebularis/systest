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
-module(systest_results).

-export([init/0, reconciled_test_results/0,
         test_results/1, add_failed/2,
         add_passed/2, add_skipped/2, reset/0, test_run/1,
         add_results/4]).

-include("systest.hrl").

init() ->
    ets:new(?MODULE, [set, named_table, public, {keypos, 2}]),
    reset(),
    ignore.

reset() ->
    true = ets:delete_all_objects(?MODULE),
    test_run(?EV_SOURCE),
    ok.

reconciled_test_results() ->
    ets:foldl(fun(#results{source=InSrc,
                           passed=InPass,
                           failed=InFail,
                           skipped=InSkip},
                  #results{source=AccSrc,
                           passed=AccPass,
                           failed=AccFail,
                           skipped=AccSkip}=Acc) ->
                      Pass = choose_result({passed, min}, InSrc,
                                           InPass, AccSrc, AccPass),
                      Fail = choose_result({failed, max}, InSrc,
                                           InFail, AccSrc, AccFail),
                      Skip = choose_result({skipped, max}, InSrc,
                                           InSkip, AccSrc, AccSkip),
                      Acc#results{passed=Pass,
                                  failed=Fail,
                                  skipped=Skip}
              end, #results{source=reconcilliation}, ?MODULE).

test_run(Source) ->
    true = ets:insert_new(?MODULE, #results{source=Source}).

test_results(Source) ->
    [Results] = ets:lookup(?MODULE, Source),
    Results.

add_results(Source, Passed, Skipped, Failed) ->
    %% we're not overly concerned with efficiency here
    P = add_passed(Source, Passed),
    S = add_skipped(Source, Skipped),
    F = add_failed(Source, Failed),
    {P, S, F}.

add_passed(Source, Incr) ->
    ets:update_counter(?MODULE, Source, {3, Incr}).

add_failed(Source, Incr) ->
    ets:update_counter(?MODULE, Source, {4, Incr}).

add_skipped(Source, Incr) ->
    ets:update_counter(?MODULE, Source, {5, Incr}).

choose_result({Type, FN}, InSrc, InVal, AccSrc, AccVal) ->
    if InVal /= AccVal ->
            systest_log:framework(
              "WARNING: test results have diverged: "
              "taking the ~p value for ~p from (~p: ~p, ~p: ~p)~n",
              [FN, Type, InSrc, InVal, AccSrc, AccVal]),
            erlang:apply(erlang, FN, [InVal, AccVal]);
       true ->
            InVal
    end.

