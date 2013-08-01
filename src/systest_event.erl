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
%% @doc Common Test Event Handler
%% ----------------------------------------------------------------------------
-module(systest_event).

-include("systest.hrl").
-include_lib("common_test/include/ct_event.hrl").

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-import(systest_log, [console/2,
                      framework/2]).

descriptor({Conf, GroupName, _}) ->
    {Conf, io_lib:format("group ~p", [GroupName])};
descriptor(Other) ->
    {Other, "test case"}.

init([]) ->
    {ok, []}.

handle_event(#event{name = start_logging}, State) ->
    %% Since R16B01 - logging to test_server outside of a commont_test
    %% run will crash the system, so we delay adding the logger until now
    systest_ct_log:start(),
    {ok, State};
handle_event(#event{name = stop_logging}, State) ->
    systest_ct_log:stop(),
    {ok, State};
handle_event(#event{name=tc_start, data={Suite,FuncOrGroup}}, State) ->
    case FuncOrGroup of
        init_per_suite ->
            systest_log:framework("starting test suite ~p~n", [Suite]);
        end_per_suite ->
            ok;
        {Conf,GroupName,_GroupProperties} ->
            case Conf of
                init_per_group ->
                    framework("starting test case group ~p~n", [GroupName]);
                end_per_group ->
                    ok
            end;
        Func ->
            framework("starting ~p test case ~p~n", [Suite, Func])
    end,
    {ok, State};
handle_event(#event{name=tc_done,
                    data={_Suite, FuncOrGroup, Result}}, State) ->
    {N, Desc} = descriptor(FuncOrGroup),
    case Result of
        {skipped, SkipReason} ->
            case SkipReason of
                {require_failed, {_, Key}} ->
                    systest_results:add_skipped(?EV_SOURCE, 1),
                    console("~s ~p failed: "
                            "required config element ~p missing~n",
                            [Desc, N, Key]);
                {failed, {_Suite, init_per_testcase, FailInfo}} ->
                    systest_results:add_failed(?EV_SOURCE, 1),
                    failed([N, init_per_testcase], Desc, FailInfo)
            end;
        {failed, FailReason} ->
            systest_results:add_failed(?EV_SOURCE, 1),
            failed(N, Desc, FailReason);
        {framework_error, Other} ->
            systest_results:add_failed(?EV_SOURCE, 1),
            failed(N, Desc, Other);
        _ ->
            systest_results:add_passed(?EV_SOURCE, 1),
            LogF = case is_ct_wrap_function(N) of
                       true -> fun systest_log:framework/2;
                       false -> fun systest_log:console/2
                   end,
            LogF("~s ~p completed successfully~n", [Desc, N])
    end,
    {ok, State};
handle_event(#event{name=tc_auto_skip, data={Suite,Func,Reason}}, State) ->
    console("skipping ~p in ~p: ~p~n", [Func, Suite, Reason]),
    {ok, State};
handle_event(#event{name=test_stats}=Ev, _State) ->
    {ok, Ev};
handle_event(#event{name=test_done},
             #event{data={Passed, Failed, SkipSet}}=S) ->
    %% sometimes test results aren't passed to the listener properly
    %% or are incomplete - thanks common_test, for making this such a mess...
    EvSource = 'systest_ev_final',
    systest_results:test_run(EvSource),
    Skipped = case SkipSet of
                  Skip when is_integer(Skip) -> Skip;
                  {UserSkip, AutoSkip}       -> UserSkip + AutoSkip
              end,
    systest_results:add_results(EvSource, Passed, Skipped, Failed),
    {ok, S};
handle_event(_Message, State) ->
    {ok, State}.

failed(What, Desc, FailInfo) when is_list(What) ->
    failed(list_to_atom(string:join(lists:flatten([
                io_lib:format("~p", [Thing]) || Thing <- What]), "|")),
          Desc, FailInfo);
failed(What, Desc, FailInfo) when is_atom(What) ->
    console("~s ~p failed.~n", [Desc, What]),
    systest_log:log(framework,
                    "~s~n", [lists:flatten(fail_info(FailInfo))]).

fail_info({error,FailInfo}) ->
    fail_info(FailInfo);
fail_info({timetrap_timeout, Value}) ->
    io_lib:format("timetrap timeout (~p)", [Value]);
fail_info({failed,{_Suite,end_per_testcase,FailInfo}}) ->
    io_lib:format("end_per_testcase failure: ~s", [fail_info(FailInfo)]);
fail_info(Other) ->
    io_lib:format("Fail Info ~p", [Other]).

is_ct_wrap_function(init_per_testcase)  -> true;
%is_ct_wrap_function(init_per_group)     -> true;
is_ct_wrap_function(init_per_suite)     -> true;
is_ct_wrap_function(end_per_testcase)   -> true;
%is_ct_wrap_function(end_per_group)      -> true;
is_ct_wrap_function(end_per_suite)      -> true;
is_ct_wrap_function(_)                  -> false.

%%
%% @private
%%
handle_call(_, State) ->
    {ok, ignored, State}.

%%
%% @private
%%
handle_info(_Info, State) ->
    {ok, State}.

%%
%% @private
%%
terminate(_Reason, _State) ->
    ok.

%%
%% @private
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
