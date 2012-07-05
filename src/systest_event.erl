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
-module(systest_event).

-include("systest.hrl").
-include_lib("common_test/include/ct_event.hrl").

-export([console/2,
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

descriptor({_Conf, GroupName, _}) ->
    {GroupName, "test case group"};
descriptor(Other) ->
    {Other, "test case"}.

console(Msg, Args) ->
    io:format(user, "[systest] " ++ Msg, Args).

init([]) ->
    {ok, []}.

handle_event(#event{name=start_info, data={Tests,Suites,_Cases}}, State) ->
    console("starting test run (~p tests, ~p suites~n", [Tests, Suites]),
    {ok, State};
handle_event(#event{name=tc_start, data={Suite,FuncOrGroup}}, State) ->
    case FuncOrGroup of
        init_per_suite ->
            console("starting test suite ~p~n", [Suite]);
        end_per_suite ->
            ok;
        {Conf,GroupName,_GroupProperties} ->
            case Conf of
                init_per_group ->
                    console("starting test case group ~p~n", [GroupName]);
                end_per_group ->
                    ok
            end;
        Func ->
            console("starting ~p test case ~p~n", [Suite, Func])
    end,
    {ok, State};
handle_event(#event{name=tc_done,
                    data={_Suite, FuncOrGroup, Result}}, State) ->
    {N, Desc} = descriptor(FuncOrGroup),
    case Result of
        ok ->
            console("~s ~p completed successfully~n", [Desc, N]);
        {skipped, SkipReason} ->
            case SkipReason of
                {require_failed, {_, Key}} ->
                    console("~s ~p failed: "
                            "required config element ~p missing~n",
                            [Desc, N, Key]);
                {failed, {_Suite, init_per_testcase, FailInfo}} ->
                    failed([N, init_per_testcase], Desc, FailInfo)
            end;
        {failed, FailReason} ->
            failed(N, Desc, FailReason)
    end,
    {ok, State};
handle_event(#event{name=tc_auto_skip, data={Suite,Func,Reason}}, State) ->
    console("skipping ~p in ~p: ~p~n", [Func, Suite, Reason]),
    {ok, State};
handle_event(#event{name=test_stats}=Ev, _State) ->
    {ok, Ev};
handle_event(#event{name=test_done}, #event{data={Ok, Failed, Skipped}}=S) ->
    application:set_env(systest, failures, Failed),
    {UserSkipped,AutoSkipped} = Skipped,
    console("test run complete:~n"
            "~p passed, ~p failed, "
            "~p skipped (user), ~p skipped (auto)~n",
            [Ok, Failed, UserSkipped, AutoSkipped]),
    {ok, S};
handle_event(_Message, State) ->
    {ok, State}.

failed(What, Desc, FailInfo) when is_list(What) ->
    failed(list_to_atom(string:join(lists:flatten([
                io_lib:format("~p", [Thing]) || Thing <- What]), "|")),
          Desc, FailInfo);
failed(What, Desc, FailInfo) when is_atom(What) ->
    console("~s ~p failed: ~s~n", [Desc, What,
                                   lists:flatten(fail_info(FailInfo))]).

fail_info({error,FailInfo}) ->
    fail_info(FailInfo);
fail_info({timetrap_timeout, Value}) ->
    io_lib:format("timetrap timeout (~p)", [Value]);
fail_info({failed,{_Suite,end_per_testcase,FailInfo}}) ->
    io_lib:format("end_per_testcase failure: ~s", [fail_info(FailInfo)]);
fail_info({RunTimeError,StackTrace}) ->
    io_lib:format("runtime error: ~p~nstacktrace: ~p",
                  [RunTimeError, StackTrace]);
fail_info(Other) ->
    io_lib:format("~p", [Other]).

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
