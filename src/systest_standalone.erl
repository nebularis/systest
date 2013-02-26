%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% ----------------------------------------------------------------------------
%%
%% Copyright (c) 2005 - 2012 Nebularis.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
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
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
%% @hidden
%% ----------------------------------------------------------------------------
-module(systest_standalone).

-behaviour(systest_runner).
-behaviour(gen_server).

-include("systest.hrl").

-export([start/0, stop/0]).

%% systest_runner Exports

-export([dryrun/1, run/1, run/2]).

%% OTP gen_server Exports

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(testrun, {client, spec, sut}).

-import(systest_log, [log/3]).
-import(systest_utils, [record_to_proplist/2]).

%%
%% Public API
%%

dryrun(RunSpec) ->
    run_it(RunSpec, true).

run(RunSpec) ->
    run_it(RunSpec, false).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

run(Pid, RunSpec) ->
    gen_server:call(Pid, {run, RunSpec}, infinity).

%%
%% OTP gen_server API
%%

init([]) ->
    process_flag(trap_exit, true),
    {ok, #testrun{}}.

handle_call({run, RunSpec}, From, State) ->
    code:ensure_loaded(timer),
    BaseConfig = systest_runner:get(base_config, RunSpec),
    SutId = list_to_atom(?REQUIRE(sut, BaseConfig)),
    State2 = State#testrun{client=From,
                           spec=RunSpec,
                           sut=SutId},
    Profile = systest_runner:get(profile, RunSpec),
    ScratchDir = systest_profile:get(output_dir, Profile),
    Dir = filename:join(ScratchDir,
                        "standalone-" ++
                            systest_env:timestamp()),
    filelib:ensure_dir(filename:join(Dir, "foo")),
    Config = [{priv_dir, Dir}|BaseConfig],
    case systest_sut:start(SutId, SutId, Config) of
        {error, Reason} ->
            {stop, Reason, State2};
        Other when is_list(Other) ->
            case ?CONFIG(active, Other, undefined) of
                none ->
                    {stop, noconfig, State2};
                undefined ->
                    {stop, unknown_error, State2};
                Pid when is_pid(Pid) ->
                    case ?CONFIG(shell, BaseConfig, false) of
                        false ->
                            TimeTrap = get_default_timetrap(Profile),
                            Timeout = systest_utils:time_to_ms(TimeTrap),
                            systest_log:framework("time trap set to ~p~n",
                                                  [Timeout]),
                            erlang:send_after(Timeout, self(), timeout);
                        true ->
                            ok
                    end,
                    {reply, ok, State2};
                Other ->
                    io:format(user, "Then what the fuck is ~p~n", [Other]),
                    {stop, what_the_fuck, State}
            end
    end;
handle_call(stop, _From, State) ->
    {stop, shutdown, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    systest_log:framework("stand-alone sut runner timed out~n", []),
    {stop, timeout, State};
handle_info({'EXIT', Pid, shutdown}, State) ->
    systest_log:framework("~p shutdown detected~n", [Pid]),
    {stop, shutdown, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(shutdown, #testrun{client=Client, sut=SutId}) ->
    systest_watchdog:force_stop(SutId),
    case systest_watchdog:exceptions(SutId) of
        [] ->
            gen_server:reply(Client, {finished, self(), ok});
        Ex ->
            log(system,
                "ERROR ~p failed - unexpected process exits detected:~n",
                [SutId]),
            [begin
                 log(system, "ERROR: ~p saw exit: ~p~n ", [SutId, Reason])
             end || {_, _, Reason} <- Ex],

            Failures = case Ex of
                           [E] -> E;
                           _   -> Ex
                       end,
            gen_server:reply(Client, {finished, self(), {failures, Failures}})
    end,
    ok;
terminate(Reason, #testrun{sut=SutId, client=Client}) ->
    systest_watchdog:force_stop(SutId),
    gen_server:reply(Client, {finished, self(), {error, Reason}}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal API
%%

get_default_timetrap(Profile) ->
    case systest_profile:get(execution_timetrap, Profile) of
        undefined -> {minutes, 1};
        Value -> Value
    end.

run_it(RunSpec, DryRun) ->
    Quiet = systest_runner:get(quiet, RunSpec),
    TestFun = if DryRun =:= false -> fun run_test/2;
                 true  -> fun print_test/2
              end,

    case TestFun(RunSpec, Quiet) of
        {error, _}=Error ->
            Error;
        ok ->
            ok;
        Other ->
            {error, Other}
    end.

run_test(RunSpec, Quiet) ->
    case Quiet of
        true  -> ok;
        false -> print_test_data(RunSpec)
    end,
    {ok, Pid} = start(),
    await_test_result(Pid, RunSpec).

await_test_result(Pid, RunSpec) ->
    erlang:monitor(process, Pid),
    case run(Pid, RunSpec) of
        ok ->
            receive
                {_Ref, {finished, Pid, Result}} ->
                    Result;
                {'DOWN', _Ref, process, Pid, Reason} ->
                    {error, {died, Reason}};
                Other ->
                    {error, Other}
            end;
        {finished, Pid, Result} ->
            Result
    end.

print_test(_, _) ->
    ok.

print_test_data(RunSpec) ->
    Profile = systest_runner:get(profile, RunSpec),
    Name = systest_profile:get(name, Profile),
    Label = lists:flatten(io_lib:format("Starting ~s", [Name])),
    Properties = record_to_proplist(RunSpec, systest_runner),
    Properties2 = lists:keydelete(profile, 1, Properties),
    Properties3 = lists:keydelete(base_config, 1, Properties2),
    systest_utils:print_section(Label, Properties3).
