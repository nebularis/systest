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
-module(systest).

-include("systest.hrl").

-export([main/1]).
-export([start/0, reset/0, sigkill/1]).
-export([start_suite/2, stop_scope/1, start/2, start/3, stop/1]).
-export([active_sut/1, suts/1, procs/1]).
-export([sut_config/1]).
-export([trace_on/2, trace_off/1]).
-export([interact/2, write_pid_file/0, write_pid_file/1, write_pid_file/2]).

%%
%% Public APIs
%%

%% escript API
main(Args) ->
    systest_main:run(Args).

%% application startup

start() ->
    error_logger:tty(false),
    application:start(?MODULE).

reset() ->
    systest_watchdog:reset(),
    ok.

%% startup/shutdown

start_suite(Suite, Config) ->
    start(systest_utils:strip_suite_suffix(Suite), Config).

stop_scope(Scope) when is_atom(Scope) ->
    systest_watchdog:force_stop(Scope).

start(Scope, Config) ->
    case systest_sut:start(Scope, Config) of
        {error, _}=Err ->
            {fail, {sut_start, Err}};
        Other ->
            Other
    end.

start(Scope, Identify, Config) ->
    case systest_sut:start(Scope, Identify, trace_on(Identify, Config)) of
        {error, _}=Err ->
            {fail, {sut_start, Err}};
        Other ->
            Other
    end.

stop(Scope) when is_pid(Scope) ->
    systest_sut:stop(Scope).

%% tracing/debugging

trace_on(Scope, Config) ->
    systest_trace:debug(Scope, Config).

trace_off(Config) ->
    systest_trace:stop(Config).

%% instrumentation

%% interactions

sigkill(Pid) ->
    ct:log("executing kill -9 ~s~n", [Pid]),
    Result = os:cmd("kill -9 " ++ Pid),
    ct:log(Result).

interact(Proc, Inputs) ->
    systest_proc:interact(Proc, Inputs).

write_pid_file() ->
    application:load(?MODULE),
    case application:get_env(?MODULE, scratch_dir) of
        {ok, {file, Path}} ->
            Pid = os:getpid(),
            write_pid_file(filename:join(Path, Pid ++ ".pid"));
        Other ->
            {error, {env, Other}}
    end.

write_pid_file(Path) ->
    Pid = os:getpid(),
    file:write_file(Path, Pid, [write]).

write_pid_file(Name, {dir, Dir}) ->
    Pid = os:getpid(),
    File = filename:join(Dir, Name),
    file:write_file(File, Pid, [write]).

%% config handling

active_sut(Config) ->
    ?REQUIRE(active, Config).

suts(Config) ->
    systest_config:read(?MODULE, Config).

procs(SutRef) ->
    systest_sut:procs(SutRef).

sut_config(Scope) ->
    ct:get_config({Scope, sut}).

