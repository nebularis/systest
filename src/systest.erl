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
%% @doc This is the primary interface to the SysTest application.
%% ----------------------------------------------------------------------------
-module(systest).

-include("systest.hrl").

-export([main/1, get_system_under_test/1]).
-export([start/0, stop/0, reset/0, sigkill/1]).
-export([start_suite/2, stop_scope/1, start/2, start/3, stop/1]).
-export([active_sut/1, suts/1, procs/1]).
-export([trace_on/2, trace_off/1]).
-export([interact/2, write_pid_file/0, write_pid_file/1, write_pid_file/2]).
-export([list_processes/1, process_data/2, process_activity_state/1,
         read_process_user_data/1]).
-export([write_process_user_data/2, restart_process/2, stop_and_wait/1]).
-export([activate_process/1, stop_no_wait/1, kill_no_wait/1]).
-export([settings/0, settings/1, config/1, env/1]).
-export([kill_after/2, kill_after/3, kill_and_wait/1]).
-export([print_sut_info/1, log_sut_info/1, log/1, log/2]).

-ignore_xref([{start, 2},
              {procs, 1},
              {write_pid_file, 1}]).

%%
%% Public APIs
%%

%% @hidden escript API
main(Args) ->
    systest_main:run(Args).

%% application startup

%% @doc Starts the systest OTP application.
%% @end
start() ->
    %% TODO: Don't make this assumption, but *do* by default! :O
    error_logger:tty(false),
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

%% @doc resets the application state
%% @end
reset() ->
    %% both these operations are synchronous
    systest_watchdog:reset(),
    systest_results:reset(),
    ok.

%% startup/shutdown

%% @doc starts any resources associated with the supplied Suite.
%% @end
start_suite(Suite, Config) ->
    start(systest_utils:strip_suite_suffix(Suite), Config).

%% @doc immediately stops any resources associated with the supplied Scope
%% @end
stop_scope(Scope) when is_atom(Scope) ->
    systest_watchdog:force_stop(Scope).

%% @doc starts any resources associated with the given scope
%% @end
start(Scope, Config) ->
    try systest_sut:start(Scope, Config) of
        {error, _}=Err ->
            {fail, {system_under_test, start, Err}};
        Other ->
            Other
    catch
        What -> {fail, {system_under_test, start, What}}
    end.

%% @doc starts any resources associated with the given scope, using the
%% provided identity when registering them with the testing framework
%% @end
start(Scope, Identify, Config) ->
    try systest_sut:start(Scope, Identify, trace_on(Identify, Config)) of
        {error, _}=Err ->
            {fail, {system_under_test, start, Err}};
        Other ->
            Other
    catch
        What -> {fail, {system_under_test, start, What}}
    end.

%% @doc politely asks any resources associated with the given scope to stop,
%% given them time to clean up and exit without causing any test case to fail
%% @end
stop(Scope) when is_pid(Scope) ->
    systest_sut:stop(Scope).

%% config/settings

settings() ->
    case systest_config:get_static(settings) of
        {settings, Settings} -> Settings;
        _                    -> []
    end.

settings(Key) when is_atom(Key) ->
    systest_config:read(Key, settings());
settings(Key) when is_list(Key) ->
    case lists:member($., Key) of
        true  -> systest_config:eval(Key, settings());
        false -> systest_config:read(Key, settings())
    end.

config(Key) ->
    systest_config:get_config(Key).

env(Key) ->
    systest_config:get_env(Key).

%% tracing/debugging

%% @doc Enables trace debugging. See the
%% <a href="https://github.com/nebularis/systest/wiki/tracing"><em>Tracing/Debugging Guide</em></a>
%% for more details.
%% @end
trace_on(Scope, Config) ->
    systest_trace:debug(Scope, Config).

%% @doc Disables trace debugging. See the
%% <a href="https://github.com/nebularis/systest/wiki/tracing"><em>Tracing/Debugging Guide</em></a>
%% for more details.
%% @end
trace_off(Config) ->
    systest_trace:stop(Config).

%% instrumentation

%% interactions

activate_process(ProcRef) ->
    systest_proc:activate(ProcRef).

%% @doc Restarts the specified {@link systest_proc. <em>Process</em>},
%% which must be registered with the supplied reference to a
%% {@link systest_sut. <em>System Under Test</em>}.
%% @end
restart_process(SutRef, ProcRef) ->
    systest_sut:restart_proc(SutRef, ProcRef).

%% @doc Sends a SIGKILL signal to the provided (OS) process id
%% @end
sigkill(Pid) ->
    systest_log:log(framework, "executing kill -9 ~s~n", [Pid]),
    Result = os:cmd("kill -9 " ++ Pid),
    systest_log:log(framework, Result).

%% @doc Stops the Systest (Operating) Process <em>ProcRef</em>
%% @end
stop_no_wait(ProcRef) ->
    systest_proc:stop(ProcRef).

%% @doc Stops the Systest (Operating) Process <em>ProcRef</em>
%% @end
kill_no_wait(ProcRef) ->
    systest_proc:kill(ProcRef).

%% @doc Instructs the {@link systest_proc. <em>Process</em>} to stop and waits
%% until it has completed its shutdown and fully stopped. Stopping a
%% {@link systest_proc. <em>Process</em>} in this way does <em>not</em> cause
%% a test case to fail, as the {@link systest_sut. <em>System Under Test</em>}
%% handler is informed that the shutdown is expected.
%% @end
stop_and_wait(ProcRef) ->
    systest_proc:stop_and_wait(ProcRef).

%% @doc Kills the {@link systest_proc. <em>Process</em>} and waits until it has
%% fully stopped. Killing a {@link systest_proc. <em>Process</em>} in this way
%% does <em>not</em> cause a test case to fail, as the
%% {@link systest_sut. <em>System Under Test</em>} handler is informed that the
%% shutdown is expected.
%% @end
kill_and_wait(ProcRef) ->
    systest_proc:kill_and_wait(ProcRef).

%% @doc Sets up a timer that will {@link systest_proc:kill/1. <em>kill</em>}
%% the {@link systest_proc. <em>Process</em>} once it expires, and returns
%% immediately. Killing a {@link systest_proc. <em>Process</em>} in this way
%% does <em>not</em> cause a test case to fail, as the
%% {@link systest_sut. <em>System Under Test</em>} handler is informed that the
%% shutdown is expected.
%% @end
kill_after(Timeout, Target) ->
    systest_proc:kill_after(Timeout, Target).

%% @hidden TODO: consider making this private
kill_after(TimeoutMs, Target, Killer) ->
    systest_proc:kill_after(TimeoutMs, Target, Killer).

%% @hidden this is really a placeholder until the <em>Activity</em> and
%% <em>Scripting</em> capabilities have been implemented, so best not to
%% expose the documentation right now.
interact(Proc, Inputs) ->
    systest_proc:interact(Proc, Inputs).

%% @hidden - do we actually need to use this any more?
write_pid_file() ->
    application:load(?MODULE),
    case application:get_env(?MODULE, scratch_dir) of
        {ok, {file, Path}} ->
            Pid = os:getpid(),
            write_pid_file(filename:join(Path, Pid ++ ".pid"));
        Other ->
            {error, {env, Other}}
    end.

%% @hidden - do we actually need to use this any more?
write_pid_file(Path) ->
    Pid = os:getpid(),
    file:write_file(Path, Pid, [write]).

%% @hidden - do we actually need to use this any more?
write_pid_file(Name, {dir, Dir}) ->
    Pid = os:getpid(),
    File = filename:join(Dir, Name),
    file:write_file(File, Pid, [write]).

%% config handling

%% @doc Returns the {@link systest_sut. <em>System Under Test</em>} associated
%% with the supplied configuration. <em>NB: this function fails the active
%% test case if no SUT is registered with the supplied Config!</em>
%% @end
get_system_under_test(Config) ->
    active_sut(Config).

%% @hidden - TODO: deprecate into get_system_under_test
active_sut(Config) ->
    ?REQUIRE(active, Config).

%% @hidden - TODO: deprecate
suts(Config) ->
    systest_config:read(?MODULE, Config).

%% @doc Lists all {@link systest_proc. <em>Processes</em>} that are part of the
%% given {@link systest_sut. <em>System Under Test</em>}
%% @end
list_processes(SutRef) ->
    procs(SutRef).

%% @hidden
procs(SutRef) when is_record(SutRef, sut) ->
    systest_sut:get(procs, SutRef);
procs(SutRef) ->
    systest_sut:procs(SutRef).

%% @doc Returns data for the required Field from a
%% {@link systest_proc. <em>Process</em>} record or <em>ProcRef</em>.
%% The version of this function taking a <em>proc</em> record is
%% intended for use in handler/user callbacks, whilst the version that takes
%% a process identifier (pid) is intended for remote calls.
%% @end
process_data(Field, ProcRec) when is_record(ProcRec, proc) ->
    %% TODO: the @@doc above should reference the record not the module
    systest_proc:get(Field, ProcRec);
process_data(Field, ProcRef) when is_pid(ProcRef) ->
    {ok, Data} = systest_proc:proc_data(ProcRef, Field),
    Data.

%% @doc Returns the activity_state flag for the supplied
%% {@link systest_proc. <em>Process</em>} - the the
%% {@link systest_proc. <i>systest_proc</i>} documentation
%% for further details.
process_activity_state(ProcRef) ->
    systest_proc:activity_state(ProcRef).

%% @doc Returns any configured user-data from the given
%% {@link systest_proc. <em>Process</em>}.
%% @end
read_process_user_data(ProcRef) ->
    systest_proc:user_data(ProcRef).

%% @doc Writes the given data (term) to the indicated
%% {@link systest_proc. <em>Process</em>}.
%% @end
write_process_user_data(ProcRef, Data) ->
    systest_proc:user_data(ProcRef, Data).

%% logging

print_sut_info(SUT) ->
    systest_sut:print_status(SUT).

log_sut_info(SUT) ->
    systest_sut:log_status(SUT).

log(Message) ->
    log(Message, []).

log(Format, Args) ->
    systest_log:log(operator, Format, Args).
