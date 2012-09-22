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
-module(systest_resource_management_SUITE).

-export([activate/1, deactivate/1]).

-behaviour(systest_resource).

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         suite/0,
         all/0]).

-export([resource_creation/1,
         active_resource_shutdown/1,
         active_resource_crash/1,
         starting_resource_crash/1,
         starting_max_lock_timeout_exceeded/1,
         starting_failures_after_max_lock_timeout_remain_locked/1,
         stopping_resource_crash/1]).

-import(systest_test_utils,
        [perm/0,
         wait_do/2,
         wait_for/1,
         choice/2,
         choose/1,
         allow/1,
         return/1,
         test_element/2,
         match_element/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/systest.hrl").

-define(START_ARGS(InitState),
        ?MODULE,
        ?MODULE,
        ?MODULE,
        InitState).

-define(assert_locked(What),
        ?assertEqual({locked, permanent}, What)).

suite() -> [{timetrap, {seconds, 20}}].

all() ->
    systest_suite:export_all(?MODULE) -- [activate, deactivate, bad_arith].

init_per_suite(Config) ->
    register(impl, spawn(fun resource_impl/0)),
    Config.

end_per_suite(_) ->
    catch( impl ! stop ),
    ok.

init_per_testcase(_, Config) ->
    register(perm, spawn(fun systest_test_utils:perm/0)),
    Config.

end_per_testcase(_, _Config) ->
    %% Resource = case ?config(saved_config, Config) of
    %%                {_SavedBy, [{resource, ResourcePid}|_]} ->
    %%                    [ResourcePid];

    %%                Other ->
    %%                    systest:log("no saved resource to release~n", []),
    %%                    {no_resource, Other}
    %%            end,
    systest_lock_timer:set_max_lock_timeout(infinity),
    case whereis(perm) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            catch( Pid ! stop ),
            erlang:monitor(process, Pid),
            receive {'DOWN', _MRef, process, Pid, _} -> ok
            end
    end.

resource_creation(_) ->
    process_flag(trap_exit, true),
    ResourcePid = test_resource(lock_timeout(5000),
                                wait_return({ms, 2000}),
                                stop_noop()),
    Pid = activate_resource(ResourcePid),
    ?assertEqual(true, (erlang:is_process_alive(ResourcePid))),
    ?assertEqual(true, (erlang:is_process_alive(whereis(perm)))),
    ?assertEqual(Pid, whereis(perm)),
    ok.

active_resource_shutdown(_) ->
    process_flag(trap_exit, true),
    ResourcePid = test_resource(),
    ManagedPid = activate_resource(ResourcePid),
    exit(ManagedPid, shutdown),
    wait_for({'EXIT', ResourcePid, normal}),
    ok.

active_resource_crash(_) ->
    process_flag(trap_exit, true),
    ResourcePid = test_resource(),
    ManagedPid = activate_resource(ResourcePid),
    exit(ManagedPid, kill),
    receive
        {'EXIT', ResourcePid, {resource_exit, killed}} ->
            ok;
        Other ->
            exit({kill_not_received, Other})
    end.

starting_resource_crash(_) ->
    process_flag(trap_exit, true),
    N = atom, K = 1,
    ResourcePid = test_resource(fun(_) -> bad_arith(K, N) end,
                                stop_noop()),
    ?assertEqual(ok, systest_resource:activate(ResourcePid)),
    receive
        {'EXIT', ResourcePid,
         {insulator_failed, starting, {badarith, _}}} ->
            ok;
        Other ->
            exit({exit_not_received, Other})
    end.

%% TODO: case where start doesn't return {ok, pid()}

stopping_resource_crash(_) ->
    N = atom, K = 1,
    Test = self(),
    ResourcePid = test_resource(start_noop(),
                                fun(_) ->
                                        Test ! stop_perm,
                                        bad_arith(K, N)
                                end),

    activate_resource(ResourcePid),
    ?assertEqual(active, systest_resource:current_state(ResourcePid)),
    ?assertEqual(ok, systest_resource:deactivate(ResourcePid)),

    wait_for(stop_perm),
    timer:sleep(1000),
    exit(whereis(perm), shutdown),

    wait_for(test_element(2, return({stopped, ?MODULE, shutdown}))),
    ?assertEqual(idle, systest_resource:current_state(ResourcePid)).

% exit(whereis(perm), shutdown),

%    receive
%        {'EXIT', ResourcePid,
%         {insulator_failed, stopping, {badarith, _}}} ->
%            ok;
%        Other ->
%            throw({exit_not_received, Other})
%    end.

starting_max_lock_timeout_exceeded(_) ->
    process_flag(trap_exit, true),
    ResourcePid = test_resource(lock_timeout(2000),
                                wait_return({ms, 10000}),
                                stop_noop()),
    ?assertEqual(ok, systest_resource:activate(ResourcePid)),
    wait_for(choose(match_element(2, {max_lock_timeout, ?MODULE, starting}))),

    ?assertEqual({locked, permanent},
                 systest_resource:current_state(ResourcePid)),
    ?assertEqual({error, {locked, permanent}},
                 systest_resource:activate(ResourcePid)).

starting_failures_after_max_lock_timeout_remain_locked(_) ->
    process_flag(trap_exit, true),
    TestPid = self(),
    ResourcePid =
        test_resource(
          lock_timeout(1000),
          wait_do({ms, 3000},
                  fun() ->
                      TestPid ! {insulator, self()},
                      receive
                          {resource, Pid} ->
                              gen_fsm:send_event(Pid,
                                                 {started, whereis(perm)}),
                              exit(die)
                      end
                  end),
          stop_noop()),

    ?assertEqual(ok, systest_resource:activate(ResourcePid)),
    {insulator, Insulator} =
        wait_for(choice(
           choose(test_element(2, fun erlang:is_pid/1)),
           allow(match_element(2, {max_lock_timeout, ?MODULE, starting})))),

    Insulator ! {resource, ResourcePid},

    %% we can be sure that the lock has expired, so waiting
    %% 5 seconds beyond that should provide ample time for the
    %% insulator to have died
    timer:sleep(5000),
    ?assertEqual({locked, permanent},
                 systest_resource:current_state(ResourcePid)),
    ?assertEqual(false,
                 erlang:is_process_alive(Insulator)).

%    receive
%        {'EXIT', ResourcePid, normal} -> ok;
%        What -> throw(What)
%    end.

%% utility functions

bad_arith(K, N) ->
     K + N.

activate_resource(ResourcePid) ->
    ?assertEqual(ok, (systest_resource:activate(ResourcePid))),
    receive
        {_Ref, {started, ?MODULE, Pid}} when is_pid(Pid) -> Pid;
        Other -> exit({unexpected_message, Other})
    end.

wait_return(Timeout) ->
    %% the systest_resource:activate/1 callback needs to return
    %% {ok, ResourcePid}, which is what we fake here
    wait_return(Timeout, {ok, whereis(perm)}).

wait_return(Timeout, Return) ->
    systest_test_utils:wait_do(Timeout, fun() -> Return end).

start_noop() ->
     fun(_) -> {ok, whereis(perm)} end.

stop_noop() -> fun(_) -> ok end.

lock_timeout(Ms) -> {ms, Ms}.

test_resource() ->
    test_resource(start_noop(), stop_noop()).

test_resource(Start, Stop) ->
    test_resource(infinity, Start, Stop).

test_resource(Timeout, Start, Stop) ->
    test_resource(Timeout, Start, Stop, []).

test_resource(Timeout, Start, Stop, InitState) ->
    impl ! {set, {Start, Stop}},
    if Timeout /= infinity ->
            systest_lock_timer:set_max_lock_timeout(Timeout);
       true ->
            ok
    end,
    {ok, Pid} = systest_resource:start_link([?MODULE,
                                             ?MODULE,
                                             ?MODULE,
                                             InitState]),
    ?assertEqual(idle, systest_resource:current_state(Pid)),
    Pid.

%% systest_resource implementation

resource_impl() ->
    resource_impl({start_noop(), stop_noop()}).

resource_impl({Start, Stop}=Impl) ->
    receive
        {set, start, Start2} ->
            resource_impl({Start2, Stop});
        {set, stop, Stop2} ->
            resource_impl({Start, Stop2});
        {set, Both} ->
            resource_impl(Both);
        {get, start, Who} ->
            Who ! {start, Start},
            resource_impl(Impl);
        {get, stop, Who} ->
            Who ! {stop, Stop},
            resource_impl(Impl);
        stop ->
            ok
    end.

activate(Resource) ->
    (get_fun(start))(Resource).

deactivate(Resource) ->
    (get_fun(stop))(Resource).

get_fun(What) ->
    impl ! {get, What, self()},
    receive
        {What, Fun} -> Fun;
        Other       -> exit({bad_impl, Other})
    end.
