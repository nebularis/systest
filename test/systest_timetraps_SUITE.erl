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
-module(systest_timetraps_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/systest.hrl").
-compile(export_all).

%%
%% Config/Fixture Definitions
%%

%% NB: we *never* hit the suite time traps here...
suite() -> [{timetrap, {minutes, 60}}].

all() ->
    [{group, test_teardown_timeouts},
     {group, test_aggressive_shutdowns}].

groups() ->
    [{test_teardown_timeouts, [],
      [%% first we run with a stuck on_stop hook
       simple_test_case_pass_through,
       %% then verify that our teardown timeout
       %% handled the stuck resources properly
       verify_hung_resource_was_handled]},
     {test_aggressive_shutdowns, [],
      [%% first we check the teardown for the
       %% previous group resources was successful
       verify_hung_resource_was_handled
%       hang_on_startup,
       %% and verify that the timeout handled it!
%       verify_hung_resource_was_handled
      ]}].

init_per_testcase(TC, Config) ->
    [{tc, TC}|Config].

end_per_testcase(simple_test_case_pass_through, _Config) ->
    ok;
end_per_testcase(TC, Config) ->
    SaveConf = case lists:keytake(tc_last, 1, Config) of
                   {value, {tc_last, _}, Rest} -> Rest;
                   false                       -> Config
               end,
    {save_config, [{tc_last, TC}|SaveConf]}.

init_per_group(Group, Config) ->
    case systest:list_processes(Group) of
        not_found -> ok;
        Procs     -> save(group_procs, Procs)
    end,
    case ?config(saved_config, Config) of
        {{group, LastG}, Config2} ->
            {LastTC, _} = ?config(saved_config, Config2),
            [{last_group, LastG},
             {last_tc, LastTC}|Config];
        _ ->
            Config
    end.

end_per_group(Group, Config) ->
    SaveConf = case lists:keytake(group_last, 1, Config) of
                   {value, {group_last, _}, Rest} -> Rest;
                   false                          -> Config
               end,
    {save_config, [{group_last, Group}|SaveConf]}.

init_per_suite(Config) ->
    Pid = spawn(fun() ->
                        register(?MODULE, self()),
                        receive stop -> ok
                        end
                end),
    ets:new(?MODULE, [ordered_set, named_table, public,
                      {read_concurrency, true},
                      {write_concurrency, true},
                      {heir, Pid, []}]),
    Config.

end_per_suite(_) ->
    catch (exit(whereis(?MODULE), kill)), %% kills off the ets table too....
    ok.

save(Key, Value) ->
    true = ets:insert_new(?MODULE, {Key, Value}).

read(Key) ->
    Val = ets:lookup_element(?MODULE, Key, 2),
    ets:delete(?MODULE, Key),
    Val.

current_group(Config) ->
    ?config(name, ?config(tc_group_properties, Config)).

get_last(Config) ->
    {?config(last_group, Config), ?config(last_tc, Config)}.

%%
%% Test Case Definitions
%%

simple_test_case_pass_through() ->
    [{userdata, [{doc, "The SUT configured for this test case "
                       "is designed to hang during shutdown, so "
                       "that we can test the behaviour of the "
                       "teardown_timetrap and aggressive_shutdown "
                       "options give in a test profile!"}]}].

simple_test_case_pass_through(Config) ->
    Sut = systest:get_system_under_test(Config),
    Procs = systest:list_processes(Sut),
    save(procs, Procs).

hang_on_startup(_Config) ->
    ok.

verify_hung_resource_was_handled(Config) ->
    case current_group(Config) of
        test_teardown_timeouts ->
            assert_cleanup_succeeded(simple_test_case_pass_through, procs);
        test_aggressive_shutdowns ->
            assert_cleanup_succeeded(test_teardown_timeouts, group_procs)
    end.

assert_cleanup_succeeded(Root, OrphansKey) ->
    Pid = whereis(Root),
    ?assertEqual(undefined, Pid),
    systest:log("waiting 10 seconds for dependent "
                "resources to be killed...~n", []),
    timer:sleep(10000),
    Procs = read(OrphansKey),
    systest:log("checking orphans ~p~n", [Procs]),
    [begin
         ?assertEqual(false,
                      erlang:is_process_alive(P))
     end || {_, P} <- Procs].

