%% file ./test/example_1_SUITE.erl
-module(example_1_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-import(systest_utils, [make_node/1]).

all() ->
    systest_suite:export_all(?MODULE).

check_that_our_nodes_are_up(_Config) ->
    ?assertEqual(pong, net_adm:ping(make_node('node1'))),
    ?assertEqual(pong, net_adm:ping(make_node('node2'))).

%% NB: this test *is* going to fail!!!
deliberately_kill_node(_Config) ->
    rpc:call(make_node('node1'), init, stop, []),
    %% we need enough time to 'detect' the node is down before the test ends
    timer:sleep(2000),
    ok.

stopping_and_restarting_nodes(Config) ->
    %% the active_sut/1 call will fail if there is no SUT configured
    %% for this particular test case
    Sut = systest:active_sut(Config),

    %% we can print out status info to the console like so:
    systest_sut:print_status(Sut),

    %% and we can get the process info for the SUT as well
    [{Id1, Ref1},{Id2, Ref2}] = systest:list_processes(Sut),

    %% let's stop one and wait long enough to ensure it shuts down...
    systest:stop_and_wait(Ref1),

    %% and let's restart the second one!
    {ok, {Id2, NewRef2}} = systest:restart_process(Sut, Ref2),

    %% and our assertions....
    ?assertEqual(pang, net_adm:ping(Id1)),
    ?assertEqual(pong, net_adm:ping(Id2)).

