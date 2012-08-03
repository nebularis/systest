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
-module(systest_eunit_listener).

-behaviour(eunit_listener).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/0, start/1]).
-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

-record(testcase,
    {
      name :: string(),
      description :: string(),
      result :: ok | {failed, tuple()} | {aborted, tuple()} | {skipped, term()},
      time :: integer(),
      output :: binary()
     }).
-record(testsuite,
    {
          id = 0 :: integer(),
      name = <<>> :: binary(),
      time = 0 :: integer(),
      output = <<>> :: binary(),
      succeeded = 0 :: integer(),
      failed = 0 :: integer(),
      aborted = 0 :: integer(),
      skipped = 0 :: integer(),
      testcases = [] :: [#testcase{}]
    }).
-record(state, {verbose = false,
        indent = 0,
        xmldir = ".",
        testsuites = [] :: [#testsuite{}]
           }).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options),
                testsuites = []},
    receive 
        {start, _Reference} -> St
    end.

terminate({ok, _Data}, St) ->
    %TestSuites = St#state.testsuites,
    %XmlDir = St#state.xmldir,
    %write_reports(TestSuites, XmlDir),
    ok;
terminate({error, _Reason}, _St) ->
    %% Don't report any errors here, since eunit_tty takes care of that.
    %% Just terminate.
    ok.

handle_begin(Kind, Data, St) when Kind == group; Kind == test ->
    %% Run this code both for groups and tests; test is a bit
    %% surprising: This is a workaround for the fact that we don't get
    %% a group (handle_begin(group, ...) for testsuites (modules)
    %% which only have one test case.  In that case we get a test case
    %% with an id comprised of just one integer - the group id.
    NewId = proplists:get_value(id, Data),
    case NewId of
    [] ->
        St;
    [GroupId] ->
        Desc = proplists:get_value(desc, Data),
            TestSuite = #testsuite{id = GroupId, name = Desc},
        St#state{testsuites=store_suite(TestSuite, St#state.testsuites)};
    %% TODO: handle subgroups
    SubGroups ->
        io:format("Ignoring subgroups: ~p~n", [SubGroups]),
        St
    end.

handle_end(group, Data, St) ->
    %% Retrieve existing test suite:
    case proplists:get_value(id, Data) of
    [] ->
        St;
    [GroupId|_] ->
            TestSuites = St#state.testsuites,
        TestSuite = lookup_suite_by_group_id(GroupId, TestSuites),

        %% Update TestSuite data:
        Time = proplists:get_value(time, Data),
        Output = proplists:get_value(output, Data),
        NewTestSuite = TestSuite#testsuite{ time = Time, output = Output },
        St#state{testsuites=store_suite(NewTestSuite, TestSuites)}
    end;
handle_end(test, Data, St) ->
    %% Retrieve existing test suite:
    [GroupId|_] = proplists:get_value(id, Data),
    TestSuites = St#state.testsuites,
    TestSuite = lookup_suite_by_group_id(GroupId, TestSuites),

    %% Create test case:
    Name = format_name(proplists:get_value(source, Data),
               proplists:get_value(line, Data)),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Result = proplists:get_value(status, Data),
    Time = proplists:get_value(time, Data),
    Output = proplists:get_value(output, Data),
    TestCase = #testcase{name = Name, description = Desc,
             time = Time,output = Output},
    NewTestSuite = add_testcase_to_testsuite(Result, TestCase, TestSuite),
    St#state{testsuites=store_suite(NewTestSuite, TestSuites)}.

%% Cancel group does not give information on the individual cancelled test case
%% We ignore this event
handle_cancel(group, _Data, State) ->
    State;
handle_cancel(test, Data, State) ->
    %% Retrieve existing test suite:
    [GroupId|_] = proplists:get_value(id, Data),
    TestSuites = St#state.testsuites,
    TestSuite = lookup_suite_by_group_id(GroupId, TestSuites),

    %% Create test case:
    Name = format_name(proplists:get_value(source, Data),
               proplists:get_value(line, Data)),
    Desc = format_desc(proplists:get_value(desc, Data)),
    Reason = proplists:get_value(reason, Data),
    TestCase = #testcase{
      name = Name, description = Desc,
      result = {skipped, Reason}, time = 0,
      output = <<>>},
    NewTestSuite = TestSuite#testsuite{
             skipped = TestSuite#testsuite.skipped+1,
             testcases=[TestCase|TestSuite#testsuite.testcases] },
    St#state{testsuites=store_suite(NewTestSuite, TestSuites)}.

%%
%% Internal API
%%

format_name({Module, Function, Arity}, Line) ->
    lists:flatten([atom_to_list(Module), ":", atom_to_list(Function), "/",
           integer_to_list(Arity), "_", integer_to_list(Line)]).
format_desc(undefined) ->
    "";
format_desc(Desc) when is_binary(Desc) ->
    binary_to_list(Desc);
format_desc(Desc) when is_list(Desc) ->
    Desc.

lookup_suite_by_group_id(GroupId, TestSuites) ->
    #testsuite{} = lists:keyfind(GroupId, #testsuite.id, TestSuites).

store_suite(#testsuite{id=GroupId} = TestSuite, TestSuites) ->
    lists:keystore(GroupId, #testsuite.id, TestSuites, TestSuite).

%% Add testcase to testsuite depending on the result of the test.
add_testcase_to_testsuite(ok, TestCaseTmp, TestSuite) ->
    TestCase = TestCaseTmp#testcase{ result = ok },
    TestSuite#testsuite{
      succeeded = TestSuite#testsuite.succeeded+1,
      testcases=[TestCase|TestSuite#testsuite.testcases] };
add_testcase_to_testsuite({error, Exception}, TestCaseTmp, TestSuite) ->
    case Exception of
    {error,{AssertionException,_},_} when
    AssertionException == assertion_failed;
    AssertionException == assertMatch_failed;
    AssertionException == assertEqual_failed;
    AssertionException == assertException_failed;
    AssertionException == assertCmd_failed;
    AssertionException == assertCmdOutput_failed
    ->
        TestCase = TestCaseTmp#testcase{ result = {failed, Exception} },
        TestSuite#testsuite{
          failed = TestSuite#testsuite.failed+1,
          testcases = [TestCase|TestSuite#testsuite.testcases] };
    _ ->
        TestCase = TestCaseTmp#testcase{ result = {aborted, Exception} },
        TestSuite#testsuite{
          aborted = TestSuite#testsuite.aborted+1,
          testcases = [TestCase|TestSuite#testsuite.testcases] }
    end.

