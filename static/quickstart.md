## Installing SysTest

To install from source, clone the repository using git, or download the version
you want to use from the [github downloads page][downloads].

To build the sources, execute the default make rule using `make`.
Although you *can* build the sources using rebar, the makefile currently assumes
some experimental features that haven't yet made it into rebar master, therefore
until these features are fully merged into rebar you'll need to use the
[systest branch][systest-rebar], which is maintained on github. The makefile
handles this by relying on this custom version of rebar and installing it
locally.

Once built, there will be an executable escript archive in `./priv/bin` inside
the main source directory. You *may* put this on your `PATH` environment
variable in order to utilise *SysTest* across multiple projects. If you are
planning on calling *SysTest* APIs in your tests, it is also worth putting the
main *SysTest* directory somewhere where it can be utilised by your tests at
runtime, or adding it to your `ERL_LIBS` environment variable. This will also
be necessary if you plan on running `xref` or `dialyzer` on your test code.

## Getting the sources for this tutorial

The source for this tutorial are included in the `./examples/quickstart` folder
inside the top level systest repository. There is also a sample `Makefile` which
exports a `test` target that you can run to see everything working as expected.

## Writing your first test case

We will consider how to configure and use *SysTest* via the Erlang/OTP
common_test framework. We *will* be calling *SysTest* APIs during our tests,
and therefore need to modify our `ERL_LIBS` to consider this. In order for this
to work, we will assume that the base *SysTest* directory (which was cloned or
downloaded from github), is present in `/usr/local/lib/systest`.

By default, *SysTest* uses the contents of the `./ebin` directory to resolve
which common test suites should be executed. If you want to have your test
beams in another location then you should consult the
[[Configuration Tutorial|configuration]] on setting up a *Profile* in which you
can override this behaviour. For the purposes of this quickstart guide, we will
stick to that convention. We also avoid assuming a default build tool, so you
should make sure that your chosen build tool outputs the test beams into the
correct location.

### Setting up test resources

First we start with a set of _resource_ definitions. These describe the two main
aspects of our test, the *System Under Test* (or _SUT_ for short) and the
various *Processes* that it is composed of. Bare in mind that our definition of
*Process* refers to an _Operational Process_ rather than Erlang/OTP's
lightweight _Processes_ (i.e., green threads). Our *SUT* will be an Erlang
cluster consisting of two nodes, so we'll need to define the nodes as our
_Operational Processes_ first. *SysTest* looks for resources in
`./resources/*.resource` by default, so we will create our node definition in
`./resource/nodes.resource`. Separating the node configuration from the *SUT*
definition is useful if we want to share it across multiple test profiles later
on.

```erlang
%% ./resources/nodes.resource
{sasl_node, [
    {startup, [{handler, systest_slave}]},
    {flags, [{start, "-boot start_sasl +W w +K true +A30 +P 1048576"
                     " -pa ${settings.base_dir}/ebin "
                     "-sasl sasl_error_logger false"}]},
    {apps, [{sasl, [{errlog_type, error}]}]}
]}.
```

It is important that any file system paths are absolute, as common test will
change to a temporary directory before running our tests and relative paths will
therefore fail to produce the desired effect. This resource definition describes
an Erlang node that is started using the [slave module][slave] from stdlib, and having
some additional command line arguments. Note the use of a _string expression_ to
evaluate the `base_dir` (the starting directory in which *SysTest* is run) from
our settings, so that we use an absolute path when setting the code path.

We now wire together two of these _Processes_ to create our *SUT*. We will put
these definitions into a file `./resources/example_1.resource` for now.

```erlang
%% ./resources/example_1.resource
{system_1, [
    {sut, [{localhost, [node1, node2]}]},
    {processes, [{node1, [sasl_node]}, {node2, [sasl_node]}]}
]}.
```

Note that the definitions used in the `processes` _configuration element_ simply
refer to the `sasl_node` resource, rather than declaring properties themselves.
If we planned on writing a test case named 'system_1' then we would be finished
at this point, but instead we will wire up the *SUT* to be used by every test
case in our soon to be written test suite.

```erlang
%% ./resources/example_1.resource
{example_1_SUITE, [{all, system_1}]}.
```

### Writing test cases

Because we've wired all test cases in `example_1_SUITE` to use the `system_1`
*SUT* configuration, we simply need to create a test suite with that name and
*SysTest* will automatically start and stop our cluster for each test case, as
well as monitoring each *Process* to ensure that no unexpected failures occur.

A minimal example looks something like this:

```erlang
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
```

Note that we can safely assume that our nodes survive for the duration of the
test case, and are shut down afterwards. Each test case will get a clean set of
nodes, freshly stopped and started since the previous test case. To have a *SUT*
and its _processes_  span the entire suite (i.e., have the _processes_ stay
online and not restart them between test cases) we would rework the example_1
resource configuration to look like this:

```erlang
%% ./resources/example_1.resource
{example_1_SUITE, system_1}.
```

To check that our test cases fail as expected, we can explicitly kill one of our
slave nodes without _informing_ *SysTest* about it, which the testing framework
will construe as a failure.

```erlang
deliberately_kill_node(_Config) ->
    rpc:call(make_node('node1'), init, stop, []),
    %% we need enough time to 'detect' the node is down before the test ends
    timer:sleep(2000),
    ok.
```

Do be aware that this test case _will_ fail when it is run. You can delete it
from the test suite (or remove the function name from the results of all/0) if
you wish to keep it but not let it run.

### Handling deliberate shut down and restarts

If the framework assumes a test case has failed whenever a _Process_ (e.g., an
Erlang node in our example) ceases unexpectedly, how are we to test situations
in which a stop or restart is actually required, for example a cluster failover
handling scenario?

*SysTest* provides an API for explicitly performing such actions without causing
test cases to fail. We will stop one node and restart another, and verify the
state of both afterwards. In order to interact with our nodes, we must acquire
references to them, using a *SUT* reference to do so. The latter reference can
be acquired from the common test config parameter.

```erlang
stopping_and_restarting_nodes(Config) ->
    %% the active_sut/1 call will fail if there is no SUT configured
    %% for this particular test case
    Sut = systest:get_system_under_test(Config),
    
    %% we can print out status info to the console like so:
    systest_sut:print_status(Sut),
    
    %% and we can get the process info for the SUT as well
    [{Id1, Ref1},{Id2, Ref2}] = systest:list_processes(Sut)],
    
    %% let's stop one and wait long enough to ensure it shuts down...
    systest:stop_and_wait(Ref1),
    
    %% and let's restart the second one!
    {ok, {Id2, NewRef2}} = systest:restart_process(Sut, Ref2),
    
    %% and our assertions....
    ?assertEqual(pang, net_adm:ping(Id1)),
    ?assertEqual(pong, net_adm:ping(Id2)).
```

As we can see, the nodes (which are our operating _Processes_ for this *SUT*)
are left in exactly the state we expect them to be in.

[wiki]: https://github.com/nebularis/systest/wiki
[downloads]: https://github.com/nebularis/systest/downloads
[systest-rebar]: https://github.com/hyperthunk/rebar/branches/systest
[slave]: http://www.erlang.org/doc/man/slave.html
