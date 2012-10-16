*SysTest* is a framework aimed at distributed systems testing, with the
express purpose of making test configuration easier and more manageable
for test authors and operators (who run tests).

The common factor that *SysTest* tries to address is the management and
monitoring of *Test Resources* required during testing. The idea of a
*Test(ing) Resource* is therefore central to *SysTest*, and as we will
see, is a pervasive concept throughout the framework. A *Test Resource*
(which we will refer to as simply *Resource* from now on) is simply
**something** that your tests require. At a high level, the kind of
*Resources* you typically need for systems and integration testing are
broken down into various categories.

#### *Test Resource* Categories

- some configured environment(s) in which your tests must be run
- configured components and/or applications that make up the system under test
- a set of dependencies between resources, configurations, and so on
- various kinds of test input data, static and/or generated

The *Resource* concept in *SysTest* provides a flexible abstraction
that allows us to support and manage *Resources* in these various categories.
All *Resources* consist of four basic building blocks, all of which are
optional, allowing the framework to handle all *Resources* in exactly
the same manner. These four building blocks are

1. *Resource* Data
2. *Resource* Lifecycles
3. *Resource* Relationships
4. *Resource* Behaviours

As we examine each of these (in the following sections) in turn, we will see
that the building blocks allow us to build up various kinds of testing
capabilities and infrastructure with a minimum of effort. We will also
(hopefully) gain some insight into how *Resources* are implemented to
deliver these gains.

#### __*Resource* Building Blocks - Data__

*Resources* provide a structured way to describe data which they
can hold. All *Resources* contain at least one mandatory data 
field: `id` which is used to identify and differentiate between
them. *Resources* can *expose* additional data items, which we will
refer to as *Resource Properties* (or just *properties*) from now on.
Any data held in *Resource Properties* has to be of a specific data type,
usually one of the intrinsic data types provided by Erlang. 

_More complex types can be defined by the user, though their uses are_
_somewhat limited unless they're accompanied by an Erlang module implementing_
_the standard set of operations that all the intrinsic types support._
_See the [user defined types][udt] wiki page for more details_

#### __*Resource* Building Blocks - Lifecycles__

A *Resources* have a clearly defined *lifecycle* and this is applied
consistently to all kinds of *Resource* regardless of their type.

Perhaps the most basic kind of resource is a file system entry,
which is identified by its path. The *Path* allows us to identify
whether two *Resources* refer to the same location, and the ability
to query this data property allows us to refer directly to this *Resource*
when we need to. For example, a web server deployment we require for
our tests might need to refer to a pre-packed set of test data stored in
a static config file, which needs to be in the right location when
the server is started up (before running some integration tests).

As *SysTest* currently uses Erlang terms to describe *Resources*, we
can write the definition of a *file* resource like so:

```erlang
%% resource type File is identified by Path.
{resource_type, 'File', [
    {identified_by, {'Path', 'path'}}
]}.
```

This is our first glimpse of resource description format *SysTest* uses.
Up until now we've been talking about *Resources* but the declaration of
*File* (above) uses the term `resource_type`; we will discuss this in
a later section, but for the time being let's just say that a
*Resource **Type*** is a kind of *Resource*.

Another thing we might notice here is the two-part identification config
element `{'Path', 'path'}` - this is a feature of the *Resource Definition*
format and should be translated as `{DataType, Name}`. 



#### *Test Resource* Uses

- *Resources* can be made to represent just about anything you want
- *Resources* ensure that testing occurs in a consistent and stable environment
- *Resources* can be composed to create arbitrarily complex test fixtures
- *Resources* are monitored to ensure it meets expected usage patterns
- *Resources* can be configured to execute a user-defined, inter-connected component lifecycle


Currently, *SysTest* provides a small number of basic *Resource* implementations
that can be combined to support a basic set of testing requirements. Users can
extend these or 

## A Brief Example

Let us assume that we want to test a database application that has
been built in-house. In order to execute our tests, we want to run the
database server and application on one physical machine, and the test
client application on another, networked machine. In this example, the
test environment has a great many moving parts! We might consider any
or all of the following:

- environment/configuration for each machine
- configuration of the network
- installation and/or configuration of application(s) on either machine
- installation and/or configuration of application code
- the presence of static configuration files

To keep things simple, we will assume that the network is pervasive,
readily available and requires no configuration. In this scenario then,
we can separate the resources we care about into a few simple categories:

1. hosts (e.g., physical or virtual machines)
2. applications[*]
3. configuration files

In *SysTest*, we can categorise our resources into resource *types* and
define resources in terms of those types. Resource types define the
values and relationships (with other resource types) that they require
in order to be used - *realised* in *SysTest* parlance - and the individual
The resources we define *inject concrete (or derived) values into those types*
in order to make them usable at runtime.

Obviously this ability to define a resource type like `Host` with some
properties such as `host_name, ip_address` and so on is lovely, but quite
useless until we can actually *do something* with those values, like start
or provision a host with those settings. Although we have the ability to
define a *SysTest Resource Configuration Script* with all manner of types
and resources injecting values into those types, they can only be made to
do something useful if there is a *resource implementation* available at
runtime which can turn the values into something *real*. In practise,
this consists of an Erlang module which conforms to the `systest_resource`
behaviour. Several of these resource type implementations are provided
with *SysTest* and it is possible to define your own, custom implementations
and use these at runtime.

```erlang
{resource_type, 'Host', [
    {identified_by, 'name'},
    {exposes, [{'IpAddress', 'ip_address'}]},
    {provided_by, systest_host}
]}.
```

Clearly this categorisation of the *Resources* we wish to use for our tests
is insufficient to actually run them, as the two applications differ in
various ways, as do the host machines on which they run and the configuration
files required to set them up! This is where the ability to compose *Resources*
and *Resource Types* comes in:

Many of these concerns have been already solved in the industry at large,
and we do **not** want to reinvent the wheel, so *SysTest* does not attempt
to deal with any of the areas where solutions already exit. For example, we
are not concerned with

- provisioning or configuring the network
- provisioning physical or virtual machines
- configuring an operational machine (virtualised or otherwise)
- installing or configuring applications

There are numerous tools and frameworks available to do all these things
for us! Assuming we will use these tools to work on our behalf then, what
*SysTest* offers us is a means to describe the setup and teardown of the
test environment in terms of using those tools, and the means to have
the environment made available for the duration of our test(s) and have it
cleanly 'torn down' thereafter.

As we promised to make this example somewhat concrete, let's attempt to
fill in the gaps. We will assume that the following services are available
to us:

- we will provision a pair of networked machines using Amazon's EC2 service
- we will configure these machines using puppet
- we will also install and configure out database server and client using puppet

We can configure our two machines (with their different, respective setups)
very easily using puppet, so we do **not** need have 

In this case, the *resources* we wish to define will be:

1. the individual machines
2. 

## What *is* a resource anyway?

*SysTest* aims to be as generic a *testing support framework* as possible,
so from our point of view, a *Resource* is 'anything you want to use
in your tests', which is fairly permissive! In practise however, there are
of course some constraints, which we will discuss shortly.



to facilitate setting up the software you're testing, ensuring it stays online
for the duration of your tests, and cleanly tearing everything down after (and
between) each test.

*SysTest* is designed to work with any testing framework you choose, although
the stable branch currently supports on the OTP [_Common Test_][ct] framework. 
*SysTest* generally uses the same testing terminology as [_Common Test_][ct], 
with a few exceptions and additions that we will cover now.

## Scopes and naming conventions

Each execution of your tests is referred to as a _test run_, and as far as the 
framework is concerned, this consists of any and all running code from the 
moment that the [_Runner_][runner] is executed to the moment that control is
returned to the calling process.

Once a _test run_ starts, tests can be subdivided into _test suites_, which map
directly to [_Common Test_ `_SUITE` modules][ct_suites] when that framework is 
in use. Other testing frameworks (such as [eunit][eunit]), may choose to map a
test suite to a module or something other construct - refer to the framework
documentation to find out.

A _test suite_ will ...

## Runtime Environment

### The _System Under Test_

### The _Process_

[runner]: https://github.com/nebularis/systest/wiki/systest_runner
[udt]: https://github.com/nebularis/systest/wiki/udt
[eunit]: http://www.erlang.org/doc/apps/eunit/chapter.html
[ct_suites]: http://www.erlang.org/doc/apps/common_test/write_test_chapter.html

