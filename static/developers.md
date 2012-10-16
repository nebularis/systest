# Developer Guide

## Building SysTest

### Prerequisites

*SysTest* should work on most POSIX compliant unices. Building on Windows is not
supported, though it may work if you skip the makefile and stick to rebar. The
whole build is in fact driven by rebar, with the makefile simply providing a
few conveniences (such as installing rebar in the first place if it is missing).

In order to build *SysTest* at all however, you will need the following 
components installed on your system:

- [Erlang/OTP >= R15][erlang]
- One of either [rebar][rebar] or a GNU compatible [make][gnu-make]
- To use (optional) `systest_native` handlers, a [gcc compatible compiler][gcc]
- To use (optional) `systest_ssh` handlers, an [ssh client][ssh]

In order to use the (optional) `systest_foreign_node` handler, you will need a 
valid install of the target language/platform on your system. For `systest_script`
interfaces, you will need to acquire the correct Lua and/or Python dependencies.
This process has not yet been fully automated.

For `{target, jvm}` you will need a JRE installed on your system and should 
ensure that your `JAVA_HOME` environment variable is set properly. You will also
need to ensure that the [jinterface][jinterface] package that is supplied with 
[Erlang/OTP][erlang] was installed correctly.

### Building from Source

The easiest way to build SysTest from source is to use the supplied makefile,
which will build the SysTest OTP application and generated an escript archive
in the `./priv/bin` folder by default. An `info` target is also provided, which
will tell you about the version of rebar that's being used and what dependencies
are required.

The project is built using [rebar][rebar], so you can, if you wish, build it 
directly using rebar instead. The build is currently dependent on a fork of
the main (upstream) rebar repository, which is available in the *systest*
branch of [hyperthunk/rebar][hyperthunk_rebar]. This dependency will be removed
before the first public 1.0.0 release.

### Running the tests

The makefile's `test` target will run a suite of integration tests, using the
`./priv/bin/systest` executable directly.

## Links

[wiki]: https://github.com/nebularis/systest/wiki
[rebar]: https://github.com/rebar/rebar
[hyperthunk_rebar]: https://github.com/hyperthunk/rebar
[gnu-make]: http://www.gnu.org/software/make/
[ssh]: http://www.openssh.org/
[gcc]: http://gcc.gnu.org/
[jinterface]: http://www.erlang.org/doc/apps/jinterface/jinterface_users_guide.html
