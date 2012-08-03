*SysTest* is aimed at distributed systems testing, where its primary purpose is
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
in use. Other testing frameworks (such as [eunit][eunit], may choose to map 

## Runtime Environment

### The _System Under Test_

### The _Process_

[runner]: https://github.com/nebularis/systest/wiki/systest_runner
[eunit]: http://www.erlang.org/doc/apps/eunit/chapter.html
[ct_suites]: http://www.erlang.org/doc/apps/common_test/write_test_chapter.html
