= Running Qpid C++ tests =

General philosophy is that "make check" run all tests by default, but
developers can run tests selectively as explained below.

== Valgrind ==

By default we run tests under valgrind to detect memory errors if valgrind
is present. ./configure --disable-valgrind will disable it.

Default valgrind options are specified in .valgrindrc-default, which a
checked-in file. The actual options used are in .valgrindrc which is a
local file. Normally it is a copy of valgrindrc-default but you can
modify at will.

Supressed errors are listed in .valgrind.supp. If you want to change
suppressions for local testing, just modify .valgrindrc to point to a
different file. Do NOT add suppressions to .valgrindrc.supp unless
they are known problems outside of Qpid that can't reasonably be
worked around in Qpid.


== Unit Tests ==
Unit tests use the boost test framework, and are compiled to rogram
 unit_test
There are several options to control how test results are displayed,
see
 http://www.boost.org/doc/libs/1_35_0/libs/test/doc/components/utf/parameters/index.html

NOTE: some unit tests are written as CppUnit plug-ins, we are moving away
from CppUnit so new tests should use the boost framework.

CppUnit tests are run by the script run-unit-tests.

== System Tests ==

System tests are self contained AMQP client executables or scripts.
They are listed in the TESTS make variable, which can be over-ridden.

The ./start_broker "test" launches the broker, ./stop_broker" stops it.
Tests in between assume the broker is running.

./python_tests: runs ../python/run_tests. This is the main set of
system testss for the broker.

Other C++ client test executables and scripts under client/test are
system tests for the client.

By setting TESTS in a make command you can run a different subset of tests
against an already-running broker.




