= INSTALLATION =

Extract the release archive into a directory of your choice and set
your PYTHONPATH accordingly:

  tar -xzf qpid-python-<version>.tar.gz -C <install-prefix>
  export PYTHONPATH=<install-prefix>/qpid-<version>/python

= GETTING STARTED =

The python client includes a simple hello-world example that publishes
and consumes a message:

  cp <install-prefix>/qpid-<version>/python/hello-world .
  ./hello-world

= EXAMPLES =

More comprehensive examples can be found here:

  cd <install-prefix>/qpid-<version>/python/examples

= RUNNING THE TESTS =

The "tests" directory contains a collection of unit tests for the
python client. The "tests_0-10", "tests_0-9", and "tests_0-8"
directories contain protocol level conformance tests for AMQP brokers
of the specified version.

The qpid-python-test script may be used to run these tests. It will by
default run the python unit tests and the 0-10 conformance tests:

  1. Run a broker on the default port

  2. ./qpid-python-test

If you wish to run the 0-8 or 0-9 conformence tests, they may be
selected as follows:

  1. Run a broker on the default port

  2. ./qpid-python-test tests_0-8.*

        -- or --

     ./qpid-python-test tests_0-9.*

See the qpid-python-test usage for for additional options:

  ./qpid-python-test -h
