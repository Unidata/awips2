This module contains integration tests, for testing a java client againt *any* broker
implementation or against other clients. These tests must not rely on starting the
Java broker in-vm but must depend on a broker being started independantly before running
the tests in this module. By default tests in this module will expect the broker to be
started on localhost on the default port, but this can be overridden by passing in a
sys property to maven. Interop tests are in this module. Java broker specific tests that
use an in-vm broker should go in the systests module.

Don't set the tests in this module to run by default as part of the maven build, until
there is a script to start and stop the broker; needed to fully automate these tests.
Interop tests will always be run using a seperate script (not from maven) but it might
be worthwile to script into the maven build starting of the Java broker, and running
these tests against it.