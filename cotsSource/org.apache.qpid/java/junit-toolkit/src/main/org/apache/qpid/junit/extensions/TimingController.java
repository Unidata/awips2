/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.junit.extensions;

/**
 * A TimingController is a interface that a test that is aware of the fact that it is being timed can use to manage
 * the timer. Using this interface tests can suspend and resume test timers. This is usefull if you want to exclude
 * some expensive preparation from being timed as part of a test. In general when timing tests to measure the
 * performance of code, you should try to set up data in the #setUp where possible, or as static members in the test
 * class. This is not always convenient, this interface gives you a way to suspend and resume, or event completely
 * restart test timers, to get accurate measurements.
 *
 * <p/>The interface can also be used to register multiple test pass/fails and timings from a single test method.
 * In some cases it is easier to write tests in this way. For example a concurrent and asynchronous test may make
 * many asynchronous requests and then wait for replies to all its requests. Writing such a test with one send/reply
 * per test method and trying to scale up using many threads will quickly run into limitations if more than about
 * 100 asynchronous calls need to be made at once. A better way to write such a test is as a single method that sends
 * many (perhaps thousands or millions) and waits for replies in two threads, one for send, one for replies. It can
 * then log pass/fails and timings on each individual reply as they come back in, even though the test has been written
 * to send thousands of requests per test method in order to do volume testing.
 *
 * <p/>If when the {@link #completeTest(boolean)} is called, the test runner decides that testing should stop (perhaps
 * because a duration test has expired), it throws an InterruptedException to indicate that the test method should stop
 * immediately. The test method can do this by allowing this exception to fall through, if no other clean-up handling
 * is necessary, or it can simply return as soon as it possibly can. The test runner will still call the tearDown
 * method in the usual way when this happens.
 *
 * <p/>Below are some examples of how this can be used. Not how checking that the timing controller is really available
 * rather than assuming it is, means that the test can run as an ordinary JUnit test under the default test runners. In
 * general code should be written to take advantage of the extended capabilities of junit toolkit, without assuming they
 * are going to be run under its test runner.
 *
 * <pre>
 * public class MyTest extends TestCase implements TimingControllerAware {
 * ...
 *
 *    timingUtils = this.getTimingController();
 *
 *    // Do expensive data preparation here...
 *
 *    if (timingUtils != null)
 *        timingUtils.restart();
 * </pre>
 *
 * <pre>
 * public class MyTest extends TestCase implements TimingControllerAware {
 * ...
 *
 *   public void myVolumeTest(int size) {
 *
 *    timingUtils = this.getTimingController();
 *
 *    boolean stopNow = false;
 *
 *    // In Sender thread.
 *      for(int i = 0; !stopNow && i < size; i++)
 *        // Send request i.
 *        ...
 *
 *    // In Receiver thread.
 *    onReceive(Object o) {
 *      try {
 *      // Check o is as expected.
 *      if (....)
 *      {
 *        if (timingUtils != null)
 *          timingUtils.completeTest(true);
 *      }
 *      else
 *      {
 *        if (timingUtils != null)
 *          timingUtils.completeTest(false);
 *      }
 *      } catch (InterruptedException e) {
 *        stopNow = true;
 *        return;
 *      }
 *    }
 * </pre>
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Allow test timers to be suspended, restarted or reset.
 * <tr><td> Allow tests to register multiple pass/fails and timings.
 * </table>
 *
 * @author Rupert Smith
 */
public interface TimingController
{
    /**
     * Gets the timing controller associated with the current test thread. Tests that use timing controller should
     * always get the timing controller from this method in the same thread that called the setUp, tearDown or test
     * method. The controller returned by this method may be called from any thread because it remembers the thread
     * id of the original test thread.
     *
     * @return The timing controller associated with the current test thread.
     */
    public TimingController getControllerForCurrentThread();

    /**
     * Suspends the test timer.
     *
     * @return The current time in nanoseconds.
     */
    public long suspend();

    /**
     * Allows the test timer to continue running after a suspend.
     *
     * @return The current time in nanoseconds.
     */
    public long resume();

    /**
     * Completely restarts the test timer from zero.
     *
     * @return The current time in nanoseconds.
     */
    public long restart();

    /**
     * Register an additional pass/fail for the current test. The test result is assumed to apply to a test of
     * 'size' parmeter 1. Use the {@link #completeTest(boolean, int)} method to register timings with parameters.
     *
     * @param testPassed Whether or not this timing is for a test pass or fail.
     *
     * @throws InterruptedException If the test runner decides that testing should stop it throws this exception to
     *                              indicate to the test method that it should stop immediately.
     */
    public void completeTest(boolean testPassed) throws InterruptedException;

    /**
     * Register an additional pass/fail for the current test. The test result is applies to a test of the specified
     * 'size' parmeter.
     *
     * @param testPassed Whether or not this timing is for a test pass or fail.
     * @param param      The test parameter size for parameterized tests.
     *
     * @throws InterruptedException If the test runner decides that testing should stop it throws this exception to
     *                              indicate to the test method that it should stop immediately.
     */
    public void completeTest(boolean testPassed, int param) throws InterruptedException;

    /**
     * Register an additional pass/fail for the current test. The test result is applies to a test of the specified
     * 'size' parmeter and allows the caller to sepecify the timing to log.
     *
     * @param testPassed Whether or not this timing is for a test pass or fail.
     * @param param      The test parameter size for parameterized tests.
     * @param timeNanos  The time in nano seconds to log the test result with.
     *
     * @throws InterruptedException If the test runner decides that testing should stop it throws this exception to
     *                              indicate to the test method that it should stop immediately.
     */
    public void completeTest(boolean testPassed, int param, long timeNanos) throws InterruptedException;
}
