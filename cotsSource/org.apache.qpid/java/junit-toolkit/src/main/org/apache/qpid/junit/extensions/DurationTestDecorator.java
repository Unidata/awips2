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

import junit.framework.Test;
import junit.framework.TestResult;

import org.apache.log4j.Logger;

import java.util.Timer;
import java.util.TimerTask;

/**
 * A test decorator that runs a test repeatedly until a specified length of time has passed.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Repeatedly run a test for a fixed length of time.
 * </table>
 *
 * @todo The count of the number of tests run is an important number to keep. Also num passed/error/failed is also
 *       important to record. What to do with these numbers? They are already logged to the test listeners.
 *
 * @todo The duration test runner wraps on top of size, repeat or thread wrappers, need a way for it to tell
 *       TKTestResult when the duration is up, so that it can terminate any repeats in progress. It should end
 *       as soon as possible once the test method exits.
 *
 * @author Rupert Smith
 */
public class DurationTestDecorator extends WrappedSuiteTestDecorator implements ShutdownHookable
{
    /** Used for logging. */
    private static final Logger log = Logger.getLogger(DurationTestDecorator.class);

    /** The test to run. */
    private Test test;

    /** The length of time to run the test for. */
    private long duration;

    /** Flag set by the shutdown hook. This decorator will not start any new tests when this is set. */
    private boolean shutdown = false;

    /**
     * Creates an active test with default multiplier (1).
     *
     * @param test The target test.
     */
    public DurationTestDecorator(WrappedSuiteTestDecorator test)
    {
        super(test);
        this.test = test;
    }

    /**
     * Creates active test with default multiplier (1).
     *
     * @param test     The target test.
     * @param duration The duration in milliseconds.
     */
    public DurationTestDecorator(WrappedSuiteTestDecorator test, long duration)
    {
        super(test);

        // log.debug("public DurationTestDecorator(Test \"" + test + "\", long " + duration + "): called");

        this.test = test;
        this.duration = duration;
    }

    /**
     * Runs the test repeatedly for the fixed duration.
     *
     * @param testResult The the results object to monitor the test results with.
     */
    public void run(TestResult testResult)
    {
        log.debug("public void run(TestResult testResult): called");

// Removing the durationTimer as this addition prevents this TestDecorator being wrapped with a Scaled Test Decorator.
        // This change will cause the tests to run for at least the specified duration
        // If we need the test to stop much closer to the specified duration then we need to
        // ensure that the solution doesn't prevent this Decorator being wrapped with other Decorators.

//        // Cast the test result to expose it as a TKTestResult if the test is running under the TKTestRunner.
//        TKTestResult tkTestResult = null;
//
//        if (testResult instanceof TKTestResult)
//        {
//            tkTestResult = (TKTestResult) testResult;
//        }
//
//        // If running under the TKTestRunner, set up a timer to notify the test framework when the test reaches its
//        // completion time.
//        Timer durationTimer = null;
//
//        if (tkTestResult != null)
//        {
//            log.debug("Creating duration timer.");
//
//            durationTimer = new Timer();
//            durationTimer.schedule(new DurationTimerTask((TKTestResult) testResult), duration);
//        }


        // Work out when the test should end.
        long now = System.nanoTime();
        long end = (duration * 1000000) + now;

        // Run the test until the duration times out or the shutdown flag is set. The test method may not exit until
        // interrupted in some cases, in which case the timer will do the interrupting.
        while ((now < end) && !shutdown)
        {
            test.run(testResult);

            now = System.nanoTime();
        }

//        // Clean up any timer that was used.
//        if (durationTimer != null)
//        {
//            log.debug("Cancelling duration timer.");
//
//            durationTimer.cancel();
//        }
    }

    /**
     * Supplies the shutdown hook. This shutdown hook does not call {@link TKTestResult#shutdownNow()} because the
     * {@link ScaledTestDecorator} already takes care of that.
     *
     * @return The shut down hook.
     */
    public Thread getShutdownHook()
    {
        return new Thread(new Runnable()
                {
                    public void run()
                    {
                        // log.debug("DurationTestDecorator::ShutdownHook: called");

                        // Set the shutdown flag so that no new tests are started.
                        shutdown = true;
                    }
                });
    }

//    /**
//     * DurationTimerTask is a timer task that is configured, upon expiry of its timer, to invoke
//     * {@link TKTestResult#shutdownNow()}, for the test result object on which it is set. It also sets
//     * the {@link DurationTestDecorator#shutdown} flag to indicate that no new tests should be run.
//     *
//     * <p/>The test loop implemented by DurationTestDecorator checks that the duration has not expired, on each
//     * test case that it runs. However, it is possible to write test cases that never return until explicitly
//     * interrupted by the test framework. This timer task exists to notify the test framework
//     */
//    private class DurationTimerTask extends TimerTask
//    {
//        /** Used for debugging purposes. */
//        private final Logger log = Logger.getLogger(DurationTimerTask.class);
//
//        /** Holds the test result for the test to which a duration limit is being applied. */
//        TKTestResult testResult;
//
//        /**
//         * Creates a duration limit timer which will notify the specified test result when the duration has
//         * expired.
//         *
//         * @param testResult The test result to notify upon expiry of the test duration.
//         */
//        public DurationTimerTask(TKTestResult testResult)
//        {
//            this.testResult = testResult;
//        }
//
//        /**
//         * The action to be performed by this timer task.
//         */
//        public void run()
//        {
//            log.debug("public void run(): called");
//
//            shutdown = true;
//            testResult.shutdownNow();
//        }
//    }
}
