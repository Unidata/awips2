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

import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

/**
 * A test decorator that runs a test many times simultaneously in many threads.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Clone a test run into many threads and run them simultaneously.
 * <tr><td> Inform the test results of the start and end of each concurrent test batch. <td> {@link TKTestResult}
 * <tr><td> Inform the test results of the concurrency level. <td> {@link TKTestResult}
 * </table>
 *
 * @author Rupert Smith
 */
public class ScaledTestDecorator extends WrappedSuiteTestDecorator implements ShutdownHookable // TestDecorator
{
    /** Used for logging. */
    // private static final Logger log = Logger.getLogger(ScaledTestDecorator.class);

    /** Determines how long to wait for tests to cleanly exit on shutdown. */
    private static final long SHUTDOWN_PAUSE = 3000;

    /**
     * The stress levels or numbers of simultaneous threads to run the test in. The test is repeated at each of
     * the concurrency levels specified here. Defaults to 1 thread.
     */
    private int[] threads = new int[] { 1 };

    /** Used to hold the number of tests currently being run in parallel. */
    private int concurrencyLevel;

    /** The test to run. */
    private WrappedSuiteTestDecorator test;

    /**
     * Used to hold the current {@link TKTestResult} for the tests currently being run. This is made available so that
     * the shutdown hook can ask it to cleanly end the current tests in the event of a shutdown.
     */
    private TKTestResult currentTestResult;

    /** Flag set by the shutdown hook. This decorator will not start any new tests when this is set. */
    private boolean shutdown = false;

    /**
     * Creates an active test with default multiplier (1).
     *
     * @param test The target test.
     */
    public ScaledTestDecorator(WrappedSuiteTestDecorator test)
    {
        super(test);
        this.test = test;
    }

    /**
     * Creates a concurrently scaled test with the specified number of threads.
     *
     * @param test       The target test.
     * @param numThreads The stress level.
     */
    public ScaledTestDecorator(WrappedSuiteTestDecorator test, int numThreads)
    {
        this(test, new int[] { numThreads });
    }

    /**
     * Creates a concurrently scaled test with the specified thread levels, the test is repeated at each level.
     *
     * @param test    The target test.
     * @param threads The concurrency levels.
     */
    public ScaledTestDecorator(WrappedSuiteTestDecorator test, int[] threads)
    {
        super(test);

        /*log.debug("public ScaledTestDecorator(WrappedSuiteTestDecorator test = \"" + test + "\", int[] threads = "
                  + MathUtils.printArray(threads) + "): called");*/

        this.test = test;
        this.threads = threads;
    }

    /**
     * Runs the test simultaneously in at the specified concurrency levels.
     *
     * @param testResult The results object to monitor the test results with.
     */
    public void run(TestResult testResult)
    {
        // log.debug("public void run(TestResult testResult = " + testResult + "): called");

        // Loop through all of the specified concurrent levels for the test, provided shutdown has not been called.
        for (int i = 0; (i < threads.length) && !shutdown; i++)
        {
            // Get the number of threads for this run.
            int numThreads = threads[i];

            // Create test thread handlers for all the threads.
            TestThreadHandler[] threadHandlers = new TestThreadHandler[numThreads];

            // Create a cyclic barrier for the test threads to synch their setups and teardowns on.
            CyclicBarrier barrier = new CyclicBarrier(numThreads);

            // Set up the test thread handlers to output results to the same test results object.
            for (int j = 0; j < numThreads; j++)
            {
                threadHandlers[j] = new TestThreadHandler(testResult, test, barrier);
            }

            // Ensure the concurrency level statistic is set up correctly.
            concurrencyLevel = numThreads;

            // Begin batch.
            if (testResult instanceof TKTestResult)
            {
                TKTestResult tkResult = (TKTestResult) testResult;
                // tkResult.notifyStartBatch();
                tkResult.setConcurrencyLevel(numThreads);

                // Set the test result for the currently running tests, so that the shutdown hook can call it if necessary.
                currentTestResult = tkResult;
            }

            // Run all the tests and wait for them all to finish.
            executeAndWaitForRunnables(threadHandlers);

            // Clear the test result for the currently running tests.
            currentTestResult = null;

            // End batch.
            if (testResult instanceof TKTestResult)
            {
                TKTestResult tkResult = (TKTestResult) testResult;
                tkResult.notifyEndBatch();
            }

            // Clear up all the test threads, they hold references to their associated TestResult object and Test object,
            // which may prevent them from being garbage collected as the TestResult and Test objects are long lived.
            for (int j = 0; j < numThreads; j++)
            {
                threadHandlers[j].testResult = null;
                threadHandlers[j].test = null;
                threadHandlers[j] = null;
            }
        }
    }

    /**
     * Reports the number of tests that the scaled decorator is currently running concurrently.
     *
     * @return The number of tests that the scaled decorator is currently running concurrently.
     */
    public int getConcurrencyLevel()
    {
        return concurrencyLevel;
    }

    /**
     * Executes all of the specifed runnable using the thread pool and waits for them all to complete.
     *
     * @param runnables The set of runnables to execute concurrently.
     */
    private void executeAndWaitForRunnables(Runnable[] runnables)
    {
        int numThreads = runnables.length;

        // Used to keep track of the test threads in order to know when they have all completed.
        Thread[] threads = new Thread[numThreads];

        // Create all the test threads.
        for (int j = 0; j < numThreads; j++)
        {
            threads[j] = new Thread(runnables[j]);
        }

        // Start all the test threads.
        for (int j = 0; j < numThreads; j++)
        {
            threads[j].start();
        }

        // Wait for all the test threads to complete.
        for (int j = 0; j < numThreads; j++)
        {
            try
            {
                threads[j].join();
            }
            catch (InterruptedException e)
            {
                // Restore the interrupted state of the thread.
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * Supplies the shut-down hook.
     *
     * @return The shut-down hook.
     */
    public Thread getShutdownHook()
    {
        return new Thread(new Runnable()
                {
                    public void run()
                    {
                        // log.debug("ScaledTestDecorator::ShutdownHook: called");

                        // Set the shutdown flag so that no new tests are started.
                        shutdown = true;

                        // Check if tests are currently running, and ask them to complete as soon as possible. Allow
                        // a short pause for this to happen.
                        TKTestResult testResult = currentTestResult;

                        if (testResult != null)
                        {
                            // log.debug("There is a test result currently running tests, asking it to terminate ASAP.");
                            testResult.shutdownNow();

                            try
                            {
                                Thread.sleep(SHUTDOWN_PAUSE);
                            }
                            catch (InterruptedException e)
                            {
                                // Restore the interrupted state of the thread.
                                Thread.currentThread().interrupt();
                            }
                        }
                    }
                });
    }

    /**
     * Prints a string summarizing this test decorator, mainly for debugging purposes.
     *
     * @return String representation for debugging purposes.
     */
    public String toString()
    {
        return "ScaledTestDecorator: [ test = " + test + ", concurrencyLevel = " + concurrencyLevel + " ]";
    }

    /**
     * TestThreadHandler is a runnable used to execute a test in. This is static to avoid implicit 'this' reference to
     * the longer lived ScaledTestDecorator class. The scaled test decorator may execute many repeats but creates fresh
     * handlers for each one. It re-uses the threads in a pool but does not re-use these handlers.
     */
    private static class TestThreadHandler implements Runnable
    {
        /** The test result object for the test to be run with. */
        TestResult testResult;

        /** The test to run. */
        WrappedSuiteTestDecorator test;

        /** Holds the cyclic barrier to synchronize on the end of the setups and before the tear downs. */
        CyclicBarrier barrier;

        /**
         * Creates a new TestThreadHandler object.
         *
         * @param testResult The test result object for the test to be run with.
         * @param test       The test to run in a sperate thread.
         * @param barrier    The barrier implementation to use to synchronize per-thread setup completion and test
         *                   completion before moving on through the setup, test, teardown phases. The barrier should
         *                   be configured for the number of test threads.
         */
        TestThreadHandler(TestResult testResult, WrappedSuiteTestDecorator test, CyclicBarrier barrier)
        {
            this.testResult = testResult;
            this.test = test;
            this.barrier = barrier;
        }

        /**
         * Runs the test associated with this pool.
         */
        public void run()
        {
            try
            {
                // Call setup on all underlying tests in the suite that are thread aware.
                for (Test childTest : test.getAllUnderlyingTests())
                {
                    // Check that the test is concurrency aware, so provides a setup method to call.
                    if (childTest instanceof TestThreadAware)
                    {
                        // Call the tests per thread setup.
                        TestThreadAware setupTest = (TestThreadAware) childTest;
                        setupTest.threadSetUp();
                    }
                }

                // Wait until all test threads have completed their setups.
                barrier.await();

                // Start timing the test batch, only after thread setups have completed.
                if (testResult instanceof TKTestResult)
                {
                    ((TKTestResult) testResult).notifyStartBatch();
                }

                // Run the tests.
                test.run(testResult);

                // Wait unitl all test threads have completed their tests.
                barrier.await();

                // Call tear down on all underlying tests in the suite that are thread aware.
                for (Test childTest : test.getAllUnderlyingTests())
                {
                    // Check that the test is concurrency aware, so provides a teardown method to call.
                    if (childTest instanceof TestThreadAware)
                    {
                        // Call the tests per thread tear down.
                        TestThreadAware setupTest = (TestThreadAware) childTest;
                        setupTest.threadTearDown();
                    }
                }
            }
            catch (InterruptedException e)
            {
                // Restore the interrupted state of the thread.
                Thread.currentThread().interrupt();
            }
            catch (BrokenBarrierException e)
            {
                // Set the interrupted state on the thread. The BrokenBarrierException may be caused where one thread
                // waiting for the barrier is interrupted, causing the remaining threads correctly waiting on the
                // barrier to fail. This condition is expected during test interruptions, and the response to it is to
                // interrupt all the other threads running in the same scaled test.
                Thread.currentThread().interrupt();
            }
        }

        /**
         * Prints the name of the test for debugging purposes.
         *
         * @return The name of the test.
         */
        public String toString()
        {
            return "ScaledTestDecorator: [test = \"" + test + "\"]";
        }
    }
}
