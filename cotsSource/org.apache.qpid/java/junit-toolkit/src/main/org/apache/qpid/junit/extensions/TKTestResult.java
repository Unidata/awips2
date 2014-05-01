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
import junit.framework.TestCase;
import junit.framework.TestResult;

import org.apache.log4j.Logger;

import org.apache.qpid.junit.extensions.listeners.TKTestListener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

/**
 * TKTestResult extends TestResult in order to calculate test timings, to pass the variable integer parameter for
 * parameterized test cases to those test cases and to introduce an optional delay before test starts. Interested
 * {@link TKTestListener}s may be attached to this and will be informed of all relevant test statistics.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Calculate test timings.
 * <tr><td> Inform timing listeners of timings.
 * <tr><td> Inform memory listeners of memory readings.
 * <tr><td> Inform parameters listeners of parameters.
 * <tr><td> Pass the integer parameter to parameterized test cases.
 * <tr><td> Provide verbose test information on test start and end.
 * </table>
 *
 * @todo Move the verbose test information on test start/end into a test listener instead. It confuses the intention
 *       of this class. Could also move the delay into a listener but that seems less appropriate as it would be a
 *       side-effecting listener. Delay and timing calculation are fundamental enough to this class.
 *
 * @todo The need for this class to act as a place-holder for the integer parameter for parameterized test cases is
 *       because this behaviour has been factored out into a test decorator class, see {@link AsymptoticTestDecorator}.
 *       The {@link AsymptoticTestDecorator#run} method takes a TestResult as an argument and cannot easily get to the
 *       {@link AsymptoticTestCase} class other than through this class. The option of using this class as a place hold
 *       for this value was chosen. Alternatively this class could provide a method for decorators to access the
 *       underlying test case through and then leave the setting of this parameter to the decorator which is a more
 *       natural home for this behaviour. It would also provide a more general framework for decorators.
 *
 * @todo The memory usage may need to be moved in closer to the test method invocation so that as little code as possible
 *       exists between it and the test or the results may be obscured. In fact it certainly does as the teardown method
 *       is getting called first. Wouldn't be a bad idea to move the timing code in closer too.
 *
 * @todo Get rid of the delay logic. Will be replaced by throttle control.
 *
 * @author Rupert Smith
 */
public class TKTestResult extends TestResult
{
    /** Used for logging. */
    private static final Logger log = Logger.getLogger(TKTestResult.class);

    /** The delay between two tests. */
    private int delay = 0;

    /**
     * This flag indicates that the #completeTest method of the timing controller has been called. Once this has
     * been called once, the end test event for the whole test method should be ignored because tests have taken
     * charge of outputing their own timings.
     */
    private boolean completeTestUsed = false;

    /**
     * Thread locals to hold test start time for non-instrumented tests. (Instrumented tests hold their own
     * measurement data).
     */
    // private Hashtable threadStartTimeMap = new Hashtable();
    private ThreadLocal<ThreadLocalSettings> threadLocals = new ThreadLocal<ThreadLocalSettings>();

    /** Used to hold the current integer parameter to pass to parameterized tests. This defaults to 1. */
    private int n = 1;

    /** The timing listeners. */
    private Collection<TKTestListener> tkListeners;

    /** The test case name. */
    private String testCaseName;

    /** Used to hold the current concurrency level, set by the {@link ScaledTestDecorator}. */
    private int concurrencyLevel = 1;

    /** Flag used to indicate that this test result should attempt to complete its current tests as soon as possible. */
    private boolean shutdownNow = false;

    /** Holds the parametes that the test is run with. */
    private Properties testParameters;

    /**
     * Creates a new TKTestResult object.
     *
     * @param delay        A delay in milliseconds to introduce before every test start.
     * @param testCaseName The name of the test case that this is the TestResult object for.
     */
    public TKTestResult(int delay, String testCaseName)
    {
        super();

        /*log.debug("public TKTestResult(PrintStream writer, int " + delay + ", boolean " + verbose + ", String "
            + testCaseName + "): called");*/

        // Keep all the parameters that this is created with.
        this.delay = delay;
        this.testCaseName = testCaseName;
    }

    /**
     * Callback method use to inform this test result that a test will be started. Waits for the configured delay time
     * if one has been set, starts the timer, then delegates to the super class implementation.
     *
     * @param test The test to be started.
     */
    public void startTest(Test test)
    {
        // log.debug("public void startTest(Test test): called");

        // If a delay time has been specified then wait for that length of time.
        if (this.delay > 0)
        {
            try
            {
                Thread.sleep(delay);
            }
            catch (InterruptedException e)
            {
                // Ignore, but restore the interrupted flag.
                Thread.currentThread().interrupt();
            }
        }

        // Create the thread local settings for the test.
        ThreadLocalSettings threadLocalSettings = new ThreadLocalSettings();
        threadLocals.set(threadLocalSettings);

        // Record the test start time against this thread for calculating the test timing. (Consider using ThreadLocal
        // instead?)
        Long startTime = System.nanoTime();
        threadLocalSettings.startTime = startTime;
        // log.debug("startTime = " + startTime);

        // Check if the test is timing controller aware, in which case set up a new timing controller and hold it
        // in the thread local settings.
        if (test instanceof TimingControllerAware)
        {
            TimingControllerAware controllerAware = (TimingControllerAware) test;
            TimingControllerImpl controller =
                new TimingControllerImpl(this, test, startTime, Thread.currentThread().getId());
            controllerAware.setTimingController(controller);

            threadLocalSettings.timingController = controller;
        }

        // Delegate to the super method to notify test event listeners.
        super.startTest(test);
    }

    /**
     * Callback method use to inform this result that a test was completed. This calculates how long the test took
     * to run, then delegates to the super class implementation.
     *
     * @param test The test that has ended.
     */
    public void endTest(Test test)
    {
        // log.debug("public void endTest(Test test): called");

        long runTime = 0;

        // Recover the thread local settings.
        ThreadLocalSettings threadLocalSettings = threadLocals.get();

        // Check if the test is an instrumented test and get the timing information from the instrumentation as this
        // will be more accurate.
        if (test instanceof InstrumentedTest)
        {
            InstrumentedTest iTest = (InstrumentedTest) test;

            // Calculate the test run time.
            runTime = iTest.getTestTime();
            // log.debug("runTime = " + runTime);

            // Calculate the test memory usage.
            long startMem = iTest.getTestStartMemory();
            long endMem = iTest.getTestEndMemory();

            // log.debug("startMem = " + startMem);
            // log.debug("endMem = " + endMem);

            // Inform any memory listeners of the test memory.
            if (tkListeners != null)
            {
                for (TKTestListener memoryListener : tkListeners)
                {
                    memoryListener.memoryUsed(test, startMem, endMem, null);
                }
            }
        }
        else
        {
            // Calculate the test run time.
            long endTime = System.nanoTime();
            Long startTime = threadLocalSettings.startTime;
            runTime = endTime - startTime;
            // log.debug("runTime = " + runTime);

            threadLocals.remove();
        }

        // Output end test stats. This is only done when the tests have not used the timing controller to output
        // mutiple timings.
        if (!completeTestUsed)
        {
            // Check if the test is an asymptotic test case and get its int parameter if so.
            if (test instanceof AsymptoticTestCase)
            {
                AsymptoticTestCase pTest = (AsymptoticTestCase) test;

                // Set the parameter.
                int paramValue = pTest.getN();

                // Inform any parameter listeners of the test parameter.
                if (tkListeners != null)
                {
                    for (TKTestListener parameterListener : tkListeners)
                    {
                        parameterListener.parameterValue(test, paramValue, null);
                    }
                }
            }

            // Inform any timing listeners of the test timing and concurrency level.
            if (tkListeners != null)
            {
                for (TKTestListener tkListener : tkListeners)
                {
                    TKTestListener next = tkListener;

                    next.timing(test, runTime, null);
                    next.concurrencyLevel(test, concurrencyLevel, null);
                }
            }

            // Call the super method to notify test event listeners of the end event.
            super.endTest(test);
        }
    }

    /**
     * Gets the integer parameter to pass to parameterized test cases.
     *
     * @return The value of the integer parameter.
     */
    public int getN()
    {
        return n;
    }

    /**
     * Sets the integer parameter to pass to parameterized test cases.
     *
     * @param n The new value of the integer parameter.
     */
    public void setN(int n)
    {
        // log.debug("public void setN(int " + n + "): called");

        this.n = n;
    }

    /**
     * Adds a timing listener to pass all timing events to.
     *
     * @param listener The timing listener to register.
     */
    public void addTKTestListener(TKTestListener listener)
    {
        // Create the collection to hold the timing listeners if it does not already exist.
        if (tkListeners == null)
        {
            tkListeners = new ArrayList<TKTestListener>();
        }

        // Keep the new timing listener.
        tkListeners.add(listener);
    }

    /**
     * Called by the test runner to notify this that a new test batch is being begun. This method forwards this
     * notification to all batch listeners.
     */
    public void notifyStartBatch()
    {
        if (tkListeners != null)
        {
            for (TKTestListener batchListener : tkListeners)
            {
                batchListener.startBatch();
            }
        }
    }

    /**
     * Called by the test runner to notify this that the current test batch has been ended. This method forwards this
     * notification to all batch listener.
     */
    public void notifyEndBatch()
    {
        // log.debug("public void notifyEndBatch(): called");

        if (tkListeners != null)
        {
            for (TKTestListener batchListener : tkListeners)
            {
                batchListener.endBatch(testParameters);
            }
        }
    }

    /**
     * Called by the test runner to notify this of the properties that the test is using.
     *
     * @param properties The tests set/read properties.
     */
    public void notifyTestProperties(Properties properties)
    {
        // log.debug("public void notifyTestProperties(Properties properties): called");

        this.testParameters = properties;

        /*
        if (tkListeners != null)
        {
            for (TKTestListener batchListener : tkListeners)
            {
                batchListener.properties(properties);
            }
        }
         */
    }

    /**
     * Intercepts the execution of a test case to pass the variable integer parameter to a test if it is a parameterized
     * test case.
     *
     * @param test The test to run.
     */
    protected void run(final TestCase test)
    {
        // log.debug("protected void run(final TestCase test): called");

        // Check if the test case is a parameterized test and set its integer parameter if so.
        if (test instanceof AsymptoticTestCase)
        {
            AsymptoticTestCase pTest = (AsymptoticTestCase) test;

            // Set up the integer parameter.
            pTest.setN(n);
        }

        // Delegate to the super method to run the test.
        super.run(test);
    }

    /**
     * Helper method that generats a String of verbose information about a test. This includes the thread name, test
     * class name and test method name.
     *
     * @param test The test to generate the info string for.
     *
     * @return Returns a string with the thread name, test class name and test method name.
     */
    protected String getTestInfo(Test test)
    {
        // log.debug("protected String getTestInfo(Test test): called");

        return "[" + Thread.currentThread().getName() + "@" + test.getClass().getName() + "."
            + ((test instanceof TestCase) ? ((TestCase) test).getName() : "") + "]";
    }

    /**
     * Sets the concurrency level to pass into the test result.
     *
     * @param concurrencyLevel The concurrency level the tests are running out.
     */
    public void setConcurrencyLevel(int concurrencyLevel)
    {
        this.concurrencyLevel = concurrencyLevel;
    }

    /**
     * Tells this test result that it should stop running tests. Once this method has been called this test result
     * will not start any new tests, and any tests that use the timing controller will be passed interrupted exceptions,
     * to indicate that they should end immediately. Usually the caller of this method will introduce a short wait
     * to allow an opporunity for running tests to complete, before forcing the shutdown of the JVM.
     */
    public void shutdownNow()
    {
        log.debug("public void shutdownNow(): called on " + this);

        shutdownNow = true;
    }

    /**
     * Prints a string summary of this class, mainly for debugging purposes.
     *
     * @return A string summary of this class, mainly for debugging purposes.
     */
    public String toString()
    {
        return "TKTestResult@" + Integer.toString(hashCode(), 16) + ": [ testCaseName = " + testCaseName + ", n = " + n
            + ", tkListeners = " + tkListeners + " ]";
    }

    /**
     * Holds things that need to be kept on a per thread basis for each test invocation, such as the test start
     * time and its timing controller.
     */
    private static class ThreadLocalSettings
    {
        /** Holds the test start time. */
        Long startTime;

        /** Holds the test threads timing controller. */
        TimingController timingController;
    }

    /**
     * Provides an implementation of the {@link TimingController} interface that timing aware tests can use to call
     * back to reset timers, and register additional test timings.
     */
    private static class TimingControllerImpl implements TimingController
    {
        /** Holds an explicit reference to the test TKTestResult that created this. */
        TKTestResult testResult;

        /** Holds a reference to the test that this is the timing controller for. */
        Test test;

        /** Holds the start time for this timing controller. This gets reset to now on each completed test. */
        long startTime;

        /**
         * Holds the thread id of the thread that started the test, so that this controller may be called from other
         * threads but still identify itself correctly to {@link TKTestListener}s as being associated with the
         * thread that called the test method.
         */
        long threadId;

        /**
         * Creates a timing controller on a specified TKTestResult and a test.
         *
         * @param testResult       The TKTestResult that this controller interacts with.
         * @param test             The test that this is the timing controller for.
         * @param startTime        The test start time in nanoseconds.
         * @param threadId         The thread id of the thread that is calling the test method.
         */
        public TimingControllerImpl(TKTestResult testResult, Test test, long startTime, long threadId)
        {
            this.testResult = testResult;
            this.test = test;
            this.startTime = startTime;
            this.threadId = threadId;
        }

        /**
         * Gets the timing controller associated with the current test thread. Tests that use timing controller should
         * always get the timing controller from this method in the same thread that called the setUp, tearDown or test
         * method. The controller returned by this method may be called from any thread because it remembers the thread
         * id of the original test thread.
         *
         * @return The timing controller associated with the current test thread.
         */
        public TimingController getControllerForCurrentThread()
        {
            // Recover the thread local settings and extract the timing controller from them.
            ThreadLocalSettings threadLocalSettings = testResult.threadLocals.get();

            return threadLocalSettings.timingController;
        }

        /**
         * Not implemented yet.
         *
         * @return Nothing.
         */
        public long suspend()
        {
            throw new RuntimeException("Method not implemented.");
        }

        /**
         * Not implemented yet.
         *
         * @return Nothing.
         */
        public long resume()
        {
            throw new RuntimeException("Method not implemented.");
        }

        /**
         * Resets the timer start time to now.
         *
         * @return The new value of the start time.
         */
        public long restart()
        {
            startTime = System.nanoTime();

            return startTime;
        }

        /**
         * Register an additional pass/fail for the current test. The test result is assumed to apply to a test of
         * 'size' parmeter 1. Use the {@link #completeTest(boolean, int)} method to register timings with parameters.
         *
         * @param testPassed Whether or not this timing is for a test pass or fail.
         *
         * @throws InterruptedException If the test runner decides that testing should stop it throws this exception to
         *                              indicate to the test method that it should stop immediately.
         */
        public void completeTest(boolean testPassed) throws InterruptedException
        {
            completeTest(testPassed, 1);
        }

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
        public void completeTest(boolean testPassed, int param) throws InterruptedException
        {
            /*log.debug("public long completeTest(boolean testPassed = " + testPassed + ", int param = " + param
                + "): called");*/

            // Calculate the test run time.
            long endTime = System.nanoTime();
            long runTime = endTime - startTime;
            // log.debug("runTime = " + runTime);

            // Reset the test start time to now, to reset the timer for the next result.
            startTime = endTime;

            completeTest(testPassed, param, runTime);
        }

        /**
         * Register an additional pass/fail for the current test. The test result is applies to a test of the specified
         * 'size' parmeter and allows the caller to sepecify the timing to log.
         *
         * @param testPassed Whether or not this timing is for a test pass or fail.
         * @param param      The test parameter size for parameterized tests.
         * @param timeNanos  The time in nano-seconds to log the test result with.
         *
         * @throws InterruptedException If the test runner decides that testing should stop it throws this exception to
         *                              indicate to the test method that it should stop immediately.
         */
        public void completeTest(boolean testPassed, int param, long timeNanos) throws InterruptedException
        {
            log.debug("public void completeTest(boolean testPassed, int param, long timeNanos): called");
            log.debug("testResult = " + testResult);

            // Tell the test result that completeTest has been used, so to not register end test events for the whole
            // test method.
            testResult.completeTestUsed = true;

            // Inform any timing listeners of the test timings and parameters and send an end test notification using
            // the thread id of the thread that started the test.
            if (testResult.tkListeners != null)
            {
                for (TKTestListener listener : testResult.tkListeners)
                {
                    listener.reset(test, threadId);
                    listener.timing(test, timeNanos, threadId);
                    listener.parameterValue(test, param, threadId);
                    listener.concurrencyLevel(test, testResult.concurrencyLevel, threadId);

                    if (!testPassed)
                    {
                        listener.addFailure(test, null, threadId);
                    }

                    listener.endTest(test, threadId);
                }
            }

            // log.debug("testResult.shutdownNow = " + testResult.shutdownNow);

            // Check if the test runner has been asked to shutdown and raise an interuppted exception if so.
            if (testResult.shutdownNow)
            {
                // log.debug("The shutdown flag is set.");

                throw new InterruptedException("Attempting clean shutdown by suspending current test.");
            }
        }
    }
}
