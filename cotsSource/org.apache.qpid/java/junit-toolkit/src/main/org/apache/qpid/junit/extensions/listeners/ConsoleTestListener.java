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
package org.apache.qpid.junit.extensions.listeners;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestListener;

import org.apache.qpid.junit.extensions.SleepThrottle;
import org.apache.qpid.junit.extensions.Throttle;

import java.util.Properties;

/**
 * ConsoleTestListener provides feedback to the console, as test timings are taken, by drawing a '.', or an 'E', or an
 * 'F', for each test that passes, is in error or fails. It does this for every test result registered with the framework,
 * not just on the completion of each test method as the JUnit one does. It also uses a throttle to cap the rate of
 * dot drawing, as exessively high rates can degrade test performance without providing much usefull feedback to the user.
 * Unlike the JUnit dot drawing feedback, this one will correctly wrap lines when tests are run concurrently (the
 * rate capping ensures that this does not become a hot-spot for thread contention).
 *
 * <p/>Where rate capping causes the conflation of multiple requested dots into a single dot, the dot that is actually
 * drawn will be the worst result within the conflation period, that is, error is worse than fail which is worse than pass.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Draw dots as each test result completes, at a capped rate.
 * </table>
 *
 * @author Rupert Smith
 */
public class ConsoleTestListener implements TestListener, TKTestListener
{
    /** Used to indicate a test pass. */
    private static final int PASS = 1;

    /** Used to indicate a test failure. */
    private static final int FAIL = 2;

    /** Used to indicate a test error. */
    private static final int ERROR = 3;

    /** Defines the maximum number of columns of dots to print. */
    private static final int MAX_COLUMNS = 80;

    /** Used to throttle the dot writing rate. */
    Throttle throttle;

    /** Tracks the worst test result so far, when the throttled print method must conflate results due to throttling. */
    private int conflatedResult = 0;

    /** Tracks the column count as dots are printed, so that newlines can be inserted at the right margin. */
    private int columnCount = 0;

    /** Used as a monitor on the print method criticial section, to ensure that line ends always happen in the right place. */
    private final Object printMonitor = new Object();

    /**
     * Creates a dot drawing feedback test listener, set by default to 80 columns at 80 dots per second capped rate.
     */
    public ConsoleTestListener()
    {
        throttle = new SleepThrottle();
        throttle.setRate(80f);
    }

    /**
     * Prints dots at a capped rate, conflating the requested type of dot to draw if this method is called at a rate
     * higher than the capped rate. The conflation works by always printing the worst result that occurs within the
     * conflation period, that is, error is worse than fail which is worse than a pass.
     *
     * @param result The type of dot to draw, {@link #PASS}, {@link #FAIL} or {@link #ERROR}.
     */
    private void throttledPrint(int result)
    {
        conflatedResult = (result > conflatedResult) ? result : conflatedResult;

        if (throttle.checkThrottle())
        {
            synchronized (printMonitor)
            {
                switch (conflatedResult)
                {
                default:
                case PASS:
                    System.out.print('.');
                    break;

                case FAIL:
                    System.out.print('F');
                    break;

                case ERROR:
                    System.out.print('E');
                    break;
                }

                columnCount = (columnCount >= MAX_COLUMNS) ? 0 : (columnCount + 1);

                if (columnCount == 0)
                {
                    System.out.print('\n');
                }

                conflatedResult = 0;
            }
        }
    }

    /**
     * An error occurred.
     *
     * @param test The test in error. Ignored.
     * @param t    The error that the test threw. Ignored.
     */
    public void addError(Test test, Throwable t)
    {
        throttledPrint(ERROR);
    }

    /**
     * A failure occurred.
     *
     * @param test The test that failed. Ignored.
     * @param t    The assertion failure that the test threw. Ignored.
     */
    public void addFailure(Test test, AssertionFailedError t)
    {
        throttledPrint(FAIL);
    }

    /**
     * A test ended.
     *
     * @param test The test that ended. Ignored.
     */
    public void endTest(Test test)
    {
        throttledPrint(PASS);
    }

    /**
     * A test started.
     *
     * @param test The test that started. Ignored.
     */
    public void startTest(Test test)
    { }

    /**
     * Resets the test results to the default state of time zero, memory usage zero, parameter zero, test passed.
     *
     * @param test     The test to resest any results for.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void reset(Test test, Long threadId)
    { }

    /**
     * Should be called every time a test completes with the run time of that test.
     *
     * @param test     The name of the test.
     * @param nanos    The run time of the test in nanoseconds.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void timing(Test test, long nanos, Long threadId)
    { }

    /**
     * Should be called every time a test completed with the amount of memory used before and after the test was run.
     *
     * @param test     The test which memory was measured for.
     * @param memStart The total JVM memory used before the test was run.
     * @param memEnd   The total JVM memory used after the test was run.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void memoryUsed(Test test, long memStart, long memEnd, Long threadId)
    { }

    /**
     * Should be called every time a parameterized test completed with the int value of its test parameter.
     *
     * @param test      The test which memory was measured for.
     * @param parameter The int parameter value.
     * @param threadId  Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void parameterValue(Test test, int parameter, Long threadId)
    { }

    /**
     * Should be called every time a test completes with the current number of test threads running.
     *
     * @param test     The test for which the measurement is being generated.
     * @param threads  The number of tests being run concurrently.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void concurrencyLevel(Test test, int threads, Long threadId)
    { }

    /**
     * Called when a test completes. Success, failure and errors. This method should be used when registering an
     * end test from a different thread than the one that started the test.
     *
     * @param test     The test which completed.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void endTest(Test test, Long threadId)
    {
        throttledPrint(PASS);
    }

    /**
     * Called when a test completes to mark it as a test fail. This method should be used when registering a
     * failure from a different thread than the one that started the test.
     *
     * @param test     The test which failed.
     * @param e        The assertion that failed the test.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void addFailure(Test test, AssertionFailedError e, Long threadId)
    {
        throttledPrint(FAIL);
    }

    /**
     * Notifies listeners of the start of a complete run of tests.
     */
    public void startBatch()
    { }

    /**
     * Notifies listeners of the end of a complete run of tests.
     *
     * @param parameters The optional test parameters to log out with the batch results.
     */
    public void endBatch(Properties parameters)
    { }

    /**
     * Notifies listeners of the tests read/set properties.
     *
     * @param properties The tests read/set properties.
     */
    public void properties(Properties properties)
    { }
}
