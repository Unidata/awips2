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
package org.apache.qpid.test.framework.listeners;

import junit.framework.AssertionFailedError;
import junit.framework.Test;
import junit.framework.TestCase;

import org.apache.log4j.Logger;

import org.apache.qpid.junit.extensions.ShutdownHookable;
import org.apache.qpid.junit.extensions.listeners.TKTestListener;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.*;

/**
 * Listens for test results for a named test and outputs these in the standard JUnit XML format to the specified
 * writer.
 *
 * <p/>The API for this listener accepts notifications about different aspects of a tests results through different
 * methods, so some assumption needs to be made as to which test result a notification refers to. For example
 * {@link #startTest} will be called, then possibly {@link #timing} will be called, even though the test instance is
 * passed in both cases, it is not enough to distinguish a particular run of the test, as the test case instance may
 * be being shared between multiple threads, or being run a repeated number of times, and can therfore be re-used
 * between calls. The listeners make the assumption that, for every test, a unique thread will call {@link #startTest}
 * and {@link #endTest} to delimit each test. All calls to set test parameters, timings, state and so on, will occur
 * between the start and end and will be given with the same thread id as the start and end, so the thread id provides
 * a unqiue value to identify a particular test run against.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Listen to test lifecycle notifications.
 * <tr><td> Listen to test errors and failures.
 * <tr><td> Listen to test timings.
 * <tr><td> Listen to test memory usages.
 * <tr><td> Listen to parameterized test parameters.
 * <tr><th> Responsibilities
 * </table>
 *
 * @todo Merge this class with CSV test listener, making the collection of results common to both, and only factoring
 *       out the results printing code into sub-classes. Provide a simple XML results formatter with the same format as
 *       the ant XML formatter, and a more structured one for outputing results with timings and summaries from
 *       performance tests.
 */
public class XMLTestListener implements TKTestListener, ShutdownHookable
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(XMLTestListener.class);

    /** The results file writer. */
    protected Writer writer;

    /** Holds the results for individual tests. */
    // protected Map<Result, Result> results = new LinkedHashMap<Result, Result>();
    // protected List<Result> results = new ArrayList<Result>();

    /**
     * Map for holding results on a per thread basis as they come in. A ThreadLocal is not used as sometimes an
     * explicit thread id must be used, where notifications come from different threads than the ones that called
     * the test method.
     */
    Map<Long, Result> threadLocalResults = Collections.synchronizedMap(new LinkedHashMap<Long, Result>());

    /**
     * Holds results for tests that have ended. Transferring these results here from the per-thread results map, means
     * that the thread id is freed for the thread to generate more results.
     */
    List<Result> results = new ArrayList<Result>();

    /** Holds the overall error count. */
    protected int errors = 0;

    /** Holds the overall failure count. */
    protected int failures = 0;

    /** Holds the overall tests run count. */
    protected int runs = 0;

    /** Holds the name of the class that tests are being run for. */
    String testClassName;

    /**
     * Creates a new XML results output listener that writes to the specified location.
     *
     * @param writer        The location to write results to.
     * @param testClassName The name of the test class to include in the test results.
     */
    public XMLTestListener(Writer writer, String testClassName)
    {
        log.debug("public XMLTestListener(Writer writer, String testClassName = " + testClassName + "): called");

        this.writer = writer;
        this.testClassName = testClassName;
    }

    /**
     * Resets the test results to the default state of time zero, memory usage zero, parameter zero, test passed.
     *
     * @param test     The test to resest any results for.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void reset(Test test, Long threadId)
    {
        log.debug("public void reset(Test test = " + test + ", Long threadId = " + threadId + "): called");

        XMLTestListener.Result r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        r.error = null;
        r.failure = null;

    }

    /**
     * Notification that a test started.
     *
     * @param test The test that started.
     */
    public void startTest(Test test)
    {
        log.debug("public void startTest(Test test = " + test + "): called");

        Result newResult = new Result(test.getClass().getName(), ((TestCase) test).getName());

        // Initialize the thread local test results.
        threadLocalResults.put(Thread.currentThread().getId(), newResult);
        runs++;
    }

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
     * Notifies listeners of the tests read/set properties.
     *
     * @param properties The tests read/set properties.
     */
    public void properties(Properties properties)
    { }

    /**
     * Notification that a test ended.
     *
     * @param test The test that ended.
     */
    public void endTest(Test test)
    {
        log.debug("public void endTest(Test test = " + test + "): called");

        // Move complete test results into the completed tests list.
        Result r = threadLocalResults.get(Thread.currentThread().getId());
        results.add(r);

        // Clear all the test results for the thread.
        threadLocalResults.remove(Thread.currentThread().getId());
    }

    /**
     * Called when a test completes. Success, failure and errors. This method should be used when registering an
     * end test from a different thread than the one that started the test.
     *
     * @param test     The test which completed.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void endTest(Test test, Long threadId)
    {
        log.debug("public void endTest(Test test = " + test + ", Long threadId = " + threadId + "): called");

        // Move complete test results into the completed tests list.
        Result r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);
        results.add(r);

        // Clear all the test results for the thread.
        threadLocalResults.remove(Thread.currentThread().getId());
    }

    /**
     * An error occurred.
     *
     * @param test The test in which the error occurred.
     * @param t    The throwable that resulted from the error.
     */
    public void addError(Test test, Throwable t)
    {
        log.debug("public void addError(Test test = " + test + ", Throwable t = " + t + "): called");

        Result r = threadLocalResults.get(Thread.currentThread().getId());
        r.error = t;
        errors++;
    }

    /**
     * A failure occurred.
     *
     * @param test The test in which the failure occurred.
     * @param t    The JUnit assertions that led to the failure.
     */
    public void addFailure(Test test, AssertionFailedError t)
    {
        log.debug("public void addFailure(Test test = " + test + ", AssertionFailedError t = " + t + "): called");

        Result r = threadLocalResults.get(Thread.currentThread().getId());
        r.failure = t;
        failures++;
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
        log.debug("public void addFailure(Test test, AssertionFailedError e, Long threadId): called");

        Result r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);
        r.failure = e;
        failures++;
    }

    /**
     * Notifies listeners of the start of a complete run of tests.
     */
    public void startBatch()
    {
        log.debug("public void startBatch(): called");

        // Reset all results counts.
        threadLocalResults = Collections.synchronizedMap(new HashMap<Long, Result>());
        errors = 0;
        failures = 0;
        runs = 0;

        // Write out the file header.
        try
        {
            writer.write("<?xml version=\"1.0\" ?>\n");
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to write the test results.", e);
        }
    }

    /**
     * Notifies listeners of the end of a complete run of tests.
     *
     * @param parameters The optional test parameters to log out with the batch results.
     */
    public void endBatch(Properties parameters)
    {
        log.debug("public void endBatch(Properties parameters = " + parameters + "): called");

        // Write out the results.
        try
        {
            // writer.write("<?xml version=\"1.0\" ?>\n");
            writer.write("<testsuite errors=\"" + errors + "\" failures=\"" + failures + "\" tests=\"" + runs + "\" name=\""
                + testClassName + "\">\n");

            for (Result result : results)
            {
                writer.write("  <testcase classname=\"" + result.testClass + "\" name=\"" + result.testName + "\">\n");

                if (result.error != null)
                {
                    writer.write("    <error type=\"" + result.error.getClass() + "\">");
                    result.error.printStackTrace(new PrintWriter(writer));
                    writer.write("    </error>");
                }
                else if (result.failure != null)
                {
                    writer.write("    <failure type=\"" + result.failure.getClass() + "\">");
                    result.failure.printStackTrace(new PrintWriter(writer));
                    writer.write("    </failure>");
                }

                writer.write("  </testcase>\n");
            }

            writer.write("</testsuite>\n");
            writer.flush();
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to write the test results.", e);
        }
    }

    /**
     * Supplies the shutdown hook.
     *
     * @return The shut down hook.
     */
    public Thread getShutdownHook()
    {
        return new Thread(new Runnable()
                {
                    public void run()
                    {
                        log.debug("XMLTestListener::ShutdownHook: called");
                    }
                });
    }

    /**
     * Used to capture the results of a particular test run.
     */
    protected static class Result
    {
        /** Holds the name of the test class. */
        public String testClass;

        /** Holds the name of the test method. */
        public String testName;

        /** Holds the exception that caused error in this test. */
        public Throwable error;

        /** Holds the assertion exception that caused failure in this test. */
        public AssertionFailedError failure;

        /**
         * Creates a placeholder for the results of a test.
         *
         * @param testClass The test class.
         * @param testName  The name of the test that was run.
         */
        public Result(String testClass, String testName)
        {
            this.testClass = testClass;
            this.testName = testName;
        }
    }
}
