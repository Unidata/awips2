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
import junit.framework.TestCase;
import junit.framework.TestListener;

import org.apache.log4j.Logger;

import org.apache.qpid.junit.extensions.ShutdownHookable;
import org.apache.qpid.junit.extensions.util.TestContextProperties;

import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TreeSet;

/**
 * CSVTestListener is both a test listener, a timings listener, a memory listener and a parameter listener. It listens for test completion events and
 * then writes out all the data that it has listened to into a '.csv' (comma seperated values) file.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Listen to test events; start, end, fail, error.
 * <tr><td> Listen to test timings.
 * <tr><td> Listen to test memory usage.
 * <tr><td> Listen to parameterized test parameters.
 * <tr><td> Output all test data to a CSV file.
 * </table>
 *
 * @author Rupert Smith
 *
 * @todo Write an XML output class. Write a transform to convert it into an HTML page with timings as graphs.
 */
public class CSVTestListener implements TestListener, TKTestListener, ShutdownHookable
{
    /** Used for logging. */
    private static final Logger log = Logger.getLogger(CSVTestListener.class);

    /** The timings file writer. */
    private Writer timingsWriter;

    /**
     * Map for holding results on a per thread basis as they come in. A ThreadLocal is not used as sometimes an
     * explicit thread id must be used, where notifications come from different threads than the ones that called
     * the test method.
     */
    Map<Long, TestResult> threadLocalResults = Collections.synchronizedMap(new HashMap<Long, TestResult>());

    /** Used to record the start time of a complete test run, for outputing statistics at the end of the test run. */
    private long batchStartTime;

    /** Used to record the number of errors accross a complete test run. */
    private int numError;

    /** Used to record the number of failures accross a complete test run. */
    private int numFailed;

    /** Used to record the number of passes accross a complete test run. */
    private int numPassed;

    /** Used to record the total tests run accross a complete test run. Always equal to passes + errors + fails. */
    private int totalTests;

    /** Used to recrod the current concurrency level for the test batch. */
    private int concurrencyLevel;

    /**
     * Used to record the total 'size' of the tests run, this is the number run times the average value of the test
     * size parameters.
     */
    private int totalSize;

    /**
     * Used to record the summation of all of the individual test timgings. Note that total time and summed time
     * are unlikely to be in agreement, exception for a single threaded test (with no setup time). Total time is
     * the time taken to run all the tests, summed time is the added up time that each individual test took. So if
     * two tests run in parallel and take one second each, total time will be one seconds, summed time will be two
     * seconds.
     */
    private long summedTime;

    /** Flag to indicate when batch has been started but not ended to ensure end batch stats are output only once. */
    private boolean batchStarted = false;

    /**
     * Creates a new CSVTestListener object.
     *
     * @param writer A writer where this CSV listener should write out its output to.
     */
    public CSVTestListener(Writer writer)
    {
        // log.debug("public CSVTestListener(Writer writer): called");

        // Keep the writer.
        this.timingsWriter = writer;
    }

    /**
     * Resets the test results to the default state of time zero, memory usage zero, test passed.
     *
     * @param test     The test to resest any results for.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void reset(Test test, Long threadId)
    {
        // log.debug("public void reset(Test test = \"" + test + "\", Long threadId = " + threadId + "): called");

        TestResult r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        r.testTime = 0L;
        r.testStartMem = 0L;
        r.testEndMem = 0L;
        r.testState = "Pass";
        r.testParam = 0;
    }

    /**
     * Called when a test results in an error.
     *
     * @param test The test which is in error.
     * @param t Any Throwable raised by the test in error.
     */
    public void addError(Test test, Throwable t)
    {
        // log.debug("public void addError(Test test, Throwable t): called");

        TestResult r = threadLocalResults.get(Thread.currentThread().getId());
        r.testState = "Error";
    }

    /**
     * Called when a test results in a failure.
     *
     * @param test The test which failed.
     * @param t The AssertionFailedError that encapsulates the test failure.
     */
    public void addFailure(Test test, AssertionFailedError t)
    {
        // log.debug("public void addFailure(Test \"" + test + "\", AssertionFailedError t): called");

        TestResult r = threadLocalResults.get(Thread.currentThread().getId());
        r.testState = "Failure";
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
        // log.debug("public void addFailure(Test test = \"" + test + "\", AssertionFailedError e, Long threadId = " + threadId
        // + "): called");

        TestResult r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        r.testState = "Failure";
    }

    /**
     * Called when a test completes. Success, failure and errors.
     *
     * @param test The test which completed.
     */
    public void endTest(Test test)
    {
        // log.debug("public void endTest(Test \"" + test + "\"): called");

        TestResult r = threadLocalResults.get(Thread.currentThread().getId());

        writeTestResults(r, test);

        // Clear all the test results for the thread.
        threadLocalResults.remove(Thread.currentThread().getId());
    }

    /**
     * Called when a test starts.
     *
     * @param test The test wich has started.
     */
    public void startTest(Test test)
    {
        // log.debug("public void startTest(Test \"" + test + "\"): called");

        // Initialize the thread local test results.
        threadLocalResults.put(Thread.currentThread().getId(), new TestResult());
    }

    /**
     * Should be called every time a test completes with the run time of that test.
     *
     * @param test     The name of the test.
     * @param nanos   The run time of the test in nanoseconds.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void timing(Test test, long nanos, Long threadId)
    {
        // log.debug("public void timing(String \"" + test + "\", long " + nanos + "): called");

        TestResult r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        r.testTime = nanos;
        summedTime += nanos;
    }

    /**
     * Should be called every time a test completed with the amount of memory used before and after the test was run.
     *
     * @param test     The test which memory was measured for.
     * @param memStart The total JVM memory used before the test was run.
     * @param memEnd   The total JVM memory used after the test was run.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void memoryUsed(Test test, long memStart, long memEnd, Long threadId)
    {
        // log.debug("public void memoryUsed(Test \"" + test + "\", long " + memStart + ", long " + memEnd + ", Long "
        // + threadId + "): called");

        TestResult r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        r.testStartMem = memStart;
        r.testEndMem = memEnd;
    }

    /**
     * Should be called every time a parameterized test completed with the int value of its test parameter.
     *
     * @param test      The test which memory was measured for.
     * @param parameter The int parameter value.
     * @param threadId  Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void parameterValue(Test test, int parameter, Long threadId)
    {
        // log.debug("public void parameterValue(Test test = \"" + test + "\", int parameter = " + parameter + "): called");

        TestResult r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        r.testParam = parameter;
        totalSize += parameter;
    }

    /**
     * Should be called every time a test completes with the current number of test threads running. This should not
     * change within a test batch, therefore it is safe to take this as a batch level property value too.
     *
     * @param test     The test for which the measurement is being generated.
     * @param threads  The number of tests being run concurrently.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void concurrencyLevel(Test test, int threads, Long threadId)
    {
        // log.debug("public void concurrencyLevel(Test test = \"" + test + "\", int threads = " + threads + "): called");

        TestResult r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        r.testConcurrency = threads;
        concurrencyLevel = threads;

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
        // log.debug("public void endTest(Test test = \"" + test + "\", Long threadId " + threadId + "): called");

        TestResult r =
            (threadId == null) ? threadLocalResults.get(Thread.currentThread().getId()) : threadLocalResults.get(threadId);

        writeTestResults(r, test);
    }

    /**
     * Takes a time stamp for the beginning of the batch and resets stats counted for the batch.
     */
    public synchronized void startBatch()
    {
        numError = 0;
        numFailed = 0;
        numPassed = 0;
        totalTests = 0;
        totalSize = 0;
        batchStartTime = System.nanoTime();
        summedTime = 0;
        batchStarted = true;

        // Write out the column headers for the batch.
        writeColumnHeaders();
    }

    /**
     * Takes a time stamp for the end of the batch to calculate the total run time.
     * Write this and other stats out to the tail of the csv file.
     *
     * @param parameters The optional test parameters, may be null.
     */
    public synchronized void endBatch(Properties parameters)
    {
        boolean noParams = (parameters == null) || (parameters.size() == 0);

        // Check that a batch has been started but not ended.
        if (batchStarted)
        {
            long batchEndTime = System.nanoTime();
            float totalTimeMillis = ((float) (batchEndTime - batchStartTime)) / 1000000f;
            float summedTimeMillis = ((float) summedTime) / 1000000f;

            // Write the stats for the batch out.
            try
            {
                synchronized (this.getClass())
                {
                    timingsWriter.write("Total Tests:, " + totalTests + ", ");
                    timingsWriter.write("Total Passed:, " + numPassed + ", ");
                    timingsWriter.write("Total Failed:, " + numFailed + ", ");
                    timingsWriter.write("Total Error:, " + numError + ", ");
                    timingsWriter.write("Total Size:, " + totalSize + ", ");
                    timingsWriter.write("Summed Time:, " + summedTimeMillis + ", ");
                    timingsWriter.write("Concurrency Level:, " + concurrencyLevel + ", ");
                    timingsWriter.write("Total Time:, " + totalTimeMillis + ", ");
                    timingsWriter.write("Test Throughput:, " + (((float) totalTests) / totalTimeMillis) + ", ");
                    timingsWriter.write("Test * Size Throughput:, " + (((float) totalSize) / totalTimeMillis)
                        + (noParams ? "\n\n" : ", "));

                    // Write out the test parameters if there are any specified.
                    if (!noParams)
                    {
                        properties(parameters);
                    }

                    timingsWriter.flush();
                }
            }
            catch (IOException e)
            {
                throw new RuntimeException("Unable to write out end batch statistics: " + e, e);
            }
        }

        // Reset the batch started flag to ensure stats are only output once.
        batchStarted = false;
    }

    /**
     * Notifies listeners of the tests read/set properties.
     *
     * @param properties The tests read/set properties.
     */
    public void properties(Properties properties)
    {
        // log.debug("public void properties(Properties properties): called");

        // Write the properties out to the results file.
        try
        {
            synchronized (this.getClass())
            {
                Set keySet = new TreeSet(properties.keySet());

                // timingsWriter.write("\n");

                for (Object key : keySet)
                {
                    timingsWriter.write(key + " = , " + properties.getProperty((String) key) + ", ");
                }

                timingsWriter.write("\n\n");
                // timingsWriter.flush();
            }
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to write out test parameters: " + e, e);
        }

        // Write out the column headers after the properties.
        // writeColumnHeaders();
    }

    /**
     * Writes out and flushes the column headers for raw test data.
     */
    private void writeColumnHeaders()
    {
        // Write the column headers for the CSV file. Any IO exceptions are ignored.
        try
        {
            timingsWriter.write("Class, ");
            timingsWriter.write("Method, ");
            timingsWriter.write("Thread, ");
            timingsWriter.write("Test Outcome, ");
            timingsWriter.write("Time (milliseconds), ");
            timingsWriter.write("Memory Used (bytes), ");
            timingsWriter.write("Concurrency level, ");
            timingsWriter.write("Test Size\n");

            timingsWriter.flush();
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to write out column headers: " + e, e);
        }
    }

    /**
     * Writes out the test results for the specified test. This outputs a single line of results to the csv file.
     *
     * @param r    The test results to write out.
     * @param test The test to write them out for.
     */
    private void writeTestResults(TestResult r, Test test)
    {
        // Update the running stats for this batch.
        if ("Error".equals(r.testState))
        {
            numError++;
        }
        else if ("Failure".equals(r.testState))
        {
            numFailed++;
        }
        else if ("Pass".equals(r.testState))
        {
            numPassed++;
        }

        totalTests++;

        // Write the test name and thread information plus all instrumenation a line of the CSV ouput. Any IO
        // exceptions are ignored.
        try
        {
            synchronized (this.getClass())
            {
                timingsWriter.write(test.getClass().getName() + ", ");
                timingsWriter.write(((test instanceof TestCase) ? ((TestCase) test).getName() : "") + ", ");
                timingsWriter.write(Thread.currentThread().getName() + ", ");
                timingsWriter.write(r.testState + ", ");
                timingsWriter.write((((float) r.testTime) / 1000000f) + ", ");
                timingsWriter.write((r.testEndMem - r.testStartMem) + ", ");
                timingsWriter.write(r.testConcurrency + ", ");
                timingsWriter.write(r.testParam + "\n");
            }
        }
        catch (IOException e)
        {
            throw new RuntimeException("Unable to write out test results: " + e, e);
        }
    }

    /**
     * Supplies the shutdown hook. This attempts to flush the results in the event of the test runner being prematurely
     * suspended before the end of the current test batch.
     *
     * @return The shut down hook.
     */
    public Thread getShutdownHook()
    {
        return new Thread(new Runnable()
                {
                    public void run()
                    {
                        log.debug("CSVTestListener::ShutdownHook: called");

                        // Complete the current test batch stats.
                        endBatch(TestContextProperties.getInstance());
                    }
                });
    }

    /** Captures test results packaged into a single object, so that it can be set up as a thread local. */
    private static class TestResult
    {
        /** Used to hold the test timing. */
        public long testTime;

        /** Used to hold the test start memory usage. */
        public long testStartMem;

        /** Used to hold the test end memory usage. */
        public long testEndMem;

        /** Used to hold the test pass/fail/error state. */
        public String testState = "Pass";

        /** Used to hold the test parameter value. */
        public int testParam;

        /** Used to hold the concurrency level under which the test was run. */
        public int testConcurrency;
    }
}
