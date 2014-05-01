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

import java.util.Properties;

/**
 * TKTestListener is a listener interface for listeners that want to be informed of the run times of tests, the memory
 * usage of tests, the 'size' parameters of parameterized tests and the begin and end events of complete test runs.
 * {@link org.apache.qpid.junit.extensions.TKTestResult} is an example of a test result class that listeners
 * interested in these events can be attached to.
 *
 * The {@link #timing(junit.framework.Test, long, Long)}, {@link #memoryUsed(junit.framework.Test, long, long, Long)},
 * {@link #parameterValue(junit.framework.Test, int, Long)} and {@link #endTest(junit.framework.Test, Long)} methods
 * all accept on optional thread id parameter.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Listen to test timings.
 * <tr><td> Listen to test memory usages.
 * <tr><td> Listen to parameterized test parameters.
 * </table>
 *
 * @author Rupert Smith
 */
public interface TKTestListener extends TestListener
{
    /**
     * Resets the test results to the default state of time zero, memory usage zero, parameter zero, test passed.
     *
     * @param test     The test to resest any results for.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void reset(Test test, Long threadId);

    /**
     * Should be called every time a test completes with the run time of that test.
     *
     * @param test     The name of the test.
     * @param nanos   The run time of the test in nanoseconds.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void timing(Test test, long nanos, Long threadId);

    /**
     * Should be called every time a test completed with the amount of memory used before and after the test was run.
     *
     * @param test     The test which memory was measured for.
     * @param memStart The total JVM memory used before the test was run.
     * @param memEnd   The total JVM memory used after the test was run.
     * @param threadId Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void memoryUsed(Test test, long memStart, long memEnd, Long threadId);

    /**
     * Should be called every time a parameterized test completed with the int value of its test parameter.
     *
     * @param test      The test which memory was measured for.
     * @param parameter The int parameter value.
     * @param threadId  Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void parameterValue(Test test, int parameter, Long threadId);

    /**
     * Should be called every time a test completes with the current number of test threads running.
     *
     * @param test    The test for which the measurement is being generated.
     * @param threads The number of tests being run concurrently.
     * @param threadId  Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void concurrencyLevel(Test test, int threads, Long threadId);

    /**
     * Called when a test completes. Success, failure and errors. This method should be used when registering an
     * end test from a different thread than the one that started the test.
     *
     * @param test The test which completed.
     * @param threadId  Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void endTest(Test test, Long threadId);

    /**
     * Called when a test completes to mark it as a test fail. This method should be used when registering a
     * failure from a different thread than the one that started the test.
     *
     * @param test      The test which failed.
     * @param e         The assertion that failed the test.
     * @param threadId  Optional thread id if not calling from thread that started the test method. May be null.
     */
    public void addFailure(Test test, AssertionFailedError e, Long threadId);

    /**
     * Notifies listeners of the start of a complete run of tests.
     */
    public void startBatch();

    /**
     * Notifies listeners of the end of a complete run of tests.
     *
     * @param parameters The optional test parameters to log out with the batch results.
     */
    public void endBatch(Properties parameters);

    /**
     * Notifies listeners of the tests read/set properties.
     *
     * @param properties The tests read/set properties.
     */
    public void properties(Properties properties);
}
