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

import junit.framework.TestResult;

import org.apache.log4j.Logger;

import org.apache.qpid.junit.extensions.util.MathUtils;

/**
 * A Decorator that runs a test repeatedly on an increasing int parameter, or for a fixed number of repeats. If both
 * a set of integer parameters and a repeat count are specified, then each test is run for the repeat count at each
 * integer parameter.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Repeat a test for each of a set of integer parameters. <td> {@link TKTestResult}
 * <tr><td> Repeat a test multiple times.
 * <tr><td>
 * </table>
 *
 * @author Rupert Smith
 */
public class AsymptoticTestDecorator extends WrappedSuiteTestDecorator
{
    /** Used for logging. */
    private static final Logger log = Logger.getLogger(AsymptoticTestDecorator.class);

    /** The int size parameters to run the test with. */
    private int[] params;

    /** The number of times the whole test should be repeated. */
    private int repeat;

    /**
     * Creates an asymptotic test decorator that wraps a test with repeats and a set of integer 'size' paramters
     * to call the test with.
     *
     * @param test   The test to wrap.
     * @param params The integer 'size' parameters.
     * @param repeat The number of times to repeat the test.
     */
    public AsymptoticTestDecorator(WrappedSuiteTestDecorator test, int[] params, int repeat)
    {
        super(test);

        log.debug("public AsymptoticTestDecorator(Test \"" + test + "\", int[] "
            + ((params == null) ? null : MathUtils.printArray(params)) + ", int " + repeat + "): called");

        this.params = params;
        this.repeat = repeat;
    }

    /**
     * Creates a new AsymptoticTestDecorator object.
     *
     * @param test   The test to decorate.
     * @param start  The starting asymptotic integer parameter value.
     * @param end    The ending asymptotic integer parameter value.
     * @param step   The increment size to move from the start to end values by.
     * @param repeat The number of times to repeat the test at each step of the cycle.
     */
    public AsymptoticTestDecorator(WrappedSuiteTestDecorator test, int start, int end, int step, int repeat)
    {
        super(test);

        if (start < 0)
        {
            throw new IllegalArgumentException("Start must be >= 0");
        }

        if (end < start)
        {
            throw new IllegalArgumentException("End must be >= start");
        }

        if (step < 1)
        {
            throw new IllegalArgumentException("Step must be >= 1");
        }

        if (repeat < 1)
        {
            throw new IllegalArgumentException("Repeat must be >= 1");
        }

        // Generate the sequence.
        params = new int[((end - start) / step) + 1];
        int i = 0;
        for (int n = start; n <= end; n += step)
        {
            params[i++] = n;
        }

        this.repeat = repeat;
    }

    /**
     * Runs the test repeatedly for each value of the int parameter specified and for the correct number of test
     * repeats.
     *
     * @param result The test result object that the tests will indicate their results to. This is also used
     *               to pass the int parameter from this class to the decorated test class.
     */
    public void run(TestResult result)
    {
        log.debug("public void run(TestResult result): called");

        if (!(result instanceof TKTestResult))
        {
            throw new IllegalArgumentException("AsymptoticTestDecorator only works with TKTestResult");
        }

        // Cast the test result into a TKTestResult to place the current parameter into.
        TKTestResult tkResult = (TKTestResult) result;

        log.debug("params = " + ((params == null) ? null : MathUtils.printArray(params)));
        log.debug("repeat = " + repeat);

        for (int n : params)
        {
            for (int j = 0; j < repeat; j++)
            {
                log.debug("n = " + n);

                // Set the integer parameter in the TKTestResult to be passed to the tests.
                tkResult.setN(n);

                if (tkResult.shouldStop())
                {
                    log.debug("tkResult.shouldStop = " + true);

                    break;
                }

                log.debug("Calling super#run");
                super.run(tkResult);
            }
        }
    }

    /**
     * Prints out the name of this test with the string "(parameterized)" appended onto it for debugging purposes.
     *
     * @return The name of this test with the string "(parameterized)" appended onto it.
     */
    public String toString()
    {
        return super.toString() + "(parameterized)";
    }
}
