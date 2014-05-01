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

import junit.framework.TestCase;

import org.apache.log4j.Logger;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * AsymptoticTestCase is an extension of TestCase for writing unit tests to analyze asymptotic time and space behaviour.
 *
 * <p>ParameterizedTestCases allow tests to be defined which have test methods that take a single int argument. Normal
 * JUnit test methods do not take any arguments. This int argument can be interpreted in any way by the test but it is
 * intended to denote the 'size' of the test to be run. For example, when testing the performance of a data structure
 * for different numbers of data elements held in the data structure the int parameter should be interpreted as the
 * number of elements. Test timings for different numbers of elements can then be captured and the asymptotic behaviour
 * of the data structure with respect to time analyzed. Any non-parameterized tests defined in extensions of this class
 * will also be run.
 *
 * <p>TestCases derived from this class may also define tear down methods to clean up their memory usage. This is
 * intended to be used in conjunction with memory listeners that report the amount of memory a test uses. The idea is
 * to write a test that allocates memory in the main test method in such a way that it leaves that memory still
 * allocated at the end of the test. The amount of memory used can then be measured before calling the tear down method
 * to clean it up. In the data structure example above, a test will allocate as many elements as are requested by the
 * int parameter and deallocate them in the tear down method. In this way memory readings for different numbers of
 * elements can be captured and the asymptotic behaviour of the data structure with respect to space analyzed.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Store the current int parameter value. <td> {@link TKTestResult} and see {@link AsymptoticTestDecorator} too.
 * <tr><td> Invoke parameterized test methods.
 * </table>
 *
 * @todo If possible try to move the code that invokes the test and setup/teardown methods into {@link TKTestResult} or
 *       {@link AsymptoticTestDecorator} rather than this class. This would mean that tests don't have to extend this
 *       class to do time and space performance analysis, these methods could be added to any JUnit TestCase class
 *       instead. This would be an improvement because existing unit tests wouldn't have to extend a different class to
 *       work with this extension, and also tests that extend other junit extension classes could have parameterized
 *       and tear down methods too.
 *
 * @author Rupert Smith
 */
public class AsymptoticTestCase extends TestCase implements InstrumentedTest
{
    /** Used for logging. */
    private static final Logger log = Logger.getLogger(AsymptoticTestCase.class);

    /** The name of the test case. */
    private String testCaseName;

    /** Thread local for holding measurements on a per thread basis. */
    ThreadLocal<TestMeasurements> threadLocalMeasurement =
        new ThreadLocal<TestMeasurements>()
        {
            /**
             * Sets up a default set test measurements (zeroed, apart from the size param which defaults to 1).
             *
             * @return A set of default test measurements.
             */
            protected synchronized TestMeasurements initialValue()
            {
                return new TestMeasurements();
            }
        };

    /**
     * Constructs a test case with the given name.
     *
     * @param name The name of the test.
     */
    public AsymptoticTestCase(String name)
    {
        super(name);

        log.debug("public AsymptoticTestCase(String " + name + "): called");
        testCaseName = name;
    }

    /**
     * Gets the current value of the integer parameter to be passed to the parameterized test.
     *
     * @return The current value of the integer parameter.
     */
    public int getN()
    {
        log.debug("public int getN(): called");
        int n = threadLocalMeasurement.get().n;

        log.debug("return: " + n);

        return n;
    }

    /**
     * Sets the current value of the integer parameter to be passed to the parameterized test.
     *
     * @param n The new current value of the integer parameter.
     */
    public void setN(int n)
    {
        log.debug("public void setN(int " + n + "): called");
        threadLocalMeasurement.get().n = n;
    }

    /**
     * Reports how long the test took to run.
     *
     * @return The time in milliseconds that the test took to run.
     */
    public long getTestTime()
    {
        log.debug("public long getTestTime(): called");
        long startTime = threadLocalMeasurement.get().startTime;
        long endTime = threadLocalMeasurement.get().endTime;
        long testTime = endTime - startTime;

        log.debug("return: " + testTime);

        return testTime;
    }

    /**
     * Reports the memory usage at the start of the test.
     *
     * @return The memory usage at the start of the test.
     */
    public long getTestStartMemory()
    {
        // log.debug("public long getTestStartMemory(): called");
        long startMem = threadLocalMeasurement.get().startMem;

        // log.debug("return: " + startMem);

        return startMem;
    }

    /**
     * Reports the memory usage at the end of the test.
     *
     * @return The memory usage at the end of the test.
     */
    public long getTestEndMemory()
    {
        // log.debug("public long getTestEndMemory(): called");
        long endMem = threadLocalMeasurement.get().endMem;

        // log.debug("return: " + endMem);
        return endMem;
    }

    /**
     * Resets the instrumentation values to zero, and nulls any references to held measurements so that the memory
     * can be reclaimed.
     */
    public void reset()
    {
        log.debug("public void reset(): called");
        threadLocalMeasurement.remove();
    }

    /**
     * Runs the test method for this test case.
     *
     * @throws Throwable Any Throwables from the test methods invoked are allowed to fall through.
     */
    protected void runTest() throws Throwable
    {
        log.debug("protected void runTest(): called");

        // Check that a test name has been set. This is used to define which method to run.
        assertNotNull(testCaseName);
        log.debug("testCaseName = " + testCaseName);

        // Try to get the method with matching name.
        Method runMethod = null;
        boolean isParameterized = false;

        // Check if a parameterized test method is available.
        try
        {
            // Use getMethod to get all public inherited methods. getDeclaredMethods returns all
            // methods of this class but excludes the inherited ones.
            runMethod = getClass().getMethod(testCaseName, int.class);
            isParameterized = true;
        }
        catch (NoSuchMethodException e)
        {
            // log.debug("Parameterized method \"" + testCaseName + "\" not found.");
            // Set run method to null (it already will be but...) to indicate that no parameterized method
            // version could be found.
            runMethod = null;
        }

        // If no parameterized method is available, try and get the unparameterized method.
        if (runMethod == null)
        {
            try
            {
                runMethod = getClass().getMethod(testCaseName);
                isParameterized = false;

            }
            catch (NoSuchMethodException e)
            {
                fail("Method \"" + testCaseName + "\" not found.");
            }
        }

        // Check that the method is publicly accessable.
        if (!Modifier.isPublic(runMethod.getModifiers()))
        {
            fail("Method \"" + testCaseName + "\" should be public.");
        }

        // Try to execute the method, passing it the current int parameter value. Allow any invocation exceptions or
        // resulting exceptions from the method to fall through.
        try
        {
            Integer paramN = getN();
            log.debug("paramN = " + paramN);

            // Calculate parameters for parameterized tests so new does not get called during memory measurement.
            Object[] params = new Object[] { paramN };

            // Take the test start memory and start time.
            threadLocalMeasurement.get().startMem = 0; // SizeOf.getUsedMemory();

            threadLocalMeasurement.get().startTime = System.nanoTime();

            if (isParameterized)
            {
                runMethod.invoke(this, params);
            }
            else
            {
                runMethod.invoke(this);
            }
        }
        catch (InvocationTargetException e)
        {
            e.fillInStackTrace();
            throw e.getTargetException();
        }
        catch (IllegalAccessException e)
        {
            e.fillInStackTrace();
            throw e;
        }
        finally
        {
            // Take the test end memory and end time and calculate how long it took to run.
            long endTime = System.nanoTime();
            threadLocalMeasurement.get().endTime = endTime;
            log.debug("startTime = " + threadLocalMeasurement.get().startTime + ", endTime = " + endTime + ", testTime = "
                + getTestTime());

            threadLocalMeasurement.get().endMem = 0; // SizeOf.getUsedMemory();
        }
    }

    /**
     * The test parameters, encapsulated as a unit for attaching on a per thread basis.
     */
    private static class TestMeasurements
    {
        /** Holds the current value of the integer parameter to run tests on. */
        public int n = 1;

        /** Holds the test start memory. */
        public long startTime = 0;

        /** Holds the test end memory. */
        public long endTime = 0;

        /** Holds the test start memory. */
        public long startMem = 0;

        /** Holds the test end memory. */
        public long endMem = 0;
    }
}
