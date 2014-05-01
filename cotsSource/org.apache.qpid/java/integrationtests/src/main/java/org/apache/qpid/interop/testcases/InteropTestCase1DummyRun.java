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
package org.apache.qpid.interop.testcases;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.FrameworkBaseCase;

import java.util.Properties;

/**
 * Coordinates test case 1, from the interop test specification. This test connects up the sender and receivers roles,
 * and gets some dummy test reports from them, in order to check that the test framework itself is operational.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Exercises the interop testing framework without actually sending any test messages.
 *     <td> {@link FrameworkBaseCase}
 * </table>
 */
public class InteropTestCase1DummyRun extends FrameworkBaseCase
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(InteropTestCase1DummyRun.class);

    /**
     * Creates a new coordinating test case with the specified name.
     *
     * @param name The test case name.
     */
    public InteropTestCase1DummyRun(String name)
    {
        super(name);
    }

    /**
     * Performs the basic P2P test case, "Test Case 2" in the specification.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testDummyRun() throws Exception
    {
        log.debug("public void testDummyRun(): called");

        Properties testConfig = new Properties();
        testConfig.put("TEST_NAME", "TC1_DummyRun");

        /*Message[] reports =*/ getCircuitFactory().sequenceTest(null, null, testConfig);

        // Compare sender and receivers reports.
        // Assert.assertEquals("Expected to get 2 dummy reports.", 2, reports.length);
    }

    /**
     * Should provide a translation from the junit method name of a test to its test case name as defined in the
     * interop testing specification. For example the method "testP2P" might map onto the interop test case name
     * "TC2_BasicP2P".
     *
     * @param methodName The name of the JUnit test method.
     * @return The name of the corresponding interop test case.
     */
    public String getTestCaseNameForTestMethod(String methodName)
    {
        return "TC1_DummyRun";
    }
}
