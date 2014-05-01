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
package org.apache.qpid.test.framework.distributedtesting;

import org.apache.qpid.test.framework.sequencers.CircuitFactory;
import org.apache.qpid.test.framework.FrameworkBaseCase;

/**
 * An OptOutTestCase is a test case that automatically fails. It is used when a list of test clients has been generated
 * from a compulsory invite, but only some of those clients have responded to a specific test case invite. The clients
 * that did not respond, may automatically be given a fail for some tests.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Fail the test with a suitable reason.
 * </table>
 */
public class OptOutTestCase extends FrameworkBaseCase
{
    /**
     * Creates a new coordinating test case with the specified name.
     *
     * @param name The test case name.
     */
    public OptOutTestCase(String name)
    {
        super(name);
    }

    /** Generates an appropriate test failure assertion. */
    public void testOptOut()
    {
        CircuitFactory circuitFactory = getCircuitFactory();

        fail("One of " + circuitFactory.getSender() + " and " + getCircuitFactory().getReceivers()
            + " opted out of the test.");
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
        return "OptOutTest";
    }
}
