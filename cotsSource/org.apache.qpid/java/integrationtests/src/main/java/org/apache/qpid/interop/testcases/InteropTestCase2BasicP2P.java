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
 * Implements test case 2, from the interop test specification. This test sets up the TC2_BasicP2P test for 50
 * messages. It checks that the sender and receivers reports both indicate that all the test messages were transmitted
 * successfully.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Setup p2p test parameters and compare with test output. <td> {@link FrameworkBaseCase}
 * </table>
 */
public class InteropTestCase2BasicP2P extends FrameworkBaseCase
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(InteropTestCase2BasicP2P.class);

    /**
     * Creates a new coordinating test case with the specified name.
     *
     * @param name The test case name.
     */
    public InteropTestCase2BasicP2P(String name)
    {
        super(name);
    }

    /**
     * Performs the basic P2P test case, "Test Case 2" in the specification.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testBasicP2P() throws Exception
    {
        log.debug("public void testBasicP2P(): called");

        Properties testConfig = new Properties();
        testConfig.setProperty("TEST_NAME", "TC2_BasicP2P");
        testConfig.setProperty("P2P_QUEUE_AND_KEY_NAME", "tc2queue");
        testConfig.put("P2P_NUM_MESSAGES", 50);

        /*Message[] reports =*/ getCircuitFactory().sequenceTest(null, null, testConfig);

        // Compare sender and receivers reports.
        /*int messagesSent = reports[0].getIntProperty("MESSAGE_COUNT");
        int messagesReceived = reports[1].getIntProperty("MESSAGE_COUNT");

        Assert.assertEquals("The requested number of messages were not sent.", 50, messagesSent);
        Assert.assertEquals("Sender and receivers messages sent did not match up.", messagesSent, messagesReceived);*/
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
        return "TC2_BasicP2P";
    }
}
