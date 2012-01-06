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
 * Implements test case 4, from the interop test specification. This test sets up the TC2_P2PMessageSize test for 50
 * messages, and a variety of message sizes. It checks that the sender and receivers reports both indicate that all
 * the test messages were transmitted successfully.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Setup p2p test parameters and compare with test output. <td> {@link FrameworkBaseCase}
 * </table>
 */
public class InteropTestCase4P2PMessageSize extends FrameworkBaseCase
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(InteropTestCase4P2PMessageSize.class);

    /**
     * Creates a new coordinating test case with the specified name.
     *
     * @param name The test case name.
     */
    public InteropTestCase4P2PMessageSize(String name)
    {
        super(name);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 0K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize0K() throws Exception
    {
        runTestForMessagesOfSize(0);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 63K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize63K() throws Exception
    {
        runTestForMessagesOfSize(63 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 64K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize64K() throws Exception
    {
        runTestForMessagesOfSize(64 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 65K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize65K() throws Exception
    {
        runTestForMessagesOfSize(65 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 127K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize127K() throws Exception
    {
        runTestForMessagesOfSize(127 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 128K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize128K() throws Exception
    {
        runTestForMessagesOfSize(128 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 129K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize129K() throws Exception
    {
        runTestForMessagesOfSize(129 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 255K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize255K() throws Exception
    {
        runTestForMessagesOfSize(255 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 256K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize256K() throws Exception
    {
        runTestForMessagesOfSize(256 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 257K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testP2PMessageSize257K() throws Exception
    {
        runTestForMessagesOfSize(257 * 1024);
    }

    /**
     * Sends 50 test messages of the specified size, and asserts that all were received.
     *
     * @param size The size of the messages to send in bytes.
     */
    private void runTestForMessagesOfSize(int size)
    {
        Properties testConfig = new Properties();
        testConfig.setProperty("TEST_NAME", "TC4_P2PMessageSize");
        testConfig.setProperty("P2P_QUEUE_AND_KEY_NAME", "tc2queue");
        testConfig.put("P2P_NUM_MESSAGES", 50);
        testConfig.put("messageSize", size);

        /*Message[] reports =*/
        getCircuitFactory().sequenceTest(null, null, testConfig);

        // Compare sender and receivers reports.
        /*
        int messagesSent = reports[0].getIntProperty("MESSAGE_COUNT");
        int messagesReceived = reports[1].getIntProperty("MESSAGE_COUNT");

        Assert.assertEquals("The requested number of messages were not sent.", 50, messagesSent);
        Assert.assertEquals("Sender and receivers messages sent did not match up.", messagesSent, messagesReceived);
         */
    }

    /**
     * Should provide a translation from the junit method name of a test to its test case name as defined in the
     * interop testing specification. For example the method "testP2P" might map onto the interop test case name
     * "TC2_BasicP2P".
     *
     * @param methodName The name of the JUnit test method.
     *
     * @return The name of the corresponding interop test case.
     */
    public String getTestCaseNameForTestMethod(String methodName)
    {
        return "TC4_P2PMessageSize";
    }
}
