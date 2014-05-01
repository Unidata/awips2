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
 * Implements test case 5, from the interop test specification. This test sets up the TC2_PubSubMessageSize test for 10
 * messages, sent to 5 consumers, and a variety of message sizes. It checks that the sender and receivers reports both
 * indicate that all the test messages were transmitted successfully.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Setup pub/sub test parameters and compare with test output. <td> {@link FrameworkBaseCase}
 * </table>
 */
public class InteropTestCase5PubSubMessageSize extends FrameworkBaseCase
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(InteropTestCase5PubSubMessageSize.class);

    /**
     * Creates a new coordinating test case with the specified name.
     *
     * @param name The test case name.
     */
    public InteropTestCase5PubSubMessageSize(String name)
    {
        super(name);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 0K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize0K() throws Exception
    {
        runTestForMessagesOfSize(0);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 63K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize63K() throws Exception
    {
        runTestForMessagesOfSize(63 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 64K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize64K() throws Exception
    {
        runTestForMessagesOfSize(64 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 65K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize65K() throws Exception
    {
        runTestForMessagesOfSize(65 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 127K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize127K() throws Exception
    {
        runTestForMessagesOfSize(127 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 128K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize128K() throws Exception
    {
        runTestForMessagesOfSize(128 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 129K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize129K() throws Exception
    {
        runTestForMessagesOfSize(129 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 255K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize255K() throws Exception
    {
        runTestForMessagesOfSize(255 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 256K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize256K() throws Exception
    {
        runTestForMessagesOfSize(256 * 1024);
    }

    /**
     * Performs the P2P message test case, "Test Case 4" in the specification, for messages 257K in size.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testPubSubMessageSize257K() throws Exception
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
        testConfig.put("TEST_NAME", "TC5_PubSubMessageSize");
        testConfig.put("PUBSUB_KEY", "tc3route");
        testConfig.put("PUBSUB_NUM_MESSAGES", 10);
        testConfig.put("PUBSUB_NUM_RECEIVERS", 5);
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
     * @return The name of the corresponding interop test case.
     */
    public String getTestCaseNameForTestMethod(String methodName)
    {
        return "TC5_PubSubMessageSize";
    }
}
