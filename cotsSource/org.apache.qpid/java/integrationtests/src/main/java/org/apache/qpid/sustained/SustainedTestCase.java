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
package org.apache.qpid.sustained;

import org.apache.log4j.Logger;

import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.framework.DropInTest;
import org.apache.qpid.test.framework.FrameworkBaseCase;

import javax.jms.JMSException;
import javax.jms.Message;

import java.util.Properties;

/**
 * SustainedTestCase is a {@link FrameworkBaseCase} that runs the "Perf_SustainedPubSub" test case. This consists of one
 * test client sending, and several receiving, and attempts to find the highest rate at which messages can be broadcast
 * to the receivers. It is also a {@link DropInTest} to which more test clients may be added during a test run.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td>
 * </table>
 */
public class SustainedTestCase extends FrameworkBaseCase implements DropInTest
{
    /** Used for debugging. */
    Logger log = Logger.getLogger(SustainedTestCase.class);

    /** Holds the root name of the topic on which to send the test messages. */
    private static final String SUSTAINED_KEY = "Perf_SustainedPubSub";

    /**
     * Creates a new coordinating test case with the specified name.
     *
     * @param name The test case name.
     */
    public SustainedTestCase(String name)
    {
        super(name);
    }

    /**
     * Performs a single test run of the sustained test.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    public void testBasicPubSub() throws Exception
    {
        log.debug("public void testSinglePubSubCycle(): called");

        Properties testConfig = new Properties();
        testConfig.put("TEST_NAME", "Perf_SustainedPubSub");
        testConfig.put("SUSTAINED_KEY", SUSTAINED_KEY);
        testConfig.put("SUSTAINED_NUM_RECEIVERS", Integer.getInteger("numReceives", 2));
        testConfig.put("SUSTAINED_UPDATE_INTERVAL", Integer.getInteger("batchSize", 1000));
        testConfig.put("SUSTAINED_UPDATE_KEY", SUSTAINED_KEY + ".UPDATE");
        testConfig.put("ACKNOWLEDGE_MODE", Integer.getInteger("ackMode", AMQSession.AUTO_ACKNOWLEDGE));

        log.info("Created Config: " + testConfig.entrySet().toArray());

        getCircuitFactory().sequenceTest(null, null, testConfig);
    }

    /**
     * Accepts a late joining client into this test case. The client will be enlisted with a control message
     * with the 'CONTROL_TYPE' field set to the value 'LATEJOIN'. It should also provide values for the fields:
     *
     * <p/><table>
     * <tr><td> CLIENT_NAME <td> A unique name for the new client.
     * <tr><td> CLIENT_PRIVATE_CONTROL_KEY <td> The key for the route on which the client receives its control messages.
     * </table>
     *
     * @param message The late joiners join message.
     *
     * @throws JMSException Any JMS Exception are allowed to fall through, indicating that the join failed.
     */
    public void lateJoin(Message message) throws JMSException
    {
        throw new RuntimeException("Not implemented.");
        /*
        // Extract the joining clients details from its join request message.
        TestClientDetails clientDetails = new TestClientDetails();
        clientDetails.clientName = message.getStringProperty("CLIENT_NAME");
        clientDetails.privateControlKey = message.getStringProperty("CLIENT_PRIVATE_CONTROL_KEY");

        // Register the joining client, but do block for confirmation as cannot do a synchronous receivers during this
        // method call, as it may have been called from an 'onMessage' method.
        assignReceiverRole(clientDetails, new Properties(), false);
         */
    }

    /**
     * Should provide a translation from the junit method name of a test to its test case name as known to the test
     * clients that will run the test. The purpose of this is to convert the JUnit method name into the correct test
     * case name to place into the test invite. For example the method "testP2P" might map onto the interop test case
     * name "TC2_BasicP2P".
     *
     * @param methodName The name of the JUnit test method.
     *
     * @return The name of the corresponding interop test case.
     */
    public String getTestCaseNameForTestMethod(String methodName)
    {
        return "Perf_SustainedPubSub";
    }
}
