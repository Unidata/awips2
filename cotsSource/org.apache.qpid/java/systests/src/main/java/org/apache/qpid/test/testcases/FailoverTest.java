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
package org.apache.qpid.test.testcases;

import org.apache.qpid.test.framework.*;
import static org.apache.qpid.test.framework.MessagingTestConfigProperties.*;
import org.apache.qpid.test.framework.localcircuit.LocalCircuitImpl;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;

import javax.jms.JMSException;
import javax.jms.MessageProducer;
import javax.jms.Session;

/**
 * FailoverTest provides testing of fail-over over a local-circuit implementation. The circuit being tested may be
 * against an in-vm broker or against an external broker, with the failure mechanism abstracted out of the test case.
 * Automatic failures can be simulated against an in-vm broker. Currently the test must interact with the user to
 * simulate failures on an external broker.
 *
 * Things to test:
 * In tx, failure duing tx causes tx to error on subsequent sends/receives or commits/rollbacks.
 * Outside of tx, reconnection allows msg flow to continue but there may be loss.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td>
 * </table>
 *
 * @todo This test is designed to be run over a local circuit only. For in-vm using automatic failures, for external
 *       brokers by prompting the user (or maybe using a script). Enforce the local-circuit only nature of the tests as
 *       well as thinking about how other local-circuit tests might be implemented. For example, could add a method
 *       to the framework base case for local only tests to call, that allows them access to the local-circuit
 *       implementation and so on.
 *
 * @todo More. Need to really expand the set of fail-over tests.
 */
public class FailoverTest extends FrameworkBaseCase
{
    /* Used for debugging purposes. */
    // private static final Logger log = Logger.getLogger(FailoverTest.class);

    /**
     * Creates a new test case with the specified name.
     *
     * @param name The test case name.
     */
    public FailoverTest(String name)
    {
        super(name);
    }

    /**
     * Checks that all messages sent within a transaction are receieved despite a fail-over occuring outside of
     * the transaction.
     *
     * @throws JMSException Allowed to fall through and fail test.
     */
    public void testTxP2PFailover() throws Exception
    {
        // Set up the test properties to match the test cases requirements.
        testProps.setProperty(TRANSACTED_PUBLISHER_PROPNAME, true);
        testProps.setProperty(ACK_MODE_PROPNAME, Session.AUTO_ACKNOWLEDGE);
        testProps.setProperty(PUBSUB_PROPNAME, false);

        // MessagingTestConfigProperties props = this.getTestParameters();

        // Create the test circuit from the test configuration parameters.
        CircuitFactory circuitFactory = getCircuitFactory();
        Circuit testCircuit = circuitFactory.createCircuit(getConnection(), testProps);

        // Create an assertion that all messages are received.
        Assertion allMessagesReceived = testCircuit.getReceiver().allMessagesReceivedAssertion(testProps);

        // This test case assumes it is using a local circuit.
        LocalCircuitImpl localCircuit = (LocalCircuitImpl) testCircuit;

        Session producerSession = localCircuit.getLocalPublisherCircuitEnd().getSession();
        MessageProducer producer = localCircuit.getLocalPublisherCircuitEnd().getProducer();
        // MessageConsumer consumer = localCircuit.getLocalReceiverCircuitEnd().getConsumer();

        // Send some test messages.
        for (int i = 0; i < 100; i++)
        {
            producer.send(TestUtils.createTestMessageOfSize(producerSession, 10));
            producerSession.commit();

            // Cause a failover.
            if (i == 50)
            {
                failureMechanism.causeFailure();
            }

            // Wait for the reconnection to complete.
        }

        // Check that trying to send within the original transaction fails.

        // Check that all messages sent were received.
        assertTrue("All messages sent were not received back again.", allMessagesReceived.apply());
    }
}
