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

import org.apache.qpid.test.framework.Circuit;
import org.apache.qpid.test.framework.FrameworkBaseCase;
import static org.apache.qpid.test.framework.MessagingTestConfigProperties.ACK_MODE_PROPNAME;
import static org.apache.qpid.test.framework.MessagingTestConfigProperties.PUBSUB_PROPNAME;
import org.apache.qpid.test.framework.TestUtils;
import org.apache.qpid.test.framework.localcircuit.LocalCircuitImpl;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;

import javax.jms.*;

import java.util.LinkedList;
import java.util.List;
import java.util.Random;

/**
 * TTLTest checks that time-to-live is applied to messages. The test sends messages with a variety of TTL stamps on them
 * then after a pause attempts to receive those messages. Only messages with a large enough TTL to have survived the pause
 * should be receiveable. This test case also applies an additional assertion against the broker, that the message store
 * is empty at the end of the test.
 *
 * <p/>This test is designed to run over local circuits only, as it must control a timed pause between sending and receiving
 * messages to that TTL can be applied to purge some of the messages.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td>
 * </table>
 *
 * @todo Use an interface or other method to mark this test as local only.
 *
 * @todo Implement the message store assertion for in-vm broker. Could also be done for external broker, for example
 *       by using diagnostic exchange.
 *
 * @todo Implement and add a queue depth assertion too. This might already be in another test to copy from.
 *
 * @todo Create variations on test theme, for different ack mode and tx and message sizes etc.
 *
 * @todo Add an allowable margin of error to the test, as ttl may not be precise.
 */
public class TTLTest extends FrameworkBaseCase
{
    /**
     * Creates a new test case with the specified name.
     *
     * @param name The test case name.
     */
    public TTLTest(String name)
    {
        super(name);
    }

    /**
     * Checks that all messages sent with a TTL shorter than a pause between sending them and attempting to receive them
     * will fail to arrive. Once all messages have been purged by TTL or received, check that they no longer exist on
     * the broker.
     *
     * @throws javax.jms.JMSException Allowed to fall through and fail test.
     */
    public void testTTLP2P() throws Exception
    {
        String errorMessages = "";
        Random r = new Random();

        // Used to accumulate correctly received messages in.
        List<Message> receivedMessages = new LinkedList<Message>();

        // Set up the test properties to match the test case requirements.
        testProps.setProperty(ACK_MODE_PROPNAME, Session.AUTO_ACKNOWLEDGE);
        testProps.setProperty(PUBSUB_PROPNAME, false);

        // Create the test circuit from the test configuration parameters.
        CircuitFactory circuitFactory = getCircuitFactory();
        Circuit testCircuit = circuitFactory.createCircuit(getConnection(), testProps);

        // This test case assumes it is using a local circuit.
        LocalCircuitImpl localCircuit = (LocalCircuitImpl) testCircuit;

        Session producerSession = localCircuit.getLocalPublisherCircuitEnd().getSession();
        MessageProducer producer = localCircuit.getLocalPublisherCircuitEnd().getProducer();
        MessageConsumer consumer = localCircuit.getLocalReceiverCircuitEnd().getConsumer();

        // Send some tests messages, with random TTLs, some shorter and some longer than the pause time.
        for (int i = 0; i < 100; i++)
        {
            Message testMessage = TestUtils.createTestMessageOfSize(producerSession, 10);

            // Set the TTL on the message and record its value in the message headers.
            long ttl = 500 + r.nextInt(1500);
            producer.setTimeToLive(ttl);
            testMessage.setLongProperty("testTTL", ttl);

            producer.send(testMessage);
            // producerSession.commit();
        }

        // Inject a pause to allow some messages to be purged by TTL.
        TestUtils.pause(1000);

        // Attempt to receive back all of the messages, confirming by the message time stamps and TTLs that only
        // those received should have avoided being purged by the TTL.
        boolean timedOut = false;

        while (!timedOut)
        {
            Message testMessage = consumer.receive(1000);

            long ttl = testMessage.getLongProperty("testTTL");
            long timeStamp = testMessage.getJMSTimestamp();
            long now = System.currentTimeMillis();

            if ((timeStamp + ttl) < now)
            {
                errorMessages +=
                    "Received message [sent: " + timeStamp + ", ttl: " + ttl + ", received: " + now
                    + "] which should have been purged by its TTL.\n";
            }
            /*else
            {
                receivedMessages.add(testMessage);
            }*/
        }

        // Check that the queue and message store on the broker are empty.
        // assertTrue("Message store is not empty.", messageStoreEmpty.apply());
        // assertTrue("Queue is not empty.", queueEmpty.apply());

        assertTrue(errorMessages, "".equals(errorMessages));
    }
}
