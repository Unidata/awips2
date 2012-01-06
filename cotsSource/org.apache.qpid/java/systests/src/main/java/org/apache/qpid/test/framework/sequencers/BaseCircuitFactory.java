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
package org.apache.qpid.test.framework.sequencers;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.Circuit;
import org.apache.qpid.test.framework.TestClientDetails;
import org.apache.qpid.test.utils.ConversationFactory;

import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

/**
 * BaseCircuitFactory provides some functionality common to all {@link CircuitFactory}s, such as the details of
 * all {@link org.apache.qpid.test.framework.distributedtesting.TestClient}s that make up the end-points of
 * the circuits that the factory creates, and an active {@link ConversationFactory} that can be used to generate
 * control conversations with those circuit end-points.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Hold the details of the sending and receiving end-points to create circuits from.
 * <tr><td> Provide a conversation factory to create control conversations with the end-points.
 * </table>
 */
public abstract class BaseCircuitFactory implements CircuitFactory
{

    /** Used for debugging. */
    private final Logger log = Logger.getLogger(BaseCircuitFactory.class);

    /** Holds the contact details for the sending test client. */
    protected TestClientDetails sender;

    /** Holds the contact details for the receving test client. */
    protected List<TestClientDetails> receivers = new LinkedList<TestClientDetails>();

    /** Holds the conversation factory over which to coordinate the test. */
    protected ConversationFactory conversationFactory;

    /**
     * Creates a test circuit for the test, configered by the test parameters specified.
     *
     * @param testProperties The test parameters.
     * @return A test circuit.
     */
    public Circuit createCircuit(Properties testProperties)
    {
        throw new RuntimeException("Not implemented.");
    }

    /**
      * Sets the sender test client to coordinate the test with.
      *
      * @param sender The contact details of the sending client in the test.
      */
    public void setSender(TestClientDetails sender)
    {
        log.debug("public void setSender(TestClientDetails sender = " + sender + "): called");

        this.sender = sender;
    }

    /**
     * Sets the receiving test client to coordinate the test with.
     *
     * @param receiver The contact details of the sending client in the test.
     */
    public void setReceiver(TestClientDetails receiver)
    {
        log.debug("public void setReceiver(TestClientDetails receivers = " + receiver + "): called");

        this.receivers.add(receiver);
    }

    /**
     * Supplies the sending test client.
     *
     * @return The sending test client.
     */
    public TestClientDetails getSender()
    {
        return sender;
    }

    /**
     * Supplies the receiving test client.
     *
     * @return The receiving test client.
     */
    public List<TestClientDetails> getReceivers()
    {
        return receivers;
    }

    /**
     * Accepts the conversation factory over which to hold the test coordinating conversation.
     *
     * @param conversationFactory The conversation factory to coordinate the test over.
     */
    public void setConversationFactory(ConversationFactory conversationFactory)
    {
        this.conversationFactory = conversationFactory;
    }

    /**
     * Provides the conversation factory for providing the distributed test sequencing conversations over the test
     * connection.
     *
     * @return The conversation factory to create test sequencing conversations with.
     */
    public ConversationFactory getConversationFactory()
    {
        return conversationFactory;
    }
}

