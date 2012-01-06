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
using log4net;

using Apache.Qpid.Integration.Tests.framework.Circuit;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using org.apache.qpid.util.ConversationFactory;

using System.Collections.Generic.LinkedList;
using System.Collections.Generic.IList;
using java.util.Properties;

namespace Apache.Qpid.Integration.Tests.framework.sequencers
{
    /// <summary>
    /// BaseCircuitFactory provides some functionality common to all <see cref="CircuitFactory"/>s, such as the details of
    /// all <see cref="Apache.Qpid.Integration.Tests.framework.distributedtesting.TestClient"/>s that make up the end-points of
    /// the circuits that the factory creates, and an active <see cref="ConversationFactory"/> that can be used to generate
    /// control conversations with those circuit end-points.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Hold the details of the sending and receiving end-points to create circuits from.
    /// <tr><td> Provide a conversation factory to create control conversations with the end-points.
    /// </table>
    /// </summary>
    public abstract class BaseCircuitFactory : CircuitFactory
    {
        /// <summary> Used for debugging. </summary>
        private static ILog log = LogManager.GetLogger(typeof(BaseCircuitFactory));

        /// <summary> Holds the contact details for the sending test client. </summary>
        protected TestClientDetails sender;

        /// <summary> Holds the contact details for the receving test client. </summary>
        protected IList<TestClientDetails> receivers = new LinkedList<TestClientDetails>();

        /// <summary> Holds the conversation factory over which to coordinate the test. </summary>
        protected ConversationFactory conversationFactory;

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        /// <return> A test circuit. </return>
        public Circuit createCircuit(Properties testProperties)
        {
            throw new RuntimeException("Not implemented.");
        }

        /// <summary>
        /// Sets the sender test client to coordinate the test with.
        /// </summary>
        /// <param name="sender"> The contact details of the sending client in the test. </param>
        public void setSender(TestClientDetails sender)
        {
            log.debug("public void setSender(TestClientDetails sender = " + sender + "): called");

            this.sender = sender;
        }

        /// <summary>
        /// Sets the receiving test client to coordinate the test with.
        /// </summary>
        /// <param name="receiver"> The contact details of the sending client in the test. </param>
        public void setReceiver(TestClientDetails receiver)
        {
            log.debug("public void setReceiver(TestClientDetails receivers = " + receiver + "): called");

            this.receivers.add(receiver);
        }

        /// <summary>
        /// Supplies the sending test client.
        /// </summary>
        /// <return> The sending test client. </return>
        public TestClientDetails getSender()
        {
            return sender;
        }

        /// <summary>
        /// Supplies the receiving test client.
        /// </summary>
        /// <return> The receiving test client. </return>
        public IList<TestClientDetails> getReceivers()
        {
            return receivers;
        }

        /// <summary>
        /// Accepts the conversation factory over which to hold the test coordinating conversation.
        /// </summary>
        /// <param name="conversationFactory"> The conversation factory to coordinate the test over. </param>
        public void setConversationFactory(ConversationFactory conversationFactory)
        {
            this.conversationFactory = conversationFactory;
        }

        /// <summary>
        /// Provides the conversation factory for providing the distributed test sequencing conversations over the test
        /// connection.
        /// </summary>
        /// <return> The conversation factory to create test sequencing conversations with. </return>
        public ConversationFactory getConversationFactory()
        {
            return conversationFactory;
        }
    }
}