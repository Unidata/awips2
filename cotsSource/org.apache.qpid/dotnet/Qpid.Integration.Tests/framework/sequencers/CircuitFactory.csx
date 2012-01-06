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
using Apache.Qpid.Integration.Tests.framework.Assertion;
using Apache.Qpid.Integration.Tests.framework.Circuit;
using Apache.Qpid.Integration.Tests.framework.TestClientDetails;
using org.apache.qpid.util.ConversationFactory;

using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;

using javax.jms.JMSException;
using javax.jms.Message;

using System.Collections.Generic.IList;
using System.Collections.Generic.IDictionary;
using java.util.Properties;

namespace Apache.Qpid.Integration.Tests.framework.sequencers
{
    /// <summary>
    /// A CircuitFactory is responsibile for creating test circuits appropriate to the context that a test case is
    /// running in, and providing an implementation of a standard test procedure over a test circuit.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Provide a standard test procedure over a test circuit.
    /// <tr><td> Construct test circuits appropriate to a tests context.
    /// </table>
    /// </summary>
    public interface CircuitFactory
    {
        /// <summary>
        /// Holds a test coordinating conversation with the test clients. This should consist of assigning the test roles,
        /// begining the test, gathering the test reports from the participants, and checking for assertion failures against
        /// the test reports.
        /// </summary>
        /// <param name="testCircuit">    The test circuit. </param>
        /// <param name="assertions">     The list of assertions to apply to the test circuit. </param>
        /// <param name="testProperties"> The test case definition. </param>
        ///
        /// @deprecated Use test circuits and Circuit.test instead.
        public void sequenceTest(Circuit testCircuit, IList<Assertion> assertions, Properties testProperties);

        /// <summary>
        /// Creates a test circuit for the test, configered by the test parameters specified.
        /// </summary>
        /// <param name="testProperties"> The test parameters. </param>
        ///
        /// <return> A test circuit. </return>
        public Circuit createCircuit(ParsedProperties testProperties);

        /// <summary>
        /// Sets the sender test client to coordinate the test with.
        /// </summary>
        /// <param name="sender"> The contact details of the sending client in the test. </param>
        public void setSender(TestClientDetails sender);

        /// <summary>
        /// Sets the receiving test client to coordinate the test with.
        /// </summary>
        /// <param name="receiver"> The contact details of the sending client in the test. </param>
        public void setReceiver(TestClientDetails receiver);

        /// <summary>
        /// Supplies the sending test client.
        /// </summary>
        /// <return> The sending test client. </return>
        public TestClientDetails getSender();

        /// <summary>
        /// Supplies the receiving test client.
        /// </summary>
        /// <return> The receiving test client. </return>
        public IList<TestClientDetails> getReceivers();

        /// <summary>
        /// Accepts the conversation factory over which to hold the test coordinating conversation.
        /// </summary>
        /// <param name="conversationFactory"> The conversation factory to coordinate the test over. </param>
        public void setConversationFactory(ConversationFactory conversationFactory);
    }
}