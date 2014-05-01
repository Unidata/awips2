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
using javax.jms.JMSException;
using javax.jms.Message;
using javax.jms.MessageListener;
using javax.jms.Session;

namespace Apache.Qpid.Integration.Tests.framework.distributedtesting
{
    /// <summary>
    /// TestClientControlledTest provides an interface that classes implementing test cases to run on a <see cref="TestClient"/>
    /// node can use. Implementations must be Java beans, that is, to provide a default constructor and to implement the
    /// <see cref="#getName"/> method.
    ///
    /// <p/>The methods specified in this interface are called when the <see cref="TestClient"/> receives control instructions to
    /// apply to the test. There are control instructions to present the test case with the test invite, so that it may
    /// choose whether or not to participate in the test, assign the test to play the sender or receiver role, start the
    /// test and obtain the test status report.
    ///
    /// <p><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities
    /// <tr><td> Supply the name of the test case that this implements.
    /// <tr><td> Accept/Reject invites based on test parameters.
    /// <tr><td> Adapt to assigned roles.
    /// <tr><td> Perform test case actions.
    /// <tr><td> Generate test reports.
    /// </table>
    /// </summary>
    public interface TestClientControlledTest
    {
        /// <summary> Defines the possible test case roles that an interop test case can take on. </summary>
        public enum Roles
        {
            /// <summary> Specifies the sender role. </summary>
            SENDER,

            /// <summary> Specifies the receivers role. </summary>
            RECEIVER
        }

        /// <summary>
        /// Should provide the name of the test case that this class implements. The exact names are defined in the
        /// interop testing spec.
        /// </summary>
        /// <return> The name of the test case that this implements. </return>
        public string getName();

        /// <summary>
        /// Determines whether the test invite that matched this test case is acceptable.
        /// </summary>
        /// <param name="inviteMessage"> The invitation to accept or reject. </param>
        ///
        /// <return> <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it. </return>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public bool acceptInvite(Message inviteMessage) throws JMSException;

        /// <summary>
        /// Assigns the role to be played by this test case. The test parameters are fully specified in the
        /// assignment message. When this method return the test case will be ready to execute.
        /// </summary>
        /// <param name="role">              The role to be played; sender or receivers. </param>
        /// <param name="assignRoleMessage"> The role assingment message, contains the full test parameters. </param>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public void assignRole(Roles role, Message assignRoleMessage) throws JMSException;

        /// <summary>
        /// Performs the test case actions. Returning from here, indicates that the sending role has completed its test.
        /// </summary>
        /// <param name="numMessages"> The number of test messages to send. </param>
        ///
        /// <exception cref="JMSException"> Any JMSException resulting from reading the message are allowed to fall through. </exception>
        public void start(int numMessages) throws JMSException;

        /// <summary>
        /// Gets a report on the actions performed by the test case in its assigned role.
        /// </summary>
        /// <param name="session"> The controlSession to create the report message in. </param>
        ///
        /// <return> The report message. </return>
        ///
        /// <exception cref="JMSException"> Any JMSExceptions resulting from creating the report are allowed to fall through. </exception>
        public Message getReport(Session session) throws JMSException;
    }
}