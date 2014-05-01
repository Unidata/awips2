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
using System;
using System.Text;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Integration.Tests.interop
{
    /// <summary> Defines the possible test case roles that an interop test case can take on. </summary>
    public enum Roles { SENDER, RECEIVER };

    /// <summary>
    /// InteropClientTestCase provides an interface that classes implementing test cases from the interop testing spec
    /// (http://cwiki.apache.org/confluence/display/qpid/Interop+Testing+Specification) should implement.
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
    interface InteropClientTestCase
    {
        /// <summary>
        /// Should provide the name of the test case that this class implements. The exact names are defined in the
        /// interop testing spec.
        /// </summary>
        ///
        /// <returns> The name of the test case that this implements. </returns>
        string GetName();

        /// <summary>
        /// Determines whether the test invite that matched this test case is acceptable.
        /// </summary>
        ///
        /// <param name="inviteMessage"> The invitation to accept or reject. </param>
        ///
        /// <returns> <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it. </returns>
        ///
        /// @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
        bool AcceptInvite(IMessage inviteMessage);

        /// <summary>
        /// Assigns the role to be played by this test case. The test parameters are fully specified in the
        /// assignment message. When this method return the test case will be ready to execute.
        /// </summary>
        ///
        /// <param name="role">              The role to be played; sender or receiver. </param>
        /// <param name="assignRoleMessage"> The role assingment message, contains the full test parameters. </param>
        void AssignRole(Roles role, IMessage assignRoleMessage);

        /// <summary>
        /// Performs the test case actions.
        /// </summary>
        void Start();

        /// <summary>
        /// Gets a report on the actions performed by the test case in its assigned role.
        /// </summary>
        ///
        /// <param name="session"> The session to create the report message in. </param>
        ///
        /// <returns> The report message. </returns>
        IMessage GetReport(IChannel channel);
    }
}
