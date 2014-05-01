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
package org.apache.qpid.interop.clienttestcases;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.distributedtesting.TestClientControlledTest;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

/**
 * Implements tet case 1, dummy run. This test case sends no test messages, it exists to confirm that the test harness
 * is interacting with the coordinator correctly.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Supply the name of the test case that this implements.
 * <tr><td> Accept/Reject invites based on test parameters.
 * <tr><td> Adapt to assigned roles.
 * <tr><td> Perform test case actions.
 * <tr><td> Generate test reports.
 * </table>
 */
public class TestCase1DummyRun implements TestClientControlledTest
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(TestCase1DummyRun.class);

    /**
     * Should provide the name of the test case that this class implements. The exact names are defined in the
     * interop testing spec.
     *
     * @return The name of the test case that this implements.
     */
    public String getName()
    {
        log.debug("public String getName(): called");

        return "TC1_DummyRun";
    }

    /**
     * Determines whether the test invite that matched this test case is acceptable.
     *
     * @param inviteMessage The invitation to accept or reject.
     *
     * @return <tt>true</tt> to accept the invitation, <tt>false</tt> to reject it.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public boolean acceptInvite(Message inviteMessage) throws JMSException
    {
        log.debug("public boolean acceptInvite(Message inviteMessage): called");

        // Test parameters don't matter, accept all invites.
        return true;
    }

    /**
     * Assigns the role to be played by this test case. The test parameters are fully specified in the
     * assignment message. When this method return the test case will be ready to execute.
     *
     * @param role              The role to be played; sender or receivers.
     * @param assignRoleMessage The role assingment message, contains the full test parameters.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public void assignRole(Roles role, Message assignRoleMessage) throws JMSException
    {
        log.debug("public void assignRole(Roles role, Message assignRoleMessage): called");

        // Do nothing, both roles are the same.
    }

    /**
     * Performs the test case actions. Returning from here, indicates that the sending role has completed its test.
     * 
     * @param numMessages The number of test messages to send.
     */
    public void start(int numMessages)
    {
        log.debug("public void start(): called");

        // Do nothing.
    }

    /**
     * Gets a report on the actions performed by the test case in its assigned role.
     *
     * @param session The controlSession to create the report message in.
     *
     * @return The report message.
     *
     * @throws JMSException Any JMSExceptions resulting from creating the report are allowed to fall through.
     */
    public Message getReport(Session session) throws JMSException
    {
        log.debug("public Message getReport(Session controlSession): called");

        // Generate a dummy report, the coordinator expects a report but doesn't care what it is.
        return session.createTextMessage("Dummy Run, Ok.");
    }

    /**
     * Handles incoming test messages. Does nothing.
     *
     * @param message The incoming test message.
     */
    public void onMessage(Message message)
    {
        log.debug("public void onMessage(Message message = " + message + "): called");

        // Ignore any messages.
    }
}
