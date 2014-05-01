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
import org.apache.qpid.test.framework.MessagingTestConfigProperties;
import static org.apache.qpid.test.framework.MessagingTestConfigProperties.*;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;

import org.apache.qpid.junit.extensions.util.ParsedProperties;
import org.apache.qpid.junit.extensions.util.TestContextProperties;

/**
 * RollbackTest tests the rollback ability of transactional messaging.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Check messages sent but rolled back are never received.
 * <tr><td> Check messages received but rolled back are redelivered on subsequent receives.
 * <tr><td> Attempting to rollback outside of a transaction results in an IllegalStateException.
 * </table>
 */
public class RollbackTest extends FrameworkBaseCase
{
    /** Used to read the tests configurable properties through. */
    ParsedProperties testProps;

    /**
     * Creates a new test case with the specified name.
     *
     * @param name The test case name.
     */
    public RollbackTest(String name)
    {
        super(name);
    }

    /** Check messages sent but rolled back are never received. */
    public void testRolledbackMessageNotDelivered() throws Exception
    {
        // Ensure transactional sessions are on.
        testProps.setProperty(TRANSACTED_PUBLISHER_PROPNAME, true);
        testProps.setProperty(ROLLBACK_PUBLISHER_PROPNAME, true);

        // Run the default test sequence over the test circuit checking for no errors.
        CircuitFactory circuitFactory = getCircuitFactory();
        Circuit testCircuit = circuitFactory.createCircuit(getConnection(), testProps);

        assertNoFailures(testCircuit.test(1,
                assertionList(testCircuit.getPublisher().noExceptionsAssertion(testProps),
                    testCircuit.getReceiver().noMessagesReceivedAssertion(testProps))));
    }

    /** Check messages received but rolled back are redelivered on subsequent receives. */
    public void testRolledbackMessagesSubsequentlyReceived() throws Exception
    {
        // Ensure transactional sessions are on.
        testProps.setProperty(TRANSACTED_RECEIVER_PROPNAME, true);
        testProps.setProperty(ROLLBACK_RECEIVER_PROPNAME, true);

        // Run the default test sequence over the test circuit checking for no errors.
        CircuitFactory circuitFactory = getCircuitFactory();
        Circuit testCircuit = circuitFactory.createCircuit(getConnection(), testProps);

        assertNoFailures(testCircuit.test(1,
                assertionList(testCircuit.getPublisher().noExceptionsAssertion(testProps),
                    testCircuit.getReceiver().allMessagesReceivedAssertion(testProps))));
    }

    /** Attempting to rollback outside of a transaction results in an IllegalStateException. */
    public void testRollbackUnavailableOutsideTransactionPublisher() throws Exception
    {
        // Ensure transactional sessions are on.
        testProps.setProperty(TRANSACTED_PUBLISHER_PROPNAME, false);
        testProps.setProperty(ROLLBACK_PUBLISHER_PROPNAME, true);

        // Run the default test sequence over the test circuit checking for no errors.
        CircuitFactory circuitFactory = getCircuitFactory();
        Circuit testCircuit = circuitFactory.createCircuit(getConnection(), testProps);

        assertNoFailures(testCircuit.test(1, assertionList(testCircuit.getPublisher().channelClosedAssertion(testProps))));
    }

    /** Attempting to rollback outside of a transaction results in an IllegalStateException. */
    public void testRollbackUnavailableOutsideTransactionReceiver() throws Exception
    {
        // Ensure transactional sessions are on.
        testProps.setProperty(TRANSACTED_RECEIVER_PROPNAME, false);
        testProps.setProperty(ROLLBACK_RECEIVER_PROPNAME, true);

        // Run the default test sequence over the test circuit checking for no errors.
        CircuitFactory circuitFactory = getCircuitFactory();
        Circuit testCircuit = circuitFactory.createCircuit(getConnection(), testProps);

        assertNoFailures(testCircuit.test(1, assertionList(testCircuit.getReceiver().channelClosedAssertion(testProps))));
    }

    /**
     * Sets up all tests to have an active outward route and consumer by default.
     *
     * @throws Exception Any exceptions are allowed to fall through and fail the test.
     */
    protected void setUp() throws Exception
    {
        super.setUp();

        testProps = TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

        /** Bind the receivers consumer by default. */
        testProps.setProperty(RECEIVER_CONSUMER_BIND_PROPNAME, true);
        testProps.setProperty(RECEIVER_CONSUMER_ACTIVE_PROPNAME, true);
    }
}
