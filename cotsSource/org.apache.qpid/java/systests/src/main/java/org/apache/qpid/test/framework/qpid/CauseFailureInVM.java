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
package org.apache.qpid.test.framework.qpid;

import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.test.framework.CauseFailure;
import org.apache.qpid.test.framework.BrokerLifecycleAware;

/**
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Cause messaging broker failure on the active in-vm broker.
 *     <td> {@link TransportConnection}, {@link ApplicationRegistry}
 * </table>
 */
public class CauseFailureInVM implements CauseFailure
{
    /** Holds the in-vm broker instrumented test case to create failures for. */
    private BrokerLifecycleAware inVMTest;

    /**
     * Creates an automated failure mechanism for testing against in-vm brokers. The test to create the mechanism
     * for is specified, and as this failure is for in-vm brokers, the test must be {@link org.apache.qpid.test.framework.BrokerLifecycleAware}. The test
     * must also report that it is currently being run against an in-vm broker, and it is a runtime error if it is not,
     * as the creator of this failure mechanism should already have checked that it is.
     *
     * @param inVMTest The test case to create an automated failure mechanism for.
     */
    public CauseFailureInVM(BrokerLifecycleAware inVMTest)
    {
        // Check that the test is really using in-vm brokers.
        if (!inVMTest.usingInVmBroker())
        {
            throw new RuntimeException(
                "Cannot create in-vm broker failure mechanism for a test that is not using in-vm brokers.");
        }

        this.inVMTest = inVMTest;
    }

    /**
     * Causes the active message broker to fail.
     */
    public void causeFailure()
    {
        int liveBroker = inVMTest.getLiveBroker();

        TransportConnection.killVMBroker(liveBroker);
        ApplicationRegistry.remove(liveBroker);
    }
}
