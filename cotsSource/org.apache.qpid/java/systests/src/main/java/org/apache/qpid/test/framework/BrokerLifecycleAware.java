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
package org.apache.qpid.test.framework;

/**
 * BrokerLifecycleAware is an awareness interface implemented by test cases that can run control the life-cycle of
 * the brokers on which they run. Its purpose is to expose additional instrumentation of brokers during testing, that
 * enables tests to use an automated failure mechanism to simulate broker failures, and to re-start failed brokers.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Indicate whether or not a test case is using an in-vm broker.
 * <tr><td> Track which in-vm broker is currently in use.
 * <tr><td> Accept setting of a failure mechanism. <td> {@link CauseFailure}.
 * </table>
 *
 * @todo Need to think about how to present the brokers through this interface. Thinking numbering the available
 *       brokers from 1 will do. Then can kill 1 and assume failing onto 2. Restart 1 and kill 2 and fail back onto
 *       1 again?
 */
public interface BrokerLifecycleAware
{
    public void setInVmBrokers();

    /**
     * Indicates whether or not a test case is using in-vm brokers.
     *
     * @return <tt>true</tt> if the test is using in-vm brokers, <tt>false</tt> otherwise.
     */
    public boolean usingInVmBroker();

    /**
     * Sets the currently live in-vm broker.
     *
     * @param i The currently live in-vm broker.
     */
    public void setLiveBroker(int i);

    /**
     * Reports the currently live in-vm broker.
     *
     * @return The currently live in-vm broker.
     */
    public int getLiveBroker();

    /**
     * Accepts a failure mechanism.
     *
     * @param failureMechanism The failure mechanism.
     */
    public void setFailureMechanism(CauseFailure failureMechanism);
}
