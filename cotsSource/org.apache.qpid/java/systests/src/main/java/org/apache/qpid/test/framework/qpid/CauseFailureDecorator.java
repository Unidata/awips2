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

import junit.framework.Test;
import junit.framework.TestResult;

import org.apache.qpid.test.framework.BrokerLifecycleAware;
import org.apache.qpid.test.framework.CauseFailureUserPrompt;

import org.apache.qpid.junit.extensions.WrappedSuiteTestDecorator;

/**
 * CauseFailureDecorator applies decorations to {@link BrokerLifecycleAware} tests, so that they may use different failure
 * mechanisms. It is capable of detecting when a test case uses in-vm brokers, and setting up an automatic failure
 * for those tests, so that the current live broker can be shut-down by test cases. For external brokers, automatic
 * failure could be implemented, for example by having a kill script. At the moment this sets up the failure to prompt
 * a user interactively to cause a failure, using {@link CauseFailureUserPrompt}.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Setup automatic failures for in-vm brokers. <td> {@link CauseFailureInVM}
 * <tr><td> Setup user generated failures for external brokers. <td> {@link CauseFailureUserPrompt}.
 * <tr><td>
 * </table>
 *
 * @todo Slight problem in that CauseFailureInVM is Qpid specific, whereas CauseFailureUserPrompt is not. Would like the
 *       failure decorator to be non-qpid specific so that it can test failure of any JMS implementation too. Either pass
 *       in class name of failure mechanism, set it up in the in-vm decorator instead of here but with prompt user as the
 *       default for when the in-vm decorator is not used?
 */
public class CauseFailureDecorator extends WrappedSuiteTestDecorator
{
    /** The test suite to run. */
    private Test test;

    /**
     * Creates a wrapped test test decorator from another one.
     *
     * @param test The test test.
     */
    public CauseFailureDecorator(WrappedSuiteTestDecorator test)
    {
        super(test);
        this.test = test;
    }

    /**
     * Runs the tests with a LocalAMQPCircuitFactory. Only tests that extend FrameworkBaseCase are decorated.
     *
     * @param testResult The the results object to monitor the test results with.
     */
    public void run(TestResult testResult)
    {
        for (Test test : getAllUnderlyingTests())
        {
            if (test instanceof BrokerLifecycleAware)
            {
                BrokerLifecycleAware failureTest = (BrokerLifecycleAware) test;
                failureTest.setFailureMechanism(new CauseFailureUserPrompt());
            }
        }

        // Run the test.
        test.run(testResult);
    }

    /**
     * Prints the name of the test for debugging purposes.
     *
     * @return The name of the test.
     */
    public String toString()
    {
        return "CauseFailureDecorator: [test = \"" + test + "\"]";
    }
}
