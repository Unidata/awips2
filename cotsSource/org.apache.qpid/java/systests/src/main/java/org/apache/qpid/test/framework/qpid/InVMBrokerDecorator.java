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

import org.apache.qpid.client.transport.TransportConnection;
import org.apache.qpid.client.vmbroker.AMQVMBrokerCreationException;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.test.framework.BrokerLifecycleAware;
import org.apache.qpid.test.framework.FrameworkBaseCase;

import org.apache.qpid.junit.extensions.SetupTaskAware;
import org.apache.qpid.junit.extensions.WrappedSuiteTestDecorator;

/**
 * InVMBrokerDecorator is a test decorator, that is activated when running tests against an in-vm broker only. Its
 * purpose is to automatically create, and close and delete an in-vm broker, during the set-up and tear-down of
 * each test case. This decorator may only be used in conjunction with tests that extend {@link FrameworkBaseCase}.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create/Destroy an in-vm broker on every test run.
 * </table>
 *
 * @todo May need to add a more fine grained injection point for the in-vm broker management, as this acts at the
 *       suite level, rather than the individual test level.
 *
 * @todo Management of in-vm brokers for failure testing. Failure test setups may need to set their connection url to
 *       use multiple broker (vm://:1;vm://:2), with fail-over between them. There is round-robin fail-over, but also
 *       retry? A test case using an in-vm broker needs to record which one it is using, so that it can be
 *       killed/restarted.
 */
public class InVMBrokerDecorator extends WrappedSuiteTestDecorator
{
    /** The test suite to run. */
    private Test test;

    /**
     * Creates a wrapped test suite decorator from another one.
     *
     * @param test The test suite.
     */
    public InVMBrokerDecorator(WrappedSuiteTestDecorator test)
    {
        super(test);
        this.test = test;
    }

    /**
     * Runs the tests with in-vm broker creation and clean-up added to the tests task stack.
     *
     * @param testResult The the results object to monitor the test results with.
     */
    public void run(TestResult testResult)
    {
        for (Test test : getAllUnderlyingTests())
        {
            // Check that the test to have an in-vm broker setup/teardown task added to it, is actually a framework
            // test that can handle setup tasks.
            if ((test instanceof SetupTaskAware))
            {
                SetupTaskAware frameworkTest = (SetupTaskAware) test;

                frameworkTest.chainSetupTask(new Runnable()
                    {
                        public void run()
                        {
                            // Ensure that the in-vm broker is created.
                            try
                            {
                                ApplicationRegistry.getInstance(1);
                                TransportConnection.createVMBroker(1);
                            }
                            catch (AMQVMBrokerCreationException e)
                            {
                                throw new RuntimeException("In-VM broker creation failed: " + e.getMessage(), e);
                            }
                        }
                    });

                frameworkTest.chainTearDownTask(new Runnable()
                    {
                        public void run()
                        {
                            // Ensure that the in-vm broker is cleaned up so that the next test starts afresh.
                            TransportConnection.killVMBroker(1);
                            ApplicationRegistry.remove(1);
                        }
                    });

                // Check if the test is aware whether or not it can control the broker life cycle, and if so provide
                // additional instrumentation for it to control the in-vm broker through.
                if (test instanceof BrokerLifecycleAware)
                {
                    BrokerLifecycleAware inVMTest = (BrokerLifecycleAware) test;
                    inVMTest.setInVmBrokers();
                    inVMTest.setLiveBroker(1);
                    inVMTest.setFailureMechanism(new CauseFailureInVM(inVMTest));
                }
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
        return "InVMBrokerDecorator: [test = " + test + "]";
    }
}
