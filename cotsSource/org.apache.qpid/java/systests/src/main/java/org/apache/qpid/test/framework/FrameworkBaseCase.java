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

import org.apache.log4j.Logger;
import org.apache.log4j.NDC;

import org.apache.qpid.test.framework.BrokerLifecycleAware;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;
import org.apache.qpid.test.utils.QpidTestCase;

import org.apache.qpid.junit.extensions.SetupTaskAware;
import org.apache.qpid.junit.extensions.SetupTaskHandler;
import org.apache.qpid.junit.extensions.util.ParsedProperties;
import org.apache.qpid.junit.extensions.util.TestContextProperties;

import java.util.ArrayList;
import java.util.List;

/**
 * FrameworkBaseCase provides a starting point for writing test cases against the test framework. Its main purpose is
 * to provide some convenience methods for testing.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Create and clean up in-vm brokers on every test case.
 * <tr><td> Produce lists of assertions from assertion creation calls.
 * <tr><td> Produce JUnit failures from assertion failures.
 * <tr><td> Convert failed assertions to error messages.
 * </table>
 */
public class FrameworkBaseCase extends QpidTestCase implements FrameworkTestContext, SetupTaskAware,
    BrokerLifecycleAware
{
    /** Used for debugging purposes. */
    private static final Logger log = Logger.getLogger(FrameworkBaseCase.class);

    /** Holds the test sequencer to create and run test circuits with. */
    protected CircuitFactory circuitFactory = new LocalAMQPCircuitFactory();

    /** Used to read the tests configurable properties through. */
    protected ParsedProperties testProps;

    /** A default setup task processor to delegate setup tasks to. */
    protected SetupTaskHandler taskHandler = new SetupTaskHandler();

    /** Flag used to track whether the test is in-vm or not. */
    protected boolean isUsingInVM;

    /** Holds the failure mechanism. */
    protected CauseFailure failureMechanism = new CauseFailureUserPrompt();

    /**
     * Creates a new test case with the specified name.
     *
     * @param name The test case name.
     */
    public FrameworkBaseCase(String name)
    {
        super(name);
    }

    /**
     * Returns the test case sequencer that provides test circuit, and test sequence implementations. The sequencer
     * that this base case returns by default is suitable for running a test circuit with both circuit ends colocated
     * on the same JVM.
     *
     * @return The test case sequencer.
     */
    protected CircuitFactory getCircuitFactory()
    {
        return circuitFactory;
    }

    /**
     * Overrides the default test circuit factory. Test decorators can use this to supply distributed test sequencers or
     * other test circuit factory specializations.
     *
     * @param circuitFactory The new test circuit factory.
     */
    public void setCircuitFactory(CircuitFactory circuitFactory)
    {
        this.circuitFactory = circuitFactory;
    }

    /**
     * Reports the current test case name.
     *
     * @return The current test case name.
     */
    public TestCaseVector getTestCaseVector()
    {
        return new TestCaseVector(this.getName(), 0);
    }

    /**
     * Reports the current test case parameters.
     *
     * @return The current test case parameters.
     */
    public MessagingTestConfigProperties getTestParameters()
    {
        return new MessagingTestConfigProperties(testProps);
    }

    /**
     * Creates a list of assertions.
     *
     * @param asserts The assertions to compile in a list.
     *
     * @return A list of assertions.
     */
    protected List<Assertion> assertionList(Assertion... asserts)
    {
        List<Assertion> result = new ArrayList<Assertion>();

        for (Assertion assertion : asserts)
        {
            result.add(assertion);
        }

        return result;
    }

    /**
     * Generates a JUnit assertion exception (failure) if any assertions are passed into this method, also concatenating
     * all of the error messages in the assertions together to form an error message to diagnose the test failure with.
     *
     * @param asserts The list of failed assertions.
     */
    protected static void assertNoFailures(List<Assertion> asserts)
    {
        log.debug("protected void assertNoFailures(List<Assertion> asserts = " + asserts + "): called");

        // Check if there are no assertion failures, and return without doing anything if so.
        if ((asserts == null) || asserts.isEmpty())
        {
            return;
        }

        // Compile all of the assertion failure messages together.
        String errorMessage = assertionsToString(asserts);

        // Fail with the error message from all of the assertions.
        fail(errorMessage);
    }

    /**
     * Converts a list of failed assertions into an error message.
     *
     * @param asserts The failed assertions.
     *
     * @return The error message.
     */
    protected static String assertionsToString(List<Assertion> asserts)
    {
        String errorMessage = "";

        for (Assertion assertion : asserts)
        {
            errorMessage += assertion.toString() + "\n";
        }

        return errorMessage;
    }

    /**
     * Ensures that the in-vm broker is created and initialized.
     *
     * @throws Exception Any exceptions allowed to fall through and fail the test.
     */
    protected void setUp() throws Exception
    {
        super.setUp();
        NDC.push(getName());

        testProps = TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);
    }

    /**
     * Ensures that the in-vm broker is cleaned up after each test run.
     */
    protected void tearDown()
    {
        NDC.pop();

        // Process all optional tear down tasks. This may include in-vm broker clean up, if a decorator has added it.
        taskHandler.runTearDownTasks();
    }

    /**
     * Adds the specified task to the tests setup.
     *
     * @param task The task to add to the tests setup.
     */
    public void chainSetupTask(Runnable task)
    {
        taskHandler.chainSetupTask(task);
    }

    /**
     * Adds the specified task to the tests tear down.
     *
     * @param task The task to add to the tests tear down.
     */
    public void chainTearDownTask(Runnable task)
    {
        taskHandler.chainTearDownTask(task);
    }

    /**
     * Should provide a translation from the junit method name of a test to its test case name as known to the test
     * clients that will run the test. The purpose of this is to convert the JUnit method name into the correct test
     * case name to place into the test invite. For example the method "testP2P" might map onto the interop test case
     * name "TC2_BasicP2P".
     *
     * @param methodName The name of the JUnit test method.
     *
     * @return The name of the corresponding interop test case.
     */
    public String getTestCaseNameForTestMethod(String methodName)
    {
        return methodName;
    }

    public void setInVmBrokers()
    {
        isUsingInVM = true;
    }

    /**
     * Indicates whether or not a test case is using in-vm brokers.
     *
     * @return <tt>true</tt> if the test is using in-vm brokers, <tt>false</tt> otherwise.
     */
    public boolean usingInVmBroker()
    {
        return isUsingInVM;
    }

    /**
     * Sets the currently live in-vm broker.
     *
     * @param i The currently live in-vm broker.
     */
    public void setLiveBroker(int i)
    { }

    /**
     * Reports the currently live in-vm broker.
     *
     * @return The currently live in-vm broker.
     */
    public int getLiveBroker()
    {
        return 0;
    }

    /**
     * Accepts a failure mechanism.
     *
     * @param failureMechanism The failure mechanism.
     */
    public void setFailureMechanism(CauseFailure failureMechanism)
    {
        this.failureMechanism = failureMechanism;
    }
}
