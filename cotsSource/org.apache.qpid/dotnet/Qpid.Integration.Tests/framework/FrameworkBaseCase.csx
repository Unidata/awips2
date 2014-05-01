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
using log4net;
using org.apache.log4j.NDC;

using Apache.Qpid.Integration.Tests.framework.BrokerLifecycleAware;
using Apache.Qpid.Integration.Tests.framework.sequencers.CircuitFactory;

using uk.co.thebadgerset.junit.extensions.AsymptoticTestCase;
using uk.co.thebadgerset.junit.extensions.SetupTaskAware;
using uk.co.thebadgerset.junit.extensions.SetupTaskHandler;
using uk.co.thebadgerset.junit.extensions.util.ParsedProperties;
using uk.co.thebadgerset.junit.extensions.util.TestContextProperties;

using java.util.ArrayList;
using System.Collections.Generic.IList;

namespace Apache.Qpid.Integration.Tests.framework
{
    /// <summary>
    /// FrameworkBaseCase provides a starting point for writing test cases against the test framework. Its main purpose is
    /// to provide some convenience methods for testing.
    ///
    /// <p/><table id="crc"><caption>CRC Card</caption>
    /// <tr><th> Responsibilities <th> Collaborations
    /// <tr><td> Create and clean up in-vm brokers on every test case.
    /// <tr><td> Produce lists of assertions from assertion creation calls.
    /// <tr><td> Produce JUnit failures from assertion failures.
    /// <tr><td> Convert failed assertions to error messages.
    /// </table>
    /// </summary>
    public class FrameworkBaseCase extends AsymptoticTestCase : FrameworkTestContext, SetupTaskAware,
        BrokerLifecycleAware
    {
        /// <summary> Used for debugging purposes. </summary>
        private static ILog log = LogManager.GetLogger(typeof(FrameworkBaseCase));

        /// <summary> Holds the test sequencer to create and run test circuits with. </summary>
        protected CircuitFactory circuitFactory = new LocalCircuitFactory();

        /// <summary> Used to read the tests configurable properties through. </summary>
        protected ParsedProperties testProps;

        /// <summary> A default setup task processor to delegate setup tasks to. </summary>
        protected SetupTaskHandler taskHandler = new SetupTaskHandler();

        /// <summary> Flag used to track whether the test is in-vm or not. </summary>
        protected bool isUsingInVM;

        /// <summary> Holds the failure mechanism. </summary>
        protected CauseFailure failureMechanism = new CauseFailureUserPrompt();

        /// <summary>
        /// Creates a new test case with the specified name.
        /// </summary>
        /// <param name="name"> The test case name. </param>
        public FrameworkBaseCase(string name)
        {
            super(name);
        }

        /// <summary>
        /// Returns the test case sequencer that provides test circuit, and test sequence implementations. The sequencer
        /// that this base case returns by default is suitable for running a test circuit with both circuit ends colocated
        /// on the same JVM.
        /// </summary>
        /// <return> The test case sequencer. </return>
        protected CircuitFactory getCircuitFactory()
        {
            return circuitFactory;
        }

        /// <summary>
        /// Overrides the default test circuit factory. Test decorators can use this to supply distributed test sequencers or
        /// other test circuit factory specializations.
        /// </summary>
        /// <param name="circuitFactory"> The new test circuit factory. </param>
        public void setCircuitFactory(CircuitFactory circuitFactory)
        {
            this.circuitFactory = circuitFactory;
        }

        /// <summary>
        /// Reports the current test case name.
        /// </summary>
        /// <return> The current test case name. </return>
        public TestCaseVector getTestCaseVector()
        {
            return new TestCaseVector(this.getName(), 0);
        }

        /// <summary>
        /// Reports the current test case parameters.
        /// </summary>
        /// <return> The current test case parameters. </return>
        public MessagingTestConfigProperties getTestParameters()
        {
            return new MessagingTestConfigProperties(testProps);
        }

        /// <summary>
        /// Creates a list of assertions.
        /// </summary>
        /// <param name="asserts"> The assertions to compile in a list. </param>
        ///
        /// <return> A list of assertions. </return>
        protected IList<Assertion> assertionList(Assertion... asserts)
        {
            IList<Assertion> result = new ArrayList<Assertion>();

            for (Assertion assertion : asserts)
            {
                result.add(assertion);
            }

            return result;
        }

        /// <summary>
        /// Generates a JUnit assertion exception (failure) if any assertions are passed into this method, also concatenating
        /// all of the error messages in the assertions together to form an error message to diagnose the test failure with.
        /// </summary>
        /// <param name="asserts"> The list of failed assertions. </param>
        protected static void assertNoFailures(List<Assertion> asserts)
        {
            log.debug("protected void assertNoFailures(List<Assertion> asserts = " + asserts + "): called");

            // Check if there are no assertion failures, and return without doing anything if so.
            if ((asserts == null) || asserts.isEmpty())
            {
                return;
            }

            // Compile all of the assertion failure messages together.
            string errorMessage = assertionsToString(asserts);

            // Fail with the error message from all of the assertions.
            fail(errorMessage);
        }

        /// <summary>
        /// Converts a list of failed assertions into an error message.
        /// </summary>
        /// <param name="asserts"> The failed assertions. </param>
        ///
        /// <return> The error message. </return>
        protected static string assertionsToString(List<Assertion> asserts)
        {
            string errorMessage = "";

            for (Assertion assertion : asserts)
            {
                errorMessage += assertion.ToString() + "\n";
            }

            return errorMessage;
        }

        /// <summary>
        /// Ensures that the in-vm broker is created and initialized.
        /// </summary>
        ///
        /// <exception cref="Exception"> Any exceptions allowed to fall through and fail the test. </exception>
        protected void setUp() throws Exception
        {
            NDC.push(getName());

            testProps = TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

            // Process all optional setup tasks. This may include in-vm broker creation, if a decorator has added it.
            taskHandler.runSetupTasks();
        }

        /// <summary> Ensures that the in-vm broker is cleaned up after each test run. </summary>
        protected void tearDown()
        {
            NDC.pop();

            // Process all optional tear down tasks. This may include in-vm broker clean up, if a decorator has added it.
            taskHandler.runTearDownTasks();
        }

        /// <summary>
        /// Adds the specified task to the tests setup.
        /// </summary>
        /// <param name="task"> The task to add to the tests setup. </param>
        public void chainSetupTask(Runnable task)
        {
            taskHandler.chainSetupTask(task);
        }

        /// <summary>
        /// Adds the specified task to the tests tear down.
        /// </summary>
        /// <param name="task"> The task to add to the tests tear down. </param>
        public void chainTearDownTask(Runnable task)
        {
            taskHandler.chainTearDownTask(task);
        }

        /// <summary>
        /// Should provide a translation from the junit method name of a test to its test case name as known to the test
        /// clients that will run the test. The purpose of this is to convert the JUnit method name into the correct test
        /// case name to place into the test invite. For example the method "testP2P" might map onto the interop test case
        /// name "TC2_BasicP2P".
        /// </summary>
        /// <param name="methodName"> The name of the JUnit test method. </param>
        ///
        /// <return> The name of the corresponding interop test case. </return>
        public string getTestCaseNameForTestMethod(string methodName)
        {
            return methodName;
        }

        public void setInVmBrokers()
        {
            isUsingInVM = true;
        }

        /// <summary>
        /// Indicates whether or not a test case is using in-vm brokers.
        /// </summary>
        /// <return> <tt>true</tt> if the test is using in-vm brokers, <tt>false</tt> otherwise. </return>
        public bool usingInVmBroker()
        {
            return isUsingInVM;
        }

        /// <summary>
        /// Sets the currently live in-vm broker.
        /// </summary>
        /// <param name="i"> The currently live in-vm broker. </param>
        public void setLiveBroker(int i)
        { }

        /// <summary>
        /// Reports the currently live in-vm broker.
        /// </summary>
        /// <return> The currently live in-vm broker. </return>
        public int getLiveBroker()
        {
            return 0;
        }

        /// <summary>
        /// Accepts a failure mechanism.
        /// </summary>
        /// <param name="failureMechanism"> The failure mechanism. </param>
        public void setFailureMechanism(CauseFailure failureMechanism)
        {
            this.failureMechanism = failureMechanism;
        }
    }
}