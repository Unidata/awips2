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
package org.apache.qpid.test.framework.distributedtesting;

import java.net.InetAddress;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;

import javax.jms.*;

import junit.framework.Test;
import junit.framework.TestResult;
import junit.framework.TestSuite;

import org.apache.log4j.Logger;
import org.apache.log4j.NDC;
import org.apache.qpid.test.framework.FrameworkBaseCase;
import org.apache.qpid.test.framework.MessagingTestConfigProperties;
import org.apache.qpid.test.framework.TestClientDetails;
import org.apache.qpid.test.framework.TestUtils;
import org.apache.qpid.test.framework.clocksynch.UDPClockReference;
import org.apache.qpid.test.utils.ConversationFactory;

import org.apache.qpid.junit.extensions.TKTestRunner;
import org.apache.qpid.junit.extensions.WrappedSuiteTestDecorator;
import org.apache.qpid.junit.extensions.util.CommandLineParser;
import org.apache.qpid.junit.extensions.util.MathUtils;
import org.apache.qpid.junit.extensions.util.ParsedProperties;
import org.apache.qpid.junit.extensions.util.TestContextProperties;

/**
 * <p/>Implements the coordinator client described in the interop testing specification
 * (http://cwiki.apache.org/confluence/display/qpid/Interop+Testing+Specification). This coordinator is built on
 * top of the JUnit testing framework.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Find out what test clients are available. <td> {@link ConversationFactory}
 * <tr><td> Decorate available tests to run on all available clients. <td> {@link DistributedTestDecorator}
 * <tr><td> Attach XML test result logger.
 * <tr><td> Terminate the interop testing framework.
 * </table>
 *
 * @todo Should accumulate failures over all tests, and return with success or fail code based on all results. May need
 *       to write a special TestResult to do this properly. At the moment only the last one used will be tested for
 *       errors, as the start method creates a fresh one for each test case run.
 */
public class Coordinator extends TKTestRunner
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(Coordinator.class);

    /** Used for reporting to the console. */
    private static final Logger console = Logger.getLogger("CONSOLE");

    /** Defines the possible distributed test engines available to run coordinated test cases with. */
    public enum TestEngine
    {
        /** Specifies the interop test engine. This tests all available clients in pairs. */
        INTEROP,

        /** Specifies the fanout test engine. This sets up one publisher role, and many reciever roles. */
        FANOUT
    }

    /**
     * Holds the test context properties that provides the default test parameters, plus command line overrides.
     * This is initialized with the default test parameters, to which command line overrides may be applied.
     */
    protected static ParsedProperties testContextProperties =
        TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

    /** Holds the URL of the broker to coordinate the tests on. */
    protected String brokerUrl;

    /** Holds the virtual host to coordinate the tests on. If <tt>null</tt>, then the default virtual host is used. */
    protected String virtualHost;

    /** Holds the list of all clients that enlisted, when the compulsory invite was issued. */
    protected Set<TestClientDetails> enlistedClients = new HashSet<TestClientDetails>();

    /** Holds the conversation helper for the control conversation. */
    protected ConversationFactory conversationFactory;

    /** Holds the connection that the coordinating messages are sent over. */
    protected Connection connection;

    /** Holds the path of the directory to output test results too, if one is defined. */
    protected String reportDir;

    /** Holds the coordinating test engine type to run the tests through. */
    protected TestEngine engine;

    /** Flag that indicates that all test clients should be terminated upon completion of the test cases. */
    protected boolean terminate;

    /**
     * Creates an interop test coordinator on the specified broker and virtual host.
     *
     * @param repetitions        The number of times to repeat the test, or test batch size.
     * @param duration           The length of time to run the tests for. -1 means no duration has been set.
     * @param threads            The concurrency levels to ramp up to.
     * @param delay              A delay in milliseconds between test runs.
     * @param params             The sets of 'size' parameters to pass to test.
     * @param testCaseName       The name of the test case to run.
     * @param reportDir          The directory to output the test results to.
     * @param runName            The name of the test run; used to name the output file.
     * @param verbose            Whether to print comments during test run.
     * @param brokerUrl          The URL of the broker to connect to.
     * @param virtualHost        The virtual host to run all tests on. Optional, may be <tt>null</tt>.
     * @param engine             The distributed test engine type to run the tests with.
     * @param terminate          <tt>true</tt> if test client nodes should be terminated at the end of the tests.
     * @param csv                <tt>true</tt> if the CSV results listener should be attached.
     * @param xml                <tt>true</tt> if the XML results listener should be attached.
     * @param decoratorFactories List of factories for user specified decorators.
     */
    public Coordinator(Integer repetitions, Long duration, int[] threads, int delay, int[] params, String testCaseName,
        String reportDir, String runName, boolean verbose, String brokerUrl, String virtualHost, TestEngine engine,
        boolean terminate, boolean csv, boolean xml, List<TestDecoratorFactory> decoratorFactories)
    {
        super(repetitions, duration, threads, delay, params, testCaseName, reportDir, runName, csv, xml, decoratorFactories);

        log.debug("public Coordinator(Integer repetitions = " + repetitions + " , Long duration = " + duration
            + ", int[] threads = " + Arrays.toString(threads) + ", int delay = " + delay + ", int[] params = "
            + Arrays.toString(params) + ", String testCaseName = " + testCaseName + ", String reportDir = " + reportDir
            + ", String runName = " + runName + ", boolean verbose = " + verbose + ", String brokerUrl = " + brokerUrl
            + ", String virtualHost =" + virtualHost + ", TestEngine engine = " + engine + ", boolean terminate = "
            + terminate + ", boolean csv = " + csv + ", boolean xml = " + xml + "): called");

        // Retain the connection parameters.
        this.brokerUrl = brokerUrl;
        this.virtualHost = virtualHost;
        this.reportDir = reportDir;
        this.engine = engine;
        this.terminate = terminate;
    }

    /**
     * The entry point for the interop test coordinator. This client accepts the following command line arguments:
     *
     * <p/><table>
     * <tr><td> -b         <td> The broker URL.   <td> Mandatory.
     * <tr><td> -h         <td> The virtual host. <td> Optional.
     * <tr><td> -o         <td> The directory to output test results to. <td> Optional.
     * <tr><td> -e         <td> The type of test distribution engine to use. <td> Optional. One of: interop, fanout.
     * <tr><td> ...        <td> Free arguments. The distributed test cases to run.
     *                     <td> Mandatory. At least one must be defined.
     * <tr><td> name=value <td> Trailing argument define name/value pairs. Added to the test contenxt properties.
     *                     <td> Optional.
     * </table>
     *
     * @param args The command line arguments.
     */
    public static void main(String[] args)
    {
        NDC.push("coordinator");
        log.debug("public static void main(String[] args = " + Arrays.toString(args) + "): called");
        console.info("Qpid Distributed Test Coordinator.");

        // Override the default broker url to be localhost:5672.
        testContextProperties.setProperty(MessagingTestConfigProperties.BROKER_PROPNAME, "tcp://localhost:5672");

        try
        {
            // Use the command line parser to evaluate the command line with standard handling behaviour (print errors
            // and usage then exist if there are errors).
            // Any options and trailing name=value pairs are also injected into the test context properties object,
            // to override any defaults that may have been set up.
            ParsedProperties options =
                new ParsedProperties(CommandLineParser.processCommandLine(args,
                        new CommandLineParser(
                            new String[][]
                            {
                                { "b", "The broker URL.", "broker", "false" },
                                { "h", "The virtual host to use.", "virtual host", "false" },
                                { "o", "The name of the directory to output test timings to.", "dir", "false" },
                                {
                                    "e", "The test execution engine to use. Default is interop.", "engine", "interop",
                                    "^interop$|^fanout$", "true"
                                },
                                { "t", "Terminate test clients on completion of tests.", null, "false" },
                                { "-csv", "Output test results in CSV format.", null, "false" },
                                { "-xml", "Output test results in XML format.", null, "false" },
                                {
                                    "-trefaddr", "To specify an alternative to hostname for time singal reference.",
                                    "address", "false"
                                },
                                {
                                    "c", "The number of tests to run concurrently.", "num", "false",
                                    MathUtils.SEQUENCE_REGEXP
                                },
                                { "r", "The number of times to repeat each test.", "num", "false" },
                                {
                                    "d", "The length of time to run the tests for.", "duration", "false",
                                    MathUtils.DURATION_REGEXP
                                },
                                {
                                    "f", "The maximum rate to call the tests at.", "frequency", "false",
                                    "^([1-9][0-9]*)/([1-9][0-9]*)$"
                                },
                                { "s", "The size parameter to run tests with.", "size", "false", MathUtils.SEQUENCE_REGEXP },
                                { "v", "Verbose mode.", null, "false" },
                                { "n", "A name for this test run, used to name the output file.", "name", "true" },
                                {
                                    "X:decorators", "A list of additional test decorators to wrap the tests in.",
                                    "\"class.name[:class.name]*\"", "false"
                                }
                            }), testContextProperties));

            // Extract the command line options.
            String brokerUrl = options.getProperty("b");
            String virtualHost = options.getProperty("h");
            String reportDir = options.getProperty("o");
            reportDir = (reportDir == null) ? "." : reportDir;
            String testEngine = options.getProperty("e");
            TestEngine engine = "fanout".equals(testEngine) ? TestEngine.FANOUT : TestEngine.INTEROP;
            boolean terminate = options.getPropertyAsBoolean("t");
            boolean csvResults = options.getPropertyAsBoolean("-csv");
            boolean xmlResults = options.getPropertyAsBoolean("-xml");
            String threadsString = options.getProperty("c");
            Integer repetitions = options.getPropertyAsInteger("r");
            String durationString = options.getProperty("d");
            String paramsString = options.getProperty("s");
            boolean verbose = options.getPropertyAsBoolean("v");
            String testRunName = options.getProperty("n");
            String decorators = options.getProperty("X:decorators");

            int[] threads = (threadsString == null) ? null : MathUtils.parseSequence(threadsString);
            int[] params = (paramsString == null) ? null : MathUtils.parseSequence(paramsString);
            Long duration = (durationString == null) ? null : MathUtils.parseDuration(durationString);

            // If broker or virtual host settings were specified as command line options, override the defaults in the
            // test context properties with them.

            // Collection all of the test cases to be run.
            Collection<Class<? extends FrameworkBaseCase>> testCaseClasses =
                new ArrayList<Class<? extends FrameworkBaseCase>>();

            // Create a list of test decorator factories for use specified decorators to be applied.
            List<TestDecoratorFactory> decoratorFactories = parseDecorators(decorators);

            // Scan for available test cases using a classpath scanner.
            // ClasspathScanner.getMatches(DistributedTestCase.class, "^Test.*", true);

            // Hard code the test classes till the classpath scanner is fixed.
            // Collections.addAll(testCaseClasses, InteropTestCase1DummyRun.class, InteropTestCase2BasicP2P.class,
            // InteropTestCase3BasicPubSub.class);

            // Parse all of the free arguments as test cases to run.
            for (int i = 1; true; i++)
            {
                String nextFreeArg = options.getProperty(Integer.toString(i));

                // Terminate the loop once all free arguments have been consumed.
                if (nextFreeArg == null)
                {
                    break;
                }

                try
                {
                    Class nextClass = Class.forName(nextFreeArg);

                    if (FrameworkBaseCase.class.isAssignableFrom(nextClass))
                    {
                        testCaseClasses.add(nextClass);
                        console.info("Found distributed test case: " + nextFreeArg);
                    }
                }
                catch (ClassNotFoundException e)
                {
                    console.info("Unable to instantiate the test case: " + nextFreeArg + ".");
                }
            }

            // Check that some test classes were actually found.
            if (testCaseClasses.isEmpty())
            {
                throw new RuntimeException(
                    "No test cases implementing FrameworkBaseCase were specified on the command line.");
            }

            // Extract the names of all the test classes, to pass to the start method.
            int i = 0;
            String[] testClassNames = new String[testCaseClasses.size()];

            for (Class testClass : testCaseClasses)
            {
                testClassNames[i++] = testClass.getName();
            }

            // Create a coordinator and begin its test procedure.
            Coordinator coordinator =
                new Coordinator(repetitions, duration, threads, 0, params, null, reportDir, testRunName, verbose, brokerUrl,
                    virtualHost, engine, terminate, csvResults, xmlResults, decoratorFactories);

            TestResult testResult = coordinator.start(testClassNames);

            // Return different error codes, depending on whether or not there were test failures.
            if (testResult.failureCount() > 0)
            {
                System.exit(FAILURE_EXIT);
            }
            else
            {
                System.exit(SUCCESS_EXIT);
            }
        }
        catch (Exception e)
        {
            log.debug("Top level handler caught execption.", e);
            console.info(e.getMessage());
            e.printStackTrace();
            System.exit(EXCEPTION_EXIT);
        }
    }

    /**
     * Starts all of the test classes to be run by this coordinator.
     *
     * @param testClassNames An array of all the coordinating test case implementations.
     *
     * @return A JUnit TestResult to run the tests with.
     *
     * @throws Exception Any underlying exceptions are allowed to fall through, and fail the test process.
     */
    public TestResult start(String[] testClassNames) throws Exception
    {
        log.debug("public TestResult start(String[] testClassNames = " + Arrays.toString(testClassNames) + ": called");

        // Connect to the broker.
        connection = TestUtils.createConnection(TestContextProperties.getInstance());
        Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        Destination controlTopic = session.createTopic("iop.control");
        Destination responseQueue = session.createQueue("coordinator");

        conversationFactory = new ConversationFactory(connection, responseQueue, LinkedBlockingQueue.class);
        ConversationFactory.Conversation conversation = conversationFactory.startConversation();

        connection.start();

        // Broadcast the compulsory invitation to find out what clients are available to test.
        Message invite = session.createMessage();
        invite.setStringProperty("CONTROL_TYPE", "INVITE");
        invite.setJMSReplyTo(responseQueue);

        conversation.send(controlTopic, invite);

        // Wait for a short time, to give test clients an opportunity to reply to the invitation.
        Collection<Message> enlists = conversation.receiveAll(0, 500);
        enlistedClients = extractEnlists(enlists);

        for (TestClientDetails client : enlistedClients)
        {
            log.debug("Got enlisted test client: " + client);
            console.info("Test node " + client.clientName + " available.");
        }

        // Start the clock reference service running.
        UDPClockReference clockReference = new UDPClockReference();
        Thread clockRefThread = new Thread(clockReference);
        registerShutdownHook(clockReference);
        clockRefThread.start();

        // Broadcast to all clients to synchronize their clocks against the coordinators clock reference.
        Message clockSynchRequest = session.createMessage();
        clockSynchRequest.setStringProperty("CONTROL_TYPE", "CLOCK_SYNCH");

        String localAddress = InetAddress.getByName(InetAddress.getLocalHost().getHostName()).getHostAddress();
        clockSynchRequest.setStringProperty("ADDRESS", localAddress);

        conversation.send(controlTopic, clockSynchRequest);

        // Run the test in the suite using JUnit.
        TestResult result = null;

        for (String testClassName : testClassNames)
        {
            // Record the current test class, so that the test results can be output to a file incorporating this name.
            this.currentTestClassName = testClassName;

            result = super.start(new String[] { testClassName });
        }

        // At this point in time, all tests have completed. Broadcast the shutdown message, if the termination option
        // was set on the command line.
        if (terminate)
        {
            Message terminate = session.createMessage();
            terminate.setStringProperty("CONTROL_TYPE", "TERMINATE");

            conversation.send(controlTopic, terminate);
        }

        return result;
    }

    /**
     * For a collection of enlist messages, this method pulls out of the client details for the enlisting clients.
     *
     * @param enlists The enlist messages.
     *
     * @return A set of enlisting clients, extracted from the enlist messages.
     *
     * @throws JMSException Any underlying JMSException is allowed to fall through.
     */
    public static Set<TestClientDetails> extractEnlists(Collection<Message> enlists) throws JMSException
    {
        log.debug("public static Set<TestClientDetails> extractEnlists(Collection<Message> enlists = " + enlists
            + "): called");

        Set<TestClientDetails> enlistedClients = new HashSet<TestClientDetails>();

        // Retain the list of all available clients.
        for (Message enlist : enlists)
        {
            TestClientDetails clientDetails = new TestClientDetails();
            clientDetails.clientName = enlist.getStringProperty("CLIENT_NAME");
            clientDetails.privateControlKey = enlist.getStringProperty("CLIENT_PRIVATE_CONTROL_KEY");

            String replyType = enlist.getStringProperty("CONTROL_TYPE");

            if ("ENLIST".equals(replyType))
            {
                enlistedClients.add(clientDetails);
            }
            else if ("DECLINE".equals(replyType))
            {
                log.debug("Test client " + clientDetails.clientName + " declined the invite.");
            }
            else
            {
                log.warn("Got an unknown reply type, " + replyType + ", to the invite.");
            }
        }

        return enlistedClients;
    }

    /**
     * Runs a test or suite of tests, using the super class implemenation. This method wraps the test to be run
     * in any test decorators needed to add in the coordinators ability to invite test clients to participate in
     * tests.
     *
     * @param test The test to run.
     * @param wait Undocumented. Nothing in the JUnit javadocs to say what this is for.
     *
     * @return The results of the test run.
     */
    public TestResult doRun(Test test, boolean wait)
    {
        log.debug("public TestResult doRun(Test \"" + test + "\", boolean " + wait + "): called");

        // Wrap all tests in the test suite with WrappedSuiteTestDecorators. This is quite ugly and a bit baffling,
        // but the reason it is done is because the JUnit implementation of TestDecorator has some bugs in it.
        WrappedSuiteTestDecorator targetTest = null;

        if (test instanceof TestSuite)
        {
            log.debug("targetTest is a TestSuite");

            TestSuite suite = (TestSuite)test;

            int numTests = suite.countTestCases();
            log.debug("There are " + numTests + " in the suite.");

            for (int i = 0; i < numTests; i++)
            {
                Test nextTest = suite.testAt(i);
                log.debug("suite.testAt(" + i + ") = " + nextTest);

                if (nextTest instanceof FrameworkBaseCase)
                {
                    log.debug("nextTest is a FrameworkBaseCase");
                }
            }

            targetTest = new WrappedSuiteTestDecorator(suite);
            log.debug("Wrapped with a WrappedSuiteTestDecorator.");
        }

        // Apply any optional user specified decorators.
        targetTest = applyOptionalUserDecorators(targetTest);

        // Wrap the tests in a suitable distributed test decorator, to perform the invite/test cycle.
        targetTest = newTestDecorator(targetTest, enlistedClients, conversationFactory, connection);

        // TestSuite suite = new TestSuite();
        // suite.addTest(targetTest);

        // Wrap the tests in a scaled test decorator to them them as a 'batch' in one thread.
        // targetTest = new ScaledTestDecorator(targetTest, new int[] { 1 });

        return super.doRun(targetTest, wait);
    }

    /**
     * Creates a wrapped test decorator, that is capable of inviting enlisted clients to participate in a specified
     * test. This is the test engine that sets up the roles and sequences a distributed test case.
     *
     * @param targetTest          The test decorator to wrap.
     * @param enlistedClients     The enlisted clients available to run the test.
     * @param conversationFactory The conversation factory used to build conversation helper over the specified connection.
     * @param connection          The connection to talk to the enlisted clients over.
     *
     * @return An invititing test decorator, that invites all the enlisted clients to participate in tests, in pairs.
     */
    protected DistributedTestDecorator newTestDecorator(WrappedSuiteTestDecorator targetTest,
        Set<TestClientDetails> enlistedClients, ConversationFactory conversationFactory, Connection connection)
    {
        switch (engine)
        {
        case FANOUT:
            return new FanOutTestDecorator(targetTest, enlistedClients, conversationFactory, connection);
        case INTEROP:
        default:
            return new InteropTestDecorator(targetTest, enlistedClients, conversationFactory, connection);
        }
    }
}
