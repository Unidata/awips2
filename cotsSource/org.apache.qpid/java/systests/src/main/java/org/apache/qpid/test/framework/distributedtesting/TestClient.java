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

import org.apache.log4j.Logger;
import org.apache.log4j.NDC;

import org.apache.qpid.test.framework.MessagingTestConfigProperties;
import org.apache.qpid.test.framework.TestUtils;
import org.apache.qpid.test.framework.clocksynch.ClockSynchThread;
import org.apache.qpid.test.framework.clocksynch.UDPClockSynchronizer;
import org.apache.qpid.test.utils.ReflectionUtils;
import org.apache.qpid.test.utils.ReflectionUtilsException;

import org.apache.qpid.junit.extensions.SleepThrottle;
import org.apache.qpid.junit.extensions.util.ParsedProperties;
import org.apache.qpid.junit.extensions.util.TestContextProperties;

import javax.jms.*;

import java.util.*;

/**
 * Implements a test client as described in the interop testing spec
 * (http://cwiki.apache.org/confluence/display/qpid/Interop+Testing+Specification). A test client is an agent that
 * reacts to control message sequences send by the test {@link Coordinator}.
 *
 * <p/><table><caption>Messages Handled by TestClient</caption>
 * <tr><th> Message               <th> Action
 * <tr><td> Invite(compulsory)    <td> Reply with Enlist.
 * <tr><td> Invite(test case)     <td> Reply with Enlist if test case available.
 * <tr><td> AssignRole(test case) <td> Reply with Accept Role if matches an enlisted test. Keep test parameters.
 * <tr><td> Start                 <td> Send test messages defined by test parameters. Send report on messages sent.
 * <tr><td> Status Request        <td> Send report on messages received.
 * <tr><td> Terminate             <td> Terminate the test client.
 * <tr><td> ClockSynch            <td> Synch clock against the supplied UDP address.
 * </table>
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Handle all incoming control messages. <td> {@link TestClientControlledTest}
 * <tr><td> Configure and look up test cases by name. <td> {@link TestClientControlledTest}
 * </table>
 */
public class TestClient implements MessageListener
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(TestClient.class);

    /** Used for reporting to the console. */
    private static final Logger console = Logger.getLogger("CONSOLE");

    /** Holds the default identifying name of the test client. */
    public static final String CLIENT_NAME = "java";

    /** Holds the URL of the broker to run the tests on. */
    public static String brokerUrl;

    /** Holds the virtual host to run the tests on. If <tt>null</tt>, then the default virtual host is used. */
    public static String virtualHost;

    /**
     * Holds the test context properties that provides the default test parameters, plus command line overrides.
     * This is initialized with the default test parameters, to which command line overrides may be applied.
     */
    public static ParsedProperties testContextProperties =
        TestContextProperties.getInstance(MessagingTestConfigProperties.defaults);

    /** Holds all the test cases loaded from the classpath. */
    Map<String, TestClientControlledTest> testCases = new HashMap<String, TestClientControlledTest>();

    /** Holds the test case currently being run by this client. */
    protected TestClientControlledTest currentTestCase;

    /** Holds the connection to the broker that the test is being coordinated on. */
    protected Connection connection;

    /** Holds the message producer to hold the test coordination over. */
    protected MessageProducer producer;

    /** Holds the JMS controlSession for the test coordination. */
    protected Session session;

    /** Holds the name of this client, with a default value. */
    protected String clientName = CLIENT_NAME;

    /** This flag indicates that the test client should attempt to join the currently running test case on start up. */
    protected boolean join;

    /** Holds the clock synchronizer for the test node. */
    ClockSynchThread clockSynchThread;

    /**
     * Creates a new interop test client, listenting to the specified broker and virtual host, with the specified client
     * identifying name.
     *
     * @param pBrokerUrl   The url of the broker to connect to.
     * @param pVirtualHost The virtual host to conect to.
     * @param clientName  The client name to use.
     * @param join        Flag to indicate that this client should attempt to join running tests.
     */
    public TestClient(String pBrokerUrl, String pVirtualHost, String clientName, boolean join)
    {
        log.debug("public TestClient(String pBrokerUrl = " + pBrokerUrl + ", String pVirtualHost = " + pVirtualHost
            + ", String clientName = " + clientName + ", boolean join = " + join + "): called");

        // Retain the connection parameters.
        brokerUrl = pBrokerUrl;
        virtualHost = pVirtualHost;
        this.clientName = clientName;
        this.join = join;
    }

    /**
     * The entry point for the interop test coordinator. This client accepts the following command line arguments:
     *
     * <p/><table>
     * <tr><td> -b         <td> The broker URL.       <td> Optional.
     * <tr><td> -h         <td> The virtual host.     <td> Optional.
     * <tr><td> -n         <td> The test client name. <td> Optional.
     * <tr><td> name=value <td> Trailing argument define name/value pairs. Added to system properties. <td> Optional.
     * </table>
     *
     * @param args The command line arguments.
     */
    public static void main(String[] args)
    {
        log.debug("public static void main(String[] args = " + Arrays.toString(args) + "): called");
        console.info("Qpid Distributed Test Client.");

        // Override the default broker url to be localhost:5672.
        testContextProperties.setProperty(MessagingTestConfigProperties.BROKER_PROPNAME, "tcp://localhost:5672");

        // Use the command line parser to evaluate the command line with standard handling behaviour (print errors
        // and usage then exist if there are errors).
        // Any options and trailing name=value pairs are also injected into the test context properties object,
        // to override any defaults that may have been set up.
        ParsedProperties options =
            new ParsedProperties(org.apache.qpid.junit.extensions.util.CommandLineParser.processCommandLine(args,
                    new org.apache.qpid.junit.extensions.util.CommandLineParser(
                        new String[][]
                        {
                            { "b", "The broker URL.", "broker", "false" },
                            { "h", "The virtual host to use.", "virtual host", "false" },
                            { "o", "The name of the directory to output test timings to.", "dir", "false" },
                            { "n", "The name of the test client.", "name", "false" },
                            { "j", "Join this test client to running test.", "false" }
                        }), testContextProperties));

        // Extract the command line options.
        String brokerUrl = options.getProperty("b");
        String virtualHost = options.getProperty("h");
        String clientName = options.getProperty("n");
        clientName = (clientName == null) ? CLIENT_NAME : clientName;
        boolean join = options.getPropertyAsBoolean("j");

        // To distinguish logging output set up an NDC on the client name.
        NDC.push(clientName);

        // Create a test client and start it running.
        TestClient client = new TestClient(brokerUrl, virtualHost, clientName, join);

        // Use a class path scanner to find all the interop test case implementations.
        // Hard code the test classes till the classpath scanner is fixed.
        Collection<Class<? extends TestClientControlledTest>> testCaseClasses =
            new ArrayList<Class<? extends TestClientControlledTest>>();
        // ClasspathScanner.getMatches(TestClientControlledTest.class, "^TestCase.*", true);
        testCaseClasses.addAll(loadTestCases("org.apache.qpid.interop.clienttestcases.TestCase1DummyRun",
                "org.apache.qpid.interop.clienttestcases.TestCase2BasicP2P",
                "org.apache.qpid.interop.clienttestcases.TestCase3BasicPubSub",
                "org.apache.qpid.interop.clienttestcases.TestCase4P2PMessageSize",
                "org.apache.qpid.interop.clienttestcases.TestCase5PubSubMessageSize",
                "org.apache.qpid.test.framework.distributedcircuit.TestClientCircuitEnd"));

        try
        {
            client.start(testCaseClasses);
        }
        catch (Exception e)
        {
            log.error("The test client was unable to start.", e);
            console.info(e.getMessage());
            System.exit(1);
        }
    }

    /**
     * Parses a list of class names, and loads them if they are available on the class path.
     *
     * @param classNames The names of the classes to load.
     *
     * @return A list of the loaded test case classes.
     */
    public static List<Class<? extends TestClientControlledTest>> loadTestCases(String... classNames)
    {
        List<Class<? extends TestClientControlledTest>> testCases =
            new LinkedList<Class<? extends TestClientControlledTest>>();

        for (String className : classNames)
        {
            try
            {
                Class<?> cls = ReflectionUtils.forName(className);
                testCases.add((Class<? extends TestClientControlledTest>) cls);
            }
            catch (ReflectionUtilsException e)
            {
                // Ignore, class could not be found, so test not available.
                console.warn("Requested class " + className + " cannot be found, ignoring it.");
            }
            catch (ClassCastException e)
            {
                // Ignore, class was not of correct type to be a test case.
                console.warn("Requested class " + className + " is not an instance of TestClientControlledTest.");
            }
        }

        return testCases;
    }

    /**
     * Starts the interop test client running. This causes it to start listening for incoming test invites.
     *
     * @param testCaseClasses The classes of the available test cases. The test case names from these are used to
     *                        matchin incoming test invites against.
     *
     * @throws JMSException Any underlying JMSExceptions are allowed to fall through.
     */
    protected void start(Collection<Class<? extends TestClientControlledTest>> testCaseClasses) throws JMSException
    {
        log.debug("protected void start(Collection<Class<? extends TestClientControlledTest>> testCaseClasses = "
            + testCaseClasses + "): called");

        // Create all the test case implementations and index them by the test names.
        for (Class<? extends TestClientControlledTest> nextClass : testCaseClasses)
        {
            try
            {
                TestClientControlledTest testCase = nextClass.newInstance();
                testCases.put(testCase.getName(), testCase);
            }
            catch (InstantiationException e)
            {
                log.warn("Could not instantiate test case class: " + nextClass.getName(), e);
                // Ignored.
            }
            catch (IllegalAccessException e)
            {
                log.warn("Could not instantiate test case class due to illegal access: " + nextClass.getName(), e);
                // Ignored.
            }
        }

        // Open a connection to communicate with the coordinator on.
        connection = TestUtils.createConnection(testContextProperties);
        session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        // Set this up to listen for control messages.
        Topic privateControlTopic = session.createTopic("iop.control." + clientName);
        MessageConsumer consumer = session.createConsumer(privateControlTopic);
        consumer.setMessageListener(this);

        Topic controlTopic = session.createTopic("iop.control");
        MessageConsumer consumer2 = session.createConsumer(controlTopic);
        consumer2.setMessageListener(this);

        // Create a producer to send replies with.
        producer = session.createProducer(null);

        // If the join flag was set, then broadcast a join message to notify the coordinator that a new test client
        // is available to join the current test case, if it supports it. This message may be ignored, or it may result
        // in this test client receiving a test invite.
        if (join)
        {
            Message joinMessage = session.createMessage();

            joinMessage.setStringProperty("CONTROL_TYPE", "JOIN");
            joinMessage.setStringProperty("CLIENT_NAME", clientName);
            joinMessage.setStringProperty("CLIENT_PRIVATE_CONTROL_KEY", "iop.control." + clientName);
            producer.send(controlTopic, joinMessage);
        }

        // Start listening for incoming control messages.
        connection.start();
    }

    /**
     * Handles all incoming control messages.
     *
     * @param message The incoming message.
     */
    public void onMessage(Message message)
    {
        NDC.push(clientName);
        log.debug("public void onMessage(Message message = " + message + "): called");

        try
        {
            String controlType = message.getStringProperty("CONTROL_TYPE");
            String testName = message.getStringProperty("TEST_NAME");

            log.debug("Received control of type '" + controlType + "' for the test '" + testName + "'");

            // Check if the message is a test invite.
            if ("INVITE".equals(controlType))
            {
                // Flag used to indicate that an enlist should be sent. Only enlist to compulsory invites or invites
                // for which test cases exist.
                boolean enlist = false;

                if (testName != null)
                {
                    log.debug("Got an invite to test: " + testName);

                    // Check if the requested test case is available.
                    TestClientControlledTest testCase = testCases.get(testName);

                    if (testCase != null)
                    {
                        log.debug("Found implementing class for test '" + testName + "', enlisting for it.");

                        // Check if the test case will accept the invitation.
                        enlist = testCase.acceptInvite(message);

                        log.debug("The test case "
                            + (enlist ? " accepted the invite, enlisting for it."
                                      : " did not accept the invite, not enlisting."));

                        // Make the requested test case the current test case.
                        currentTestCase = testCase;
                    }
                    else
                    {
                        log.debug("Received an invite to the test '" + testName + "' but this test is not known.");
                    }
                }
                else
                {
                    log.debug("Got a compulsory invite, enlisting for it.");

                    enlist = true;
                }

                if (enlist)
                {
                    // Reply with the client name in an Enlist message.
                    Message enlistMessage = session.createMessage();
                    enlistMessage.setStringProperty("CONTROL_TYPE", "ENLIST");
                    enlistMessage.setStringProperty("CLIENT_NAME", clientName);
                    enlistMessage.setStringProperty("CLIENT_PRIVATE_CONTROL_KEY", "iop.control." + clientName);
                    enlistMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                    log.debug("Sending enlist message '" + enlistMessage + "' to " + message.getJMSReplyTo());

                    producer.send(message.getJMSReplyTo(), enlistMessage);
                }
                else
                {
                    // Reply with the client name in an Decline message.
                    Message enlistMessage = session.createMessage();
                    enlistMessage.setStringProperty("CONTROL_TYPE", "DECLINE");
                    enlistMessage.setStringProperty("CLIENT_NAME", clientName);
                    enlistMessage.setStringProperty("CLIENT_PRIVATE_CONTROL_KEY", "iop.control." + clientName);
                    enlistMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                    log.debug("Sending decline message '" + enlistMessage + "' to " + message.getJMSReplyTo());

                    producer.send(message.getJMSReplyTo(), enlistMessage);
                }
            }
            else if ("ASSIGN_ROLE".equals(controlType))
            {
                // Assign the role to the current test case.
                String roleName = message.getStringProperty("ROLE");

                log.debug("Got a role assignment to role: " + roleName);

                TestClientControlledTest.Roles role = Enum.valueOf(TestClientControlledTest.Roles.class, roleName);

                currentTestCase.assignRole(role, message);

                // Reply by accepting the role in an Accept Role message.
                Message acceptRoleMessage = session.createMessage();
                acceptRoleMessage.setStringProperty("CLIENT_NAME", clientName);
                acceptRoleMessage.setStringProperty("CONTROL_TYPE", "ACCEPT_ROLE");
                acceptRoleMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                log.debug("Sending accept role message '" + acceptRoleMessage + "' to " + message.getJMSReplyTo());

                producer.send(message.getJMSReplyTo(), acceptRoleMessage);
            }
            else if ("START".equals(controlType) || "STATUS_REQUEST".equals(controlType))
            {
                if ("START".equals(controlType))
                {
                    log.debug("Got a start notification.");

                    // Extract the number of test messages to send from the start notification.
                    int numMessages;

                    try
                    {
                        numMessages = message.getIntProperty("MESSAGE_COUNT");
                    }
                    catch (NumberFormatException e)
                    {
                        // If the number of messages is not specified, use the default of one.
                        numMessages = 1;
                    }

                    // Start the current test case.
                    currentTestCase.start(numMessages);
                }
                else
                {
                    log.debug("Got a status request.");
                }

                // Generate the report from the test case and reply with it as a Report message.
                Message reportMessage = currentTestCase.getReport(session);
                reportMessage.setStringProperty("CLIENT_NAME", clientName);
                reportMessage.setStringProperty("CONTROL_TYPE", "REPORT");
                reportMessage.setJMSCorrelationID(message.getJMSCorrelationID());

                log.debug("Sending report message '" + reportMessage + "' to " + message.getJMSReplyTo());

                producer.send(message.getJMSReplyTo(), reportMessage);
            }
            else if ("TERMINATE".equals(controlType))
            {
                console.info("Received termination instruction from coordinator.");

                // Is a cleaner shutdown needed?
                connection.close();
                System.exit(0);
            }
            else if ("CLOCK_SYNCH".equals(controlType))
            {
                log.debug("Received clock synch command.");
                String address = message.getStringProperty("ADDRESS");

                log.debug("address = " + address);

                // Re-create (if necessary) and start the clock synch thread to synch the clock every ten seconds.
                if (clockSynchThread != null)
                {
                    clockSynchThread.terminate();
                }

                SleepThrottle throttle = new SleepThrottle();
                throttle.setRate(0.1f);

                clockSynchThread = new ClockSynchThread(new UDPClockSynchronizer(address), throttle);
                clockSynchThread.start();
            }
            else
            {
                // Log a warning about this but otherwise ignore it.
                log.warn("Got an unknown control message, controlType = " + controlType + ", message = " + message);
            }
        }
        catch (JMSException e)
        {
            // Log a warning about this, but otherwise ignore it.
            log.warn("Got JMSException whilst handling message: " + message, e);
        }
        // Log any runtimes that fall through this message handler. These are fatal errors for the test client.
        catch (RuntimeException e)
        {
            log.error("The test client message handler got an unhandled exception: ", e);
            console.info("The message handler got an unhandled exception, terminating the test client.");
            System.exit(1);
        }
        finally
        {
            NDC.pop();
        }
    }
}
