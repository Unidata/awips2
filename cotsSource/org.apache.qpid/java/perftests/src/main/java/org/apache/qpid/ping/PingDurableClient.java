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
package org.apache.qpid.ping;

import org.apache.log4j.Logger;

import org.apache.qpid.requestreply.PingPongProducer;
import org.apache.qpid.util.CommandLineParser;

import org.apache.qpid.junit.extensions.util.MathUtils;
import org.apache.qpid.junit.extensions.util.ParsedProperties;

import javax.jms.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * PingDurableClient is a variation of the {@link PingPongProducer} ping tool. Instead of sending its pings and
 * receiving replies to them at the same time, this tool sends pings until it is signalled by some 'event' to stop
 * sending. It then waits for another signal before it re-opens a fresh connection and attempts to receive all of the
 * pings that it has succesfully sent. It is intended to be an interactive test that lets a user experiment with
 * failure conditions when using durable messaging.
 *
 * <p/>The events that can stop it from sending are input from the user on the console, failure of its connection to
 * the broker, completion of sending a specified number of messages, or expiry of a specified duration. In all cases
 * it will do its best to clean up and close the connection before opening a fresh connection to receive the pings
 * with.
 *
 * <p/>The event to re-connect and attempt to recieve the pings is input from the user on the console.
 *
 * <p/>This ping client inherits the configuration properties of its parent class ({@link PingPongProducer}) and
 * additionally accepts the following parameters:
 *
 * <p/><table><caption>Parameters</caption>
 * <tr><th> Parameter           <th> Default  <th> Comments
 * <tr><td> numMessages         <td> 100      <td> The total number of messages to send.
 * <tr><td> numMessagesToAction <td> -1       <td> The number of messages to send before taking a custom 'action'.
 * <tr><td> duration            <td> 30S      <td> The length of time to ping for. (Format dDhHmMsS, for d days, h hours,
 *                                                 m minutes and s seconds).
 * </table>
 *
 * <p/>This ping client also overrides some of the defaults of its parent class, to provide a reasonable set up
 * when no parameters are specified.
 *
 * <p/><table><caption>Parameters</caption>
 * <tr><th> Parameter        <th> Default  <th> Comments
 * <tr><td> uniqueDests      <td> false    <td> Prevents destination names being timestamped.
 * <tr><td> transacted       <td> true     <td> Only makes sense to test with transactions.
 * <tr><td> persistent       <td> true     <td> Only makes sense to test persistent.
 * <tr><td> durableDests     <td> true     <td> Should use durable queues with persistent messages.
 * <tr><td> commitBatchSize  <td> 10
 * <tr><td> rate             <td> 20       <td> Total default test time is 5 seconds.
 * </table>
 *
 * <p/>When a number of messages or duration is specified, this ping client will ping until the first of those limits
 * is reached. Reaching the limit will be interpreted as the first signal to stop sending, and the ping client will
 * wait for the second signal before receiving its pings.
 *
 * <p/>This class provides a mechanism for extensions to add arbitrary actions, after a particular number of messages
 * have been sent. When the number of messages equal the value set in the 'numMessagesToAction' property is method,
 * the {@link #takeAction} method is called. By default this does nothing, but extensions of this class can provide
 * custom behaviour with alternative implementations of this method (for example taking a backup).
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Send and receive pings.
 * <tr><td> Accept user input to signal stop sending.
 * <tr><td> Accept user input to signal start receiving.
 * <tr><td> Provide feedback on pings sent versus pings received.
 * <tr><td> Provide extension point for arbitrary action on a particular message count.
 * </table>
 */
public class PingDurableClient extends PingPongProducer implements ExceptionListener
{
    private static final Logger log = Logger.getLogger(PingDurableClient.class);

    public static final String NUM_MESSAGES_PROPNAME = "numMessages";
    public static final String NUM_MESSAGES_DEFAULT = "100";
    public static final String DURATION_PROPNAME = "duration";
    public static final String DURATION_DEFAULT = "30S";
    public static final String NUM_MESSAGES_TO_ACTION_PROPNAME = "numMessagesToAction";
    public static final String NUM_MESSAGES_TO_ACTION_DEFAULT = "-1";

    /** The maximum length of time to wait whilst receiving pings before assuming that no more are coming. */
    private static final long TIME_OUT = 3000;

    static
    {
        defaults.setProperty(NUM_MESSAGES_PROPNAME, NUM_MESSAGES_DEFAULT);
        defaults.setProperty(DURATION_PROPNAME, DURATION_DEFAULT);
        defaults.setProperty(UNIQUE_DESTS_PROPNAME, "false");
        defaults.setProperty(TRANSACTED_PROPNAME, "true");
        defaults.setProperty(PERSISTENT_MODE_PROPNAME, "true");
        defaults.setProperty(TX_BATCH_SIZE_PROPNAME, "10");
        defaults.setProperty(RATE_PROPNAME, "20");
        defaults.setProperty(DURABLE_DESTS_PROPNAME, "true");
        defaults.setProperty(NUM_MESSAGES_TO_ACTION_PROPNAME, NUM_MESSAGES_TO_ACTION_DEFAULT);
    }

    /** Specifies the number of pings to send, if larger than 0. 0 means send until told to stop. */
    private int numMessages;

    /** Holds the number of messages to send before taking triggering the action. */
    private int numMessagesToAction;

    /** Sepcifies how long to ping for, if larger than 0. 0 means send until told to stop. */
    private long duration;

    /** Used to indciate that this application should terminate. Set by the shutdown hook. */
    private boolean terminate = false;

    /**
     * @throws Exception Any exceptions are allowed to fall through.
     */
    public PingDurableClient(Properties overrides) throws Exception
    {
        super(overrides);
        log.debug("public PingDurableClient(Properties overrides = " + overrides + "): called");

        // Extract the additional configuration parameters.
        ParsedProperties properties = new ParsedProperties(defaults);
        properties.putAll(overrides);

        numMessages = properties.getPropertyAsInteger(NUM_MESSAGES_PROPNAME);
        String durationSpec = properties.getProperty(DURATION_PROPNAME);
        numMessagesToAction = properties.getPropertyAsInteger(NUM_MESSAGES_TO_ACTION_PROPNAME);

        if (durationSpec != null)
        {
            duration = MathUtils.parseDuration(durationSpec) * 1000000;
        }
    }

    /**
     * Starts the ping/wait/receive process.
     *
     * @param args The command line arguments.
     */
    public static void main(String[] args)
    {
        try
        {
            // Create a ping producer overriding its defaults with all options passed on the command line.
            Properties options =
                CommandLineParser.processCommandLine(args, new CommandLineParser(new String[][] {}), System.getProperties());
            PingDurableClient pingProducer = new PingDurableClient(options);

            // Create a shutdown hook to terminate the ping-pong producer.
            Runtime.getRuntime().addShutdownHook(pingProducer.getShutdownHook());

            // Ensure that the ping pong producer is registered to listen for exceptions on the connection too.
            // pingProducer.getConnection().setExceptionListener(pingProducer);

            // Run the test procedure.
            int sent = pingProducer.send();
            pingProducer.closeConnection();
            pingProducer.waitForUser("Press return to begin receiving the pings.");
            pingProducer.receive(sent);

            System.exit(0);
        }
        catch (Exception e)
        {
            System.err.println(e.getMessage());
            log.error("Top level handler caught execption.", e);
            System.exit(1);
        }
    }

    /**
     * Performs the main test procedure implemented by this ping client. See the class level comment for details.
     */
    protected int send() throws Exception
    {
        log.debug("public void sendWaitReceive(): called");

        log.debug("duration = " + duration);
        log.debug("numMessages = " + numMessages);

        if (duration > 0)
        {
            System.out.println("Sending for up to " + (duration / 1000000000f) + " seconds.");
        }

        if (_rate > 0)
        {
            System.out.println("Sending at " + _rate + " messages per second.");
        }

        if (numMessages > 0)
        {
            System.out.println("Sending up to " + numMessages + " messages.");
        }

        // Establish the connection and the message producer.
        establishConnection(true, false);
        _connection.start();

        Message message = getTestMessage(getReplyDestinations().get(0), _messageSize, _persistent);

        // Send pings until a terminating condition is received.
        boolean endCondition = false;
        int messagesSent = 0;
        int messagesCommitted = 0;
        int messagesNotCommitted = 0;
        long start = System.nanoTime();

        // Clear console in.
        clearConsole();

        while (!endCondition)
        {
            boolean committed = false;

            try
            {
                committed = sendMessage(messagesSent, message) && _transacted;

                messagesSent++;
                messagesNotCommitted++;

                // Keep count of the number of messsages currently committed and pending commit.
                if (committed)
                {
                    log.debug("Adding " + messagesNotCommitted + " messages to the committed count.");
                    messagesCommitted += messagesNotCommitted;
                    messagesNotCommitted = 0;

                    System.out.println("Commited: " + messagesCommitted);
                }
            }
            catch (JMSException e)
            {
                log.debug("Got JMSException whilst sending.");
                _publish = false;
            }

            // Perform the arbitrary action if the number of messages sent has reached the right number.
            if (messagesSent == numMessagesToAction)
            {
                System.out.println("At action point, Messages sent = " + messagesSent + ", Messages Committed = "
                    + messagesCommitted + ", Messages not Committed = " + messagesNotCommitted);
                takeAction();
            }

            // Determine if the end condition has been met, based on the number of messages, time passed, errors on
            // the connection or user input.
            long now = System.nanoTime();

            if ((duration != 0) && ((now - start) > duration))
            {
                System.out.println("Send halted because duration expired.");
                endCondition = true;
            }
            else if ((numMessages != 0) && (messagesSent >= numMessages))
            {
                System.out.println("Send halted because # messages completed.");
                endCondition = true;
            }
            else if (System.in.available() > 0)
            {
                System.out.println("Send halted by user input.");
                endCondition = true;

                clearConsole();
            }
            else if (!_publish)
            {
                System.out.println("Send halted by error on the connection.");
                endCondition = true;
            }
        }

        log.debug("messagesSent = " + messagesSent);
        log.debug("messagesCommitted = " + messagesCommitted);
        log.debug("messagesNotCommitted = " + messagesNotCommitted);

        System.out.println("Messages sent: " + messagesSent + ", Messages Committed = " + messagesCommitted
            + ", Messages not Committed = " + messagesNotCommitted);

        return messagesSent;
    }

    protected void closeConnection()
    {
        // Clean up the connection.
        try
        {
            close();
        }
        catch (JMSException e)
        {
            log.debug("There was an error whilst closing the connection: " + e, e);
            System.out.println("There was an error whilst closing the connection.");

            // Ignore as did best could manage to clean up.
        }
    }

    protected void receive(int messagesSent) throws Exception
    {
        // Re-establish the connection and the message consumer.
        _queueJVMSequenceID = new AtomicInteger();
        _queueSharedID = new AtomicInteger();

        establishConnection(false, true);
        _consumer[0].setMessageListener(null);
        _consumerConnection[0].start();

        // Try to receive all of the pings that were successfully sent.
        int messagesReceived = 0;
        boolean endCondition = false;

        while (!endCondition)
        {
            // Message received = _consumer.receiveNoWait();
            Message received = _consumer[0].receive(TIME_OUT);
            log.debug("received = " + received);

            if (received != null)
            {
                messagesReceived++;
            }

            // Determine if the end condition has been met, based on the number of messages and time passed since last
            // receiving a message.
            if (received == null)
            {
                System.out.println("Timed out.");
                endCondition = true;
            }
            else if (messagesReceived >= messagesSent)
            {
                System.out.println("Got all messages.");
                endCondition = true;
            }
        }

        // Ensure messages received are committed.
        if (_consTransacted)
        {
            try
            {
                _consumerSession[0].commit();
                System.out.println("Committed for all messages received.");
            }
            catch (JMSException e)
            {
                log.debug("Error during commit: " + e, e);
                System.out.println("Error during commit.");
                try
                {
                    _consumerSession[0].rollback();
                    System.out.println("Rolled back on all messages received.");
                }
                catch (JMSException e2)
                {
                    log.debug("Error during rollback: " + e, e);
                    System.out.println("Error on roll back of all messages received.");
                }

            }
        }

        log.debug("messagesReceived = " + messagesReceived);

        System.out.println("Messages received: " + messagesReceived);

        // Clean up the connection.
        close();
    }

    /**
     * Clears any pending input from the console.
     */
    private void clearConsole()
    {
        try
        {
            BufferedReader bis = new BufferedReader(new InputStreamReader(System.in));

            // System.in.skip(System.in.available());
            while (bis.ready())
            {
                bis.readLine();
            }
        }
        catch (IOException e)
        { }
    }

    /**
     * Returns the ping destinations themselves as the reply destinations for this pinger to listen to. This has the
     * effect of making this pinger listen to its own pings.
     *
     * @return The ping destinations.
     */
    public List<Destination> getReplyDestinations()
    {
        return _pingDestinations;
    }

    /**
     * Gets a shutdown hook that will cleanly shut this down when it is running the ping loop. This can be registered with
     * the runtime system as a shutdown hook. This shutdown hook sets an additional terminate flag, compared with the
     * shutdown hook in {@link PingPongProducer}, because the publish flag is used to indicate that sending or receiving
     * message should stop, not that the application should termiante.
     *
     * @return A shutdown hook for the ping loop.
     */
    public Thread getShutdownHook()
    {
        return new Thread(new Runnable()
                {
                    public void run()
                    {
                        stop();
                        terminate = true;
                    }
                });
    }

    /**
     * Performs an aribtrary action once the 'numMesagesToAction' count is reached on sending messages. This default
     * implementation does nothing.
     */
    public void takeAction()
    { }
}
