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
package org.apache.qpid.sustained;

import org.apache.log4j.Logger;

import org.apache.qpid.client.AMQNoConsumersException;
import org.apache.qpid.client.AMQNoRouteException;
import org.apache.qpid.test.framework.distributedtesting.TestClient;
import org.apache.qpid.interop.clienttestcases.TestCase3BasicPubSub;
import org.apache.qpid.test.framework.TestUtils;

import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

/**
 * Implements test case 3, basic pub/sub. Sends/received a specified number of messages to a specified route on the
 * default topic exchange, using the specified number of receivers connections. Produces reports on the actual number of
 * messages sent/received.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Supply the name of the test case that this implements.
 * <tr><td> Accept/Reject invites based on test parameters.
 * <tr><td> Adapt to assigned roles.
 * <tr><td> Send required number of test messages using pub/sub. <tr><td> Generate test reports.
 * </table>
 */
public class SustainedClientTestCase extends TestCase3BasicPubSub implements ExceptionListener, MessageListener
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(SustainedClientTestCase.class);

    /** Used to log to the console. */
    private static final Logger console = Logger.getLogger("SustainedTest");

    /** The role to be played by the test. */
    private Roles role;

    /** The number of receivers connection to use. */
    private int numReceivers;

    /** The routing key to send them to on the default direct exchange. */
    private Destination sendDestination;

    /** The routing key to send updates to on the default direct exchange. */
    private Destination sendUpdateDestination;

    /** The connections to send/receive the test messages on. */
    private Connection[] connection;

    /** The sessions to send/receive the test messages on. */
    private Session[] session;

    /** The producer to send the test messages with. */
    MessageProducer producer;

    /** Adapter that adjusts the send rate based on the updates from clients. */
    SustainedRateAdapter _rateAdapter;

    /**  */
    int _batchSize;

    private static final long TEN_MILLI_SEC = 10000000;
    private static final int DEBUG_LOG_UPATE_INTERVAL = 10;
    private static final int LOG_UPATE_INTERVAL = 10;
    private static final boolean SLEEP_PER_MESSAGE = Boolean.getBoolean("sleepPerMessage");

    /**
     * Should provide the name of the test case that this class implements. The exact names are defined in the interop
     * testing spec.
     *
     * @return The name of the test case that this implements.
     */
    public String getName()
    {
        log.debug("public String getName(): called");

        return "Perf_SustainedPubSub";
    }

    /**
     * Assigns the role to be played by this test case. The test parameters are fully specified in the assignment
     * message. When this method return the test case will be ready to execute.
     *
     * @param role              The role to be played; sender or receivers.
     * @param assignRoleMessage The role assingment message, contains the full test parameters.
     *
     * @throws JMSException Any JMSException resulting from reading the message are allowed to fall through.
     */
    public void assignRole(Roles role, Message assignRoleMessage) throws JMSException
    {
        log.debug("public void assignRole(Roles role = " + role + ", Message assignRoleMessage = " + assignRoleMessage
            + "): called");

        // Take note of the role to be played.
        this.role = role;

        // Extract and retain the test parameters.
        numReceivers = assignRoleMessage.getIntProperty("SUSTAINED_NUM_RECEIVERS");
        _batchSize = assignRoleMessage.getIntProperty("SUSTAINED_UPDATE_INTERVAL");
        String sendKey = assignRoleMessage.getStringProperty("SUSTAINED_KEY");
        String sendUpdateKey = assignRoleMessage.getStringProperty("SUSTAINED_UPDATE_KEY");
        int ackMode = assignRoleMessage.getIntProperty("ACKNOWLEDGE_MODE");
        String clientName = assignRoleMessage.getStringProperty("CLIENT_NAME");

        if (log.isDebugEnabled())
        {
            log.debug("numReceivers = " + numReceivers);
            log.debug("_batchSize = " + _batchSize);
            log.debug("ackMode = " + ackMode);
            log.debug("sendKey = " + sendKey);
            log.debug("sendUpdateKey = " + sendUpdateKey);
            log.debug("role = " + role);
        }

        switch (role)
        {
        // Check if the sender role is being assigned, and set up a single message producer if so.
        case SENDER:
            console.info("Creating Sender");
            // Create a new connection to pass the test messages on.
            connection = new Connection[1];
            session = new Session[1];

            connection[0] = TestUtils.createConnection(TestClient.testContextProperties);
            session[0] = connection[0].createSession(false, ackMode);

            // Extract and retain the test parameters.
            sendDestination = session[0].createTopic(sendKey);

            connection[0].setExceptionListener(this);

            producer = session[0].createProducer(sendDestination);

            sendUpdateDestination = session[0].createTopic(sendUpdateKey);
            MessageConsumer updateConsumer = session[0].createConsumer(sendUpdateDestination);

            _rateAdapter = new SustainedRateAdapter(this);
            updateConsumer.setMessageListener(_rateAdapter);

            break;

        // Otherwise the receivers role is being assigned, so set this up to listen for messages on the required number
        // of receivers connections.
        case RECEIVER:
            console.info("Creating Receiver");
            // Create the required number of receivers connections.
            connection = new Connection[numReceivers];
            session = new Session[numReceivers];

            for (int i = 0; i < numReceivers; i++)
            {
                connection[i] = TestUtils.createConnection(TestClient.testContextProperties);
                session[i] = connection[i].createSession(false, ackMode);

                sendDestination = session[i].createTopic(sendKey);

                sendUpdateDestination = session[i].createTopic(sendUpdateKey);

                MessageConsumer consumer = session[i].createConsumer(sendDestination);

                consumer.setMessageListener(new SustainedListener(clientName + "-" + i, _batchSize, session[i],
                        sendUpdateDestination));
            }

            break;
        }

        // Start all the connection dispatcher threads running.
        for (int i = 0; i < connection.length; i++)
        {
            connection[i].start();
        }
    }

    /** Performs the test case actions.
     * @param numMessages*/
    public void start(int numMessages) throws JMSException
    {
        log.debug("public void start(): called");

        // Check that the sender role is being performed.
        switch (role)
        {
        // Check if the sender role is being assigned, and set up a single message producer if so.
        case SENDER:
            _rateAdapter.run();
            break;
        case RECEIVER:

        }

        // return from here when you have finished the test.. this will signal the controller and
    }

    public void terminate() throws JMSException, InterruptedException
    {
        if (_rateAdapter != null)
        {
            _rateAdapter.stop();
        }
    }

    /**
     * Gets a report on the actions performed by the test case in its assigned role.
     *
     * @param session The controlSession to create the report message in.
     *
     * @return The report message.
     *
     * @throws JMSException Any JMSExceptions resulting from creating the report are allowed to fall through.
     */
    public Message getReport(Session session) throws JMSException
    {
        log.debug("public Message getReport(Session controlSession): called");

        // Close the test connections.
        for (int i = 0; i < connection.length; i++)
        {
            connection[i].close();
        }

        Message report = session.createMessage();
        report.setStringProperty("CONTROL_TYPE", "REPORT");

        return report;
    }

    public void onException(JMSException jmsException)
    {
        Exception linked = jmsException.getLinkedException();

        if (linked != null)
        {
            if (log.isDebugEnabled())
            {
                log.debug("Linked Exception:" + linked);
            }

            if ((linked instanceof AMQNoRouteException) || (linked instanceof AMQNoConsumersException))
            {
                if (log.isDebugEnabled())
                {
                    if (linked instanceof AMQNoConsumersException)
                    {
                        log.warn("No clients currently available for message:"
                            + ((AMQNoConsumersException) linked).getUndeliveredMessage());
                    }
                    else
                    {
                        log.warn("No route for message");
                    }
                }

                // Tell the rate adapter that there are no clients ready yet
                _rateAdapter.NO_CLIENTS = true;
            }
        }
        else
        {
            log.warn("Exception:" + linked);
        }
    }

    /**
     * Inner class that listens for messages and sends a report for the time taken between receiving the 'start' and
     * 'end' messages.
     */
    class SustainedListener implements MessageListener
    {
        /** Number of messages received */
        private long _received = 0;
        /** The number of messages in the batch */
        private int _batchSize = 0;
        /** Record of the when the 'start' messagse was sen */
        private Long _startTime;
        /** Message producer to use to send reports */
        MessageProducer _updater;
        /** Session to create the report message on */
        Session _session;
        /** Record of the client ID used for this SustainedListnener */
        String _client;

        /**
         * Main Constructor
         *
         * @param clientname      The _client id used to identify this connection.
         * @param batchSize       The number of messages that are to be sent per batch. Note: This is not used to
         *                        control the interval between sending reports.
         * @param session         The controlSession used for communication.
         * @param sendDestination The destination that update reports should be sent to.
         *
         * @throws JMSException My occur if creatingthe Producer fails
         */
        public SustainedListener(String clientname, int batchSize, Session session, Destination sendDestination)
            throws JMSException
        {
            _batchSize = batchSize;
            _client = clientname;
            _session = session;
            _updater = session.createProducer(sendDestination);
        }

        public void onMessage(Message message)
        {
            if (log.isDebugEnabled())
            {
                log.debug("Message " + _received + "received in listener");
            }

            if (message instanceof TextMessage)
            {
                try
                {
                    _received++;
                    if (((TextMessage) message).getText().equals("start"))
                    {
                        log.debug("Starting Batch");
                        _startTime = System.nanoTime();
                    }
                    else if (((TextMessage) message).getText().equals("end"))
                    {
                        if (_startTime != null)
                        {
                            long currentTime = System.nanoTime();
                            sendStatus(currentTime - _startTime, _received, message.getIntProperty("BATCH"));
                            log.debug("End Batch");
                        }
                    }
                }
                catch (JMSException e)
                {
                    // ignore error
                }
            }

        }

        /**
         * sendStatus creates and sends the report back to the publisher
         *
         * @param time     taken for the the last batch
         * @param received Total number of messages received.
         * @param batchNumber the batch number
         * @throws JMSException if an error occurs during the send
         */
        private void sendStatus(long time, long received, int batchNumber) throws JMSException
        {
            Message updateMessage = _session.createTextMessage("update");
            updateMessage.setStringProperty("CLIENT_ID", ":" + _client);
            updateMessage.setStringProperty("CONTROL_TYPE", "UPDATE");
            updateMessage.setLongProperty("RECEIVED", received);
            updateMessage.setIntProperty("BATCH", batchNumber);
            updateMessage.setLongProperty("DURATION", time);

            if (log.isInfoEnabled())
            {
                log.info("**** SENDING [" + batchNumber + "]**** " + "CLIENT_ID:" + _client + " RECEIVED:" + received
                    + " BATCH:" + batchNumber + " DURATION:" + time);
            }

            // Output on the main console.info the details of this batch
            if ((batchNumber % 10) == 0)
            {
                console.info("Sending Report [" + batchNumber + "] " + "CLIENT_ID:" + _client + " RECEIVED:" + received
                    + " BATCH:" + batchNumber + " DURATION:" + time);
            }

            _updater.send(updateMessage);
        }
    }

    /**
     * This class is used here to adjust the _delay value which in turn is used to control the number of messages/second
     * that are sent through the test system.
     *
     * By keeping a record of the messages recevied and the average time taken to process the batch size can be
     * calculated and so the delay can be adjusted to maintain that rate.
     *
     * Given that delays of < 10ms can be rounded up the delay is only used between messages if the _delay > 10ms * no
     * messages in the batch. Otherwise the delay is used at the end of the batch.
     */
    class SustainedRateAdapter implements MessageListener, Runnable
    {
        private SustainedClientTestCase _client;
        private long _batchVariance = Integer.getInteger("batchVariance", 3); // no. batches to allow drifting
        private long _timeVariance = TEN_MILLI_SEC * 5; // no. nanos between send and report delay (10ms)
        private volatile long _delay; // in nanos
        private long _sent;
        private Map<String, Long> _slowClients = new HashMap<String, Long>();
        private static final long PAUSE_SLEEP = TEN_MILLI_SEC / 1000; // 10 ms
        private static final long NO_CLIENT_SLEEP = 1000; // 1s
        private volatile boolean NO_CLIENTS = true;
        private int _delayShifting;
        private final int REPORTS_WITHOUT_CHANGE = Integer.getInteger("stableReportCount", 5);
        private boolean _warmedup = false;
        private static final long EXPECTED_TIME_PER_BATCH = 100000L;
        private int _warmUpBatches = Integer.getInteger("warmUpBatches", 10);

        SustainedRateAdapter(SustainedClientTestCase client)
        {
            _client = client;
        }

        public void onMessage(Message message)
        {
            if (log.isDebugEnabled())
            {
                log.debug("SustainedRateAdapter onMessage(Message message = " + message + "): called");
            }

            try
            {
                String controlType = message.getStringProperty("CONTROL_TYPE");

                // Check if the message is a test invite.
                if ("UPDATE".equals(controlType))
                {
                    NO_CLIENTS = false;
                    long duration = message.getLongProperty("DURATION");
                    long totalReceived = message.getLongProperty("RECEIVED");
                    String client = message.getStringProperty("CLIENT_ID");
                    int batchNumber = message.getIntProperty("BATCH");

                    if (log.isInfoEnabled() && ((batchNumber % DEBUG_LOG_UPATE_INTERVAL) == 0))
                    {
                        log.info("Update Report: CLIENT_ID:" + client + " RECEIVED:" + totalReceived + " Recevied BATCH:"
                            + batchNumber + " DURATION:" + duration);
                    }

                    recordSlow(client, totalReceived, batchNumber);

                    adjustDelay(client, batchNumber, duration);

                    // Warm up completes when:
                    // we haven't warmed up
                    // and the number of batches sent to each client is at least half of the required warmup batches
                    if (!_warmedup && (batchNumber >= _warmUpBatches))
                    {
                        _warmedup = true;
                        _warmup.countDown();

                    }
                }
            }
            catch (JMSException e)
            {
                //
            }
        }

        CountDownLatch _warmup = new CountDownLatch(1);

        int _numBatches = 10000;

        // long[] _timings = new long[_numBatches];
        private boolean _running = true;

        public void run()
        {
            console.info("Warming up");

            doBatch(_warmUpBatches);

            try
            {
                // wait for warmup to complete.
                _warmup.await();

                // set delay to the average length of the batches
                _delay = _totalDuration / _warmUpBatches / delays.size();

                console.info("Warmup complete delay set : " + _delay + " based on _totalDuration: " + _totalDuration
                    + " over no. batches: " + _warmUpBatches + " with client count: " + delays.size());

                _totalDuration = 0L;
                _totalReceived = 0L;
                _sent = 0L;
            }
            catch (InterruptedException e)
            {
                //
            }

            doBatch(_numBatches);

        }

        private void doBatch(int batchSize) // long[] timings,
        {
            TextMessage testMessage = null;
            try
            {
                testMessage = _client.session[0].createTextMessage("start");

                for (int batch = 0; batch <= batchSize; batch++)
                // while (_running)
                {
                    long start = System.nanoTime();

                    testMessage.setText("start");
                    testMessage.setIntProperty("BATCH", batch);

                    _client.producer.send(testMessage);
                    _rateAdapter.sentMessage();

                    testMessage.setText("test");
                    // start at 2 so start and end count as part of batch
                    for (int m = 2; m < _batchSize; m++)
                    {
                        _client.producer.send(testMessage);
                        _rateAdapter.sentMessage();
                    }

                    testMessage.setText("end");
                    _client.producer.send(testMessage);
                    _rateAdapter.sentMessage();

                    long end = System.nanoTime();

                    long sendtime = end - start;

                    if (log.isDebugEnabled())
                    {
                        log.info("Sent batch[" + batch + "](" + _batchSize + ") in " + sendtime); // timings[batch]);
                    }

                    if ((batch % LOG_UPATE_INTERVAL) == 0)
                    {
                        console.info("Sent Batch[" + batch + "](" + _batchSize + ")" + status());
                    }

                    _rateAdapter.sleepBatch();

                }
            }
            catch (JMSException e)
            {
                console.error("Runner ended");
            }
        }

        private String status()
        {
            return " TotalDuration: " + _totalDuration + " for " + delays.size() + " consumers" + " Delay is " + _delay
                + " resulting in "
                + ((_delay > (TEN_MILLI_SEC * _batchSize)) ? ((_delay / _batchSize) + "/msg") : (_delay + "/batch"));
        }

        private void sleepBatch()
        {
            if (checkForSlowClients())
            { // if there werwe slow clients we have already slept so don't sleep anymore again.
                return;
            }

            if (!SLEEP_PER_MESSAGE)
            {
                // per batch sleep.. if sleep is to small to spread over the batch.
                if (_delay <= (TEN_MILLI_SEC * _batchSize))
                {
                    sleepLong(_delay);
                }
                else
                {
                    log.info("Not sleeping _delay > ten*batch is:" + _delay);
                }
            }
        }

        public void stop()
        {
            _running = false;
        }

        Map<String, Long> delays = new HashMap<String, Long>();
        Long _totalReceived = 0L;
        Long _totalDuration = 0L;
        int _skipUpdate = 0;

        /**
         * Adjust the delay for sending messages based on this update from the client
         *
         * @param client        The client that send this update
         * @param duration      The time taken for the last batch of messagse
         * @param batchNumber   The reported batchnumber from the client
         */
        private void adjustDelay(String client, int batchNumber, long duration)
        {
            // Retrieve the current total time taken for this client.
            Long currentTime = delays.get(client);

            // Add the new duration time to this client
            if (currentTime == null)
            {
                currentTime = duration;
            }
            else
            {
                currentTime += duration;
            }

            delays.put(client, currentTime);

            long batchesSent = _sent / _batchSize;

            // ensure we don't divide by zero
            if (batchesSent == 0)
            {
                batchesSent = 1L;
            }

            _totalReceived += _batchSize;
            _totalDuration += duration;

            // calculate average duration accross clients per batch
            long averageDuration = _totalDuration / delays.size() / batchesSent;

            // calculate the difference between current send delay and average report delay
            long diff = (duration) - averageDuration;

            if (log.isInfoEnabled() && ((batchNumber % DEBUG_LOG_UPATE_INTERVAL) == 0))
            {
                log.info("TotalDuration:" + _totalDuration + " for " + delays.size() + " consumers." + " on batch: "
                    + batchesSent + " received batch: " + batchNumber + " Batch Duration: " + duration + " Average: "
                    + averageDuration + " so diff: " + diff + " for : " + client + " Delay is " + _delay + " resulting in "
                    + ((_delay > (TEN_MILLI_SEC * _batchSize)) ? ((_delay / _batchSize) + "/msg") : (_delay + "/batch")));
            }

            // if the averageDuration differs from the current by more than the specified variane then adjust delay.
            if (Math.abs(diff) > _timeVariance)
            {

                // if the the _delay is larger than the required duration to send report
                // speed up
                if (diff > TEN_MILLI_SEC)
                {
                    _delay -= TEN_MILLI_SEC;

                    if (_delay < 0)
                    {
                        _delay = 0;
                        log.info("Reset _delay to 0");
                        delayStable();
                    }
                    else
                    {
                        delayChanged();
                    }

                }
                else if (diff < 0) // diff < 0 diff cannot be 0 as it is > _timeVariance
                {
                    // the report took longer
                    _delay += TEN_MILLI_SEC;
                    delayChanged();
                }
            }
            else
            {
                delayStable();
            }

            // If we have a consumer that is behind with the batches.
            if ((batchesSent - batchNumber) > _batchVariance)
            {
                log.debug("Increasing _delay as sending more than receiving");

                _delay += 2 * TEN_MILLI_SEC;
                delayChanged();
            }

        }

        /** Reset the number of iterations before we say the delay has stabilised. */
        private void delayChanged()
        {
            _delayShifting = REPORTS_WITHOUT_CHANGE;
        }

        /**
         * Record the fact that delay has stabilised If delay has stablised for REPORTS_WITHOUT_CHANGE then it will
         * output Delay stabilised
         */
        private void delayStable()
        {
            _delayShifting--;

            if (_delayShifting < 0)
            {
                _delayShifting = 0;
                console.debug("Delay stabilised:" + _delay);
            }
        }

        /**
         * Checks that the client has received enough messages. If the client has fallen behind then they are put in the
         * _slowClients lists which will increase the delay.
         *
         * @param client   The client identifier to check
         * @param received the number of messages received by that client
         * @param batchNumber
         */
        private void recordSlow(String client, long received, int batchNumber)
        {
            if (Math.abs(batchNumber - (_sent / _batchSize)) > _batchVariance)
            {
                _slowClients.put(client, received);
            }
            else
            {
                _slowClients.remove(client);
            }
        }

        /** Incrment the number of sent messages and then sleep, if required. */
        public void sentMessage()
        {

            _sent++;

            if (_delay > (TEN_MILLI_SEC * _batchSize))
            {
                long batchDelay = _delay / _batchSize;
                // less than 10ms sleep doesn't always work.
                // _delay is in nano seconds
                // if (batchDelay < (TEN_MILLI_SEC))
                // {
                // sleep(0, (int) batchDelay);
                // }
                // else
                {
                    // if (batchDelay < 30000000000L)
                    {
                        sleepLong(batchDelay);
                    }
                }
            }
            else
            {
                if (SLEEP_PER_MESSAGE && (_delay > 0))
                {
                    sleepLong(_delay / _batchSize);
                }
            }
        }

        /**
         * Check at the end of each batch and pause sending messages to allow slow clients to catch up.
         *
         * @return true if there were slow clients that caught up.
         */
        private boolean checkForSlowClients()
        {
            // This will allways be true as we are running this at the end of each batchSize
            // if (_sent % _batchSize == 0)
            {
                // Cause test to pause when we have slow
                if (!_slowClients.isEmpty() || NO_CLIENTS)
                {

                    while (!_slowClients.isEmpty())
                    {
                        if (log.isInfoEnabled() && ((_sent / _batchSize % DEBUG_LOG_UPATE_INTERVAL) == 0))
                        {
                            String clients = "";
                            Iterator it = _slowClients.keySet().iterator();
                            while (it.hasNext())
                            {
                                clients += it.next();
                                if (it.hasNext())
                                {
                                    clients += ", ";
                                }
                            }

                            log.info("Pausing for slow clients:" + clients);
                        }

                        if (console.isDebugEnabled() && ((_sent / _batchSize % LOG_UPATE_INTERVAL) == 0))
                        {
                            console.debug(_slowClients.size() + " slow clients.");
                        }

                        sleep(PAUSE_SLEEP);
                    }

                    if (NO_CLIENTS)
                    {
                        sleep(NO_CLIENT_SLEEP);
                    }

                    log.debug("Continuing");

                    return true;
                }
                else
                {
                    if ((_sent / _batchSize % LOG_UPATE_INTERVAL) == 0)
                    {
                        console.info("Total Delay :" + _delay + " "
                            + ((_delayShifting == 0) ? "Stablised" : ("Not Stablised(" + _delayShifting + ")")));
                    }
                }

            }

            return false;
        }

        /**
         * Sleep normally takes micro-seconds this allows the use of a nano-second value.
         *
         * @param delay nanoseconds to sleep for.
         */
        private void sleepLong(long delay)
        {
            sleep(delay / 1000000, (int) (delay % 1000000));
        }

        /**
         * Sleep for the specified micro-seconds.
         * @param sleep microseconds to sleep for.
         */
        private void sleep(long sleep)
        {
            sleep(sleep, 0);
        }

        /**
         * Perform the sleep , swallowing any InteruptException.
         *
         * NOTE: If a sleep request is > 10s then reset only sleep for 5s
         *
         * @param milli to sleep for
         * @param nano sub miliseconds to sleep for
         */
        private void sleep(long milli, int nano)
        {
            try
            {
                log.debug("Sleep:" + milli + ":" + nano);
                if (milli > 10000)
                {

                    if (_delay == milli)
                    {
                        _totalDuration = _totalReceived / _batchSize * EXPECTED_TIME_PER_BATCH;
                        log.error("Sleeping for more than 10 seconds adjusted to 5s!:" + (milli / 1000)
                            + "s. Reset _totalDuration:" + _totalDuration);
                    }
                    else
                    {
                        log.error("Sleeping for more than 10 seconds adjusted to 5s!:" + (milli / 1000) + "s");
                    }

                    milli = 5000;
                }

                Thread.sleep(milli, nano);
            }
            catch (InterruptedException e)
            {
                //
            }
        }

        public void setClient(SustainedClientTestCase client)
        {
            _client = client;
        }
    }

}
