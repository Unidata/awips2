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
package org.apache.qpid.topic;

import java.util.Random;

import javax.jms.*;

import org.apache.log4j.Logger;
import org.apache.log4j.NDC;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.exchange.ExchangeDefaults;

/**
 * This class has not kept up to date with the topic_listener in the cpp tests. It should provide identical behaviour for
 * cross testing the java and cpp clients.
 *
 * <p/>How the cpp topic_publisher operates:
 * It publishes text messages to the default topic exchange, on virtual host "/test", on the topic "topic_control", for
 * the specified number of test messages to be sent.
 * It publishes a report request message (on same topic), with the header text field "TYPE", value "REPORT_REQUEST",
 * optionally within a transaction, and waits for the specified number of consumers to reply to this request. The
 * listeners should reply to this message on a queue named "response", on virtual host "/test", with some sort of message
 * about the number of messages received and how long it took, although the publisher never looks at the message content.
 * The publisher then send a message (on the same topic), with the header text field "TYPE", value "TERMINATION_REQUEST",
 * which the listener should close its connection and terminate upon receipt of.
 *
 * @todo I've added lots of field table types in the report message, just to check if the other end can decode them
 *       correctly. Not really the right place to test this, so remove them from
 *       {@link #createReportResponseMessage(String)} once a better test exists.
 */
public class Listener implements MessageListener
{
    private static Logger log = Logger.getLogger(Listener.class);

    public static final String CONTROL_TOPIC = "topic_control";
    public static final String RESPONSE_QUEUE = "response";

    private final Topic _topic;
    //private final Topic _control;

    private final Queue _response;

    /** Holds the connection to listen on. */
    private final Connection _connection;

    /** Holds the producer to send control messages on. */
    private final MessageProducer _controller;

    /** Holds the JMS session. */
    private final javax.jms.Session _session;

    /** Holds a flag to indicate that a timer has begun on the first message. Reset when report is sent. */
    private boolean init;

    /** Holds the count of messages received by this listener. */
    private int count;

    /** Used to hold the start time of the first message. */
    private long start;
    private static String clientId;

    Listener(Connection connection, int ackMode, String name) throws Exception
    {
        log.debug("Listener(Connection connection = " + connection + ", int ackMode = " + ackMode + ", String name = " + name
                  + "): called");

        _connection = connection;
        _session = connection.createSession(false, ackMode);

        if (_session instanceof AMQSession)
        {
            _topic = new AMQTopic(ExchangeDefaults.TOPIC_EXCHANGE_NAME, CONTROL_TOPIC);
            //_control = new AMQTopic(CONTROL_TOPIC);
            _response = new AMQQueue(ExchangeDefaults.DIRECT_EXCHANGE_NAME, RESPONSE_QUEUE);
        }
        else
        {
            _topic = _session.createTopic(CONTROL_TOPIC);
            //_control = _session.createTopic(CONTROL_TOPIC);
            _response = _session.createQueue(RESPONSE_QUEUE);
        }

        //register for events
        if (name == null)
        {
            log.debug("Calling _factory.createTopicConsumer().setMessageListener(this)");
            createTopicConsumer().setMessageListener(this);
        }
        else
        {
            log.debug("Calling createDurableTopicConsumer(name).setMessageListener(this)");
            createDurableTopicConsumer(name).setMessageListener(this);
        }

        _connection.start();

        _controller = createControlPublisher();
        System.out.println("Waiting for messages " + Config.getAckModeDescription(ackMode)
                           +
                           ((name == null)
                            ? "" : (" (subscribed with name " + name + " and client id " + connection.getClientID() + ")"))
                           + "...");
    }

    public static void main(String[] argv) throws Exception
    {
        clientId = "Listener-" + System.currentTimeMillis();

        NDC.push(clientId);

        Config config = new Config();
        config.setOptions(argv);

        //Connection con = config.createConnection();
        Connection con =
            new AMQConnection("amqp://guest:guest@testid/test?brokerlist='" + config.getHost() + ":" + config.getPort()
                              + "'");

        if (config.getClientId() != null)
        {
            con.setClientID(config.getClientId());
        }

        new Listener(con, config.getAckMode(), config.getSubscriptionId());

        NDC.pop();
        NDC.remove();
    }

    /**
     * Checks whether or not a text field on a message has the specified value.
     *
     * @param m         The message to check.
     * @param fieldName The name of the field to check.
     * @param value     The expected value of the field to compare with.
     *
     * @return <tt>true</tt>If the specified field has the specified value, <tt>fals</tt> otherwise.
     *
     * @throws JMSException Any JMSExceptions are allowed to fall through.
     */
    private static boolean checkTextField(Message m, String fieldName, String value) throws JMSException
    {
        log.debug("private static boolean checkTextField(Message m = " + m + ", String fieldName = " + fieldName
                  + ", String value = " + value + "): called");

        String comp = m.getStringProperty(fieldName);
        log.debug("comp = " + comp);

        boolean result = (comp != null) && comp.equals(value);
        log.debug("result = " + result);

        return result;
    }

    public void onMessage(Message message)
    {
        NDC.push(clientId);

        log.debug("public void onMessage(Message message = " + message + "): called");

        if (!init)
        {
            start = System.nanoTime() / 1000000;
            count = 0;
            init = true;
        }

        try
        {
            if (isShutdown(message))
            {
                log.debug("Got a shutdown message.");
                shutdown();
            }
            else if (isReport(message))
            {
                log.debug("Got a report request message.");

                // Send the report.
                report();
                init = false;
            }
        }
        catch (JMSException e)
        {
            log.warn("There was a JMSException during onMessage.", e);
        }
        finally
        {
            NDC.pop();
        }
    }

    Message createReportResponseMessage(String msg) throws JMSException
    {
        Message message = _session.createTextMessage(msg);

        // Shove some more field table type in the message just to see if the other end can handle it.
        message.setBooleanProperty("BOOLEAN", true);
        message.setByteProperty("BYTE", (byte) 5);
        message.setDoubleProperty("DOUBLE", Math.PI);
        message.setFloatProperty("FLOAT", 1.0f);
        message.setIntProperty("INT", 1);
        message.setShortProperty("SHORT", (short) 1);
        message.setLongProperty("LONG", (long) 1827361278);
        message.setStringProperty("STRING", "hello");

        return message;
    }

    boolean isShutdown(Message m) throws JMSException
    {
        boolean result = checkTextField(m, "TYPE", "TERMINATION_REQUEST");

        //log.debug("isShutdown = " + result);

        return result;
    }

    boolean isReport(Message m) throws JMSException
    {
        boolean result = checkTextField(m, "TYPE", "REPORT_REQUEST");

        //log.debug("isReport = " + result);

        return result;
    }

    MessageConsumer createTopicConsumer() throws Exception
    {
        return _session.createConsumer(_topic);
    }

    MessageConsumer createDurableTopicConsumer(String name) throws Exception
    {
        return _session.createDurableSubscriber(_topic, name);
    }

    MessageProducer createControlPublisher() throws Exception
    {
        return _session.createProducer(_response);
    }

    private void shutdown()
    {
        try
        {
            _session.close();
            _connection.stop();
            _connection.close();
        }
        catch (Exception e)
        {
            e.printStackTrace(System.out);
        }
    }

    private void report()
    {
        log.debug("private void report(): called");

        try
        {
            String msg = getReport();
            _controller.send(createReportResponseMessage(msg));
            log.debug("Sent report: " + msg);
        }
        catch (Exception e)
        {
            e.printStackTrace(System.out);
        }
    }

    private String getReport()
    {
        long time = ((System.nanoTime() / 1000000) - start);

        return "Received " + count + " in " + time + "ms";
    }
}
