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
package org.apache.qpid.test.unit.message;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQHeadersExchange;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.configuration.ClientProperties;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.url.AMQBindingURL;
import org.apache.qpid.url.BindingURL;
import org.apache.qpid.test.utils.QpidTestCase;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageEOFException;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.StreamMessage;

/**
 * @author Apache Software Foundation
 */
public class StreamMessageTest extends QpidTestCase
{

    private static final Logger _logger = LoggerFactory.getLogger(StreamMessageTest.class);

    public String _connectionString = "vm://:1";

    protected void setUp() throws Exception
    {
        super.setUp();
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testStreamMessageEOF() throws Exception
    {
        Connection con = (AMQConnection) getConnection("guest", "guest");
        AMQSession consumerSession = (AMQSession) con.createSession(false, Session.CLIENT_ACKNOWLEDGE);

        AMQHeadersExchange queue =
            new AMQHeadersExchange(new AMQBindingURL(
                    ExchangeDefaults.HEADERS_EXCHANGE_CLASS + "://" + ExchangeDefaults.HEADERS_EXCHANGE_NAME
                    + "/test/queue1?" + BindingURL.OPTION_ROUTING_KEY + "='F0000=1'"));
        FieldTable ft = new FieldTable();
        ft.setString("F1000", "1");
        MessageConsumer consumer =
            consumerSession.createConsumer(queue, Integer.parseInt(ClientProperties.MAX_PREFETCH_DEFAULT), Integer.parseInt(ClientProperties.MAX_PREFETCH_DEFAULT), false, false, (String) null, ft);

        // force synch to ensure the consumer has resulted in a bound queue
        // ((AMQSession) consumerSession).declareExchangeSynch(ExchangeDefaults.HEADERS_EXCHANGE_NAME, ExchangeDefaults.HEADERS_EXCHANGE_CLASS);
        // This is the default now

        Connection con2 = (AMQConnection) getConnection("guest", "guest");

        AMQSession producerSession = (AMQSession) con2.createSession(false, Session.CLIENT_ACKNOWLEDGE);

        // Need to start the "producer" connection in order to receive bounced messages
        _logger.info("Starting producer connection");
        con2.start();

        MessageProducer mandatoryProducer = producerSession.createProducer(queue);

        // Third test - should be routed
        _logger.info("Sending isBound message");
        StreamMessage msg = producerSession.createStreamMessage();

        msg.setStringProperty("F1000", "1");

        msg.writeByte((byte) 42);

        mandatoryProducer.send(msg);

        _logger.info("Starting consumer connection");
        con.start();

        StreamMessage msg2 = (StreamMessage) consumer.receive(2000);
        assertNotNull(msg2);

        msg2.readByte();
        try
        {
            msg2.readByte();
        }
        catch (Exception e)
        {
            assertTrue("Expected MessageEOFException: " + e, e instanceof MessageEOFException);
        }
        con.close();
        con2.close();
    }

    public void testModifyReceivedMessageExpandsBuffer() throws Exception
    {
        AMQConnection con = (AMQConnection) getConnection("guest", "guest");
        AMQSession consumerSession = (AMQSession) con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        AMQQueue queue = new AMQQueue(con.getDefaultQueueExchangeName(), new AMQShortString("testQ"));
        MessageConsumer consumer = consumerSession.createConsumer(queue);
        consumer.setMessageListener(new MessageListener()
            {

                public void onMessage(Message message)
                {
                    StreamMessage sm = (StreamMessage) message;
                    try
                    {
                        sm.clearBody();
                        sm.writeString("dfgjshfslfjshflsjfdlsjfhdsljkfhdsljkfhsd");
                    }
                    catch (JMSException e)
                    {
                        _logger.error("Error when writing large string to received msg: " + e, e);
                        fail("Error when writing large string to received msg" + e);
                    }
                }
            });

        Connection con2 = (AMQConnection) getConnection("guest", "guest");
        AMQSession producerSession = (AMQSession) con2.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        MessageProducer mandatoryProducer = producerSession.createProducer(queue);
        con.start();
        StreamMessage sm = producerSession.createStreamMessage();
        sm.writeInt(42);
        mandatoryProducer.send(sm);
        Thread.sleep(2000);
        con.close();
        con2.close();
    }
}
