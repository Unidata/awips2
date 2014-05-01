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
package org.apache.qpid.server.logging;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.framing.AMQFrame;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ExchangeDeleteBody;
import org.apache.qpid.framing.ExchangeDeleteOkBody;
import org.apache.qpid.framing.amqp_8_0.MethodRegistry_8_0;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.Session;
import java.io.IOException;
import java.util.List;

/**
 * Exchange
 *
 * The Exchange test suite validates that the follow log messages as specified in the Functional Specification.
 *
 * This suite of tests validate that the Exchange messages occur correctly and according to the following format:
 *
 * EXH-1001 : Create : [Durable] Type:<value> Name:<value>
 * EXH-1002 : Deleted
 */
public class ExchangeLoggingTest extends AbstractTestLogging
{

    static final String EXH_PREFIX = "EXH-";

    Connection _connection;
    Session _session;
    Queue _queue;
    String _name;
    String _type;

    @Override
    public void setUp() throws Exception
    {
        super.setUp();

        _connection = getConnection();

        _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        _type = "direct";
        _name = "testName";

        _queue = _session.createQueue(_type + "://" + _name + "/queue/queue");

    }

    /**
     * Description:
     * When a durable exchange is created an EXH-1001 message is logged with the Durable tag. This will be the first message from this exchange.
     * Input:
     *
     * 1. Running broker
     * 2. Client requests a durable exchange be created.
     * Output:
     *
     * <date> EXH-1001 : Create : Durable Type:<value> Name:<value>
     *
     * Validation Steps:
     * 3. The EXH ID is correct
     * 4. The Durable tag is present in the message
     */

    public void testExchangeCreateDurable() throws JMSException, IOException
    {
        // The client cannot create durable exchanges lets just look at the
        // ones the broker creates at startup.

        // They should all be durable

        List<String> results = _monitor.findMatches(EXH_PREFIX);

        assertTrue("No Results found for Exchange.", results.size()>0);

        String log = getLog(results.get(0));

        validateMessageID("EXH-1001", log);

        String message = getMessageString(fromMessage(log));
        assertTrue("Log Message does not start with create:" + message,
                   message.startsWith("Create"));

        assertTrue("Log Message does not contain Durable:" + message,
                   message.contains("Durable"));

    }

    /**
     * Description:
     * When an exchange is created an EXH-1001 message is logged. This will be the first message from this exchange.
     * Input:
     *
     * 1. Running broker
     * 2. Client requests an exchange be created.
     * Output:
     *
     * <date> EXH-1001 : Create : Type:<value> Name:<value>
     *
     * Validation Steps:
     * 3. The EXH ID is correct
     */
    public void testExchangeCreate() throws JMSException, IOException
    {
        //Ignore broker startup messages
        _monitor.reset();

        _session.createConsumer(_queue);

        List<String> results = _monitor.findMatches(EXH_PREFIX);

        assertEquals("Result set larger than expected.", 1, results.size());

        String log = getLog(results.get(0));

        validateMessageID("EXH-1001", log);

        String message = getMessageString(fromMessage(log));
        assertTrue("Log Message does not start with create:" + message,
                   message.startsWith("Create"));
        assertTrue("Log Message does not contain Type:" + message,
                   message.contains("Type: " + _type));
        assertTrue("Log Message does not contain Name:" + message,
                   message.contains("Name: " + _name));
    }

    /**
     * Description:
     * An Exchange can be deleted through an AMQP ExchangeDelete method. When this is successful an EXH-1002 Delete message will be logged. This will be the last message from this exchange.
     * Input:
     *
     * 1. Running broker
     * 2. A new Exchange has been created
     * 3. Client requests that the new exchange be deleted.
     * Output:
     *
     * <date> EXH-1002 : Deleted
     *
     * Validation Steps:
     * 4. The EXH ID is correct
     * 5. There is a corresponding EXH-1001 Create message logged.
     */
    public void testExchangeDelete() throws Exception, IOException
    {
        //Ignore broker startup messages
        _monitor.reset();

        _session.createConsumer(_queue);

        MethodRegistry_8_0 registry = new MethodRegistry_8_0();

        ExchangeDeleteBody body = registry.createExchangeDeleteBody(0, new AMQShortString(_name), false, true);

        AMQFrame exchangeDeclare = body.generateFrame(0);

        ((AMQConnection) _connection).getProtocolHandler().syncWrite(exchangeDeclare, ExchangeDeleteOkBody.class);

        List<String> results = _monitor.findMatches(EXH_PREFIX);

        assertEquals("Result set larger than expected.", 2, results.size());

        String log = getLog(results.get(0));

        validateMessageID("EXH-1001", log);

        String message = getMessageString(fromMessage(log));
        assertTrue("Log Message does start with Create",
                   message.startsWith("Create"));

        log = getLog(results.get(1));
        validateMessageID("EXH-1002", log);

        message = getMessageString(fromMessage(log));
        assertEquals("Log Message not as expected", "Deleted", message);

    }

}
