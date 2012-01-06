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
package org.apache.qpid.headers;

import org.apache.qpid.client.AMQSession;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.FieldTableFactory;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.TextMessage;

/**
 */
class MessageFactory
{
    private static final char[] DATA = "abcdefghijklmnopqrstuvwxyz".toCharArray();

    private final AMQSession _session;
    private final byte[] _payload;

    private String[] _headerNames;

    MessageFactory(AMQSession session)
    {
        this(session, Integer.getInteger("amqj.test.message_size", 256).intValue(), 5);
    }

    MessageFactory(AMQSession session, int payloadSize, int headerCount)
    {
        if (headerCount < 1)
        {
            throw new IllegalArgumentException("Header count must be positive");
        }
        _session = session;
        _payload = new byte[payloadSize];
        for (int i = 0; i < _payload.length; i++)
        {
            _payload[i] = (byte) DATA[i % DATA.length];
        }
        _headerNames = new String[headerCount];
        // note that with the standard encoding the headers get prefixed with an S to indicate their type
        for (int i = 0; i < _headerNames.length; i++)
        {
            if (i < 10)
            {
                _headerNames[i] = "F000" + i;
            }
            else if (i >= 10 && i < 100)
            {
                _headerNames[i] = "F00" + i;
            }
            else
            {
                _headerNames[i] = "F0" + i;
            }
        }
    }

    Message createEventMessage() throws JMSException
    {
        BytesMessage msg = _session.createBytesMessage();
        if (_payload.length != 0)
        {
            msg.writeBytes(_payload);
        }
        return setHeaders(msg, _headerNames);
    }

    Message createShutdownMessage() throws JMSException
    {
        return setHeaders(_session.createMessage(), new String[]{"F0000", "SHUTDOWN"});
    }

    Message createReportRequestMessage() throws JMSException
    {
        return setHeaders(_session.createMessage(), new String[]{"F0000", "REPORT"});
    }

    Message createReportResponseMessage(String msg) throws JMSException
    {
        return setHeaders(_session.createTextMessage(msg), new String[]{"CONTROL", "REPORT"});
    }

    boolean isShutdown(Message m)
    {
        return checkPresent(m, "SHUTDOWN");
    }

    boolean isReport(Message m)
    {
        return checkPresent(m, "REPORT");
    }

    Object getReport(Message m)
    {
        try
        {
            return ((TextMessage) m).getText();
        }
        catch (JMSException e)
        {
            e.printStackTrace(System.out);
            return e.toString();
        }
    }

    FieldTable getConsumerBinding()
    {
        FieldTable binding = FieldTableFactory.newFieldTable();
        binding.setString("SF0000", "value");
        return binding;
    }

    FieldTable getControllerBinding()
    {
        FieldTable binding = FieldTableFactory.newFieldTable();
        binding.setString("SCONTROL", "value");
        return binding;
    }

    MessageConsumer createConsumer(Destination source) throws Exception
    {
        return _session.createConsumer(source, 0, false, true, null, getConsumerBinding());
    }

    MessageConsumer createController(Destination source) throws Exception
    {
        return _session.createConsumer(source, 0, false, true, null, getControllerBinding());
    }

    private static boolean checkPresent(Message m, String s)
    {
        try
        {
            return m.getStringProperty(s) != null;
        }
        catch (JMSException e)
        {
            e.printStackTrace(System.out);
            return false;
        }
    }

    private static Message setHeaders(Message m, String[] headers) throws JMSException
    {
        for (int i = 0; i < headers.length; i++)
        {
            // the value in GRM is 5 bytes
            m.setStringProperty(headers[i], "value");
        }
        return m;
    }
}
