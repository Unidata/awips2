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
package org.apache.qpid.oldtopic;

import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;

import javax.jms.*;

/**
 */
class MessageFactory
{
    private static final char[] DATA = "abcdefghijklmnopqrstuvwxyz".toCharArray();

    private final Session _session;
    private final Topic _topic;
    private final Topic _control;
    private final byte[] _payload;


    MessageFactory(Session session) throws JMSException
    {
        this(session, 256);
    }

    MessageFactory(Session session, int size) throws JMSException
    {
        _session = session;
/*        if(session instanceof AMQSession)
        {
            _topic = new AMQTopic("topictest.messages");
            _control = new AMQTopic("topictest.control");
        }
        else*/
        {
            _topic = session.createTopic("topictest.messages");
            _control = session.createTopic("topictest.control");
        }
        _payload = new byte[size];

        for(int i = 0; i < size; i++)
        {
            _payload[i] = (byte) DATA[i % DATA.length];
        }
    }

    Topic getTopic()
    {
        return _topic;
    }

    Message createEventMessage() throws JMSException
    {
        BytesMessage msg = _session.createBytesMessage();
        msg.writeBytes(_payload);
        return msg;
    }

    Message createShutdownMessage() throws JMSException
    {
        return _session.createTextMessage("SHUTDOWN");
    }

    Message createReportRequestMessage() throws JMSException
    {
        return _session.createTextMessage("REPORT");
    }

    Message createReportResponseMessage(String msg) throws JMSException
    {
        return _session.createTextMessage(msg);
    }

    boolean isShutdown(Message m)
    {
        return checkText(m, "SHUTDOWN");
    }

    boolean isReport(Message m)
    {
        return checkText(m, "REPORT");
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

    MessageConsumer createTopicConsumer() throws Exception
    {
        return _session.createConsumer(_topic);
    }

    MessageConsumer createDurableTopicConsumer(String name) throws Exception
    {
        return _session.createDurableSubscriber(_topic, name);
    }

    MessageConsumer createControlConsumer() throws Exception
    {
        return _session.createConsumer(_control);
    }

    MessageProducer createTopicPublisher() throws Exception
    {
        return _session.createProducer(_topic);
    }

    MessageProducer createControlPublisher() throws Exception
    {
        return _session.createProducer(_control);
    }

    private static boolean checkText(Message m, String s)
    {
        try
        {
            return m instanceof TextMessage && ((TextMessage) m).getText().equals(s);
        }
        catch (JMSException e)
        {
            e.printStackTrace(System.out);
            return false;
        }
    }
}
