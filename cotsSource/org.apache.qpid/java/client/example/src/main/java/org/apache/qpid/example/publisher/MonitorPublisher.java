/*
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
 */
package org.apache.qpid.example.publisher;

import org.apache.qpid.client.BasicMessageProducer;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

import javax.jms.DeliveryMode;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

/**
 * Subclass of Publisher which uses QPID functionality to send a heartbeat message Note immediate flag not available via
 * JMS MessageProducer
 */
public class MonitorPublisher extends Publisher
{

    private static final Logger _log = LoggerFactory.getLogger(Publisher.class);

    BasicMessageProducer _producer;

    public MonitorPublisher()
    {
        super();
    }

    /*
    * Publishes a message using given details
    */
    public boolean sendMessage(Session session, Message message, int deliveryMode,
                                        boolean immediate, boolean commit) throws UndeliveredMessageException
    {
        try
        {
            _producer = (BasicMessageProducer) session.createProducer(_destination);

            _producer.send(message, deliveryMode, immediate);

            if (commit)
            {
                //commit the message send and close the transaction
                _session.commit();
            }

        }
        catch (JMSException e)
        {
            //Have to assume our commit failed but do not rollback here as channel closed
            _log.error("JMSException", e);
            e.printStackTrace();
            throw new UndeliveredMessageException("Cannot deliver immediate message", e);
        }

        _log.info(_name + " finished sending message: " + message);
        return true;
    }

    /*
    * Publishes a non-persistent message using transacted session
    */
    public boolean sendImmediateMessage(Message message) throws UndeliveredMessageException
    {
        try
        {
            _producer = (BasicMessageProducer) _session.createProducer(_destination);

            //Send message via our producer which is not persistent and is immediate
            //NB: not available via jms interface MessageProducer
            _producer.send(message, DeliveryMode.NON_PERSISTENT, true);

            //commit the message send and close the transaction
            _session.commit();

        }
        catch (JMSException e)
        {
            //Have to assume our commit failed but do not rollback here as channel closed
            _log.error("JMSException", e);
            e.printStackTrace();
            throw new UndeliveredMessageException("Cannot deliver immediate message", e);
        }

        _log.info(_name + " finished sending message: " + message);
        return true;
    }
}
