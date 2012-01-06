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
package org.apache.qpid.test.unit.client.forwardall;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.utils.QpidTestCase;

/**
 * Declare a queue and bind it to amq.direct with a 'well known' routing key,
 * register a consumer for this queue and send a response to every message received.
 */
public class Service implements MessageListener
{
    private final AMQConnection _connection;
    private final AMQSession _session;

    private static QpidTestCase _qct;
     

    public static void setQTC(QpidTestCase qtc)
    {
        _qct = qtc;
    }
    Service(String broker) throws Exception
    {
        this(connect(broker));
    }

    Service(AMQConnection connection) throws Exception
    {
        _connection = connection;
        //AMQQueue queue = new SpecialQueue(connection, "ServiceQueue");
        _session = (AMQSession) _connection.createSession(true, AMQSession.NO_ACKNOWLEDGE);
          AMQQueue queue  = (AMQQueue)  _session.createQueue("ServiceQueue") ;
        _session.createConsumer(queue).setMessageListener(this);
        _connection.start();
    }

    public void onMessage(Message request)
    {
        try
        {
            Message response = _session.createTextMessage("Response!");
            Destination replyTo = request.getJMSReplyTo();
            _session.createProducer(replyTo).send(response);
            _session.commit();
        }
        catch (Exception e)
        {
            e.printStackTrace(System.out);
        }
    }

    public void close() throws JMSException
    {
        _connection.close();
    }

    static AMQConnection connect(String broker) throws Exception
    {
        //return new AMQConnection(broker, "guest", "guest", "Client" + System.currentTimeMillis(), "test");
          return (AMQConnection) _qct.getConnection("guest", "guest") ;
    }

//    public static void main(String[] argv) throws Exception
//    {
//        String broker = argv.length == 0? "localhost:5672" : argv[0];
//        new Service(broker);
//    }
}
