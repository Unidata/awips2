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
package org.apache.qpid.flow;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;

import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;

public class ChannelFlowTest implements MessageListener
{
    private int sent;
    private int received;

    ChannelFlowTest(String broker) throws Exception
    {
        this(new AMQConnection(broker, "guest", "guest", randomize("Client"), "/test"));
    }

    ChannelFlowTest(AMQConnection connection) throws Exception
    {
        this(connection, new AMQQueue(connection.getDefaultQueueExchangeName(), new AMQShortString(randomize("ChannelFlowTest")), true));
    }

    ChannelFlowTest(AMQConnection connection, AMQDestination destination) throws Exception
    {
        AMQSession session = (AMQSession) connection.createSession(false, AMQSession.NO_ACKNOWLEDGE, 50,25);

        //set up a slow consumer
        session.createConsumer(destination).setMessageListener(this);
        connection.start();

        //create a publisher
        MessageProducer producer = session.createProducer(destination);
        Message msg = session.createTextMessage("Message");

        //publish in bursts that are fast enough to cause channel flow control
        for(int i = 0; i < 10; i++)
        {
            for(int j = 0; j < 100; j++)
            {
                producer.send(msg);
                sent++;
            }
            waitUntilReceived(sent - 40);
        }

        waitUntilReceived(sent);

        session.close();
        connection.close();
    }


    private synchronized void waitUntilReceived(int count) throws InterruptedException
    {
        while(received <count)
        {
            wait();
        }
    }

    public synchronized void onMessage(Message message)
    {
        try
        {
            Thread.sleep(50);

            received++;
            notify();
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();
        }
    }

    private static String randomize(String in)
    {
        return in + System.currentTimeMillis();
    }

    public static void main(String[] argv) throws Exception
    {
        new ChannelFlowTest(argv.length == 0 ? "localhost:5672" : argv[0]);
    }

}
