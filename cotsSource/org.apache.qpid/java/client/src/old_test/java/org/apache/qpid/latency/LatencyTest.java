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
package org.apache.qpid.latency;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;

import javax.jms.MessageProducer;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.JMSException;
import javax.jms.TextMessage;
import javax.jms.BytesMessage;

public class LatencyTest implements MessageListener
{
    private volatile boolean waiting;
    private int sent;
    private int received;

    private final byte[] data;

    private long min = Long.MAX_VALUE;
    private long max = 0;
    private long total = 0;

    LatencyTest(String broker, int count, int delay, int length) throws Exception
    {
        this(new AMQConnection(broker, "guest", "guest", randomize("Client"), "/test"), count, delay, length);
    }

    LatencyTest(AMQConnection connection, int count, int delay, int length) throws Exception
    {
        this(connection, new AMQQueue(connection.getDefaultQueueExchangeName(), new AMQShortString(randomize("LatencyTest")), true), count, delay, length);
    }

    LatencyTest(AMQConnection connection, AMQDestination destination, int count, int delay, int length) throws Exception
    {
        AMQSession session = (AMQSession) connection.createSession(false, AMQSession.NO_ACKNOWLEDGE);

        data = new byte[length];
        for(int i = 0; i < data.length; i++)
        {
            data[i] = (byte) (i % 100);
        }

        //set up a consumer
        session.createConsumer(destination).setMessageListener(this);
        connection.start();

        //create a publisher
        MessageProducer producer = session.createProducer(destination, false, false, true);

        //publish at a low volume
        for(int i = 0; i < count; i++)
        {
            BytesMessage msg = session.createBytesMessage();
            msg.writeBytes(data);
            msg.setStringProperty("sent-at", Long.toString(System.nanoTime()));
            producer.send(msg);
            Thread.sleep(delay);
            if(++sent % 100 == 0)
            {
                System.out.println("Sent " + sent + " of " + count);
            }
        }

        waitUntilReceived(sent);

        session.close();
        connection.close();

        System.out.println("Latency (in nanoseconds): avg=" + (total/sent) + ", min=" + min + ", max=" + max
        + ", avg(discarding min and max)=" + ((total - min - max) / (sent - 2)));
    }


    private synchronized void waitUntilReceived(int count) throws InterruptedException
    {
        waiting = true;
        while(received < count)
        {
            wait();
        }
        waiting = false;
    }

    public void onMessage(Message message)
    {
        received++;
        try
        {
            long sent = Long.parseLong(message.getStringProperty("sent-at"));
            long time = System.nanoTime() - sent;
            total += time;
            min = Math.min(min, time);
            max = Math.max(max, time);
        }
        catch (JMSException e)
        {
            e.printStackTrace();
        }

        if(waiting){
            synchronized(this)
            {
                notify();
            }
        }
    }

    private static String randomize(String in)
    {
        return in + System.currentTimeMillis();
    }

    public static void main(String[] argv) throws Exception
    {
        String host = argv.length > 0 ? argv[0] : "localhost:5672";
        if("-help".equals(host))
        {
            System.out.println("Usage: <broker> <message count> <delay between messages> <message size>");
        }
        int count = argv.length > 1 ? Integer.parseInt(argv[1]) : 1000;
        int delay = argv.length > 2 ? Integer.parseInt(argv[2]) : 1000;
        int size = argv.length > 3 ? Integer.parseInt(argv[3]) : 512;
        new LatencyTest(host, count, delay, size);
    }


}
