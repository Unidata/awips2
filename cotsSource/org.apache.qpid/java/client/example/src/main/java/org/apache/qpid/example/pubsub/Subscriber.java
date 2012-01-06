/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.    
 *
 * 
 */
package org.apache.qpid.example.pubsub;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import java.util.concurrent.CountDownLatch;


/**
 * Simple client that listens for the specified number of msgs on the given Destinaton
 *
 * The class can take two arguments.
 * java Subscriber <destination> <msgCount>
 * Where:
 * destination is either 'topic' or 'queue'  (Default: topic)
 * msgCount is the number of messages to send (Default : 100)
 */
public class Subscriber extends Client implements MessageListener
{

    CountDownLatch _count;

    public Subscriber(String destination, int msgCount)
    {
        super(destination);
        _count = new CountDownLatch(msgCount);
    }


    public void start()
    {
        try
        {
            _session = _connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

            _session.createDurableSubscriber((Topic) _setup.getDestination(ConnectionSetup.TOPIC_JNDI_NAME),
                                             "exampleClient").setMessageListener(this);
            _connection.start();
            _count.await();

            System.out.println("Done");

            _connection.close();
        }
        catch (JMSException e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    public static void main(String[] args)
    {
        String destination = args.length > 2 ? args[1] : null;
        int msgCount = args.length > 2 ? Integer.parseInt(args[2]) : 100;

        new Subscriber(destination, msgCount).start();
    }

    public void onMessage(Message message)
    {
        try
        {
            _count.countDown();
            System.out.println("Received msg:" + ((TextMessage) message).getText());
        }
        catch (JMSException e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }
}
