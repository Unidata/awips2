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
package org.apache.qpid.example.amqpexample.headers;


import org.apache.qpid.transport.*;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class Listener implements SessionListener
{
    private static CountDownLatch _countDownLatch = new CountDownLatch(1);

    public void opened(Session ssn) {}

    public void resumed(Session ssn) {}

    public void message(Session ssn, MessageTransfer xfr)
    {
        String body = xfr.getBodyString();
        System.out.println("Message: " + body);
        if ( body.equals("That's all, folks!"))
        {
            System.out.println("Received final message");
            _countDownLatch.countDown();
        }
    }

    public void exception(Session ssn, SessionException exc)
    {
        exc.printStackTrace();
    }

    public void closed(Session ssn) {}

    /**
     * Receives messages from queue ANY and then ALL
     */
    public static void main(String[] args) throws InterruptedException
    {
        // Create connection
        Connection con = new Connection();
        con.connect("localhost", 5672, "test", "guest", "guest",false);

        // Create session
        Session session = con.createSession(0);
        // we expect to receive all the messages  
        Consume(session, "headers_queue_any");
        // we expect to receive only messages that have both properties set.
        Consume(session, "headers_queue_all");

        //cleanup
        session.close();
        con.close();
    }

    private static void Consume(Session session, String queueName) throws InterruptedException
    {
        System.out.println("Consuming messages for queue " + queueName);
        _countDownLatch = new CountDownLatch(1);
        // Create an instance of the listener
        Listener listener = new Listener();
        session.setSessionListener(listener);

        // create a subscription
        session.messageSubscribe(queueName,
                                 "listener_destination",
                                 MessageAcceptMode.NONE,
                                 MessageAcquireMode.PRE_ACQUIRED,
                                 null, 0, null);


        // issue credits
        session.messageFlow("listener_destination", MessageCreditUnit.BYTE, Session.UNLIMITED_CREDIT);
        session.messageFlow("listener_destination", MessageCreditUnit.MESSAGE, 100);
        // confirm completion
        session.sync();

        // wait to receive all the messages
        System.out.println("Waiting 100 seconds for messages from queue " + queueName);

        _countDownLatch.await(30, TimeUnit.SECONDS);
        System.out.println("Shutting down listener for " + queueName);
        System.out.println("=========================================");
        session.messageCancel("listener_destination");
    }

}
