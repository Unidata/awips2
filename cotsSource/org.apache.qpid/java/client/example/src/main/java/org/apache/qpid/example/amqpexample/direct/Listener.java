package org.apache.qpid.example.amqpexample.direct;
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


import java.nio.ByteBuffer;

import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.MessageAcceptMode;
import org.apache.qpid.transport.MessageAcquireMode;
import org.apache.qpid.transport.MessageCreditUnit;
import org.apache.qpid.transport.MessageTransfer;
import org.apache.qpid.transport.Session;
import org.apache.qpid.transport.SessionException;
import org.apache.qpid.transport.SessionListener;

/**
 * This listens to messages on a queue and terminates
 * when it sees the final message
 *
 */
public class Listener implements SessionListener
{

    public void opened(Session ssn) {}

    public void resumed(Session ssn) {}

    public void message(Session ssn, MessageTransfer xfr)
    {
        System.out.println("Message: " + xfr);
    }

    public void exception(Session ssn, SessionException exc)
    {
        exc.printStackTrace();
    }

    public void closed(Session ssn) {}

    /**
     *  This sends 10 messages to the
     *  amq.direct exchange using the
     *  routing key as "routing_key"
     *
     */
    public static void main(String[] args) throws InterruptedException
    {
        // Create connection
        Connection con = new Connection();
        con.connect("localhost", 5672, "test", "guest", "guest",false);

        // Create session
        Session session = con.createSession(0);

        // Create an instance of the listener
        Listener listener = new Listener();
        session.setSessionListener(listener);

        // create a subscription
        session.messageSubscribe("message_queue",
                                 "listener_destination",
                                 MessageAcceptMode.NONE,
                                 MessageAcquireMode.PRE_ACQUIRED,
                                 null, 0, null);


        // issue credits
        // XXX
        session.messageFlow("listener_destination", MessageCreditUnit.BYTE, Session.UNLIMITED_CREDIT);
        session.messageFlow("listener_destination", MessageCreditUnit.MESSAGE, 11);

        // confirm completion
        session.sync();

        // wait to receive all the messages
        System.out.println("Waiting 100 seconds for messages from listener_destination");
        Thread.sleep(100*1000);
        System.out.println("Shutting down listener for listener_destination");
        session.messageCancel("listener_destination");

        //cleanup
        session.close();
        con.close();
    }

}
