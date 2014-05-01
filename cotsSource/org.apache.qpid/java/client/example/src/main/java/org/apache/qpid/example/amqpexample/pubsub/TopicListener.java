package org.apache.qpid.example.amqpexample.pubsub;
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
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.transport.MessageAcceptMode;
import org.apache.qpid.transport.MessageAcquireMode;
import org.apache.qpid.transport.MessageCreditUnit;
import org.apache.qpid.transport.MessageTransfer;
import org.apache.qpid.transport.Option;
import org.apache.qpid.transport.Session;
import org.apache.qpid.transport.SessionException;
import org.apache.qpid.transport.SessionListener;


public class TopicListener implements SessionListener
{

    public void opened(Session ssn) {}

    public void resumed(Session ssn) {}

    public void message(Session ssn, MessageTransfer xfr)
    {
        DeliveryProperties dp = xfr.getHeader().get(DeliveryProperties.class);
        System.out.println("Message: " + xfr + " with routing_key " + dp.getRoutingKey());
    }

    public void exception(Session ssn, SessionException exc)
    {
        exc.printStackTrace();
    }

    public void closed(Session ssn) {}

    public void prepareQueue(Session session,String queueName,String bindingKey)
    {
        session.queueDeclare(queueName, null, null, Option.EXCLUSIVE, Option.AUTO_DELETE);
        session.exchangeBind(queueName, "amq.topic", bindingKey, null);
        session.exchangeBind(queueName, "amq.topic", "control", null);

        session.messageSubscribe(queueName, queueName,
                                 MessageAcceptMode.NONE,
                                 MessageAcquireMode.PRE_ACQUIRED,
                                 null, 0, null);
        // issue credits
        // XXX: need to be able to set to null
        session.messageFlow(queueName, MessageCreditUnit.BYTE, Session.UNLIMITED_CREDIT);
        session.messageFlow(queueName, MessageCreditUnit.MESSAGE, 24);
    }

    public void cancelSubscription(Session session,String dest)
    {
        session.messageCancel(dest);
    }

    public static void main(String[] args) throws InterruptedException
    {
        // Create connection
        Connection con = new Connection();
        con.connect("localhost", 5672, "test", "guest", "guest",false);

        // Create session
        Session session = con.createSession(0);

        // Create an instance of the listener
        TopicListener listener = new TopicListener();
        session.setSessionListener(listener);

        listener.prepareQueue(session,"usa", "usa.#");
        listener.prepareQueue(session,"europe", "europe.#");
        listener.prepareQueue(session,"news", "#.news");
        listener.prepareQueue(session,"weather", "#.weather");

        // confirm completion
        session.sync();

        System.out.println("Waiting 100 seconds for messages");
        Thread.sleep(100*1000);

        System.out.println("Shutting down listeners");
        listener.cancelSubscription(session,"usa");
        listener.cancelSubscription(session,"europe");
        listener.cancelSubscription(session,"news");
        listener.cancelSubscription(session,"weather");

        //cleanup
        session.close();
        con.close();
    }

}
