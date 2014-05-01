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
package org.apache.qpid.test.unit.client.connection;

import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.transport.util.Logger;

import java.util.HashMap;
import java.util.Map;

import javax.jms.Connection;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.TextMessage;

/**
 * ConnectionCloseTest
 *
 */

public class ConnectionCloseTest extends QpidTestCase
{

    private static final Logger log = Logger.get(ConnectionCloseTest.class);

    public void testSendReceiveClose() throws Exception
    {
        Map<Thread,StackTraceElement[]> before = Thread.getAllStackTraces();

        for (int i = 0; i < 500; i++)
        {
            if ((i % 10) == 0)
            {
                log.warn("%d messages sent and received", i);
            }

            Connection receiver = getConnection();
            receiver.start();
            Session rssn = receiver.createSession(false, Session.AUTO_ACKNOWLEDGE);
            Queue queue = rssn.createQueue("connection-close-test-queue");
            MessageConsumer cons = rssn.createConsumer(queue);

            Connection sender = getConnection();
            sender.start();
            Session sssn = sender.createSession(false, Session.AUTO_ACKNOWLEDGE);
            MessageProducer prod = sssn.createProducer(queue);
            prod.send(sssn.createTextMessage("test"));
            sender.close();

            TextMessage m = (TextMessage) cons.receive(2000);
            assertNotNull("message was lost", m);
            assertEquals(m.getText(), "test");
            receiver.close();
        }

        // The finalizer is notifying connector thread waiting on a selector key.
        // This should leave the finalizer enough time to notify those threads 
        synchronized (this)
        {
            this.wait(10000);
        }

        Map<Thread,StackTraceElement[]> after = Thread.getAllStackTraces();

        Map<Thread,StackTraceElement[]> delta = new HashMap<Thread,StackTraceElement[]>(after);
        for (Thread t : before.keySet())
        {
            delta.remove(t);
        }

        dumpStacks(delta);

        assertTrue("Spurious thread creation exceeded threshold, " +
                   delta.size() + " threads created.",
                   delta.size() < 10);
    }

    private void dumpStacks(Map<Thread,StackTraceElement[]> map)
    {
        for (Map.Entry<Thread,StackTraceElement[]> entry : map.entrySet())
        {
            Throwable t = new Throwable();
            t.setStackTrace(entry.getValue());
            log.warn(t, entry.getKey().toString());
        }
    }

}
