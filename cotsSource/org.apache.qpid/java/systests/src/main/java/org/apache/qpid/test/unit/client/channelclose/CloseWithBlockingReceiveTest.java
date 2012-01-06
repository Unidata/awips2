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
package org.apache.qpid.test.unit.client.channelclose;

import javax.jms.MessageConsumer;
import javax.jms.Session;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.test.utils.QpidTestCase;

/**
 * @author Apache Software Foundation
 */
public class CloseWithBlockingReceiveTest extends QpidTestCase
{


    protected void setUp() throws Exception
    {
        super.setUp();
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }


    public void testReceiveReturnsNull() throws Exception
    {
        final AMQConnection connection =  (AMQConnection) getConnection("guest", "guest");
        Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
        MessageConsumer consumer = session.createConsumer(new AMQTopic(connection, "banana"));
        connection.start();

        Runnable r = new Runnable()
        {

            public void run()
            {
                try
                {
                    Thread.sleep(1000);
                    connection.close();
                }
                catch (Exception e)
                {
                }
            }
        };
        long startTime = System.currentTimeMillis();
        new Thread(r).start();
        consumer.receive(10000);
        assertTrue(System.currentTimeMillis() - startTime < 10000);
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(CloseWithBlockingReceiveTest.class);
    }

}
