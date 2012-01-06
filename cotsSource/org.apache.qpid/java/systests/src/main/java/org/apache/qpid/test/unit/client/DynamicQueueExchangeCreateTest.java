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
package org.apache.qpid.test.unit.client;

import org.apache.qpid.test.utils.QpidTestCase;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.Session;

/**
 * QPID-155
 *
 * Test to validate that setting the respective qpid.declare_queues,
 * qpid.declare_exchanges system properties functions as expected.
 * 
 */
public class DynamicQueueExchangeCreateTest extends QpidTestCase
{

    public void testQueueDeclare() throws Exception
    {
        setSystemProperty("qpid.declare_queues", "false");

        Connection connection = getConnection();

        Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        Queue queue = session.createQueue(getTestQueueName());

        try
        {
            session.createConsumer(queue);
            fail("JMSException should be thrown as the queue does not exist");
        }
        catch (JMSException e)
        {           
            assertTrue("Exception should be that the queue does not exist :" +
                       e.getMessage(),
                       e.getMessage().contains("does not exist"));

        }
    }

    public void testExchangeDeclare() throws Exception
    {
        setSystemProperty("qpid.declare_exchanges", "false");

        Connection connection = getConnection();

        Session session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        String EXCHANGE_TYPE = "test.direct";
        Queue queue = session.createQueue("direct://" + EXCHANGE_TYPE + "/queue/queue");

        try
        {
            session.createConsumer(queue);
            fail("JMSException should be thrown as the exchange does not exist");
        }
        catch (JMSException e)
        {
            assertTrue("Exception should be that the exchange does not exist :" +
                       e.getMessage(),
                       e.getMessage().contains("Exchange " + EXCHANGE_TYPE + " does not exist"));
        }
    }

}
