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
package org.apache.qpid.test.unit.basic;

import javax.jms.Connection;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.test.utils.QpidTestCase;

/**
 * @author Apache Software Foundation
 */
public class PubSubTwoConnectionTest extends QpidTestCase
{
    protected void setUp() throws Exception
    {
        super.setUp();
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    /**
     * This tests that a consumer is set up synchronously
     * @throws Exception
     */
    public void testTwoConnections() throws Exception
    {

        AMQConnection con1 = (AMQConnection) getConnection("guest", "guest");

        Topic topic = new AMQTopic(con1, "MyTopic");

        Session session1 = con1.createSession(false, AMQSession.NO_ACKNOWLEDGE);
        MessageProducer producer = session1.createProducer(topic);

        Connection con2 = (AMQConnection) getConnection("guest", "guest") ;
        Session session2 = con2.createSession(false, AMQSession.NO_ACKNOWLEDGE);
        MessageConsumer consumer = session2.createConsumer(topic);
        con2.start();        
        producer.send(session1.createTextMessage("Hello"));
        TextMessage tm1 = (TextMessage) consumer.receive(2000);
        assertNotNull(tm1);
        assertEquals("Hello", tm1.getText());
        con1.close();
        con2.close();
    }
}
