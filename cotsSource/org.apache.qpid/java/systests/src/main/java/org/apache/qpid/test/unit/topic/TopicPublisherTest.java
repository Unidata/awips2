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
package org.apache.qpid.test.unit.topic;

import javax.jms.MessageConsumer;
import javax.jms.TextMessage;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.test.utils.QpidTestCase;

/**
 * @author Apache Software Foundation
 */
public class TopicPublisherTest extends QpidTestCase
{
    protected void setUp() throws Exception
    {
        super.setUp();
    }

    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testUnidentifiedProducer() throws Exception
    {

        AMQConnection con =  (AMQConnection) getConnection("guest", "guest");
        AMQTopic topic = new AMQTopic(con,"MyTopic");
        TopicSession session1 = con.createTopicSession(false, AMQSession.NO_ACKNOWLEDGE);
        TopicPublisher publisher = session1.createPublisher(null);
        MessageConsumer consumer1 = session1.createConsumer(topic);
        con.start();
        publisher.publish(topic, session1.createTextMessage("Hello"));
        TextMessage m = (TextMessage) consumer1.receive(2000);
        assertNotNull(m);
        try
        {
            publisher.publish(session1.createTextMessage("Goodbye"));
            fail("Did not throw UnsupportedOperationException");
        }
        catch (UnsupportedOperationException e)
        {
            // PASS
        }
        con.close();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(TopicPublisherTest.class);
    }
}
