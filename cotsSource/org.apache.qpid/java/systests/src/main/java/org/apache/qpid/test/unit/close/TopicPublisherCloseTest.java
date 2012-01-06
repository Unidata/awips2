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
 */package org.apache.qpid.test.unit.close;

import javax.jms.Session;
import javax.jms.Topic;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.test.utils.QpidTestCase;

/**
 * @author Apache Software Foundation
 */
public class TopicPublisherCloseTest extends QpidTestCase
{

    protected void setUp() throws Exception
    {
        super.setUp();
    }


    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testAllMethodsThrowAfterConnectionClose() throws Exception
    {
        // give external brokers a chance to start up
        Thread.sleep(3000);

        AMQConnection connection =   (AMQConnection) getConnection("guest", "guest");

        Topic destination1 = new AMQTopic(connection, "t1");
        TopicSession session1 = connection.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
        TopicPublisher pub = session1.createPublisher(destination1);
        connection.close();
        try
        {
            pub.getDeliveryMode();
            fail("Expected exception not thrown");
        }
        catch (javax.jms.IllegalStateException e)
        {
            // PASS
        }
    }
}
