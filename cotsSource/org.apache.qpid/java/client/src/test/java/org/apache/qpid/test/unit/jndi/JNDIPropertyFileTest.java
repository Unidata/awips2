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
package org.apache.qpid.test.unit.jndi;

import java.util.Properties;

import javax.jms.Queue;
import javax.jms.Topic;
import javax.naming.Context;
import javax.naming.InitialContext;

import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.framing.AMQShortString;

import junit.framework.TestCase;

public class JNDIPropertyFileTest extends TestCase
{
    Context ctx;
    
    public JNDIPropertyFileTest() throws Exception
    {
        Properties properties = new Properties();
        properties.load(this.getClass().getResourceAsStream("JNDITest.properties"));

        //Create the initial context
        ctx = new InitialContext(properties);
    }
    
    public void testQueueNamesWithTrailingSpaces() throws Exception
    {
        Queue queue = (Queue)ctx.lookup("QueueNameWithSpace");
        assertEquals("QueueNameWithSpace",queue.getQueueName()); 
    }
    
    public void testTopicNamesWithTrailingSpaces() throws Exception
    {
        Topic topic = (Topic)ctx.lookup("TopicNameWithSpace");
        assertEquals("TopicNameWithSpace",topic.getTopicName()); 
    }
    
    public void testMultipleTopicNamesWithTrailingSpaces() throws Exception
    {
        Topic topic = (Topic)ctx.lookup("MultipleTopicNamesWithSpace");
        int i = 0;
        for (AMQShortString bindingKey: ((AMQDestination)topic).getBindingKeys())
        {
            i++;
            assertEquals("Topic" + i + "WithSpace",bindingKey.asString());            
        }
    }
}
