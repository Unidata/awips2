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
import java.util.Map;
import java.util.HashMap;


public class Producer
{
    /**
     * Sends 10 messages with a single property and 10 messages
     * with 2 properties to a headers exchange.  
     */
    public static void main(String[] args)
    {      
            // Create connection
            org.apache.qpid.transport.Connection con = new org.apache.qpid.transport.Connection();
            con.connect("localhost", 5672, "test", "guest", "guest",false);

            // Create session
            org.apache.qpid.transport.Session session = con.createSession(0);
            DeliveryProperties deliveryProps = new DeliveryProperties();

            // set message headers
            MessageProperties messageProperties = new MessageProperties();
            Map<String, Object> messageHeaders = new HashMap<String, Object>();
            // set the message property
            messageHeaders.put("h1", "v1");
            messageProperties.setApplicationHeaders(messageHeaders);
            Header header = new Header(deliveryProps, messageProperties);

            for (int i=0; i<10; i++)
            {
                session.messageTransfer("test.headers", MessageAcceptMode.EXPLICIT,MessageAcquireMode.PRE_ACQUIRED,
                                        header,
                                        "Message H1: " + i);
            }

            // set message headers
            messageProperties = new MessageProperties();
            messageHeaders = new HashMap<String, Object>();
            // set the message properties
            messageHeaders.put("h1", "v1");
            messageHeaders.put("h2", "v2");
            messageProperties.setApplicationHeaders(messageHeaders);
            header = new Header(deliveryProps, messageProperties);

            for (int i=0; i<10; i++)
            {
                session.messageTransfer("test.headers", MessageAcceptMode.EXPLICIT,MessageAcquireMode.PRE_ACQUIRED,
                                        header,
                                        "Message H1 and H2: " + i);
            }


            session.messageTransfer("test.headers", MessageAcceptMode.EXPLICIT,MessageAcquireMode.PRE_ACQUIRED,
                                              header,
                                              "That's all, folks!" );

            // confirm completion
            session.sync();

            //cleanup
            session.close();
            con.close();
    }

}
