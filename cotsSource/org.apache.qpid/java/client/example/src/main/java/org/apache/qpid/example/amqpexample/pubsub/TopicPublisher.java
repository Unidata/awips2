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


import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.DeliveryProperties;
import org.apache.qpid.transport.Header;
import org.apache.qpid.transport.MessageAcceptMode;
import org.apache.qpid.transport.MessageAcquireMode;
import org.apache.qpid.transport.Session;

public class TopicPublisher
{

    public void publishMessages(Session session, String routing_key)
    {
      // Set the routing key once, we'll use the same routing key for all
      // messages.

      DeliveryProperties deliveryProps =  new DeliveryProperties();
      deliveryProps.setRoutingKey(routing_key);

      for (int i=0; i<5; i++) {
          session.messageTransfer("amq.topic", MessageAcceptMode.EXPLICIT, MessageAcquireMode.PRE_ACQUIRED,
                                  new Header(deliveryProps), "Message " + i);
      }

    }

    public void noMoreMessages(Session session)
    {
        session.messageTransfer("amq.topic", MessageAcceptMode.EXPLICIT, MessageAcquireMode.PRE_ACQUIRED,
                                new Header(new DeliveryProperties().setRoutingKey("control")),
                                "That's all, folks!");
    }

    public static void main(String[] args)
    {
        // Create connection
        Connection con = new Connection();
        con.connect("localhost", 5672, "test", "guest", "guest",false);

        // Create session
        Session session = con.createSession(0);

        // Create an instance of the listener
        TopicPublisher publisher = new TopicPublisher();

        publisher.publishMessages(session, "usa.news");
        publisher.publishMessages(session, "usa.weather");
        publisher.publishMessages(session, "europe.news");
        publisher.publishMessages(session, "europe.weather");

        // confirm completion
        session.sync();

        //cleanup
        session.close();
        con.close();
    }
}
