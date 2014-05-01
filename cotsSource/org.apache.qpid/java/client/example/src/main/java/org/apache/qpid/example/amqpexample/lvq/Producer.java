package org.apache.qpid.example.amqpexample.lvq;
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

import java.util.Map;
import java.util.HashMap;
import org.apache.qpid.transport.*;

public class Producer
{

    public static void main(String[] args)
    {
        // Create connection
        Connection con = new Connection();
        con.connect("localhost", 5672, "test", "guest", "guest",false);

        // Create session
        Session session = con.createSession(0);
        DeliveryProperties deliveryProps = new DeliveryProperties();
        deliveryProps.setRoutingKey("routing_key");

        // set message headers
        MessageProperties messageProperties = new MessageProperties();
        Map<String, Object> messageHeaders = new HashMap<String, Object>();
        // set the message key
        messageHeaders.put("qpid.LVQ_key", "test");
        messageProperties.setApplicationHeaders(messageHeaders);

        Header header = new Header(deliveryProps, messageProperties);

        for (int i=0; i<10; i++)
        {
            session.messageTransfer("amq.direct", MessageAcceptMode.EXPLICIT,MessageAcquireMode.PRE_ACQUIRED,
                                    header,
                                    "Message " + i);
        }

        session.messageTransfer("amq.direct", MessageAcceptMode.EXPLICIT,MessageAcquireMode.PRE_ACQUIRED,
                                     header,
                                     "That's all, folks!");

        // confirm completion
        session.sync();

        //cleanup
        session.close();
        con.close();
    }

}
