package org.apache.qpid.example.amqpexample.headers;
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
import org.apache.qpid.transport.Session;

import java.util.Map;
import java.util.HashMap;


public class DeclareQueue
{

    /**
     * Creates 2 queues and bind them to an headers exchange. One queue receives messages with both
     * properties H1 and H2 and the other queue receives messages with either one of those properties.
     */
    public static void main(String[] args)
    {
        // Create connection
        Connection con = new Connection();
        con.connect("localhost", 5672, "test", "guest", "guest",false);

        // Create session
        Session session = con.createSession(0);

        // declare and bind queues
        session.queueDeclare("headers_queue_any", null, null);
        session.queueDeclare("headers_queue_all", null, null);
        // we need to declare the header: name, type, alternate exchange
        session.exchangeDeclare("test.headers", "headers", "amq.direct", null);
        // The matching algorithm is controlled by 'x-match' property
        // 'x-match' can take one of two values,
        // (i) 'all' implies that all the other pairs must match the headers
        // property of a message for that message to be routed (i.e. an AND match)
        // (ii) 'any' implies that the message should be routed if any of the
        // fields in the headers property match one of the fields in the arguments table (i.e. an OR match)
        Map<String, Object> arguments = new HashMap<String, Object>();
        arguments.put("x-match", "any");
        arguments.put("h1", "v1");
        arguments.put("h2", "v2");
        session.exchangeBind("headers_queue_any", "test.headers", "useless", arguments);
        arguments = new HashMap<String, Object>();
        arguments.put("x-match", "all");
        arguments.put("h1", "v1");
        arguments.put("h2", "v2");
        session.exchangeBind("headers_queue_all", "test.headers", "useless", arguments);
        // confirm completion
        session.sync();
        //cleanup
        session.close();
        con.close();
    }
}
