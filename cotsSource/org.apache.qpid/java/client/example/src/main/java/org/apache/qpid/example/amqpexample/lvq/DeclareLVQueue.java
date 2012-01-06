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


import org.apache.qpid.transport.Connection;
import org.apache.qpid.transport.Session;

import java.util.Map;
import java.util.HashMap;

/**
 *  This creates a queue a LVQueue with key test and binds it to the
 *  amq.direct exchange
 *
 */
public class DeclareLVQueue
{

    public static void main(String[] args)
    {
        // Create connection
        Connection con = new Connection();
        con.connect("localhost", 5672, "test", "guest", "guest",false);

        // Create session
        Session session = con.createSession(0);

        // declare and bind queue
        Map<String, Object> arguments = new HashMap<String, Object>();
        // We use a lvq
        arguments.put("qpid.last_value_queue", true);
        // We want this queue to use the key test
        arguments.put("qpid.LVQ_key", "test");
        session.queueDeclare("message_queue", null, arguments);
        session.exchangeBind("message_queue", "amq.direct", "routing_key", null);

        // confirm completion
        session.sync();

        //cleanup
        session.close();
        con.close();
    }
}

