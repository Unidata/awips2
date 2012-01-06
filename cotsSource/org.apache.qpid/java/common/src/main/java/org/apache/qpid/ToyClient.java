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
package org.apache.qpid;

import java.nio.*;
import java.util.*;

import org.apache.qpid.transport.*;
import org.apache.qpid.transport.network.mina.MinaHandler;


/**
 * ToyClient
 *
 * @author Rafael H. Schloming
 */

class ToyClient implements SessionListener
{
    public void opened(Session ssn) {}

    public void resumed(Session ssn) {}

    public void exception(Session ssn, SessionException exc)
    {
        exc.printStackTrace();
    }

    public void message(Session ssn, MessageTransfer xfr)
    {
        System.out.println("msg: " + xfr);
    }

    public void closed(Session ssn) {}

    public static final void main(String[] args)
    {
        Connection conn = new Connection();
        conn.connect("0.0.0.0", 5672, null, "guest", "guest", false);
        Session ssn = conn.createSession();
        ssn.setSessionListener(new ToyClient());

        ssn.queueDeclare("asdf", null, null);
        ssn.sync();

        Map<String,Object> nested = new LinkedHashMap<String,Object>();
        nested.put("list", Arrays.asList("one", "two", "three"));
        Map<String,Object> map = new LinkedHashMap<String,Object>();

        map.put("str", "this is a string");

        map.put("+int", 3);
        map.put("-int", -3);
        map.put("maxint", Integer.MAX_VALUE);
        map.put("minint", Integer.MIN_VALUE);

        map.put("+short", (short) 1);
        map.put("-short", (short) -1);
        map.put("maxshort", (short) Short.MAX_VALUE);
        map.put("minshort", (short) Short.MIN_VALUE);

        map.put("float", (float) 3.3);
        map.put("double", 4.9);
        map.put("char", 'c');

        map.put("table", nested);
        map.put("list", Arrays.asList(1, 2, 3));
        map.put("binary", new byte[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});

        ssn.messageTransfer("asdf", MessageAcceptMode.EXPLICIT,
                            MessageAcquireMode.PRE_ACQUIRED,
                            new Header(new DeliveryProperties(),
                                       new MessageProperties()
                                       .setApplicationHeaders(map)),
                            "this is the data");

        ssn.messageTransfer("fdsa", MessageAcceptMode.EXPLICIT,
                            MessageAcquireMode.PRE_ACQUIRED,
                            null,
                            "this should be rejected");
        ssn.sync();

        Future<QueueQueryResult> future = ssn.queueQuery("asdf");
        System.out.println(future.get().getQueue());
        ssn.sync();
        ssn.close();
        conn.close();
    }

}
