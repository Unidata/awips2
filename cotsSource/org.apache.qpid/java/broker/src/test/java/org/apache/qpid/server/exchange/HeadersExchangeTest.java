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
package org.apache.qpid.server.exchange;

import org.apache.qpid.AMQException;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.protocol.InternalTestProtocolSession;
import org.apache.qpid.server.protocol.AMQProtocolSession;

public class HeadersExchangeTest extends AbstractHeadersExchangeTestBase
{
    AMQProtocolSession _protocolSession;

    protected void setUp() throws Exception
    {
        super.setUp();
        // AR will use the NullAR by default
        // Just use the first vhost.
        VirtualHost
                virtualHost = ApplicationRegistry.getInstance().getVirtualHostRegistry().getVirtualHosts().iterator().next();
        _protocolSession = new InternalTestProtocolSession(virtualHost);
    }

    protected void tearDown()
    {
        // Correctly Close the AR that we created above
        ApplicationRegistry.remove();
    }

    public void testSimple() throws AMQException
    {
        TestQueue q1 = bindDefault("F0000");
        TestQueue q2 = bindDefault("F0000=Aardvark");
        TestQueue q3 = bindDefault("F0001");
        TestQueue q4 = bindDefault("F0001=Bear");
        TestQueue q5 = bindDefault("F0000", "F0001");
        TestQueue q6 = bindDefault("F0000=Aardvark", "F0001=Bear");
        TestQueue q7 = bindDefault("F0000", "F0001=Bear");
        TestQueue q8 = bindDefault("F0000=Aardvark", "F0001");

        routeAndTest(new Message(_protocolSession, "Message1", "F0000"), q1);
        routeAndTest(new Message(_protocolSession, "Message2", "F0000=Aardvark"), q1, q2);
        routeAndTest(new Message(_protocolSession, "Message3", "F0000=Aardvark", "F0001"), q1, q2, q3, q5, q8);
        routeAndTest(new Message(_protocolSession, "Message4", "F0000", "F0001=Bear"), q1, q3, q4, q5, q7);
        routeAndTest(new Message(_protocolSession, "Message5", "F0000=Aardvark", "F0001=Bear"),
                     q1, q2, q3, q4, q5, q6, q7, q8);
        routeAndTest(new Message(_protocolSession, "Message6", "F0002"));

        Message m7 = new Message(_protocolSession, "Message7", "XXXXX");

        MessagePublishInfoImpl pb7 = (MessagePublishInfoImpl) (m7.getMessagePublishInfo());
        pb7.setMandatory(true);
        routeAndTest(m7,true);

        Message m8 = new Message(_protocolSession, "Message8", "F0000");
        MessagePublishInfoImpl pb8 = (MessagePublishInfoImpl)(m8.getMessagePublishInfo());
        pb8.setMandatory(true);
        routeAndTest(m8,false,q1);


    }

    public void testAny() throws AMQException
    {
        TestQueue q1 = bindDefault("F0000", "F0001", "X-match=any");
        TestQueue q2 = bindDefault("F0000=Aardvark", "F0001=Bear", "X-match=any");
        TestQueue q3 = bindDefault("F0000", "F0001=Bear", "X-match=any");
        TestQueue q4 = bindDefault("F0000=Aardvark", "F0001", "X-match=any");
        TestQueue q6 = bindDefault("F0000=Apple", "F0001", "X-match=any");

        routeAndTest(new Message(_protocolSession, "Message1", "F0000"), q1, q3);
        routeAndTest(new Message(_protocolSession, "Message2", "F0000=Aardvark"), q1, q2, q3, q4);
        routeAndTest(new Message(_protocolSession, "Message3", "F0000=Aardvark", "F0001"), q1, q2, q3, q4, q6);
        routeAndTest(new Message(_protocolSession, "Message4", "F0000", "F0001=Bear"), q1, q2, q3, q4, q6);
        routeAndTest(new Message(_protocolSession, "Message5", "F0000=Aardvark", "F0001=Bear"), q1, q2, q3, q4, q6);
        routeAndTest(new Message(_protocolSession, "Message6", "F0002"));
    }

    public void testMandatory() throws AMQException
    {
        bindDefault("F0000");
        Message m1 = new Message(_protocolSession, "Message1", "XXXXX");
        Message m2 = new Message(_protocolSession, "Message2", "F0000");
        MessagePublishInfoImpl pb1 = (MessagePublishInfoImpl) (m1.getMessagePublishInfo());
        pb1.setMandatory(true);
        MessagePublishInfoImpl pb2 = (MessagePublishInfoImpl) (m2.getMessagePublishInfo());
        pb2.setMandatory(true);
        routeAndTest(m1,true);
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(HeadersExchangeTest.class);
    }
}
