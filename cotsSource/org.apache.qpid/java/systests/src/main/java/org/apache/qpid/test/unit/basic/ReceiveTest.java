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
package org.apache.qpid.test.unit.basic;

import javax.jms.MessageConsumer;
import javax.jms.Message;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.utils.QpidTestCase;

public class ReceiveTest extends QpidTestCase
{
    private AMQConnection _connection;
    private AMQDestination _destination;
    private AMQSession _session;
    private MessageConsumer _consumer;

    protected void setUp() throws Exception
    {
        super.setUp();
       init((AMQConnection) getConnection("guest", "guest"));
    }

   protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    private void init(AMQConnection connection) throws Exception
    {
        init(connection, new AMQQueue(connection,"ReceiveTest", true));
    }

    private void init(AMQConnection connection, AMQDestination destination) throws Exception
    {
        _connection = connection;
        _destination = destination;
        _session = (AMQSession) connection.createSession(true, AMQSession.NO_ACKNOWLEDGE);
        _consumer = _session.createConsumer(_destination);
        _connection.start();
    }

    public void test() throws Exception
    {
        Message m = _consumer.receive(5000);
        assertNull("should not have received a message", m);
        _connection.close();
    }


    public static junit.framework.Test suite()
    {
        // TODO: note that this test doesn't use the VMBrokerSetup
        // test helper class to create and tear down its
        // VMBroker. This is because the main() above seems to
        // indicate that it's also used outside of the surefire test
        // framework. If it isn't, then this test should also be
        // changed to use VMBrokerSetup here.
        return new junit.framework.TestSuite(ReceiveTest.class);
    }
}
