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
package org.apache.qpid.server.store;

import org.apache.qpid.server.util.InternalBrokerBaseCase;
import org.apache.qpid.server.protocol.InternalTestProtocolSession;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;

import java.util.List;

public class MessageStoreShutdownTest extends InternalBrokerBaseCase
{

    public void test()
    {
        subscribe(_session, _channel, _queue);

        try
        {
            publishMessages(_session, _channel, 1);
        }
        catch (AMQException e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            fail(e.getMessage());
        }

        try
        {
            _registry.close();
        }
        catch (Exception e)
        {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            fail(e.getMessage());
        }

        assertTrue("Session should now be closed", _session.isClosed());


        //Test attempting to modify the broker state after session has been closed.

        //The Message should have been removed from the unacked list.

        //Ack Messages
        List<InternalTestProtocolSession.DeliveryPair> list = _session.getDelivers(_channel.getChannelId(), new AMQShortString("sgen_1"), 1);

        InternalTestProtocolSession.DeliveryPair pair = list.get(0);

        try
        {
            // The message should now be requeued and so unable to ack it.
            _channel.acknowledgeMessage(pair.getDeliveryTag(), false);
        }
        catch (AMQException e)
        {
            assertEquals("Incorrect exception thrown", "Single ack on delivery tag 1 not known for channel:1", e.getMessage());
        }

    }

}
