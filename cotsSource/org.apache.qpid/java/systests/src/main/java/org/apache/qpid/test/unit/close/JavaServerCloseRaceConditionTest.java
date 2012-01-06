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
package org.apache.qpid.test.unit.close;

import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQDestination;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.framing.AMQFrame;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.ExchangeDeclareBody;
import org.apache.qpid.framing.ExchangeDeclareOkBody;
import org.apache.qpid.test.utils.QpidTestCase;

import javax.jms.Session;

/** QPID-1809
 *
 * Race condition on error handling and close logic.
 *
 * See most often with SimpleACLTest as this test is the expects the server to
 * shut the connection/channels. This sort of testing is not performed by many,
 * if any, of the other system tests.
 *
 * The problem is that we have two threads
 *
 *  MainThread              Exception(Mina)Thread
 *     |                          |
 *    Performs                    |
 *     ACtion                     |
 *     |                      Receives Server
 *     |                        Close
 *  Blocks for                    |
 *    Response                    |
 *      |                     Starts To Notify
 *      |                        client
 *      |                         |
 *      |             <----- Notify Main Thread
 *    Notification                |
 *     wakes client               |
 *      |                         |
 *     Client then                |
 * processes Error.               |
 *      |                         |
 *   Potentially Attempting      Close Channel/Connection
 *      Connection Close
 *
 * The two threads both attempt to close the connection but the main thread does
 * so assuming that the connection is open and valid.
 *
 * The Exception thread must modify the connection so that no furter syncWait
 * commands are performed.
 *
 * This test sends an ExchangeDeclare that is Asynchronous and will fail and
 * so cause a ChannelClose error but we perform a syncWait so that we can be
 * sure to test that the BlockingWaiter is correctly awoken.
 *  
 */
public class JavaServerCloseRaceConditionTest extends QpidTestCase
{
    private static final String EXCHANGE_NAME = "NewExchangeNametoFailLookup";

    public void test() throws Exception
    {

        AMQConnection connection = (AMQConnection) getConnection();

        AMQSession session = (AMQSession) connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        // Set no wait true so that we block the connection
        // Also set a different exchange class string so the attempt to declare
        // the exchange causes an exchange. 
        ExchangeDeclareBody body = session.getMethodRegistry().createExchangeDeclareBody(session.getTicket(), new AMQShortString(EXCHANGE_NAME), null,
                                                                                         true, false, false, false, true, null);

        AMQFrame exchangeDeclare = body.generateFrame(session.getChannelId());

        try
        {
            // block our thread so that can times out
            connection.getProtocolHandler().syncWrite(exchangeDeclare, ExchangeDeclareOkBody.class);
        }
        catch (Exception e)
        {
            assertTrue("Exception should say the exchange is not known.", e.getMessage().contains("Unknown exchange: " + EXCHANGE_NAME));
        }

        try
        {
            // Depending on if the notification thread has closed the connection
            // or not we may get an exception here when we attempt to close the
            // connection. If we do get one then it should be the same as above
            // an AMQAuthenticationException.
            connection.close();
        }
        catch (Exception e)
        {
            assertTrue("Exception should say the exchange is not known.", e.getMessage().contains("Unknown exchange: " + EXCHANGE_NAME));
        }

    }
}
