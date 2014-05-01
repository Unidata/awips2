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
package org.apache.qpid.test.unit.client.connection;

import org.apache.qpid.AMQException;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.url.URLSyntaxException;

import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import java.util.concurrent.CountDownLatch;

public class CloseAfterConnectionFailureTest extends QpidTestCase implements ExceptionListener
{
    private int sessionCount = 0;
    AMQConnection connection;
    Session session;
    MessageConsumer consumer;
    private CountDownLatch _latch = new CountDownLatch(1);
    private JMSException _fail;

    public void testNoFailover() throws URLSyntaxException, Exception,
                                        InterruptedException, JMSException
    {
        //This test uses hard coded connection string so only runs on InVM case
        if (!isExternalBroker())
        {
            String connectionString = "amqp://guest:guest@/test?brokerlist='vm://:1?connectdelay='500',retries='3'',failover='nofailover'";

            AMQConnectionURL url = new AMQConnectionURL(connectionString);

            try
            {
                //Start the connection so it will use the retries
                connection = new AMQConnection(url, null);

                connection.setExceptionListener(this);

                session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);
                consumer = session.createConsumer(session.createQueue(this.getName()));

                //Kill connection
                stopBroker();

                _latch.await();

                if (_fail != null)
                {
                    _fail.printStackTrace(System.out);
                    fail("Exception thrown:" + _fail.getMessage());
                }
            }
            catch (AMQException e)
            {
                fail(e.getMessage());
            }
        }
    }

    public void onException(JMSException e)
    {
        System.out.println("Connection isClosed after connection Falure?:" + connection.isClosed());
        try
        {
            consumer.close();
        }
        catch (JMSException jmse)
        {
            System.out.println("Consumer close failed with:" + jmse.getMessage());
            _fail = jmse;
        }
        System.out.println("Connection isClosed after connection Falure?:" + connection.isClosed());
        try
        {
            //Note that if we actually do session.close() we will lock up as the session will never receive a frame
            // from the
            ((AMQSession) session).close(10);
        }
        catch (JMSException jmse)
        {
            System.out.println("Session close failed with:" + jmse.getMessage());
            _fail = jmse;
        }
        System.out.println("Connection isClosed after connection Falure?:" + connection.isClosed());

        try
        {
            connection.close();
        }
        catch (JMSException jmse)
        {
            System.out.println("Session close failed with:" + jmse.getMessage());
            _fail = jmse;
        }
        System.out.println("Connection isClosed after connection Falure?:" + connection.isClosed());

        _latch.countDown();

    }

}
