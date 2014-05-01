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

import org.apache.qpid.AMQConnectionFailureException;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQUnresolvedAddressException;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.test.utils.QpidTestCase;
import org.apache.qpid.client.AMQAuthenticationException;
import org.apache.qpid.client.AMQConnection;
import org.apache.qpid.client.AMQQueue;
import org.apache.qpid.client.AMQSession;
import org.apache.qpid.client.AMQTopic;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.jms.Session;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.jms.BrokerDetails;

import javax.jms.Connection;
import javax.jms.QueueSession;
import javax.jms.TopicSession;
import javax.naming.NamingException;

public class ConnectionTest extends QpidTestCase
{

    String _broker_NotRunning = "vm://:2";
    String _broker_BadDNS = "tcp://hg3sgaaw4lgihjs";

    public void testSimpleConnection() throws Exception
    {
        AMQConnection conn = null;
        try
        {
            conn = new AMQConnection(getBroker().toString(), "guest", "guest", "fred", "test");
        }
        catch (Exception e)
        {
            fail("Connection to " + getBroker() + " should succeed. Reason: " + e);
        }
        finally
        {
            conn.close();
        }
    }

    public void testDefaultExchanges() throws Exception
    {
        AMQConnection conn = null;
        try
        {
            BrokerDetails broker = getBroker();
            broker.setProperty(BrokerDetails.OPTIONS_RETRY, "1");
            ConnectionURL url = new AMQConnectionURL("amqp://guest:guest@clientid/test?brokerlist='"
                                     + broker
                                     + "'&defaultQueueExchange='test.direct'"
                                     + "&defaultTopicExchange='test.topic'"
                                     + "&temporaryQueueExchange='tmp.direct'"
                                     + "&temporaryTopicExchange='tmp.topic'");

            System.err.println(url.toString());
            conn = new AMQConnection(url, null);


            AMQSession sess = (AMQSession) conn.createSession(false, Session.AUTO_ACKNOWLEDGE);
            
            sess.declareExchange(new AMQShortString("test.direct"), 
                    ExchangeDefaults.DIRECT_EXCHANGE_CLASS, false);

            sess.declareExchange(new AMQShortString("tmp.direct"), 
                    ExchangeDefaults.DIRECT_EXCHANGE_CLASS, false);

            sess.declareExchange(new AMQShortString("tmp.topic"), 
                    ExchangeDefaults.TOPIC_EXCHANGE_CLASS, false);

            sess.declareExchange(new AMQShortString("test.topic"), 
                    ExchangeDefaults.TOPIC_EXCHANGE_CLASS, false);

            QueueSession queueSession = conn.createQueueSession(false, Session.AUTO_ACKNOWLEDGE);

            AMQQueue queue = (AMQQueue) queueSession.createQueue("MyQueue");

            assertEquals(queue.getExchangeName().toString(), "test.direct");

            AMQQueue tempQueue = (AMQQueue) queueSession.createTemporaryQueue();

            assertEquals(tempQueue.getExchangeName().toString(), "tmp.direct");

            queueSession.close();

            TopicSession topicSession = conn.createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
            
            AMQTopic topic = (AMQTopic) topicSession.createTopic("silly.topic");

            assertEquals(topic.getExchangeName().toString(), "test.topic");

            AMQTopic tempTopic = (AMQTopic) topicSession.createTemporaryTopic();

            assertEquals(tempTopic.getExchangeName().toString(), "tmp.topic");

            topicSession.close();

        }
        catch (Exception e)
        {
            fail("Connection to " + getBroker() + " should succeed. Reason: " + e);
        }
        finally
        {
            conn.close();
        }
    }

    public void testPasswordFailureConnection() throws Exception
    {
        AMQConnection conn = null;
        try
        {
            BrokerDetails broker = getBroker();
            broker.setProperty(BrokerDetails.OPTIONS_RETRY, "0");
            conn = new AMQConnection("amqp://guest:rubbishpassword@clientid/test?brokerlist='" + broker + "'");
            fail("Connection should not be established password is wrong.");
        }
        catch (AMQConnectionFailureException amqe)
        {
            assertNotNull("No cause set:" + amqe.getMessage(), amqe.getCause());
            assertEquals("Exception was wrong type", AMQAuthenticationException.class, amqe.getCause().getClass());
        }
        finally
        {
            if (conn != null)
            {
                conn.close();
            }
        }
    }

    public void testConnectionFailure() throws Exception
    {
        AMQConnection conn = null;
        try
        {
            conn = new AMQConnection("amqp://guest:guest@clientid/testpath?brokerlist='" + _broker_NotRunning + "?retries='0''");
            fail("Connection should not be established");
        }
        catch (AMQException amqe)
        {
            if (!(amqe instanceof AMQConnectionFailureException))
            {
                fail("Correct exception not thrown. Excpected 'AMQConnectionException' got: " + amqe);
            }
        }
        finally
        {
            if (conn != null)
            {
                conn.close();
            }
        }

    }

    public void testUnresolvedHostFailure() throws Exception
    {
        AMQConnection conn = null;
        try
        {
            conn = new AMQConnection("amqp://guest:guest@clientid/testpath?brokerlist='" + _broker_BadDNS + "?retries='0''");
            fail("Connection should not be established");
        }
        catch (AMQException amqe)
        {
            if (!(amqe instanceof AMQUnresolvedAddressException))
            {
                fail("Correct exception not thrown. Excpected 'AMQUnresolvedAddressException' got: " + amqe);
            }
        }
        finally
        {
            if (conn != null)
            {
                conn.close();
            }
        }

    }

    public void testUnresolvedVirtualHostFailure() throws Exception
    {
        AMQConnection conn = null;
        try
        {
            BrokerDetails broker = getBroker();
            broker.setProperty(BrokerDetails.OPTIONS_RETRY, "0");
            conn = new AMQConnection("amqp://guest:guest@clientid/rubbishhost?brokerlist='" + broker + "'");
            fail("Connection should not be established");
        }
        catch (AMQException amqe)
        {
            if (!(amqe instanceof AMQConnectionFailureException))
            {
                fail("Correct exception not thrown. Excpected 'AMQConnectionFailureException' got: " + amqe);
            }
        }
        finally
        {
            if (conn != null)
            {
                conn.close();
            }
        }
    }

    public void testClientIdCannotBeChanged() throws Exception
    {
        Connection connection = new AMQConnection(getBroker().toString(), "guest", "guest",
                                                  "fred", "test");
        try
        {
            connection.setClientID("someClientId");
            fail("No IllegalStateException thrown when resetting clientid");
        }
        catch (javax.jms.IllegalStateException e)
        {
            // PASS
        }
        finally
        {
            if (connection != null)
            {
                connection.close();
            }
        }
    }

    public void testClientIdIsPopulatedAutomatically() throws Exception
    {
        Connection connection = new AMQConnection(getBroker().toString(), "guest", "guest",
                                                  null, "test");
        try
        {
            assertNotNull(connection.getClientID());
        }
        finally
        {
            connection.close();
        }
        connection.close();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(ConnectionTest.class);
    }
}
