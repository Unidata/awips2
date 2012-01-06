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
package org.apache.qpid.test.unit.client.connectionurl;

import junit.framework.TestCase;

import org.apache.qpid.client.AMQBrokerDetails;
import org.apache.qpid.client.AMQConnectionURL;
import org.apache.qpid.jms.BrokerDetails;
import org.apache.qpid.jms.ConnectionURL;
import org.apache.qpid.url.URLSyntaxException;

public class ConnectionURLTest extends TestCase
{

    public void testFailoverURL() throws URLSyntaxException
    {
        String url = "amqp://ritchiem:bob@/test?brokerlist='tcp://localhost:5672;tcp://fancyserver:3000/',failover='roundrobin?cyclecount='100''";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod().equals("roundrobin"));
        assertEquals("100", connectionurl.getFailoverOption(ConnectionURL.OPTIONS_FAILOVER_CYCLE));        
        assertTrue(connectionurl.getUsername().equals("ritchiem"));
        assertTrue(connectionurl.getPassword().equals("bob"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 2);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));
        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);

        service = connectionurl.getBrokerDetails(1);

        assertTrue(service.getTransport().equals("tcp"));
        assertTrue(service.getHost().equals("fancyserver"));
        assertTrue(service.getPort() == 3000);

    }

    public void testSingleTransportUsernamePasswordURL() throws URLSyntaxException
    {
        String url = "amqp://ritchiem:bob@/test?brokerlist='tcp://localhost:5672'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("ritchiem"));
        assertTrue(connectionurl.getPassword().equals("bob"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));
        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);
    }

    public void testSingleTransportUsernameBlankPasswordURL() throws URLSyntaxException
    {
        String url = "amqp://ritchiem:@/test?brokerlist='tcp://localhost:5672'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("ritchiem"));
        assertTrue(connectionurl.getPassword().equals(""));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));
        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);
    }

    public void testFailedURLNullPassword()
    {
        String url = "amqp://ritchiem@/test?brokerlist='tcp://localhost:5672'";

        try
        {
            new AMQConnectionURL(url);
            fail("URL has null password");
        }
        catch (URLSyntaxException e)
        {
            assertTrue(e.getReason().equals("Null password in user information not allowed."));
            assertTrue(e.getIndex() == 7);
        }
    }


    public void testSingleTransportURL() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='tcp://localhost:5672'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);


        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));


        assertTrue(connectionurl.getBrokerCount() == 1);


        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));
        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);
    }

    public void testSingleTransportWithClientURLURL() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@clientname/test?brokerlist='tcp://localhost:5672'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);


        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));
        assertTrue(connectionurl.getClientName().equals("clientname"));


        assertTrue(connectionurl.getBrokerCount() == 1);


        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));
        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);
    }

    public void testSingleTransport1OptionURL() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='tcp://localhost:5672',routingkey='jim'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));


        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));

        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);
        assertTrue(connectionurl.getOption("routingkey").equals("jim"));
    }

    public void testSingleTransportDefaultedBroker() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='localhost'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));


        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));

        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);
    }

    public void testSingleTransportDefaultedBrokerWithPort() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='localhost:1234'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));


        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));

        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 1234);
    }

    public void testSingleTransportDefaultedBrokerWithIP() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='127.0.0.1'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));


        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));

        assertTrue(service.getHost().equals("127.0.0.1"));
        assertTrue(service.getPort() == 5672);
    }

    public void testSingleTransportDefaultedBrokerWithIPandPort() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='127.0.0.1:1234'";

//        ConnectionURL connectionurl = new AMQConnectionURL(url);
//
//        assertTrue(connectionurl.getFailoverMethod() == null);
//        assertTrue(connectionurl.getUsername().equals("guest"));
//        assertTrue(connectionurl.getPassword().equals("guest"));
//        assertTrue(connectionurl.getVirtualHost().equals("/temp"));
//
//
//        assertTrue(connectionurl.getBrokerCount() == 1);
//
//        BrokerDetails service = connectionurl.getBrokerDetails(0);
//
//        assertTrue(service.getTransport().equals("tcp"));
//
//        assertTrue(service.getHost().equals("127.0.0.1"));
//        assertTrue(service.getPort() == 1234);
    }


    public void testSingleTransportMultiOptionURL() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='tcp://localhost:5672?foo='jim'&bar='bob'&fred='jimmy'',routingkey='jim',timeout='200',immediatedelivery='true'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));

        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);

        assertTrue(connectionurl.getOption("routingkey").equals("jim"));
        assertTrue(connectionurl.getOption("timeout").equals("200"));
        assertTrue(connectionurl.getOption("immediatedelivery").equals("true"));
    }

    public void testSinglevmURL() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='vm://:2'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("vm"));
        assertTrue(service.getHost().equals(""));
        assertTrue(service.getPort() == 2);

    }

    public void testFailoverVMURL() throws URLSyntaxException
    {
        String url = "amqp://ritchiem:bob@/test?brokerlist='vm://:2;vm://:3',failover='roundrobin'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod().equals("roundrobin"));
        assertTrue(connectionurl.getUsername().equals("ritchiem"));
        assertTrue(connectionurl.getPassword().equals("bob"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 2);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("vm"));
        assertTrue(service.getHost().equals(""));
        assertTrue(service.getPort() == 2);

        service = connectionurl.getBrokerDetails(1);
        assertTrue(service.getTransport().equals("vm"));
        assertTrue(service.getHost().equals(""));
        assertTrue(service.getPort() == 3);
    }


    public void testNoVirtualHostURL()
    {
        String url = "amqp://user@?brokerlist='tcp://localhost:5672'";

        try
        {
            new AMQConnectionURL(url);
            fail("URL has no virtual host should not parse");
        }
        catch (URLSyntaxException e)
        {
            // This should occur.
        }
    }

    public void testNoClientID() throws URLSyntaxException
    {
        String url = "amqp://user:@/test?brokerlist='tcp://localhost:5672'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getUsername().equals("user"));
        assertTrue(connectionurl.getPassword().equals(""));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 1);
    }

    public void testClientIDWithUnderscore() throws URLSyntaxException
    {
        String url = "amqp://user:pass@client_id/test?brokerlist='tcp://localhost:5672'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getUsername().equals("user"));
        assertTrue(connectionurl.getPassword().equals("pass"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));
        assertTrue(connectionurl.getClientName().equals("client_id"));
        
        assertTrue(connectionurl.getBrokerCount() == 1);
    }

    public void testWrongOptionSeparatorInOptions()
    {
        String url = "amqp://guest:guest@/test?brokerlist='tcp://localhost:5672;tcp://localhost:5673'+failover='roundrobin'";
        try
        {
            new AMQConnectionURL(url);
            fail("URL Should not parse");
        }
        catch (URLSyntaxException urise)
        {
            assertTrue(urise.getReason().equals("Unterminated option. Possible illegal option separator:'+'"));
        }

    }


    public void testNoUserDetailsProvidedWithClientID()

    {
        String url = "amqp://clientID/test?brokerlist='tcp://localhost:5672;tcp://localhost:5673'";
        try
        {
            new AMQConnectionURL(url);
            fail("URL Should not parse");
        }
        catch (URLSyntaxException urise)
        {
            assertTrue(urise.getMessage().startsWith("User information not found on url"));
        }

    }

    public void testNoUserDetailsProvidedNOClientID()

    {
        String url = "amqp:///test?brokerlist='tcp://localhost:5672;tcp://localhost:5673'";
        try
        {
            new AMQConnectionURL(url);
            fail("URL Should not parse");
        }
        catch (URLSyntaxException urise)
        {
            assertTrue(urise.getMessage().startsWith("User information not found on url"));
        }

    }

    public void testCheckVirtualhostFormat() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/t.-_+!=:?brokerlist='tcp://localhost:5672'";

        AMQConnectionURL connection = new AMQConnectionURL(url);
        assertTrue(connection.getVirtualHost().equals("/t.-_+!=:"));
    }

    public void testCheckDefaultPort() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test=:?brokerlist='tcp://localhost'";

        AMQConnectionURL connection = new AMQConnectionURL(url);

        BrokerDetails broker = connection.getBrokerDetails(0);
        assertTrue(broker.getPort() == AMQBrokerDetails.DEFAULT_PORT);

    }

    public void testCheckMissingFinalQuote() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@id/test" + "?brokerlist='tcp://localhost:5672";

        try
        {
            new AMQConnectionURL(url);
        }
        catch (URLSyntaxException e)
        {
            assertEquals(e.getMessage(), "Unterminated option at index 32: brokerlist='tcp://localhost:5672");
        }
    }


    public void testDefaultExchanges() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@id/test" + "?defaultQueueExchange='test.direct'&defaultTopicExchange='test.topic'&temporaryQueueExchange='tmp.direct'&temporaryTopicExchange='tmp.topic'";

        AMQConnectionURL conn = new AMQConnectionURL(url);

        assertEquals(conn.getDefaultQueueExchangeName(),"test.direct");

        assertEquals(conn.getDefaultTopicExchangeName(),"test.topic");

        assertEquals(conn.getTemporaryQueueExchangeName(),"tmp.direct");

        assertEquals(conn.getTemporaryTopicExchangeName(),"tmp.topic");

    }

    public void testSocketProtocol() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@id/test" + "?brokerlist='socket://VM-Unique-socketID'";

        try
        {
            AMQConnectionURL curl = new AMQConnectionURL(url);
            assertNotNull(curl);
            assertEquals(1, curl.getBrokerCount());
            assertNotNull(curl.getBrokerDetails(0));
            assertEquals(BrokerDetails.SOCKET, curl.getBrokerDetails(0).getTransport());
            assertEquals("VM-Unique-socketID", curl.getBrokerDetails(0).getHost());
            assertEquals("URL does not toString as expected",
                         url.replace(":guest", ":********"), curl.toString());
        }
        catch (URLSyntaxException e)
        {
            fail(e.getMessage());
        }
    }

    public void testSingleTransportMultiOptionOnBrokerURL() throws URLSyntaxException
    {
        String url = "amqp://guest:guest@/test?brokerlist='tcp://localhost:5672?foo='jim'&bar='bob'&fred='jimmy'',routingkey='jim',timeout='200',immediatedelivery='true'";

        ConnectionURL connectionurl = new AMQConnectionURL(url);

        assertTrue(connectionurl.getFailoverMethod() == null);
        assertTrue(connectionurl.getUsername().equals("guest"));
        assertTrue(connectionurl.getPassword().equals("guest"));
        assertTrue(connectionurl.getVirtualHost().equals("/test"));

        assertTrue(connectionurl.getBrokerCount() == 1);

        BrokerDetails service = connectionurl.getBrokerDetails(0);

        assertTrue(service.getTransport().equals("tcp"));

        
        assertTrue(service.getHost().equals("localhost"));
        assertTrue(service.getPort() == 5672);
        assertEquals("jim",service.getProperty("foo"));
        assertEquals("bob",service.getProperty("bar"));
        assertEquals("jimmy",service.getProperty("fred"));

        assertTrue(connectionurl.getOption("routingkey").equals("jim"));
        assertTrue(connectionurl.getOption("timeout").equals("200"));
        assertTrue(connectionurl.getOption("immediatedelivery").equals("true"));
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(ConnectionURLTest.class);
    }
}

