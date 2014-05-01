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
package org.apache.qpid.test.unit.client.destinationurl;

import junit.framework.TestCase;

import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.url.AMQBindingURL;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URISyntaxException;

public class DestinationURLTest extends TestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(DestinationURLTest.class);

    public void testFullURL() throws URISyntaxException
    {

        String url = "exchange.Class://exchangeName/Destination/Queue";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(url.equals(dest.toString()));

        assertTrue(dest.getExchangeClass().equals("exchange.Class"));
        assertTrue(dest.getExchangeName().equals("exchangeName"));
        assertTrue(dest.getDestinationName().equals("Destination"));
        assertTrue(dest.getQueueName().equals("Queue"));
    }

    public void testQueue() throws URISyntaxException
    {

        String url = "exchangeClass://exchangeName//Queue";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(url.equals(dest.toString()));

        assertTrue(dest.getExchangeClass().equals("exchangeClass"));
        assertTrue(dest.getExchangeName().equals("exchangeName"));
        assertTrue(dest.getDestinationName().equals(""));
        assertTrue(dest.getQueueName().equals("Queue"));
    }

    public void testQueueWithOption() throws URISyntaxException
    {

        String url = "exchangeClass://exchangeName//Queue?option='value'";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(url.equals(dest.toString()));

        assertTrue(dest.getExchangeClass().equals("exchangeClass"));
        assertTrue(dest.getExchangeName().equals("exchangeName"));
        assertTrue(dest.getDestinationName().equals(""));
        assertTrue(dest.getQueueName().equals("Queue"));
        assertTrue(dest.getOption("option").equals("value"));
    }


    public void testDestination() throws URISyntaxException
    {

        String url = "exchangeClass://exchangeName/Destination/";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(url.equals(dest.toString()));

        assertTrue(dest.getExchangeClass().equals("exchangeClass"));
        assertTrue(dest.getExchangeName().equals("exchangeName"));
        assertTrue(dest.getDestinationName().equals("Destination"));
        assertTrue(dest.getQueueName().equals(""));
    }

    public void testDestinationWithOption() throws URISyntaxException
    {

        String url = "exchangeClass://exchangeName/Destination/?option='value'";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(url.equals(dest.toString()));

        assertTrue(dest.getExchangeClass().equals("exchangeClass"));
        assertTrue(dest.getExchangeName().equals("exchangeName"));
        assertTrue(dest.getDestinationName().equals("Destination"));
        assertTrue(dest.getQueueName().equals(""));

        assertTrue(dest.getOption("option").equals("value"));
    }

    public void testDestinationWithMultiOption() throws URISyntaxException
    {

        String url = "exchangeClass://exchangeName/Destination/?option='value',option2='value2'";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(dest.getExchangeClass().equals("exchangeClass"));
        assertTrue(dest.getExchangeName().equals("exchangeName"));
        assertTrue(dest.getDestinationName().equals("Destination"));
        assertTrue(dest.getQueueName().equals(""));

        assertTrue(dest.getOption("option").equals("value"));
        assertTrue(dest.getOption("option2").equals("value2"));
    }

    public void testDestinationWithNoExchangeDefaultsToDirect() throws URISyntaxException
    {

        String url = "IBMPerfQueue1?durable='true'";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(dest.getExchangeClass().equals(ExchangeDefaults.DIRECT_EXCHANGE_CLASS));
        assertTrue(dest.getExchangeName().equals(""));
        assertTrue(dest.getDestinationName().equals(""));
        assertTrue(dest.getQueueName().equals("IBMPerfQueue1"));

        assertTrue(dest.getOption("durable").equals("true"));
    }

    public void testDestinationWithMultiBindingKeys() throws URISyntaxException
    {

        String url = "exchangeClass://exchangeName/Destination/?bindingkey='key1',bindingkey='key2'";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(dest.getExchangeClass().equals("exchangeClass"));
        assertTrue(dest.getExchangeName().equals("exchangeName"));
        assertTrue(dest.getDestinationName().equals("Destination"));
        assertTrue(dest.getQueueName().equals(""));

        assertTrue(dest.getBindingKeys().length == 2);
    }

    // You can only specify only a routing key or binding key, but not both.
    public void testDestinationIfOnlyRoutingKeyOrBindingKeyIsSpecified() throws URISyntaxException
    {

        String url = "exchangeClass://exchangeName/Destination/?bindingkey='key1',routingkey='key2'";
        boolean exceptionThrown = false;
        try
        {

            AMQBindingURL dest = new AMQBindingURL(url);
        }
        catch(URISyntaxException e)
        {
            exceptionThrown = true;
            _logger.info("Exception thrown",e);
        }

        assertTrue("Failed to throw an URISyntaxException when both the bindingkey and routingkey is specified",exceptionThrown);
    }
    
    public void testDestinationWithDurableTopic() throws URISyntaxException
    {

        String url = "topic://amq.topic//testTopicD?durable='true'&autodelete='true'&clientid='test'&subscription='testQueueD'";

        AMQBindingURL dest = new AMQBindingURL(url);

        assertTrue(dest.getExchangeClass().equals("topic"));
        assertTrue(dest.getExchangeName().equals("amq.topic"));
        assertTrue(dest.getQueueName().equals("test:testQueueD"));
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(DestinationURLTest.class);
    }
}
