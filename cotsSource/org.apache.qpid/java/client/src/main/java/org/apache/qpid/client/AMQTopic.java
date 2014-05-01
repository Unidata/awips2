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
package org.apache.qpid.client;

import javax.jms.JMSException;
import javax.jms.Topic;

import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.url.BindingURL;

public class AMQTopic extends AMQDestination implements Topic
{
    /**
     * Constructor for use in creating a topic using a BindingURL.
     *
     * @param binding The binding url object.
     */
    public AMQTopic(BindingURL binding)
    {
        super(binding);
    }

//    public AMQTopic(String exchangeName, String routingKey)
//    {
//        this(new AMQShortString(exchangeName), new AMQShortString(routingKey));
//    }

    public AMQTopic(AMQShortString exchange, AMQShortString routingKey, AMQShortString queueName)
    {
        super(exchange, ExchangeDefaults.TOPIC_EXCHANGE_CLASS, routingKey, true, true, queueName, false);
    }

    public AMQTopic(AMQShortString exchange, AMQShortString routingKey, AMQShortString queueName,AMQShortString[] bindingKeys)
    {
        super(exchange, ExchangeDefaults.TOPIC_EXCHANGE_CLASS, routingKey, true, true, queueName, false,bindingKeys);
    }

    public AMQTopic(AMQConnection conn, String routingKey)
    {
        this(conn.getDefaultTopicExchangeName(), new AMQShortString(routingKey));
    }


    public AMQTopic(AMQShortString exchangeName, String routingKey)
    {
        this(exchangeName, new AMQShortString(routingKey));
    }

    public AMQTopic(AMQShortString exchangeName, AMQShortString routingKey)
    {
        this(exchangeName, routingKey, null);
    }

    public AMQTopic(AMQShortString exchangeName, AMQShortString name, boolean isAutoDelete, AMQShortString queueName, boolean isDurable)
    {
        super(exchangeName, ExchangeDefaults.TOPIC_EXCHANGE_CLASS, name, true, isAutoDelete,
              queueName, isDurable);
    }

    protected AMQTopic(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, boolean isExclusive,
                               boolean isAutoDelete, AMQShortString queueName, boolean isDurable)
    {
        super(exchangeName, exchangeClass, routingKey, isExclusive, isAutoDelete, queueName, isDurable );
    }

    protected AMQTopic(AMQShortString exchangeName, AMQShortString exchangeClass, AMQShortString routingKey, boolean isExclusive,
            boolean isAutoDelete, AMQShortString queueName, boolean isDurable,AMQShortString[] bindingKeys)
    {
        super(exchangeName, exchangeClass, routingKey, isExclusive, isAutoDelete, queueName, isDurable,bindingKeys);
    }

    public static AMQTopic createDurableTopic(AMQTopic topic, String subscriptionName, AMQConnection connection)
            throws JMSException
    {
        return new AMQTopic(topic.getExchangeName(), topic.getRoutingKey(), false,
                            getDurableTopicQueueName(subscriptionName, connection),
                            true);
    }

    public static AMQTopic createDurable010Topic(AMQTopic topic, String subscriptionName, AMQConnection connection)
            throws JMSException
    {
        return new AMQTopic(topic.getExchangeName(), ExchangeDefaults.TOPIC_EXCHANGE_CLASS, topic.getRoutingKey(), true, false,
              getDurableTopicQueueName(subscriptionName, connection), true);
    }

    public static AMQShortString getDurableTopicQueueName(String subscriptionName, AMQConnection connection) throws JMSException
    {
        return new AMQShortString(connection.getClientID() + ":" + subscriptionName);
    }

    public String getTopicName() throws JMSException
    {
        return super.getRoutingKey().toString();
    }

    public AMQShortString getRoutingKey()
    {
        return super.getRoutingKey();
    }

    public boolean isNameRequired()
    {
        return !isDurable();
    }

    /**
     * Override since the queue is always private and we must ensure it remains null. If not,
     * reuse of the topic when registering consumers will make all consumers listen on the same (private) queue rather
     * than getting their own (private) queue.
     * <p/>
     * This is relatively nasty but it is difficult to come up with a more elegant solution, given
     * the requirement in the case on AMQQueue and possibly other AMQDestination subclasses to
     * use the underlying queue name even where it is server generated.
     */
    public void setQueueName(String queueName)
    {
    }

    public boolean equals(Object o)
    {
        return (o instanceof AMQTopic)
               && ((AMQTopic)o).getExchangeName().equals(getExchangeName())
               && ((AMQTopic)o).getRoutingKey().equals(getRoutingKey());

    }

    public int hashCode()
    {
        return getExchangeName().hashCode() + getRoutingKey().hashCode();
    }
}
