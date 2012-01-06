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

import javax.jms.Queue;

import org.apache.qpid.exchange.ExchangeDefaults;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.url.BindingURL;

public class AMQQueue extends AMQDestination implements Queue
{

    /**
     * Create a reference to a non temporary queue using a BindingURL object.
     * Note this does not actually imply the queue exists.
     * @param binding a BindingURL object
     */
    public AMQQueue(BindingURL binding)
    {
         super(binding);
    }

    /**
     * Create a reference to a non temporary queue. Note this does not actually imply the queue exists.
     * @param name the name of the queue
     */
    public AMQQueue(AMQShortString exchangeName, String name)
    {
        this(exchangeName, new AMQShortString(name));
    }


    /**
     * Create a reference to a non temporary queue. Note this does not actually imply the queue exists.
     * @param name the name of the queue
     */
    public AMQQueue(AMQShortString exchangeName, AMQShortString name)
    {
        this(exchangeName, name, false);
    }

    public AMQQueue(AMQShortString exchangeName, AMQShortString routingKey, AMQShortString queueName)
    {
        super(exchangeName, ExchangeDefaults.DIRECT_EXCHANGE_CLASS, routingKey, false,
              false, queueName, false);
    }

    public AMQQueue(AMQShortString exchangeName, AMQShortString routingKey, AMQShortString queueName,AMQShortString[] bindingKeys)
    {
        super(exchangeName, ExchangeDefaults.DIRECT_EXCHANGE_CLASS, routingKey, false,
              false, queueName, false,bindingKeys);
    }

    /**
     * Create a reference to a non temporary queue. Note this does not actually imply the queue exists.
     * @param name the name of the queue
     */
    public AMQQueue(String exchangeName, String name)
    {
        this(new AMQShortString(exchangeName), new AMQShortString(name), false);
    }


    public AMQQueue(AMQConnection connection, String name)
    {
        this(connection.getDefaultQueueExchangeName(),name);
    }

    public AMQQueue(AMQConnection connection, String name, boolean temporary)
    {
        this(connection.getDefaultQueueExchangeName(), new AMQShortString(name),temporary);
    }


    /**
     * Create a queue with a specified name.
     *
     * @param name the destination name (used in the routing key)
     * @param temporary if true the broker will generate a queue name, also if true then the queue is autodeleted
     * and exclusive
     */
    public AMQQueue(String exchangeName, String name, boolean temporary)
    {
        this(new AMQShortString(exchangeName), new AMQShortString(name),temporary);
    }


    /**
     * Create a queue with a specified name.
     *
     * @param name the destination name (used in the routing key)
     * @param temporary if true the broker will generate a queue name, also if true then the queue is autodeleted
     * and exclusive
     */
    public AMQQueue(AMQShortString exchangeName, AMQShortString name, boolean temporary)
    {
        // queue name is set to null indicating that the broker assigns a name in the case of temporary queues
        // temporary queues are typically used as response queues
        this(exchangeName, name, temporary?null:name, temporary, temporary, !temporary);

    }

    /**
     * Create a reference to a queue. Note this does not actually imply the queue exists.
     * @param exchangeName the exchange name we want to send the message to
     * @param routingKey the routing key
     * @param queueName the queue name
     * @param exclusive true if the queue should only permit a single consumer
     * @param autoDelete true if the queue should be deleted automatically when the last consumers detaches
     */
    public AMQQueue(AMQShortString exchangeName, AMQShortString routingKey, AMQShortString queueName, boolean exclusive, boolean autoDelete)
    {
        this(exchangeName, routingKey, queueName, exclusive, autoDelete, false);
    }

    public AMQQueue(AMQShortString exchangeName, AMQShortString routingKey, AMQShortString queueName, boolean exclusive, boolean autoDelete, boolean durable)
    {
        this(exchangeName,routingKey,queueName,exclusive,autoDelete,durable,null);
    }

    public AMQQueue(AMQShortString exchangeName, AMQShortString routingKey, AMQShortString queueName, boolean exclusive, boolean autoDelete, boolean durable,AMQShortString[] bindingKeys)
    {
        super(exchangeName, ExchangeDefaults.DIRECT_EXCHANGE_CLASS, routingKey, exclusive,
              autoDelete, queueName, durable, bindingKeys);
    }

    public AMQShortString getRoutingKey()
    {
        //return getAMQQueueName();
        if (getAMQQueueName() != null && getAMQQueueName().equals(super.getRoutingKey()))
        {
            return getAMQQueueName();
        }
        else
        {
            return super.getRoutingKey();
        }
    }

    public boolean isNameRequired()
    {
        //If the name is null, we require one to be generated by the client so that it will#
        //remain valid if we failover (see BLZ-24)
        return getQueueName() == null;
    }
}
