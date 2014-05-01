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
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;

import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.message.InboundMessage;

import javax.management.JMException;
import java.util.ArrayList;

public interface Exchange extends ExchangeReferrer
{
    AMQShortString getName();

    AMQShortString getType();

    void initialise(VirtualHost host, AMQShortString name, boolean durable, int ticket, boolean autoDelete)
            throws AMQException, JMException;

    boolean isDurable();

    /**
     * @return true if the exchange will be deleted after all queues have been detached
     */
    boolean isAutoDelete();

    int getTicket();

    void close() throws AMQException;

    void registerQueue(AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException;



    void deregisterQueue(AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException;

    ArrayList<AMQQueue> route(InboundMessage message);


    /**
     * Determines whether a message would be isBound to a particular queue using a specific routing key and arguments
     * @param routingKey
     * @param arguments
     * @param queue
     * @return
     * @throws AMQException
     */
    boolean isBound(AMQShortString routingKey, FieldTable arguments, AMQQueue queue);

    /**
     * Determines whether a message would be isBound to a particular queue using a specific routing key
     * @param routingKey
     * @param queue
     * @return
     * @throws AMQException
     */
    boolean isBound(AMQShortString routingKey, AMQQueue queue);

    /**
     * Determines whether a message is routing to any queue using a specific _routing key
     * @param routingKey
     * @return
     * @throws AMQException
     */
    boolean isBound(AMQShortString routingKey);

    boolean isBound(AMQQueue queue);

    /**
     * Returns true if this exchange has at least one binding associated with it.
     * @return
     * @throws AMQException
     */
    boolean hasBindings();


    boolean isBound(String bindingKey, AMQQueue queue);

    boolean isBound(String bindingKey);

    Exchange getAlternateExchange();

    void setAlternateExchange(Exchange exchange);

    void removeReference(ExchangeReferrer exchange);

    void addReference(ExchangeReferrer exchange);

    boolean hasReferrers();
}
