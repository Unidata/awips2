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
package org.apache.qpid.jms;

import org.apache.qpid.framing.AMQShortString;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;


public interface Session extends javax.jms.Session
{
    /**
     * Indicates that no client acknowledgements are required. Broker assumes that once it has delivered
     * a message packet successfully it is acknowledged.
     */
    static final int NO_ACKNOWLEDGE = 257;

    /**
     * Pre acknowledge means that an ack is sent per message but sent before user code has processed
     * the message (i.e. before the onMessage() call or the receive() method has returned).
     */
    static final int PRE_ACKNOWLEDGE = 258;

    MessageConsumer createConsumer(Destination destination,
                                   int prefetch,
                                   boolean noLocal,
                                   boolean exclusive,
                                   String selector) throws JMSException;

       MessageConsumer createConsumer(Destination destination,
                                   int prefetchHigh,
                                   int prefetchLow,
                                   boolean noLocal,
                                   boolean exclusive,
                                   String selector) throws JMSException;

    /**
     * @return the prefetch value used by default for consumers created on this session.
     */
    int getDefaultPrefetch();

    /**
     * @return the High water prefetch value used by default for consumers created on this session.
     */
    int getDefaultPrefetchHigh();

    /**
     * @return the Low water prefetch value used by default for consumers created on this session.
     */
    int getDefaultPrefetchLow();

    /**
     * Create a producer
     * @param destination
     * @param mandatory the value of the mandatory flag used by default on the producer
     * @param immediate the value of the immediate flag used by default on the producer
     * @return
     * @throws JMSException
     */
    MessageProducer createProducer(Destination destination, boolean mandatory, boolean immediate)
            throws JMSException;

    /**
     * Create a producer
     * @param destination     
     * @param immediate the value of the immediate flag used by default on the producer
     * @return
     * @throws JMSException
     */
    MessageProducer createProducer(Destination destination, boolean immediate)
            throws JMSException;

    AMQShortString getTemporaryTopicExchangeName();

    AMQShortString getDefaultQueueExchangeName();

    AMQShortString getDefaultTopicExchangeName();

    AMQShortString getTemporaryQueueExchangeName();
}
