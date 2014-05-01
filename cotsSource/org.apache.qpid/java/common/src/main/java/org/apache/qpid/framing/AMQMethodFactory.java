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
package org.apache.qpid.framing;

import org.apache.mina.common.ByteBuffer;


public interface AMQMethodFactory
{

    // Connection Methods

    ConnectionCloseBody createConnectionClose();

    // Access Methods

    AccessRequestBody createAccessRequest(boolean active, boolean exclusive, boolean passive, boolean read, AMQShortString realm, boolean write);


    // Tx Methods

    TxSelectBody createTxSelect();

    TxCommitBody createTxCommit();

    TxRollbackBody createTxRollback();

    // Channel Methods

    ChannelOpenBody createChannelOpen();

    ChannelCloseBody createChannelClose(int replyCode, AMQShortString replyText);

    ChannelFlowBody createChannelFlow(boolean active);


    // Exchange Methods


    ExchangeBoundBody createExchangeBound(AMQShortString exchangeName,
                                          AMQShortString queueName,
                                          AMQShortString routingKey);

    ExchangeDeclareBody createExchangeDeclare(AMQShortString name, AMQShortString type, int ticket);        


    // Queue Methods

    QueueDeclareBody createQueueDeclare(AMQShortString name, FieldTable arguments, boolean autoDelete, boolean durable, boolean exclusive, boolean passive, int ticket);

    QueueBindBody createQueueBind(AMQShortString queueName, AMQShortString exchangeName, AMQShortString routingKey, FieldTable arguments, int ticket);

    QueueDeleteBody createQueueDelete(AMQShortString queueName, boolean ifEmpty, boolean ifUnused, int ticket);


    // Message Methods

    // In different versions of the protocol we change the class used for message transfer
    // abstract this out so the appropriate methods are created
    AMQMethodBody createRecover(boolean requeue);

    AMQMethodBody createConsumer(AMQShortString tag, AMQShortString queueName, FieldTable arguments, boolean noAck, boolean exclusive, boolean noLocal, int ticket);

    AMQMethodBody createConsumerCancel(AMQShortString consumerTag);

    AMQMethodBody createAcknowledge(long deliveryTag, boolean multiple);

    AMQMethodBody createRejectBody(long deliveryTag, boolean requeue);

    AMQMethodBody createMessageQos(int prefetchCount, int prefetchSize);
 
}
