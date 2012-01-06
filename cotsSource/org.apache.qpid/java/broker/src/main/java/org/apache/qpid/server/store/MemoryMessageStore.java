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
package org.apache.qpid.server.store;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.abstraction.ContentChunk;
import org.apache.qpid.server.message.MessageMetaData;
import org.apache.qpid.server.exchange.Exchange;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.logging.LogSubject;
import org.apache.qpid.server.logging.messages.MessageStoreMessages;
import org.apache.qpid.server.logging.messages.ConfigStoreMessages;
import org.apache.qpid.server.logging.actors.CurrentActor;
import org.apache.qpid.server.message.ServerMessage;
import org.apache.commons.configuration.Configuration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.nio.ByteBuffer;

/** A simple message store that stores the messages in a threadsafe structure in memory. */
public class MemoryMessageStore implements MessageStore
{
    private static final Logger _log = Logger.getLogger(MemoryMessageStore.class);

    private static final int DEFAULT_HASHTABLE_CAPACITY = 50000;

    private static final String HASHTABLE_CAPACITY_CONFIG = "hashtable-capacity";


    private final AtomicLong _messageId = new AtomicLong(1);
    private AtomicBoolean _closed = new AtomicBoolean(false);
    private LogSubject _logSubject;

    private static final Transaction IN_MEMORY_TRANSACTION = new Transaction()
    {
        public void enqueueMessage(TransactionLogResource  queue, Long messageId) throws AMQException
        {
        }

        public void dequeueMessage(TransactionLogResource  queue, Long messageId) throws AMQException
        {
        }

        public void commitTran() throws AMQException
        {
        }

        public StoreFuture commitTranAsync() throws AMQException
        {
            return IMMEDIATE_FUTURE;
        }

        public void abortTran() throws AMQException
        {
        }

    };

    public void configureConfigStore(String name, ConfigurationRecoveryHandler handler, Configuration configuration, LogSubject logSubject) throws Exception
    {
        _logSubject = logSubject;
        CurrentActor.get().message(_logSubject, ConfigStoreMessages.CFG_1001(this.getClass().getName()));


    }

    public void configureMessageStore(String name,
                                      MessageStoreRecoveryHandler recoveryHandler,
                                      Configuration config,
                                      LogSubject logSubject) throws Exception
    {
        if(_logSubject == null)
        {
            _logSubject = logSubject;
        }
        int hashtableCapacity = config.getInt(name + "." + HASHTABLE_CAPACITY_CONFIG, DEFAULT_HASHTABLE_CAPACITY);
        _log.info("Using capacity " + hashtableCapacity + " for hash tables");
        CurrentActor.get().message(_logSubject, MessageStoreMessages.MST_CREATED(this.getClass().getName()));
    }

    public void close() throws Exception
    {
        _closed.getAndSet(true);
        CurrentActor.get().message(_logSubject,MessageStoreMessages.MST_CLOSED());

    }

    public StoredMessage addMessage(StorableMessageMetaData metaData)
    {
        final long id = _messageId.getAndIncrement();
        StoredMemoryMessage message = new StoredMemoryMessage(id, metaData);

        return message;
    }


    public void createExchange(Exchange exchange) throws AMQException
    {

    }

    public void removeExchange(Exchange exchange) throws AMQException
    {

    }

    public void bindQueue(Exchange exchange, AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException
    {

    }

    public void unbindQueue(Exchange exchange, AMQShortString routingKey, AMQQueue queue, FieldTable args) throws AMQException
    {

    }


    public void createQueue(AMQQueue queue) throws AMQException
    {
        // Not requred to do anything
    }

    public void createQueue(AMQQueue queue, FieldTable arguments) throws AMQException
    {
        // Not required to do anything
    }

    public void removeQueue(final AMQQueue queue) throws AMQException
    {
        // Not required to do anything
    }

    public void configureTransactionLog(String name,
                                        TransactionLogRecoveryHandler recoveryHandler,
                                        Configuration storeConfiguration,
                                        LogSubject logSubject) throws Exception
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public Transaction newTransaction()
    {
        return IN_MEMORY_TRANSACTION;
    }


    public List<AMQQueue> createQueues() throws AMQException
    {
        return null;
    }

    public Long getNewMessageId()
    {
        return _messageId.getAndIncrement();
    }

    public boolean isPersistent()
    {
        return false;
    }

    private void checkNotClosed() throws MessageStoreClosedException
     {
        if (_closed.get())
        {
            throw new MessageStoreClosedException();
        }
    }


}
