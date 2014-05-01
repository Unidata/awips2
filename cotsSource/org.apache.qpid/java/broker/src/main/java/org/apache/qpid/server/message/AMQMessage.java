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
package org.apache.qpid.server.message;

import org.apache.log4j.Logger;
import org.apache.qpid.AMQException;
import org.apache.qpid.framing.ContentHeaderBody;
import org.apache.qpid.framing.abstraction.MessagePublishInfo;
import org.apache.qpid.server.store.StoredMessage;
import org.apache.qpid.server.queue.AMQQueue;


import java.util.concurrent.atomic.AtomicInteger;
import java.nio.ByteBuffer;

/**
 * A deliverable message.
 */
public class AMQMessage implements ServerMessage
{
    /** Used for debugging purposes. */
    private static final Logger _log = Logger.getLogger(AMQMessage.class);

    private final AtomicInteger _referenceCount = new AtomicInteger(0);

    /** Flag to indicate that this message requires 'immediate' delivery. */

    private static final byte IMMEDIATE = 0x01;

    /**
     * Flag to indicate whether this message has been delivered to a consumer. Used in implementing return functionality
     * for messages published with the 'immediate' flag.
     */

    private static final byte DELIVERED_TO_CONSUMER = 0x02;

    private byte _flags = 0;

    private long _expiration;

    private final long _size;

    private Object _sessionIdentifier;
    private static final byte IMMEDIATE_AND_DELIVERED = (byte) (IMMEDIATE | DELIVERED_TO_CONSUMER);

    private final StoredMessage<MessageMetaData> _handle;


    public AMQMessage(StoredMessage<MessageMetaData> handle)
    {
        _handle = handle;
        final MessageMetaData metaData = handle.getMetaData();
        _size = metaData.getContentSize();
        final MessagePublishInfo messagePublishInfo = metaData.getMessagePublishInfo();

        if(messagePublishInfo.isImmediate())
        {
            _flags |= IMMEDIATE;
        }
    }


    public String debugIdentity()
    {
        return "(HC:" + System.identityHashCode(this) + " ID:" + getMessageId() + " Ref:" + _referenceCount.get() + ")";
    }

    public void setExpiration(final long expiration)
    {

        _expiration = expiration;

    }

    public boolean isReferenced()
    {
        return _referenceCount.get() > 0;
    }

    public MessageMetaData getMessageMetaData()
    {
        return _handle.getMetaData();
    }

    public ContentHeaderBody getContentHeaderBody() throws AMQException
    {
        return getMessageMetaData().getContentHeaderBody();
    }



    public Long getMessageId()
    {
        return _handle.getMessageNumber();
    }

    /**
     * Creates a long-lived reference to this message, and increments the count of such references, as an atomic
     * operation.
     */
    public AMQMessage takeReference()
    {
        incrementReference(); // _referenceCount.incrementAndGet();

        return this;
    }

    public boolean incrementReference()
    {
        return incrementReference(1);
    }

    /* Threadsafe. Increment the reference count on the message. */
    public boolean incrementReference(int count)
    {

        if(_referenceCount.addAndGet(count) <= 0)
        {
            _referenceCount.addAndGet(-count);
            return false;
        }
        else
        {
            return true;
        }

    }

    /**
     * Threadsafe. This will decrement the reference count and when it reaches zero will remove the message from the
     * message store.
     *
     *
     * @throws org.apache.qpid.server.queue.MessageCleanupException when an attempt was made to remove the message from the message store and that
     *                                 failed
     */
    public void decrementReference()
    {
        int count = _referenceCount.decrementAndGet();

        // note that the operation of decrementing the reference count and then removing the message does not
        // have to be atomic since the ref count starts at 1 and the exchange itself decrements that after
        // the message has been passed to all queues. i.e. we are
        // not relying on the all the increments having taken place before the delivery manager decrements.
        if (count == 0)
        {
            // set the reference count way below 0 so that we can detect that the message has been deleted
            // this is to guard against the message being spontaneously recreated (from the mgmt console)
            // by copying from other queues at the same time as it is being removed.
            _referenceCount.set(Integer.MIN_VALUE/2);

            // must check if the handle is null since there may be cases where we decide to throw away a message
            // and the handle has not yet been constructed
            if (_handle != null)
            {
                _handle.remove();

            }
        }
        else
        {
            if (count < 0)
            {
                throw new RuntimeException("Reference count for message id " + debugIdentity()
                                                  + " has gone below 0.");
            }
        }
    }


    /**
     * Called selectors to determin if the message has already been sent
     *
     * @return _deliveredToConsumer
     */
    public boolean getDeliveredToConsumer()
    {
        return (_flags & DELIVERED_TO_CONSUMER) != 0;
    }

    public String getRoutingKey()
    {
        // TODO
        return null;
    }

    public AMQMessageHeader getMessageHeader()
    {
        return getMessageMetaData().getMessageHeader();
    }

    public boolean isPersistent()
    {
        return getMessageMetaData().isPersistent();
    }

    /**
     * Called to enforce the 'immediate' flag.
     *
     * @returns  true if the message is marked for immediate delivery but has not been marked as delivered
     *                              to a consumer
     */
    public boolean immediateAndNotDelivered()
    {

        return (_flags & IMMEDIATE_AND_DELIVERED) == IMMEDIATE;

    }

    public MessagePublishInfo getMessagePublishInfo() throws AMQException
    {
        return getMessageMetaData().getMessagePublishInfo();
    }

    public long getArrivalTime()
    {
        return getMessageMetaData().getArrivalTime();
    }

    /**
     * Checks to see if the message has expired. If it has the message is dequeued.
     *
     * @param queue The queue to check the expiration against. (Currently not used)
     *
     * @return true if the message has expire
     *
     * @throws AMQException
     */
    public boolean expired(AMQQueue queue) throws AMQException
    {

        if (_expiration != 0L)
        {
            long now = System.currentTimeMillis();

            return (now > _expiration);
        }

        return false;
    }

    /**
     * Called when this message is delivered to a consumer. (used to implement the 'immediate' flag functionality).
     * And for selector efficiency.
     */
    public void setDeliveredToConsumer()
    {
        _flags |= DELIVERED_TO_CONSUMER;
    }

    public long getSize()
    {
        return _size;

    }

    public boolean isImmediate()
    {
        return (_flags & IMMEDIATE) == IMMEDIATE;
    }

    public long getExpiration()
    {
        return _expiration;
    }

    public MessageReference newReference()
    {
        return new AMQMessageReference(this);
    }

    public Long getMessageNumber()
    {
        return getMessageId();
    }


    public Object getPublisherIdentifier()
    {
        //todo store sessionIdentifier/client id with message in store
        //Currently the _sessionIdentifier will be null if the message has been
        // restored from a message Store

        return _sessionIdentifier;

    }

    public void setClientIdentifier(final Object sessionIdentifier)
    {
        _sessionIdentifier = sessionIdentifier;
    }


    public String toString()
    {
        // return "Message[" + debugIdentity() + "]: " + _messageId + "; ref count: " + _referenceCount + "; taken : " +
        // _taken + " by :" + _takenBySubcription;

        return "Message[" + debugIdentity() + "]: " + getMessageId() + "; ref count: " + _referenceCount;
    }

    public int getContent(ByteBuffer buf, int offset)
    {
        return _handle.getContent(offset, buf);
    }

    public StoredMessage<MessageMetaData> getStoredMessage()
    {
        return _handle;
    }
}
