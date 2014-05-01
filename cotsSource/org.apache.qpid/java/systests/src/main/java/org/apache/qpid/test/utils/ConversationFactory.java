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
package org.apache.qpid.test.utils;

import org.apache.log4j.Logger;
import org.apache.qpid.test.utils.ReflectionUtils;

import javax.jms.*;

import java.util.*;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

/**
 * A conversation helper, uses a message correlation id pattern to match up sent and received messages as a conversation
 * over JMS messaging. Incoming message traffic is divided up by correlation id. Each id has a queue (behaviour dependant
 * on the queue implementation). Clients of this de-multiplexer can wait on messages, defined by message correlation ids.
 *
 * <p/>One use of this is as a conversation synchronizer where multiple threads are carrying out conversations over a
 * multiplexed messaging route. This can be usefull, as JMS sessions are not multi-threaded. Setting up the conversation
 * with synchronous queues will allow these threads to be written in a synchronous style, but with their execution order
 * governed by the asynchronous message flow. For example, something like the following code could run a multi-threaded
 * conversation (the conversation methods can be called many times in parallel):
 *
 * <p/><pre>
 * class Initiator
 * {
 * ConversationHelper conversation = new ConversationHelper(connection, null,
 *                                                          java.util.concurrent.LinkedBlockingQueue.class);
 *
 * initiateConversation()
 * {
 *  try {
 *   // Exchange greetings.
 *   conversation.send(sendDestination, conversation.getSession().createTextMessage("Hello."));
 *   Message greeting = conversation.receive();
 *
 *   // Exchange goodbyes.
 *   conversation.send(conversation.getSession().createTextMessage("Goodbye."));
 *   Message goodbye = conversation.receive();
 *  } finally {
 *   conversation.end();
 *  }
 * }
 * }
 *
 * class Responder
 * {
 * ConversationHelper conversation = new ConversationHelper(connection, receiveDestination,
 *                                                          java.util.concurrent.LinkedBlockingQueue.class);
 *
 * respondToConversation()
 * {
 *   try {
 *   // Exchange greetings.
 *   Message greeting = conversation.receive();
 *   conversation.send(conversation.getSession().createTextMessage("Hello."));
 *
 *   // Exchange goodbyes.
 *   Message goodbye = conversation.receive();
 *   conversation.send(conversation.getSession().createTextMessage("Goodbye."));
 *  } finally {
 *   conversation.end();
 *  }
 * }
 * }
 * </pre>
 *
 * <p/>Conversation correlation id's are generated on a per thread basis.
 *
 * <p/>The same controlSession is shared amongst all conversations. Calls to send are therefore synchronized because JMS
 * sessions are not multi-threaded.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Associate messages to an ongoing conversation using correlation ids.
 * <tr><td> Auto manage sessions for conversations.
 * <tr><td> Store messages not in a conversation in dead letter box.
 * </table>
 */
public class ConversationFactory
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(ConversationFactory.class);

    /** Holds a map from correlation id's to queues. */
    private Map<Long, BlockingQueue<Message>> idsToQueues = new HashMap<Long, BlockingQueue<Message>>();

    /** Holds the connection over which the conversation is conducted. */
    private Connection connection;

    /** Holds the controlSession over which the conversation is conduxted. */
    private Session session;

    /** The message consumer for incoming messages. */
    MessageConsumer consumer;

    /** The message producer for outgoing messages. */
    MessageProducer producer;

    /** The well-known or temporary destination to receive replies on. */
    Destination receiveDestination;

    /** Holds the queue implementation class for the reply queue. */
    Class<? extends BlockingQueue> queueClass;

    /** Used to hold any replies that are received outside of the context of a conversation. */
    BlockingQueue<Message> deadLetterBox = new LinkedBlockingQueue<Message>();

    /* Used to hold conversation state on a per thread basis. */
    /*
    ThreadLocal<Conversation> threadLocals =
        new ThreadLocal<Conversation>()
        {
            protected Conversation initialValue()
            {
                Conversation settings = new Conversation();
                settings.conversationId = conversationIdGenerator.getAndIncrement();

                return settings;
            }
        };
     */

    /** Generates new coversation id's as needed. */
    AtomicLong conversationIdGenerator = new AtomicLong();

    /**
     * Creates a conversation helper on the specified connection with the default sending destination, and listening
     * to the specified receiving destination.
     *
     * @param connection         The connection to build the conversation helper on.
     * @param receiveDestination The destination to listen to for incoming messages. This may be null to use a temporary
     *                           queue.
     * @param queueClass         The queue implementation class.
     *
     * @throws JMSException All underlying JMSExceptions are allowed to fall through.
     */
    public ConversationFactory(Connection connection, Destination receiveDestination,
        Class<? extends BlockingQueue> queueClass) throws JMSException
    {
        log.debug("public ConversationFactory(Connection connection, Destination receiveDestination = " + receiveDestination
            + ", Class<? extends BlockingQueue> queueClass = " + queueClass + "): called");

        this.connection = connection;
        this.queueClass = queueClass;

        session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE);

        // Check if a well-known receive destination has been provided, or use a temporary queue if not.
        this.receiveDestination = (receiveDestination != null) ? receiveDestination : session.createTemporaryQueue();

        consumer = session.createConsumer(receiveDestination);
        producer = session.createProducer(null);

        consumer.setMessageListener(new Receiver());
    }

    /**
     * Creates a new conversation context.
     *
     * @return A new conversation context.
     */
    public Conversation startConversation()
    {
        log.debug("public Conversation startConversation(): called");

        Conversation conversation = new Conversation();
        conversation.conversationId = conversationIdGenerator.getAndIncrement();

        return conversation;
    }

    /**
     * Ensures that the reply queue for a conversation exists.
     *
     * @param conversationId The conversation correlation id.
     */
    private void initQueueForId(long conversationId)
    {
        if (!idsToQueues.containsKey(conversationId))
        {
            idsToQueues.put(conversationId, ReflectionUtils.<BlockingQueue>newInstance(queueClass));
        }
    }

    /**
     * Clears the dead letter box, returning all messages that were in it.
     *
     * @return All messages in the dead letter box.
     */
    public Collection<Message> emptyDeadLetterBox()
    {
        log.debug("public Collection<Message> emptyDeadLetterBox(): called");

        Collection<Message> result = new ArrayList<Message>();
        deadLetterBox.drainTo(result);

        return result;
    }

    /**
     * Gets the controlSession over which the conversation is conducted.
     *
     * @return The controlSession over which the conversation is conducted.
     */
    public Session getSession()
    {
        // Conversation settings = threadLocals.get();

        return session;
    }

    /**
     * Used to hold a conversation context. This consists of a correlating id for the conversation, and a reply
     * destination automatically updated to the last received reply-to destination.
     */
    public class Conversation
    {
        /** Holds the correlation id for the context. */
        long conversationId;

        /**
         * Holds the send destination for the context. This will automatically be updated to the most recently received
         * reply-to destination.
         */
        Destination sendDestination;

        /**
         * Sends a message to the default sending location. The correlation id of the message will be assigned by this
         * method, overriding any previously set value.
         *
         * @param sendDestination The destination to send to. This may be null to use the last received reply-to
         *                        destination.
         * @param message         The message to send.
         *
         * @throws JMSException All undelying JMSExceptions are allowed to fall through. This will also be thrown if no
         *                      send destination is specified and there is no most recent reply-to destination available
         *                      to use.
         */
        public void send(Destination sendDestination, Message message) throws JMSException
        {
            log.debug("public void send(Destination sendDestination = " + sendDestination + ", Message message = " + message
                + "): called");

            // Conversation settings = threadLocals.get();
            // long conversationId = conversationId;
            message.setJMSCorrelationID(Long.toString(conversationId));
            message.setJMSReplyTo(receiveDestination);

            // Ensure that the reply queue for this conversation exists.
            initQueueForId(conversationId);

            // Check if an overriding send to destination has been set or use the last reply-to if not.
            Destination sendTo = null;

            if (sendDestination != null)
            {
                sendTo = sendDestination;
            }
            else if (sendDestination != null)
            {
                sendTo = sendDestination;
            }
            else
            {
                throw new JMSException("The send destination was specified, and no most recent reply-to available to use.");
            }

            // Send the message.
            synchronized (this)
            {
                producer.send(sendTo, message);
            }
        }

        /**
         * Gets the next message in an ongoing conversation. This method may block until such a message is received.
         *
         * @return The next incoming message in the conversation.
         *
         * @throws JMSException All undelying JMSExceptions are allowed to fall through. Thrown if the received message
         *                      did not have its reply-to destination set up.
         */
        public Message receive() throws JMSException
        {
            log.debug("public Message receive(): called");

            // Conversation settings = threadLocals.get();
            // long conversationId = settings.conversationId;

            // Ensure that the reply queue for this conversation exists.
            initQueueForId(conversationId);

            BlockingQueue<Message> queue = idsToQueues.get(conversationId);

            try
            {
                Message result = queue.take();

                // Keep the reply-to destination to send replies to.
                sendDestination = result.getJMSReplyTo();

                return result;
            }
            catch (InterruptedException e)
            {
                return null;
            }
        }

        /**
         * Gets many messages in an ongoing conversation. If a limit is specified, then once that many messages are
         * received they will be returned. If a timeout is specified, then all messages up to the limit, received within
         * that timespan will be returned. At least one of the message count or timeout should be set to a value of
         * 1 or greater.
         *
         * @param num     The number of messages to receive, or all if this is less than 1.
         * @param timeout The timeout in milliseconds to receive the messages in, or forever if this is less than 1.
         *
         * @return All messages received within the count limit and the timeout.
         *
         * @throws JMSException All undelying JMSExceptions are allowed to fall through.
         */
        public Collection<Message> receiveAll(int num, long timeout) throws JMSException
        {
            log.debug("public Collection<Message> receiveAll(int num = " + num + ", long timeout = " + timeout
                + "): called");

            // Check that a timeout or message count was set.
            if ((num < 1) && (timeout < 1))
            {
                throw new IllegalArgumentException("At least one of message count (num) or timeout must be set.");
            }

            // Ensure that the reply queue for this conversation exists.
            initQueueForId(conversationId);
            BlockingQueue<Message> queue = idsToQueues.get(conversationId);

            // Used to collect the received messages in.
            Collection<Message> result = new ArrayList<Message>();

            // Used to indicate when the timeout or message count has expired.
            boolean receiveMore = true;

            int messageCount = 0;

            // Receive messages until the timeout or message count expires.
            do
            {
                try
                {
                    Message next = null;

                    // Try to receive the message with a timeout if one has been set.
                    if (timeout > 0)
                    {
                        next = queue.poll(timeout, TimeUnit.MILLISECONDS);

                        // Check if the timeout expired, and stop receiving if so.
                        if (next == null)
                        {
                            receiveMore = false;
                        }
                    }
                    // Receive the message without a timeout.
                    else
                    {
                        next = queue.take();
                    }

                    // Increment the message count if a message was received.
                    messageCount += (next != null) ? 1 : 0;

                    // Check if all the requested messages were received, and stop receiving if so.
                    if ((num > 0) && (messageCount >= num))
                    {
                        receiveMore = false;
                    }

                    // Keep the reply-to destination to send replies to.
                    sendDestination = (next != null) ? next.getJMSReplyTo() : sendDestination;

                    if (next != null)
                    {
                        result.add(next);
                    }
                }
                catch (InterruptedException e)
                {
                    // Restore the threads interrupted status.
                    Thread.currentThread().interrupt();

                    // Stop receiving but return the messages received so far.
                    receiveMore = false;
                }
            }
            while (receiveMore);

            return result;
        }

        /**
         * Completes the conversation. Any correlation id's pertaining to the conversation are no longer valid, and any
         * incoming messages using them will go to the dead letter box.
         */
        public void end()
        {
            log.debug("public void end(): called");

            // Ensure that the thread local for the current thread is cleaned up.
            // Conversation settings = threadLocals.get();
            // long conversationId = settings.conversationId;
            // threadLocals.remove();

            // Ensure that its queue is removed from the queue map.
            BlockingQueue<Message> queue = idsToQueues.remove(conversationId);

            // Move any outstanding messages on the threads conversation id into the dead letter box.
            queue.drainTo(deadLetterBox);
        }
    }

    /**
     * Implements the message listener for this conversation handler.
     */
    protected class Receiver implements MessageListener
    {
        /**
         * Handles all incoming messages in the ongoing conversations. These messages are split up by correaltion id
         * and placed into queues.
         *
         * @param message The incoming message.
         */
        public void onMessage(Message message)
        {
            log.debug("public void onMessage(Message message = " + message + "): called");

            try
            {
                Long conversationId = Long.parseLong(message.getJMSCorrelationID());

                // Find the converstaion queue to place the message on. If there is no conversation for the message id,
                // the the dead letter box queue is used.
                BlockingQueue<Message> queue = idsToQueues.get(conversationId);
                queue = (queue == null) ? deadLetterBox : queue;

                queue.put(message);
            }
            catch (JMSException e)
            {
                throw new RuntimeException(e);
            }
            catch (InterruptedException e)
            {
                throw new RuntimeException(e);
            }
        }
    }
}
