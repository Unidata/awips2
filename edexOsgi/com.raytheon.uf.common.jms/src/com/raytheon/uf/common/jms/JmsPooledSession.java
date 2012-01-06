/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.jms;

import java.io.Serializable;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.ObjectMessage;
import javax.jms.Queue;
import javax.jms.QueueBrowser;
import javax.jms.Session;
import javax.jms.StreamMessage;
import javax.jms.TemporaryQueue;
import javax.jms.TemporaryTopic;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicSubscriber;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsPooledSession implements Session {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledSession.class);

    private final long createTime = System.currentTimeMillis();

    private final JmsPooledConnection conn;

    private final Session sess;

    private String threadKey;

    private boolean isOpen = true;

    private boolean exceptionOccurred = false;

    private Throwable trappedExc = null;

    private boolean inUse = true;

    // keeps track of number of creates vs. closes to know when it can be
    // returned to the pool
    private int references = 0;

    private HashMap<String, AvailableJmsPooledObject<JmsPooledConsumer>> availableConsumers = new HashMap<String, AvailableJmsPooledObject<JmsPooledConsumer>>();

    private HashMap<String, AvailableJmsPooledObject<JmsPooledProducer>> availableProducers = new HashMap<String, AvailableJmsPooledObject<JmsPooledProducer>>();

    private HashMap<String, JmsPooledConsumer> inUseConsumers = new HashMap<String, JmsPooledConsumer>();

    private HashMap<String, JmsPooledProducer> inUseProducers = new HashMap<String, JmsPooledProducer>();

    public JmsPooledSession(JmsPooledConnection conn, Session sess) {
        this.conn = conn;
        this.sess = sess;
    }

    public long getCreateTime() {
        return createTime;
    }

    public Session getInternalSession() {
        return sess;
    }

    /**
     * Returns the connection associated with this session.
     * 
     * @return
     */
    public JmsPooledConnection getConnection() {
        return conn;
    }

    public String getThreadKey() {
        return threadKey;
    }

    public void setThreadKey(String threadKey) {
        this.threadKey = threadKey;
    }

    public boolean isInUse() {
        return inUse;
    }

    public void setInUse(boolean inUse) {
        this.inUse = inUse;
    }

    public boolean isValid() {
        return isOpen && !exceptionOccurred && conn.isConnected()
                && conn.getConnectionStartTime() <= createTime;
    }

    public boolean isExceptionOccurred() {
        return exceptionOccurred;
    }

    /**
     * Closing all resources that have been idle in the pool for more than
     * resourceRetention millis.
     * 
     * @param resourceRentention
     * @return
     */
    public int closeOldPooledResources(int resourceRentention) {
        int count = 0;
        List<AvailableJmsPooledObject<JmsPooledProducer>> producersToClose = new LinkedList<AvailableJmsPooledObject<JmsPooledProducer>>();
        List<AvailableJmsPooledObject<JmsPooledConsumer>> consumersToClose = new LinkedList<AvailableJmsPooledObject<JmsPooledConsumer>>();
        long curTime = System.currentTimeMillis();
        synchronized (availableProducers) {
            for (AvailableJmsPooledObject<JmsPooledProducer> wrapper : availableProducers
                    .values()) {
                if (wrapper.expired(curTime, resourceRentention)) {
                    // not immediately closing producer so that we minimize time
                    // in sync block
                    producersToClose.add(wrapper);
                }
            }
        }

        for (AvailableJmsPooledObject<JmsPooledProducer> wrapper : producersToClose) {
            String key = wrapper.getPooledObject().getDestKey();
            AvailableJmsPooledObject<JmsPooledProducer> pooledWrapper = null;
            synchronized (availableProducers) {
                pooledWrapper = availableProducers.remove(key);
            }

            // ensure it was still in the available pool and hadn't been updated
            // in the mean time
            if (pooledWrapper != null && pooledWrapper.equals(wrapper)) {
                count++;
                wrapper.getPooledObject().closeInternal();
            }
        }

        synchronized (availableConsumers) {
            for (AvailableJmsPooledObject<JmsPooledConsumer> wrapper : availableConsumers
                    .values()) {
                if (wrapper.expired(curTime, resourceRentention)) {
                    // not immediately closing producer so that we minimize time
                    // in sync block
                    consumersToClose.add(wrapper);
                }
            }
        }

        for (AvailableJmsPooledObject<JmsPooledConsumer> wrapper : consumersToClose) {
            String key = wrapper.getPooledObject().getDestKey();
            AvailableJmsPooledObject<JmsPooledConsumer> pooledWrapper = null;
            synchronized (availableConsumers) {
                pooledWrapper = availableConsumers.remove(key);
            }

            // ensure it was still in the available pool and hadn't been updated
            // in the mean time
            if (pooledWrapper != null && pooledWrapper.equals(wrapper)) {
                count++;
                wrapper.getPooledObject().closeInternal();
            }
        }
        return count;
    }

    public void closePooledConsumersProducers() {
        synchronized (availableProducers) {
            for (AvailableJmsPooledObject<JmsPooledProducer> wrapper : availableProducers
                    .values()) {
                wrapper.getPooledObject().closeInternal();
            }
            availableProducers.clear();
        }

        synchronized (availableConsumers) {
            for (AvailableJmsPooledObject<JmsPooledConsumer> wrapper : availableConsumers
                    .values()) {
                wrapper.getPooledObject().closeInternal();
            }
            availableConsumers.clear();
        }

        synchronized (inUseConsumers) {
            for (JmsPooledConsumer consumer : inUseConsumers.values()) {
                consumer.closeInternal();
            }
            inUseConsumers.clear();
        }

        synchronized (inUseProducers) {
            for (JmsPooledProducer producer : inUseProducers.values()) {
                producer.closeInternal();
            }
            inUseProducers.clear();
        }
    }

    public void closeInternal() {
        boolean canClose = false;
        if (isOpen) {
            synchronized (this) {
                if (isOpen) {
                    canClose = true;
                    isOpen = false;

                    if (trappedExc != null) {
                        statusHandler.handle(Priority.INFO,
                                "Trapped internal exception", trappedExc);
                    }
                }
            }
        }

        if (canClose) {
            // synchronize on the connection to avoid deadlock conditions in
            // qpid
            synchronized (conn) {
                closePooledConsumersProducers();

                try {
                    sess.close();
                } catch (Exception e) {
                    statusHandler.handle(Priority.INFO,
                            "Failed to close session", e);
                }
            }
        }
    }

    public void returnProducerToPool(JmsPooledProducer producer) {
        if (isOpen && !producer.isExceptionOccurred()) {
            // done in order so producer is not dropped if close occurs
            synchronized (availableProducers) {
                availableProducers.put(producer.getDestKey(),
                        new AvailableJmsPooledObject<JmsPooledProducer>(
                                producer));
            }
            synchronized (inUseProducers) {
                inUseProducers.remove(producer.getDestKey());
            }
        } else {
            synchronized (inUseProducers) {
                inUseProducers.remove(producer.getDestKey());
            }
            producer.closeInternal();
            if (producer.isExceptionOccurred()) {
                try {
                    close();
                } catch (JMSException e) {
                    // ignore
                }
            }
        }
    }

    public void returnConsumerToPool(JmsPooledConsumer consumer) {
        if (isOpen && !consumer.isExceptionOccurred()) {
            try {
                if (consumer.getMessageListener() != null) {
                    statusHandler
                            .warn("Consumer returned with MessageListener still set, clearing listener");
                    consumer.setMessageListener(null);
                }
            } catch (Throwable e) {
                statusHandler.error(
                        "Error occured checking/clearing the messageListener",
                        e);
            }

            // done in order so consumer is not dropped if close occurs
            synchronized (availableConsumers) {
                availableConsumers.put(consumer.getDestKey(),
                        new AvailableJmsPooledObject<JmsPooledConsumer>(
                                consumer));
            }
            synchronized (inUseConsumers) {
                inUseConsumers.remove(consumer.getDestKey());
            }
        } else {
            synchronized (inUseConsumers) {
                inUseConsumers.remove(consumer.getDestKey());
            }
            consumer.closeInternal();
            if (consumer.isExceptionOccurred()) {
                try {
                    close();
                } catch (JMSException e) {
                    // ignore
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#close()
     */
    @Override
    public void close() throws JMSException {
        if (isOpen) {
            synchronized (this) {
                references--;
            }

            if (references <= 0) {
                conn.returnSessionToPool(this);
            }
        }
    }

    /**
     * Increments the reference count. Should be called when pooled session is
     * returned from a createSession call.
     */
    public void incSessionRefs() {
        synchronized (this) {
            references++;
        }
    }

    public int getSessionRefs() {
        return references;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#commit()
     */
    @Override
    public void commit() throws JMSException {
        try {
            sess.commit();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createBrowser(javax.jms.Queue)
     */
    @Override
    public QueueBrowser createBrowser(Queue queue) throws JMSException {
        try {
            return sess.createBrowser(queue);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createBrowser(javax.jms.Queue, java.lang.String)
     */
    @Override
    public QueueBrowser createBrowser(Queue queue, String messageSelector)
            throws JMSException {
        try {
            return sess.createBrowser(queue, messageSelector);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createBytesMessage()
     */
    @Override
    public BytesMessage createBytesMessage() throws JMSException {
        try {
            return sess.createBytesMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createConsumer(javax.jms.Destination)
     */
    @Override
    public MessageConsumer createConsumer(Destination destination)
            throws JMSException {
        return createConsumer(destination, null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createConsumer(javax.jms.Destination,
     * java.lang.String)
     */
    @Override
    public MessageConsumer createConsumer(Destination destination,
            String messageSelector) throws JMSException {
        if (!inUse) {
            inUse = true;
            closePooledConsumersProducers();
        }

        String destKey = null;
        if (destination instanceof Queue) {
            destKey = ((Queue) destination).getQueueName();
        } else if (destination instanceof Topic) {
            destKey = ((Topic) destination).getTopicName();
        }

        if (messageSelector != null) {
            destKey += "-" + messageSelector;
        }

        AvailableJmsPooledObject<JmsPooledConsumer> wrapper = null;
        synchronized (availableConsumers) {
            wrapper = availableConsumers.remove(destKey);
        }

        JmsPooledConsumer consumer = null;

        if (wrapper != null) {
            consumer = wrapper.getPooledObject();
        }

        if (consumer == null) {
            try {
                // consumers not pooled due to inherit issue in stopping
                // messages to consumer
                consumer = new JmsPooledConsumer(this, sess.createConsumer(
                        destination, messageSelector), destKey);
                synchronized (inUseConsumers) {
                    inUseConsumers.put(destKey, consumer);
                }
            } catch (Throwable e) {
                exceptionOccurred = true;
                trappedExc = e;
                statusHandler.handle(Priority.INFO,
                        "Failed to create consumer for dest " + destKey + ", ["
                                + destination + "]", e);

                JMSException exc = new JMSException(
                        "Exception occurred on pooled session");
                exc.initCause(e);
                throw exc;
            }
        }
        return consumer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createConsumer(javax.jms.Destination,
     * java.lang.String, boolean)
     */
    @Override
    public MessageConsumer createConsumer(Destination destination,
            String messageSelector, boolean noLocal) throws JMSException {
        try {
            // no pooling for now
            return sess.createConsumer(destination, messageSelector, noLocal);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createDurableSubscriber(javax.jms.Topic,
     * java.lang.String)
     */
    @Override
    public TopicSubscriber createDurableSubscriber(Topic topic, String name)
            throws JMSException {
        try {
            // no pooling for now
            return sess.createDurableSubscriber(topic, name);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createDurableSubscriber(javax.jms.Topic,
     * java.lang.String, java.lang.String, boolean)
     */
    @Override
    public TopicSubscriber createDurableSubscriber(Topic topic, String name,
            String messageSelector, boolean noLocal) throws JMSException {
        try {
            // no pooling for now
            return sess.createDurableSubscriber(topic, name, messageSelector,
                    noLocal);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createMapMessage()
     */
    @Override
    public MapMessage createMapMessage() throws JMSException {
        try {
            return sess.createMapMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createMessage()
     */
    @Override
    public Message createMessage() throws JMSException {
        try {
            return sess.createMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createObjectMessage()
     */
    @Override
    public ObjectMessage createObjectMessage() throws JMSException {
        try {
            return sess.createObjectMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createObjectMessage(java.io.Serializable)
     */
    @Override
    public ObjectMessage createObjectMessage(Serializable obj)
            throws JMSException {
        try {
            return sess.createObjectMessage(obj);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createProducer(javax.jms.Destination)
     */
    @Override
    public MessageProducer createProducer(Destination destination)
            throws JMSException {
        String destKey = null;
        if (destination instanceof Queue) {
            destKey = ((Queue) destination).getQueueName();
        } else if (destination instanceof Topic) {
            destKey = ((Topic) destination).getTopicName();
        }

        AvailableJmsPooledObject<JmsPooledProducer> wrapper = null;
        synchronized (availableProducers) {
            wrapper = availableProducers.remove(destKey);
        }

        JmsPooledProducer producer = null;

        if (wrapper != null) {
            producer = wrapper.getPooledObject();
        }

        if (producer == null) {
            if (!inUse) {
                inUse = true;
                // this was a recycled session and the producer that was
                // requested was not previously created close down previous
                // consumer/producer
                closePooledConsumersProducers();
            }

            try {
                producer = new JmsPooledProducer(this,
                        sess.createProducer(destination), destKey);
            } catch (Throwable e) {
                exceptionOccurred = true;
                trappedExc = e;
                statusHandler.handle(Priority.INFO,
                        "Failed to create producer for dest " + destKey + ", ["
                                + destination + "]", e);

                JMSException exc = new JMSException(
                        "Exception occurred on pooled session");
                exc.initCause(e);
                throw exc;
            }
        } else {
            inUse = true;
        }

        synchronized (inUseProducers) {
            inUseProducers.put(destKey, producer);
        }

        return producer;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createQueue(java.lang.String)
     */
    @Override
    public Queue createQueue(String queueName) throws JMSException {
        try {
            return sess.createQueue(queueName);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createStreamMessage()
     */
    @Override
    public StreamMessage createStreamMessage() throws JMSException {
        try {
            return sess.createStreamMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTemporaryQueue()
     */
    @Override
    public TemporaryQueue createTemporaryQueue() throws JMSException {
        try {
            return sess.createTemporaryQueue();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTemporaryTopic()
     */
    @Override
    public TemporaryTopic createTemporaryTopic() throws JMSException {
        try {
            return sess.createTemporaryTopic();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTextMessage()
     */
    @Override
    public TextMessage createTextMessage() throws JMSException {
        try {
            return sess.createTextMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTextMessage(java.lang.String)
     */
    @Override
    public TextMessage createTextMessage(String text) throws JMSException {
        try {
            return sess.createTextMessage(text);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTopic(java.lang.String)
     */
    @Override
    public Topic createTopic(String topicName) throws JMSException {
        try {
            return sess.createTopic(topicName);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#getAcknowledgeMode()
     */
    @Override
    public int getAcknowledgeMode() throws JMSException {
        try {
            return sess.getAcknowledgeMode();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#getMessageListener()
     */
    @Override
    public MessageListener getMessageListener() throws JMSException {
        try {
            return sess.getMessageListener();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#getTransacted()
     */
    @Override
    public boolean getTransacted() throws JMSException {
        try {
            return sess.getTransacted();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#recover()
     */
    @Override
    public void recover() throws JMSException {
        try {
            sess.recover();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#rollback()
     */
    @Override
    public void rollback() throws JMSException {
        try {
            sess.rollback();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#run()
     */
    @Override
    public void run() {
        try {
            sess.run();
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            RuntimeException exc = new RuntimeException(
                    "Exception occurred on pooled session", e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#setMessageListener(javax.jms.MessageListener)
     */
    @Override
    public void setMessageListener(MessageListener listener)
            throws JMSException {
        try {
            sess.setMessageListener(listener);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#unsubscribe(java.lang.String)
     */
    @Override
    public void unsubscribe(String name) throws JMSException {
        try {
            sess.unsubscribe(name);
        } catch (Throwable e) {
            exceptionOccurred = true;
            trappedExc = e;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }
}
