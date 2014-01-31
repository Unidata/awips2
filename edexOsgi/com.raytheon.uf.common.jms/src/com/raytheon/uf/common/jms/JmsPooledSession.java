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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Queue;
import javax.jms.Session;
import javax.jms.Topic;

import com.raytheon.uf.common.jms.wrapper.JmsConsumerWrapper;
import com.raytheon.uf.common.jms.wrapper.JmsProducerWrapper;
import com.raytheon.uf.common.jms.wrapper.JmsSessionWrapper;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Jms Pooled Session. Tracks references to the session to know when the session
 * can be released to the pool. Any exception will close pooled session instead
 * of returning to the pool. The consumers/producers are tracked in both active
 * and available states. An available consumer/producer can be reused by the
 * next client.
 * 
 * Synchronization Principle To prevent deadlocks: Chained sync blocks can only
 * happen in a downward direction. A manager has a synchronized lock can make a
 * call down to a wrapper, but not nice versa. Also a session inside a sync
 * block can make a call down to a producer but not vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2011            rjpeter     Initial creation
 * Mar 08, 2012 194        njensen     Improved logging
 * Feb 21, 2013 1642       rjpeter     Fix deadlock scenario
 * Jan 26, 2014 2357       rjpeter     Close a session when it has no producers or consumers.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class JmsPooledSession {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledSession.class);

    private final long createTime = System.currentTimeMillis();

    private final JmsPooledConnection conn;

    private final Session sess;

    // The thread this session was most recently used by for tracking a pending
    // session that is being reserved for a given thread.
    private String threadKey;

    private volatile boolean exceptionOccurred = false;

    private final Object stateLock = new Object();

    private volatile State state = State.InUse;

    // keeps track of number of creates vs. closes to know when it can be
    // returned to the pool
    private final List<JmsSessionWrapper> references = new ArrayList<JmsSessionWrapper>(
            1);

    private final Map<String, AvailableJmsPooledObject<JmsPooledConsumer>> availableConsumers = new HashMap<String, AvailableJmsPooledObject<JmsPooledConsumer>>();

    private final Map<String, AvailableJmsPooledObject<JmsPooledProducer>> availableProducers = new HashMap<String, AvailableJmsPooledObject<JmsPooledProducer>>();

    private final Map<String, JmsPooledConsumer> inUseConsumers = new HashMap<String, JmsPooledConsumer>();

    private final Map<String, JmsPooledProducer> inUseProducers = new HashMap<String, JmsPooledProducer>();

    public JmsPooledSession(JmsPooledConnection conn, Session sess) {
        this.conn = conn;
        this.sess = sess;
    }

    public long getCreateTime() {
        return createTime;
    }

    public Session getSession() {
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

    public boolean isValid() {
        return isValid(State.Closed, false);
    }

    /**
     * Verifies if an exception has occurred, the state is the desired state,
     * and the underlying resource is still valid.
     * 
     * @param requiredState
     * @param mustBeRequiredState
     *            If true, current state must match requiredState for isValid to
     *            be true. If false, current state must not be the
     *            requiredState.
     * @return
     */
    public boolean isValid(State requiredState, boolean mustBeRequiredState) {
        boolean valid = false;
        if (!exceptionOccurred) {
            valid = state.equals(requiredState);
            if (!mustBeRequiredState) {
                valid = !valid;
            }

            if (valid) {
                // check underlying resource
                try {
                    if (sess != null) {
                        sess.getAcknowledgeMode();
                    }
                } catch (JMSException e) {
                    // underlying session has been closed
                    valid = false;
                }
            }
        }
        return valid;
    }

    public boolean isExceptionOccurred() {
        return exceptionOccurred;
    }

    public void setExceptionOccurred(boolean exceptionOccurred) {
        this.exceptionOccurred = exceptionOccurred;
    }

    public JmsSessionWrapper createReference() {
        synchronized (stateLock) {
            if (isValid(State.InUse, true)) {
                JmsSessionWrapper wrapper = new JmsSessionWrapper(this);
                references.add(wrapper);
                return wrapper;
            }
        }

        return null;
    }

    /**
     * Closing all resources that have been idle in the pool for more than
     * resourceRetention millis.
     * 
     * @param resourceRetention
     * @return
     */
    public int closeOldPooledResources(int resourceRetention) {
        int count = 0;
        List<AvailableJmsPooledObject<JmsPooledProducer>> producersToClose = new LinkedList<AvailableJmsPooledObject<JmsPooledProducer>>();
        List<AvailableJmsPooledObject<JmsPooledConsumer>> consumersToClose = new LinkedList<AvailableJmsPooledObject<JmsPooledConsumer>>();
        long curTime = System.currentTimeMillis();
        synchronized (availableProducers) {
            Iterator<AvailableJmsPooledObject<JmsPooledProducer>> iter = availableProducers
                    .values().iterator();
            while (iter.hasNext()) {
                AvailableJmsPooledObject<JmsPooledProducer> wrapper = iter
                        .next();
                if (wrapper.expired(curTime, resourceRetention)) {
                    // not immediately closing producer so that we minimize time
                    // in sync block
                    producersToClose.add(wrapper);
                    iter.remove();
                }
            }
        }

        for (AvailableJmsPooledObject<JmsPooledProducer> wrapper : producersToClose) {
            wrapper.getPooledObject().close();
            count++;
        }

        synchronized (availableConsumers) {
            Iterator<AvailableJmsPooledObject<JmsPooledConsumer>> iter = availableConsumers
                    .values().iterator();
            while (iter.hasNext()) {
                AvailableJmsPooledObject<JmsPooledConsumer> wrapper = iter
                        .next();
                if (wrapper.expired(curTime, resourceRetention)) {
                    // not immediately closing producer so that we minimize time
                    // in sync block
                    consumersToClose.add(wrapper);
                    iter.remove();
                }
            }
        }

        for (AvailableJmsPooledObject<JmsPooledConsumer> wrapper : consumersToClose) {
            wrapper.getPooledObject().close();
            count++;
        }

        return count;
    }

    public void closePooledConsumersProducers() {
        List<JmsPooledProducer> producersToClose = new ArrayList<JmsPooledProducer>(
                inUseProducers.size() + availableProducers.size());
        synchronized (inUseProducers) {
            producersToClose.addAll(inUseProducers.values());
            inUseProducers.clear();
        }

        synchronized (availableProducers) {
            producersToClose = new ArrayList<JmsPooledProducer>(
                    availableProducers.size());
            for (AvailableJmsPooledObject<JmsPooledProducer> wrapper : availableProducers
                    .values()) {
                producersToClose.add(wrapper.getPooledObject());
            }
            availableProducers.clear();
        }

        List<JmsPooledConsumer> consumersToClose = new ArrayList<JmsPooledConsumer>(
                inUseConsumers.size() + availableConsumers.size());

        synchronized (inUseConsumers) {
            consumersToClose.addAll(inUseConsumers.values());
            inUseConsumers.clear();
        }

        synchronized (availableConsumers) {
            for (AvailableJmsPooledObject<JmsPooledConsumer> wrapper : availableConsumers
                    .values()) {
                consumersToClose.add(wrapper.getPooledObject());
            }
            availableConsumers.clear();
        }

        for (JmsPooledProducer producer : producersToClose) {
            producer.close();
        }

        for (JmsPooledConsumer consumer : consumersToClose) {
            consumer.close();
        }
    }

    /**
     * 
     * @param producer
     * @return True if the producer was successfully returned to the pool, false
     *         otherwise.
     */
    protected boolean returnProducerToPool(JmsPooledProducer producer) {
        boolean success = false;
        String destKey = producer.getDestKey();

        synchronized (inUseProducers) {
            JmsPooledProducer inUse = inUseProducers.remove(destKey);

            // make sure the one we removed is indeed this producer, 99%
            // of time this is correct
            success = inUse == producer;

            if (!success && (inUse != null)) {
                // put the bad removal back in. Done this way instead of
                // get/remove as 99% of time remove is correct, this
                // really only here for bullet proofing code against bad
                // use of pool
                inUseProducers.put(destKey, inUse);
                statusHandler
                        .handle(Priority.INFO,
                                "Another producer already in use for this destination, not returning this producer to pool");
            }
        }

        synchronized (stateLock) {
            if (success && !State.Closed.equals(state) && producer.isValid()) {
                // only put it in the available pool if it was successfully
                // remove from the inUse pool
                synchronized (availableProducers) {
                    AvailableJmsPooledObject<JmsPooledProducer> prev = availableProducers
                            .put(destKey,
                                    new AvailableJmsPooledObject<JmsPooledProducer>(
                                            producer));
                    if (prev != null) {
                        // there was a previous producer registered to
                        // this destination, close it down
                        prev.getPooledObject().close();
                        statusHandler
                                .handle(Priority.INFO,
                                        "Another producer already pooled this destination, closing previous producer");
                    }
                }
            } else {
                success = false;
            }
        }

        return success;
    }

    /**
     * Removes the producer from the pool.
     * 
     * @param producer
     * @return
     */
    protected boolean removeProducerFromPool(JmsPooledProducer producer) {
        String destKey = producer.getDestKey();
        boolean removed = false;

        synchronized (inUseProducers) {
            JmsPooledProducer inUse = inUseProducers.remove(destKey);
            removed = inUse == producer;

            if (!removed && (inUse != null)) {
                // put the bad removal back in. Done this way instead of
                // get/remove as 95% of time remove is correct, this
                // really only here for bullet proofing code against bad
                // use of pool
                inUseProducers.put(destKey, inUse);
            }
        }

        return removed;
    }

    protected boolean returnConsumerToPool(JmsPooledConsumer consumer) {
        boolean success = false;
        String destKey = consumer.getDestKey();

        synchronized (inUseConsumers) {
            JmsPooledConsumer inUse = inUseConsumers.remove(destKey);

            // make sure the one we removed is indeed this consumer, 99%
            // of time this is correct
            success = inUse == consumer;

            if (!success) {
                // put the bad removal back in. Done this way instead of
                // get/remove as 99% of time remove is correct
                if (inUse != null) {
                    inUseConsumers.put(destKey, inUse);
                    statusHandler
                            .handle(Priority.DEBUG,
                                    "Another consumer already in use for this destination, not returning this consumer to pool");
                }
            }
        }

        synchronized (stateLock) {
            if (success && !State.Closed.equals(state) && consumer.isValid()) {
                try {
                    if (consumer.getConsumer().getMessageListener() != null) {
                        statusHandler
                                .warn("Consumer returned with MessageListener still set, clearing listener");
                        consumer.getConsumer().setMessageListener(null);
                    }
                } catch (Throwable e) {
                    statusHandler
                            .error("Error occured checking/clearing the messageListener",
                                    e);
                }

                // only put it in the available pool if it was successfully
                // remove from the inUse pool
                synchronized (availableConsumers) {
                    AvailableJmsPooledObject<JmsPooledConsumer> prev = availableConsumers
                            .put(destKey,
                                    new AvailableJmsPooledObject<JmsPooledConsumer>(
                                            consumer));
                    if (prev != null) {
                        // there was a previous consumer registered to
                        // this destination, close it down
                        prev.getPooledObject().close();
                        statusHandler
                                .handle(Priority.INFO,
                                        "Another consumer already pooled this destination, closing previous consumer");
                    }
                }
            } else {
                success = false;
            }
        }

        return success;
    }

    /**
     * Removes the consumer from the pool.
     * 
     * @param consumer
     * @return
     */
    protected boolean removeConsumerFromPool(JmsPooledConsumer consumer) {
        String destKey = consumer.getDestKey();
        boolean removed = false;

        synchronized (inUseConsumers) {
            JmsPooledConsumer inUse = inUseConsumers.remove(destKey);
            removed = inUse == consumer;

            if (!removed && (inUse != null)) {
                // put the bad removal back in. Done this way instead of
                // get/remove as 95% of time remove is correct, this
                // really only here for bullet proofing code against bad
                // use of pool
                inUseConsumers.put(destKey, inUse);
            }
        }

        return removed;
    }

    public void close() {
        boolean canClose = false;
        synchronized (stateLock) {
            if (!State.Closed.equals(state)) {
                canClose = true;
                state = State.Closed;
            }
        }

        if (canClose) {
            closePooledConsumersProducers();

            // need to close down all wrappers
            for (JmsSessionWrapper wrapper : references) {
                wrapper.closeWrapper();
            }

            references.clear();

            conn.removeSession(this);

            try {
                sess.close();
            } catch (Exception e) {
                statusHandler.handle(Priority.WARN, "Failed to close session "
                        + sess, e);
            }
        }
    }

    public JmsConsumerWrapper getConsumer(Destination destination,
            String messageSelector) throws JMSException {
        // pooled objects are always valid, the underlying object may not be
        if (!isValid()) {
            // throw exception
            throw new IllegalStateException("Session is closed");
        }

        String destKey = null;
        if (destination instanceof Queue) {
            destKey = ((Queue) destination).getQueueName();
        } else if (destination instanceof Topic) {
            destKey = ((Topic) destination).getTopicName();
        }

        if (messageSelector != null) {
            destKey += messageSelector;
        }

        JmsPooledConsumer consumer = null;
        synchronized (inUseConsumers) {
            consumer = inUseConsumers.get(destKey);
            if (consumer != null) {
                JmsConsumerWrapper ref = consumer.createReference();
                if (ref != null) {
                    return ref;
                } else {
                    inUseConsumers.remove(destKey);
                    consumer.close();
                }
            }

            AvailableJmsPooledObject<JmsPooledConsumer> wrapper = null;
            synchronized (availableConsumers) {
                wrapper = availableConsumers.remove(destKey);
            }

            if (wrapper != null) {
                consumer = wrapper.getPooledObject();
                if (consumer != null) {
                    synchronized (consumer.getStateLock()) {
                        if (!State.Closed.equals(consumer.getState())) {
                            consumer.setState(State.InUse);
                            JmsConsumerWrapper ref = consumer.createReference();
                            if (ref != null) {
                                inUseConsumers.put(destKey, consumer);
                                return ref;
                            } else {
                                consumer.close();
                            }
                        } else {
                            // closed on another thread
                            consumer = null;
                        }
                    }
                }
            }

            if (consumer == null) {
                consumer = new JmsPooledConsumer(this, destKey, destination,
                        messageSelector);
            }

            inUseConsumers.put(destKey, consumer);
        }

        return consumer.createReference();

    }

    public JmsProducerWrapper getProducer(Destination destination)
            throws JMSException {
        // pooled objects are always valid, the underlying object may not be
        if (!isValid()) {
            // throw exception
            throw new IllegalStateException("Session is closed");
        }

        String destKey = null;
        if (destination instanceof Queue) {
            destKey = ((Queue) destination).getQueueName();
        } else if (destination instanceof Topic) {
            destKey = ((Topic) destination).getTopicName();
        }

        JmsPooledProducer producer = null;
        synchronized (inUseProducers) {
            producer = inUseProducers.get(destKey);
            if (producer != null) {
                JmsProducerWrapper ref = producer.createReference();
                if (ref != null) {
                    return ref;
                } else {
                    inUseProducers.remove(destKey);
                    producer.close();
                }
            }

            AvailableJmsPooledObject<JmsPooledProducer> wrapper = null;
            synchronized (availableProducers) {
                wrapper = availableProducers.remove(destKey);
            }

            if (wrapper != null) {
                producer = wrapper.getPooledObject();
                if (producer != null) {
                    synchronized (producer.getStateLock()) {
                        if (!State.Closed.equals(producer.getState())) {
                            producer.setState(State.InUse);
                            JmsProducerWrapper ref = producer.createReference();
                            if (ref != null) {
                                inUseProducers.put(destKey, producer);
                                return ref;
                            } else {
                                producer.close();
                            }
                        } else {
                            // closed on another thread
                            producer = null;
                        }
                    }
                }
            }

            if (producer == null) {
                producer = new JmsPooledProducer(this, destKey,
                        sess.createProducer(destination));
            }

            inUseProducers.put(destKey, producer);
        }

        return producer.createReference();
    }

    public void removeReference(JmsSessionWrapper wrapper) {
        boolean returnToPool = false;
        synchronized (stateLock) {
            if (references.remove(wrapper) && references.isEmpty()
                    && State.InUse.equals(state)) {
                state = State.Available;
                returnToPool = true;
            }
        }

        boolean valid = isValid() && hasProducersOrConsumers();
        if (valid && returnToPool) {
            valid = conn.returnSessionToPool(this);
        }

        if (!valid) {
            close();
        }
    }

    public int getReferenceCount() {
        synchronized (references) {
            return references.size();
        }
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
    }

    public Object getStateLock() {
        return stateLock;
    }

    public boolean hasProducersOrConsumers() {
        return !inUseConsumers.isEmpty() || !inUseProducers.isEmpty()
                || !availableConsumers.isEmpty()
                || !availableProducers.isEmpty();
    }
}
