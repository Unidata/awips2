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
package com.raytheon.uf.common.jms.wrapper;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.IllegalStateException;
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

import com.raytheon.uf.common.jms.JmsPooledSession;

/**
 * Wrapper class for jms session pooling. Tracks wrapped consumers/producers
 * created from this wrapped session to know when the session can be returned to
 * the pool.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            rjpeter     Initial creation
 * Feb 26, 2013 1642       rjpeter     Added volatile references for better concurrency handling.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsSessionWrapper implements Session {
    private final JmsPooledSession mgr;

    private volatile boolean closed = false;

    private volatile boolean exceptionOccurred = false;

    private final List<JmsProducerWrapper> producers = new ArrayList<JmsProducerWrapper>(
            1);;

    private final List<JmsConsumerWrapper> consumers = new ArrayList<JmsConsumerWrapper>(
            1);

    public JmsSessionWrapper(JmsPooledSession mgr) {
        this.mgr = mgr;
    }

    /**
     * Closes down this wrapper. Doesn't interact back with manager. For manager
     * interaction use close().
     * 
     * @return True if this wrapper hasn't been closed before, false otherwise.
     */
    public boolean closeWrapper() {
        synchronized (this) {
            if (!closed) {
                closed = true;
                for (JmsConsumerWrapper consumer : consumers) {
                    try {
                        consumer.close();
                    } catch (JMSException e) {

                    }
                }

                for (JmsProducerWrapper producer : producers) {
                    try {
                        producer.close();
                    } catch (JMSException e) {

                    }
                }

                if (exceptionOccurred) {
                    mgr.setExceptionOccurred(true);
                }
                return true;
            }
        }

        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#close()
     */
    @Override
    public void close() throws JMSException {
        if (closeWrapper()) {
            // remove this wrapper from the manager
            mgr.removeReference(this);

            if (exceptionOccurred) {
                mgr.close();
            }

        }
    }

    public Session getSession() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return mgr.getSession();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#commit()
     */
    @Override
    public void commit() throws JMSException {
        Session sess = getSession();

        try {
            sess.commit();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createBrowser(queue);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createBrowser(queue, messageSelector);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createBytesMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        try {
            JmsConsumerWrapper consumer = mgr.getConsumer(destination,
                    messageSelector);

            if (consumer != null) {
                consumers.add(consumer);
            } else {
                throw new IllegalStateException("Underlying consumer is closed");
            }

            return consumer;
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
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
        if (noLocal) {
            Session sess = getSession();

            try {
                // no pooling for now
                return sess.createConsumer(destination, messageSelector,
                        noLocal);
            } catch (Throwable e) {
                exceptionOccurred = true;
                JMSException exc = new JMSException(
                        "Exception occurred on pooled session");
                exc.initCause(e);
                throw exc;
            }
        } else {
            return createConsumer(destination, messageSelector);
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
        Session sess = getSession();

        try {
            // no pooling for now
            return sess.createDurableSubscriber(topic, name);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            // no pooling for now
            return sess.createDurableSubscriber(topic, name, messageSelector,
                    noLocal);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createMapMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createObjectMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createObjectMessage(obj);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        try {
            JmsProducerWrapper producer = mgr.getProducer(destination);

            if (producer != null) {
                producers.add(producer);
            } else {
                throw new IllegalStateException("Underlying producer is closed");
            }

            return producer;
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createQueue(java.lang.String)
     */
    @Override
    public Queue createQueue(String queueName) throws JMSException {
        Session sess = getSession();

        try {
            return sess.createQueue(queueName);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createStreamMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createTemporaryQueue();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createTemporaryTopic();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createTextMessage();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createTextMessage(text);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.createTopic(topicName);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.getAcknowledgeMode();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.getMessageListener();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            return sess.getTransacted();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            sess.recover();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            sess.rollback();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
            Session sess = getSession();
            sess.run();
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            sess.setMessageListener(listener);
        } catch (Throwable e) {
            exceptionOccurred = true;
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
        Session sess = getSession();

        try {
            sess.unsubscribe(name);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled session");
            exc.initCause(e);
            throw exc;
        }
    }
}
