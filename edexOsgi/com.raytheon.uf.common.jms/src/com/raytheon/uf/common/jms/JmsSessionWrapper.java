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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsSessionWrapper implements Session {
    private JmsPooledSession session = null;

    private boolean closed = false;

    public JmsSessionWrapper(JmsPooledSession session) {
        this.session = session;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#close()
     */
    @Override
    public void close() throws JMSException {
        if (!closed) {
            synchronized (this) {
                // verify wasn't closed on another thread
                if (!closed) {
                    closed = true;
                    session.close();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#commit()
     */
    @Override
    public void commit() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        session.commit();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createBrowser(javax.jms.Queue)
     */
    @Override
    public QueueBrowser createBrowser(Queue queue) throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createBrowser(queue);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createBrowser(javax.jms.Queue, java.lang.String)
     */
    @Override
    public QueueBrowser createBrowser(Queue queue, String messageSelector)
            throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createBrowser(queue, messageSelector);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createBytesMessage()
     */
    @Override
    public BytesMessage createBytesMessage() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createBytesMessage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createConsumer(javax.jms.Destination)
     */
    @Override
    public MessageConsumer createConsumer(Destination destination)
            throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createConsumer(destination);
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
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createConsumer(destination, messageSelector);
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
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createConsumer(destination, messageSelector, noLocal);
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
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createDurableSubscriber(topic, name);
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
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createDurableSubscriber(topic, name, messageSelector,
                noLocal);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createMapMessage()
     */
    @Override
    public MapMessage createMapMessage() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createMapMessage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createMessage()
     */
    @Override
    public Message createMessage() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createMessage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createObjectMessage()
     */
    @Override
    public ObjectMessage createObjectMessage() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createObjectMessage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createObjectMessage(java.io.Serializable)
     */
    @Override
    public ObjectMessage createObjectMessage(Serializable obj)
            throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createObjectMessage(obj);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createProducer(javax.jms.Destination)
     */
    @Override
    public MessageProducer createProducer(Destination destination)
            throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createProducer(destination);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createQueue(java.lang.String)
     */
    @Override
    public Queue createQueue(String queueName) throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createQueue(queueName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createStreamMessage()
     */
    @Override
    public StreamMessage createStreamMessage() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createStreamMessage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTemporaryQueue()
     */
    @Override
    public TemporaryQueue createTemporaryQueue() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createTemporaryQueue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTemporaryTopic()
     */
    @Override
    public TemporaryTopic createTemporaryTopic() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createTemporaryTopic();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTextMessage()
     */
    @Override
    public TextMessage createTextMessage() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createTextMessage();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTextMessage(java.lang.String)
     */
    @Override
    public TextMessage createTextMessage(String text) throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createTextMessage(text);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#createTopic(java.lang.String)
     */
    @Override
    public Topic createTopic(String topicName) throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.createTopic(topicName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#getAcknowledgeMode()
     */
    @Override
    public int getAcknowledgeMode() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.getAcknowledgeMode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#getMessageListener()
     */
    @Override
    public MessageListener getMessageListener() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.getMessageListener();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#getTransacted()
     */
    @Override
    public boolean getTransacted() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        return session.getTransacted();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#recover()
     */
    @Override
    public void recover() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        session.recover();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#rollback()
     */
    @Override
    public void rollback() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        session.rollback();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#run()
     */
    @Override
    public void run() {
        if (!closed) {
            session.run();
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
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        session.setMessageListener(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.Session#unsubscribe(java.lang.String)
     */
    @Override
    public void unsubscribe(String name) throws JMSException {
        if (closed) {
            throw new IllegalStateException("Session closed");
        }

        session.unsubscribe(name);
    }
}
