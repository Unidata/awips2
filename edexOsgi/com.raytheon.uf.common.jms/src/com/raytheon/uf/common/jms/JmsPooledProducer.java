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

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageProducer;

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
 * Apr 18, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsPooledProducer implements MessageProducer {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledConsumer.class);

    private final JmsPooledSession sess;

    private final MessageProducer producer;

    private final String destKey;

    private boolean exceptionOccurred = false;

    public JmsPooledProducer(JmsPooledSession sess, MessageProducer producer,
            String destKey) {
        this.sess = sess;
        this.producer = producer;
        this.destKey = destKey;
    }

    public String getDestKey() {
        return destKey;
    }

    public boolean isExceptionOccurred() {
        return exceptionOccurred;
    }

    public void closeInternal() {
        try {
            producer.close();
        } catch (Throwable e) {
            statusHandler.handle(Priority.INFO, "Failed to close producer", e);
            exceptionOccurred = true;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#close()
     */
    @Override
    public void close() throws JMSException {
        sess.returnProducerToPool(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#getDeliveryMode()
     */
    @Override
    public int getDeliveryMode() throws JMSException {
        try {
            return producer.getDeliveryMode();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#getDestination()
     */
    @Override
    public Destination getDestination() throws JMSException {
        try {
            return producer.getDestination();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#getDisableMessageID()
     */
    @Override
    public boolean getDisableMessageID() throws JMSException {
        try {
            return producer.getDisableMessageID();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#getDisableMessageTimestamp()
     */
    @Override
    public boolean getDisableMessageTimestamp() throws JMSException {
        try {
            return producer.getDisableMessageTimestamp();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#getPriority()
     */
    @Override
    public int getPriority() throws JMSException {
        try {
            return producer.getPriority();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#getTimeToLive()
     */
    @Override
    public long getTimeToLive() throws JMSException {
        try {
            return producer.getTimeToLive();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#send(javax.jms.Message)
     */
    @Override
    public void send(Message message) throws JMSException {
        try {
            producer.send(message);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#send(javax.jms.Destination,
     * javax.jms.Message)
     */
    @Override
    public void send(Destination destination, Message message)
            throws JMSException {
        try {
            producer.send(destination, message);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#send(javax.jms.Message, int, int, long)
     */
    @Override
    public void send(Message message, int deliveryMode, int priority,
            long timeToLive) throws JMSException {
        try {
            producer.send(message, deliveryMode, priority, timeToLive);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#send(javax.jms.Destination,
     * javax.jms.Message, int, int, long)
     */
    @Override
    public void send(Destination destination, Message message,
            int deliveryMode, int priority, long timeToLive)
            throws JMSException {
        try {
            producer.send(destination, message, deliveryMode, priority,
                    timeToLive);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#setDeliveryMode(int)
     */
    @Override
    public void setDeliveryMode(int deliveryMode) throws JMSException {
        try {
            producer.setDeliveryMode(deliveryMode);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#setDisableMessageID(boolean)
     */
    @Override
    public void setDisableMessageID(boolean value) throws JMSException {
        try {
            producer.setDisableMessageID(value);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#setDisableMessageTimestamp(boolean)
     */
    @Override
    public void setDisableMessageTimestamp(boolean value) throws JMSException {
        try {
            producer.setDisableMessageTimestamp(value);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#setPriority(int)
     */
    @Override
    public void setPriority(int defaultPriority) throws JMSException {
        try {
            producer.setPriority(defaultPriority);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#setTimeToLive(long)
     */
    @Override
    public void setTimeToLive(long timeToLive) throws JMSException {
        try {
            producer.setTimeToLive(timeToLive);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled producer");
            exc.initCause(e);
            throw exc;
        }
    }

}
