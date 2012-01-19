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

import javax.jms.Destination;
import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageProducer;

import com.raytheon.uf.common.jms.JmsPooledProducer;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 8, 2011            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsProducerWrapper implements MessageProducer {
    private JmsPooledProducer mgr = null;

    private boolean exceptionOccurred = false;

    private boolean closed = false;

    public JmsProducerWrapper(JmsPooledProducer mgr) {
        this.mgr = mgr;
    }

    /**
     * Closes down this wrapper. Doesn't interact back with manager. For manager
     * interaction use close().
     * 
     * @return True if this wrapper hasn't been closed before, false otherwise.
     */
    public boolean closeInternal() {
        boolean close = false;

        synchronized (this) {
            if (!closed) {
                closed = true;
                close = true;
            }
        }

        if (close && exceptionOccurred) {
            mgr.setExceptionOccurred(true);
        }

        return close;
    }

    /*
     * This should only be called by the client or the session wrapper. Will
     * close down this producer wrapper and if an error has occurred will also
     * close down the underlying pooled producer chaining to the other wrappers
     * of this producer.
     * 
     * @see javax.jms.MessageProducer#close()
     */
    @Override
    public void close() throws JMSException {
        if (closeInternal()) {
            mgr.removeReference(this);

            if (exceptionOccurred) {
                mgr.close();
            }
        }
    }

    private MessageProducer getProducer() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Producer closed");
        }

        return mgr.getProducer();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageProducer#getDeliveryMode()
     */
    @Override
    public int getDeliveryMode() throws JMSException {
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
        MessageProducer producer = getProducer();

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
