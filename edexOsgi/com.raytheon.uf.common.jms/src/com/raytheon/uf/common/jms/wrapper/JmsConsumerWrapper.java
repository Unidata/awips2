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

import javax.jms.IllegalStateException;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;

import com.raytheon.uf.common.jms.JmsPooledConsumer;

/**
 * Wrapper class for jms consumer pooling. Helps track references to the pooled
 * consumer to know when the consumer can be closed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2011            rjpeter     Initial creation.
 * Feb 26, 2013 1642       rjpeter     Added volatile references for better concurrency handling.
 * Feb 07, 2014 2357       rjpeter     Set linked exception in exception handling.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsConsumerWrapper implements MessageConsumer {
    private final JmsPooledConsumer mgr;

    private volatile boolean exceptionOccurred = false;

    private volatile boolean closed = false;

    public JmsConsumerWrapper(JmsPooledConsumer mgr) {
        this.mgr = mgr;
    }

    public boolean isExceptionOccurred() {
        return exceptionOccurred;
    }

    private MessageConsumer getConsumer() throws JMSException {
        if (closed) {
            throw new IllegalStateException("Consumer closed");
        }

        // should we check exception occurred and close if one occurred?
        return mgr.getConsumer();
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

                if (exceptionOccurred) {
                    mgr.setExceptionOccurred(true);
                }

                return true;
            }
        }

        return false;
    }

    /*
     * This should only be called by the client or the session wrapper. Will
     * close down this consumer wrapper and if an error has occurred will also
     * close down the underlying pooled consumer chaining to the other wrappers
     * of this consumer.
     * 
     * @see javax.jms.MessageProducer#close()
     */
    @Override
    public void close() throws JMSException {
        if (closeWrapper()) {
            mgr.removeReference(this);

            if (exceptionOccurred) {
                mgr.close();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageConsumer#getMessageListener()
     */
    @Override
    public MessageListener getMessageListener() throws JMSException {
        MessageConsumer consumer = getConsumer();

        try {
            return consumer.getMessageListener();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer in getMessageListener");
            exc.initCause(e);
            if (e instanceof Exception) {
                exc.setLinkedException((Exception) e);
            }
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageConsumer#getMessageSelector()
     */
    @Override
    public String getMessageSelector() throws JMSException {
        MessageConsumer consumer = getConsumer();

        try {
            return consumer.getMessageSelector();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer in getMessageSelector");
            exc.initCause(e);
            if (e instanceof Exception) {
                exc.setLinkedException((Exception) e);
            }
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageConsumer#receive()
     */
    @Override
    public Message receive() throws JMSException {
        MessageConsumer consumer = getConsumer();

        try {
            return consumer.receive();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer in receive");
            exc.initCause(e);
            if (e instanceof Exception) {
                exc.setLinkedException((Exception) e);
            }
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageConsumer#receive(long)
     */
    @Override
    public Message receive(long timeout) throws JMSException {
        MessageConsumer consumer = getConsumer();

        try {
            return consumer.receive(timeout);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer in receive(timeout)");
            exc.initCause(e);
            if (e instanceof Exception) {
                exc.setLinkedException((Exception) e);
            }
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageConsumer#receiveNoWait()
     */
    @Override
    public Message receiveNoWait() throws JMSException {
        MessageConsumer consumer = getConsumer();

        try {
            return consumer.receiveNoWait();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer in receiveNoWait");
            exc.initCause(e);
            if (e instanceof Exception) {
                exc.setLinkedException((Exception) e);
            }
            throw exc;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.jms.MessageConsumer#setMessageListener(javax.jms.MessageListener)
     */
    @Override
    public void setMessageListener(MessageListener listener)
            throws JMSException {
        MessageConsumer consumer = getConsumer();

        try {
            consumer.setMessageListener(listener);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer in setMessageLister");
            exc.initCause(e);
            if (e instanceof Exception) {
                exc.setLinkedException((Exception) e);
            }
            throw exc;
        }
    }
}
