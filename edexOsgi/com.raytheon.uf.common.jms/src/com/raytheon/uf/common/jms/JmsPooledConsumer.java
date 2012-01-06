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

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;

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

public class JmsPooledConsumer implements MessageConsumer {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledConsumer.class);

    private final JmsPooledSession sess;

    private final MessageConsumer consumer;

    private final String destKey;

    private boolean exceptionOccurred = false;

    public JmsPooledConsumer(JmsPooledSession sess, MessageConsumer consumer,
            String destKey) {
        this.sess = sess;
        this.consumer = consumer;
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
            consumer.close();
        } catch (Throwable e) {
            exceptionOccurred = true;
            statusHandler.handle(Priority.INFO, "Failed to close consumer", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageConsumer#close()
     */
    @Override
    public void close() throws JMSException {
        sess.returnConsumerToPool(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.jms.MessageConsumer#getMessageListener()
     */
    @Override
    public MessageListener getMessageListener() throws JMSException {
        try {
            return consumer.getMessageListener();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer");
            exc.initCause(e);
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
        try {
            return consumer.getMessageSelector();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer");
            exc.initCause(e);
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
        try {
            return consumer.receive();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer");
            exc.initCause(e);
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
        try {
            return consumer.receive(timeout);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer");
            exc.initCause(e);
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
        try {
            return consumer.receiveNoWait();
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer");
            exc.initCause(e);
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
        try {
            consumer.setMessageListener(listener);
        } catch (Throwable e) {
            exceptionOccurred = true;
            JMSException exc = new JMSException(
                    "Exception occurred on pooled consumer");
            exc.initCause(e);
            throw exc;
        }
    }
}
