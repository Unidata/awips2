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
import java.util.List;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;

import com.raytheon.uf.common.jms.wrapper.JmsConsumerWrapper;
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
 * Mar 08, 2012   194   njensen   Improved logging
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class JmsPooledConsumer {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(JmsPooledConsumer.class);

    private final JmsPooledSession sess;

    private final Destination destination;

    private final String messageSelector;

    private MessageConsumer consumer;

    private final String destKey;

    private boolean exceptionOccurred = false;

    private Object stateLock = new Object();

    private State state = State.InUse;

    /**
     * Technically a pooled consumer should only have 1 reference at a time.
     * Bullet proofing in case 3rd party ever tries to get multiple consumers to
     * the same destination.
     */
    private List<JmsConsumerWrapper> references = new ArrayList<JmsConsumerWrapper>(
            1);

    public JmsPooledConsumer(JmsPooledSession sess, String destKey,
            Destination destination, String messageSelector) {
        this.sess = sess;
        this.destKey = destKey;
        this.destination = destination;
        this.messageSelector = messageSelector;
    }

    public String getDestKey() {
        return destKey;
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
                if (consumer != null) {
                    try {
                        consumer.getMessageSelector();
                    } catch (JMSException e) {
                        // underlying consumer has been closed
                        valid = false;
                    }
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

    /**
     * Close down this pooled producer, closes the internal producer reference,
     * and removes from session pool.
     */
    public void close() {
        boolean close = false;

        // only thing in sync block is setting close to prevent dead locking
        // between manager and wrapper, general design principal on sync blocks
        // is chained blocks only in a downward direction (i.e. a
        synchronized (stateLock) {
            if (!State.Closed.equals(state)) {
                state = State.Closed;
                close = true;

                for (JmsConsumerWrapper wrapper : references) {
                    wrapper.closeInternal();
                }

                references.clear();
            }
        }

        if (close) {
            if (consumer != null) {
                try {
                    statusHandler.info("Closing consumer " + consumer); // njensen
                    consumer.close();
                } catch (Throwable e) {
                    statusHandler.handle(Priority.INFO,
                            "Failed to close consumer " + consumer, e);
                }
                consumer = null;
            }

            sess.removeConsumerFromPool(this);
        }
    }

    public JmsConsumerWrapper createReference() {
        synchronized (stateLock) {
            if (isValid(State.InUse, true)) {
                JmsConsumerWrapper wrapper = new JmsConsumerWrapper(this);
                references.add(wrapper);
                return wrapper;
            }
        }

        return null;
    }

    public void removeReference(JmsConsumerWrapper wrapper) {
        boolean returnToPool = false;
        synchronized (stateLock) {
            if (references.remove(wrapper) && references.isEmpty()
                    && State.InUse.equals(state)) {
                returnToPool = true;
            }
        }

        boolean valid = isValid();
        if (valid && returnToPool) {
            valid = sess.returnConsumerToPool(this);
        }

        if (!valid) {
            close();
        }
    }

    public MessageConsumer getConsumer() throws JMSException {
        // TODO: allow this to automatically grab a new consumer if the current
        // one fails, try up to 3 times so that we don't always drop messages on
        // a single failure
        if (consumer == null) {
            synchronized (this) {
                if (consumer == null) {
                    consumer = sess.getSession().createConsumer(destination,
                            messageSelector);
                }
            }
        }

        return consumer;
    }

    /**
     * @return the state
     */
    public State getState() {
        return state;
    }

    /**
     * @param state
     *            the state to set
     */
    public void setState(State state) {
        this.state = state;
    }

    /**
     * @return the stateLock
     */
    public Object getStateLock() {
        return stateLock;
    }
}
