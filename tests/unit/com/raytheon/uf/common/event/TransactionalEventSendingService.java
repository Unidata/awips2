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
package com.raytheon.uf.common.event;

import org.hibernate.HibernateException;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.stats.ProcessEvent;

/**
 * Provides methods providing several different forms of transaction behavior,
 * which allows testing of various transaction issues with {@link Event}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2013 1802       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
@Service
@Transactional
public class TransactionalEventSendingService implements
        ApplicationContextAware {

    public static final String OUTER_TRANSACTION_EVENT_MESSAGE = "this event occurs in the outer transaction";

    public static final String REQUIRES_NEW_TRANSACTION_EVENT_MESSAGE = "this event occurs in the requires new transaction";

    private ApplicationContext applicationContext;

    /**
     * Sends a {@link ProcessEvent}, invokes a requires new transaction, and
     * rolls back the original transaction which means the event should NOT be
     * published.
     * 
     * @throws HibernateException
     *             every time
     */
    public void sendEventFromTransactionThenInvokeRequiresNewTransactionAndRollbackOriginal()
            throws HibernateException {
        // The event sent from this method should never be received
        sendOuterTransactionEvent();

        applicationContext.getBean(TransactionalEventSendingService.class)
                .requiresNewTransactionSendsEvent();

        throw new HibernateException("Rolling back transaction");
    }

    /**
     * Sends a {@link ProcessEvent}, invokes a requires new transaction, and
     * commits the original transaction which means the event should be
     * published.
     */
    public void sendEventFromTransactionThenInvokeRequiresNewTransactionAndCommitOriginal() {
        sendOuterTransactionEvent();

        applicationContext.getBean(TransactionalEventSendingService.class)
                .requiresNewTransactionSendsEvent();

        // No exception thrown, will commit successfully
    }

    /**
     * Sends a {@link ProcessEvent}, invokes a requires new transaction that
     * rolls back, and commits the original transaction which means the event
     * should be published but not the requires new transaction event.
     * 
     * @throws IllegalStateException
     *             if the requires new transaction does not throw an exception
     *             and rollback
     */
    @Test
    public void sendEventFromTransactionThenInvokeRequiresNewTransactionThatRollsBackAndCommitOriginal()
            throws IllegalStateException {
        sendOuterTransactionEvent();

        try {
            applicationContext.getBean(TransactionalEventSendingService.class)
                    .requiresNewTransactionSendsEventThatRollsBack();

            throw new IllegalStateException(
                    "Expected the requires new transaction to throw a HibernateException!");
        } catch (HibernateException e) {
            // Expected path
        }
    }

    /**
     * Starts a new transaction, sends an event, and rolls back the transaction
     * by throwing an exception.
     * 
     * @throws {@link HibernateException} every time
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void requiresNewTransactionSendsEventThatRollsBack()
            throws HibernateException {
        final ProcessEvent event = new ProcessEvent();
        event.setMessage(REQUIRES_NEW_TRANSACTION_EVENT_MESSAGE);
        EventBus.publish(event);

        throw new HibernateException(
                "this requires new transaction will roll back");
    }

    /**
     * Starts a new transaction, sends an event, and commits the transaction.
     */
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void requiresNewTransactionSendsEvent() {
        final ProcessEvent event = new ProcessEvent();
        event.setMessage(REQUIRES_NEW_TRANSACTION_EVENT_MESSAGE);
        EventBus.publish(event);
    }

    private void sendOuterTransactionEvent() {
        final ProcessEvent event = new ProcessEvent();
        event.setMessage(OUTER_TRANSACTION_EVENT_MESSAGE);
        EventBus.publish(event);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setApplicationContext(ApplicationContext applicationContext)
            throws BeansException {
        this.applicationContext = applicationContext;
    }

}
