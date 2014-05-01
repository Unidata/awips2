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
package com.raytheon.uf.edex.event;

import java.util.Collection;

import org.springframework.transaction.support.TransactionSynchronization;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Spring {@link TransactionSynchronization} that will post or discard events
 * based on whether or not the transaction commits/rolls back. Intentionally
 * package-private as the class is not part of the public API.
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
class EventTransactionSynchronization implements
        TransactionSynchronization {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EventTransactionSynchronization.class);

    private final Object event;

    private final Collection<EventBus> googleEventBuses;

    public EventTransactionSynchronization(Object event,
            Collection<EventBus> googleEventBuses) {
        this.event = event;
        this.googleEventBuses = googleEventBuses;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void suspend() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void resume() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void flush() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beforeCommit(boolean readOnly) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void beforeCompletion() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void afterCommit() {
    }

    @Override
    public void afterCompletion(int status) {
        if (status == TransactionSynchronization.STATUS_COMMITTED) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Posting event of type ["
                        + event.getClass().getName() + "] on the event bus");
            }

            for (EventBus eventBus : googleEventBuses) {
                eventBus.post(event);
            }
        } else if (status == TransactionSynchronization.STATUS_ROLLED_BACK) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Discarding event of type ["
                        + event.getClass().getName()
                        + "] due to transaction rolling back.");
            }
        }
    }

}
