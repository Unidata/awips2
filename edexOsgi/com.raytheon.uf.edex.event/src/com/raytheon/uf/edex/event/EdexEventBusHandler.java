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

import java.util.List;

import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.Lists;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.event.IEventBusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * EDEX implementation of {@link IEventBusHandler}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2013  1580       mpduff      Initial creation.
 * 3/18/2013    1802       bphillip    Modified to use transaction synchronization
 * May 9, 2013  1989       njensen     Spring 3.1.4 compatibility
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class EdexEventBusHandler implements IEventBusHandler,
        TransactionSynchronization {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EdexEventBusHandler.class);

    private static ThreadLocal<List<Event>> eventStorageList = new ThreadLocal<List<Event>>() {

        @Override
        protected List<Event> initialValue() {
            return Lists.newArrayList();
        }

    };

    @VisibleForTesting
    EdexEventBusHandler(GoogleEventBusFactory eventBusFactory) {
        this.googleEventBus = eventBusFactory.getEventBus();
    }

    /**
     * The actual Google EventBus being wrapped.
     */
    private final com.google.common.eventbus.EventBus googleEventBus;

    /**
     * Constructor.
     */
    public EdexEventBusHandler() {
        this(new AsynchronousEventBusFactory());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void publish(Event event) {
        if (TransactionSynchronizationManager.isActualTransactionActive()) {

            if (TransactionSynchronizationManager.isSynchronizationActive()) {
                if (!TransactionSynchronizationManager.getSynchronizations()
                        .contains(this)) {
                    TransactionSynchronizationManager
                            .registerSynchronization(this);
                }
            }
            eventStorageList.get().add(event);
        } else {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler
                        .debug("Sending event from non-transactional operation");
            }
            this.googleEventBus.post(event);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void register(Object subscriber) {
        this.googleEventBus.register(subscriber);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unregister(Object subscriber) {
        this.googleEventBus.unregister(subscriber);
    }

    @Override
    public void suspend() {
    }

    @Override
    public void resume() {
    }

    @Override
    public void beforeCommit(boolean readOnly) {
    }

    @Override
    public void beforeCompletion() {
    }

    @Override
    public void afterCommit() {

    }

    @Override
    public void afterCompletion(int status) {
        List<Event> list = eventStorageList.get();
        if (status == TransactionSynchronization.STATUS_COMMITTED) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Posting " + list.size()
                        + " events on the event bus");
            }
            for (Event event : list) {
                this.googleEventBus.post(event);
            }
        } else if (status == TransactionSynchronization.STATUS_ROLLED_BACK) {
            statusHandler.info("Transaction rolled back. Discarding "
                    + list.size() + " events.");
        }
        list.clear();
    }

    @Override
    public void flush() {
    }
}
