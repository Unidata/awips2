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

import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Set;

import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import com.google.common.collect.Lists;
import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.event.IBaseEventBusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * EDEX implementation of {@link IBaseEventBusHandler}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2013 1650       djohnson     Simplified and extracted from {@link EdexEventBusHandler}.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class BaseEdexEventBusHandler<T> implements
        IBaseEventBusHandler<T>, TransactionSynchronization {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BaseEdexEventBusHandler.class);

    private final ThreadLocal<List<T>> eventStorageList = new ThreadLocal<List<T>>() {

        @Override
        protected List<T> initialValue() {
            return Lists.newArrayList();
        }

    };

    private static final String NULL_SUBSCRIBER = "Ignoring a null subscriber.";

    // Set that keeps a reference to all objects which have registered, which
    // simplifies whether or not an object should be unregistered since google
    // eventbus doesn't have a way of knowing who did or did not register
    private final Set<Object> registeredObjects = Collections
            .synchronizedSet(Collections
                    .<Object> newSetFromMap(new IdentityHashMap<Object, Boolean>()));

    /**
     * The actual Google EventBus instances being wrapped.
     */
    protected final List<com.google.common.eventbus.EventBus> googleEventBuses;

    /**
     * Constructor.
     */
    public BaseEdexEventBusHandler() {
        this(new AsynchronousEventBusFactory());
    }

    /**
     * Constructor specifying how to create the EventBus instances.
     * 
     * @param eventBusFactory
     *            the factory
     */
    protected BaseEdexEventBusHandler(GoogleEventBusFactory eventBusFactory) {
        this.googleEventBuses = eventBusFactory.getEventBuses();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void publish(T event) {
        if (event == null) {
            throw new IllegalArgumentException("Cannot publish a null event");
        }

        if (isTransactionActive()) {

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
            publishInternal(event);
        }
    }

    /**
     * Publish the actual event object.
     * 
     * @param event
     *            the event
     */
    protected abstract void publishInternal(T event);

    /**
     * Check to see if a transaction is active.
     * 
     * @return true if a transaction is active
     */
    protected boolean isTransactionActive() {
        return TransactionSynchronizationManager.isActualTransactionActive();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object register(Object subscriber) {
        if (subscriber != null) {
            final boolean registered = registeredObjects.add(subscriber);
            if (registered) {
                for (EventBus eventBus : googleEventBuses) {
                    eventBus.register(subscriber);
                }
            }

            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                final String logMsg = (registered) ? "Registered subscriber of type ["
                        + subscriber.getClass().getName()
                        + "] with the event bus."
                        : "Ignoring request to register subscriber of type ["
                                + subscriber.getClass().getName()
                                + "] from the event bus, as it was already registered!";

                statusHandler.handle(Priority.DEBUG, logMsg);
            }
        } else {
            statusHandler.handle(Priority.WARN, NULL_SUBSCRIBER,
                    new IllegalArgumentException(NULL_SUBSCRIBER));
        }

        return subscriber;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unregister(Object subscriber) {
        if (subscriber != null) {
            final boolean removed = registeredObjects.remove(subscriber);
            if (removed) {
                for (EventBus eventBus : googleEventBuses) {
                    eventBus.unregister(subscriber);
                }
            }

            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                final String logMsg = (removed) ? "Unregistered subscriber of type ["
                        + subscriber.getClass().getName()
                        + "] from the event bus."
                        : "Ignoring request to unregister subscriber of type ["
                                + subscriber.getClass().getName()
                                + "] from the event bus, as it was never registered!";

                statusHandler.handle(Priority.DEBUG, logMsg);
            }
        } else {
            statusHandler.handle(Priority.WARN, NULL_SUBSCRIBER,
                    new IllegalArgumentException(NULL_SUBSCRIBER));
        }
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
        List<T> list = eventStorageList.get();
        if (status == TransactionSynchronization.STATUS_COMMITTED) {
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Posting " + list.size()
                        + " objects on the event bus");
            }
            for (T event : list) {
                for (EventBus eventBus : googleEventBuses) {
                    eventBus.post(event);
                }
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
