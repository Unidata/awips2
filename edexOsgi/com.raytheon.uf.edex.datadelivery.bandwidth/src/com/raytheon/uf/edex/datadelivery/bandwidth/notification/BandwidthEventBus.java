package com.raytheon.uf.edex.datadelivery.bandwidth.notification;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.SubscriptionRetrievalFulfilled;

/**
 * Class encapsulating the notification system used by BandwidthManager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 02, 2012 726        jspinks     Initial creation
 * Oct 10, 2012 0726       djohnson    Make buses final.
 * Dec 11, 2012 1286       djohnson    Create a factory to hold Google event buses.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public class BandwidthEventBus {

    @VisibleForTesting
    static BandwidthEventBusFactory eventBusFactory = new BandwidthAsyncEventBusFactory();

    /**
     * Holder class which allows safe, concurrent, and on-demand creation of the
     * EventBus. It also enforces the singleton contract.
     */
    private static class EventBusHolder {
        private static final com.google.common.eventbus.EventBus dataSetBus = eventBusFactory
                .getDataSetBus();

        private static final com.google.common.eventbus.EventBus subscriptionBus = eventBusFactory
                .getSubscriptionBus();

        private static final com.google.common.eventbus.EventBus retrievalBus = eventBusFactory
                .getRetrievalBus();
    }

    /**
     * Registers an object with the event bus.
     * 
     * @param subscriber
     */
    public static void register(Object subscriber) {
        EventBusHolder.retrievalBus.register(subscriber);
        EventBusHolder.subscriptionBus.register(subscriber);
        EventBusHolder.dataSetBus.register(subscriber);
    }

    /**
     * Unregister an Object with the event bus.
     * 
     * @param subscriber
     */
    public static void unregister(Object subscriber) {
        EventBusHolder.retrievalBus.unregister(subscriber);
        EventBusHolder.subscriptionBus.unregister(subscriber);
        EventBusHolder.dataSetBus.unregister(subscriber);
    }

    /**
     * Publishes events for all subscribers to receive
     * 
     * @param event
     */
    public static void publish(Object object) {
        if (object instanceof SubscriptionRetrieval) {
            EventBusHolder.retrievalBus.post(object);
        } else if (object instanceof SubscriptionRetrievalFulfilled) {
            EventBusHolder.subscriptionBus.post(object);
        } else if (object instanceof DataSetMetaData) {
            EventBusHolder.dataSetBus.post(object);
        } else if (object instanceof Subscription) {
            EventBusHolder.subscriptionBus.post(object);
        } else if (object instanceof RemoveRegistryEvent) {
            EventBusHolder.subscriptionBus.post(object);
        } else {
            throw new IllegalArgumentException("Object type ["
                    + object.getClass().getName()
                    + "] not supported in BandwidthEventBus");
        }
    }

}
