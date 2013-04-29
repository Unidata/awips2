package com.raytheon.uf.common.event;

import java.util.ServiceLoader;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * The EventBus.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 11, 2012 1261       djohnson     Add SW history, create constants for fields, 
 *                                      add unregister.
 * Dec 11, 2012 1407       djohnson     Separate the creation of the Google EventBus from the wrapper class.
 * Feb 05, 2013 1580       mpduff       Moved to common, use IEventBusHandler.
 * Apr 29, 2013 1910       djohnson     Watch for NPEs and errors unregistering.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public final class EventBus {

    private static final IEventBusHandler handler;
    static {
        handler = ServiceLoader.<IEventBusHandler> load(IEventBusHandler.class)
                .iterator().next();
    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EventBus.class);

    private static final String NULL_SUBSCRIBER = "Ignoring a null subscriber.";

    private EventBus() {

    }

    /**
     * Register with the EventBus.
     * 
     * @param subscriber
     *            The subscriber to register
     */
    public static void register(Object subscriber) {
        if (subscriber != null) {
            handler.register(subscriber);
        } else {
            statusHandler.handle(Priority.WARN, NULL_SUBSCRIBER,
                    new IllegalArgumentException(NULL_SUBSCRIBER));
        }
    }

    /**
     * Unregister from the EventBus.
     * 
     * @param subscriber
     *            The subscriber to unregister
     */
    public static void unregister(Object subscriber) {
        if (subscriber != null) {
            try {
                handler.unregister(subscriber);
            } catch (Throwable t) {
                statusHandler.handle(Priority.WARN,
                        "Unable to unregister subscriber of type ["
                                + subscriber.getClass().getName()
                                + "] from the retrieval event bus!", t);
            }
        } else {
            statusHandler.handle(Priority.WARN, NULL_SUBSCRIBER,
                    new IllegalArgumentException(NULL_SUBSCRIBER));
        }
    }

    /**
     * Publish the event.
     * 
     * @param event
     *            The event to publish
     */
    public static void publish(Event event) {
        if (event == null) {
            throw new IllegalArgumentException("Cannot publish a null event");
        }

        handler.publish(event);
    }
}
