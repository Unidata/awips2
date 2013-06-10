package com.raytheon.uf.common.event;

import java.util.ServiceLoader;

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
 * May 28, 2013 1650       djohnson     Simplify and extract out the general event bus handling for reuse.
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

    private EventBus() {

    }

    /**
     * Register with the EventBus.
     * 
     * @param subscriber
     *            The subscriber to register
     */
    public static void register(Object subscriber) {
        handler.register(subscriber);
    }

    /**
     * Unregister from the EventBus.
     * 
     * @param subscriber
     *            The subscriber to unregister
     */
    public static void unregister(Object subscriber) {
        handler.unregister(subscriber);
    }

    /**
     * Publish the event.
     * 
     * @param event
     *            The event to publish
     */
    public static void publish(Event event) {
        handler.publish(event);
    }
}
