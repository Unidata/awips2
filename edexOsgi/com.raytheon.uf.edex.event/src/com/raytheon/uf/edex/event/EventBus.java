package com.raytheon.uf.edex.event;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.event.Event;

/**
 * 
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EventBus {

    @VisibleForTesting
    static GoogleEventBusFactory eventBusFactory = new AsynchronousEventBusFactory();

    /**
     * Holder class which allows safe, concurrent, and on-demand creation of the
     * EventBus. It also enforces the singleton contract.
     */
    private static class EventBusHolder {
        private static com.google.common.eventbus.EventBus eventBus = eventBusFactory
                .getEventBus();

        private static final EventBus instance = new EventBus(eventBus);
    }

    /**
     * Returns the singleton instance of the data delivery event bus
     * 
     * @return the singleton instance
     */
    public static EventBus getInstance() {
        return EventBusHolder.instance;
    }

    /**
     * The actual Google EventBus being wrapped.
     */
    private final com.google.common.eventbus.EventBus googleEventBus;

    /**
     * Constructor that accepts a Google EventBus.
     * 
     * @param eventBus
     *            the Google EventBus
     */
    private EventBus(com.google.common.eventbus.EventBus eventBus) {
        this.googleEventBus = eventBus;
    }

    /**
     * Register an object with the event bus.
     * 
     * @param subscriber
     *            the subscriber to register
     */
    public void register(Object subscriber) {
        this.googleEventBus.register(subscriber);
    }

    /**
     * Publishes events for all subscribers to receive
     * 
     * @param event
     *            the event
     */
    public void publish(Event event) {
        this.googleEventBus.post(event);
    }

    /**
     * Unregister an object with the event bus.
     * 
     * @param subscriber
     *            the object subscribed to the event buss
     */
    public void unregister(Object subscriber) {
        this.googleEventBus.unregister(subscriber);
    }
}
