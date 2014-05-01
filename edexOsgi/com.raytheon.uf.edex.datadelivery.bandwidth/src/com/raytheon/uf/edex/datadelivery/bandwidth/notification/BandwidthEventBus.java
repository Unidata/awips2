package com.raytheon.uf.edex.datadelivery.bandwidth.notification;

import java.util.ServiceLoader;

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
 * Feb 07, 2013 1543       djohnson    Changed to behave similarly to EventBus.
 * Apr 29, 2013 1910       djohnson    Watch for NPEs and errors unregistering.
 * May 28, 2013 1650       djohnson    Simplify and use the extracted general event bus handling.
 * 
 * </pre>
 * 
 * @version 1.0
 */
public final class BandwidthEventBus {

    private static final IBandwidthEventBusHandler handler;
    static {
        handler = ServiceLoader
                .<IBandwidthEventBusHandler> load(
                        IBandwidthEventBusHandler.class).iterator().next();
    }

    private BandwidthEventBus() {

    }

    /**
     * Registers an object with the event bus.
     * 
     * @param subscriber
     */
    public static void register(Object subscriber) {
        handler.register(subscriber);
    }

    /**
     * Unregister an Object with the event bus.
     * 
     * @param subscriber
     */
    public static void unregister(Object subscriber) {
        handler.unregister(subscriber);
    }

    /**
     * Publishes events for all subscribers to receive
     * 
     * @param object
     */
    public static void publish(Object object) {
        handler.publish(object);
    }

}
