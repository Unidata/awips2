package com.raytheon.uf.edex.event;

import java.util.concurrent.Executors;

import com.google.common.eventbus.AsyncEventBus;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EventBus {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EventBus.class);

    private static final EventBus instance = new EventBus();

    private static final AsyncEventBus asyncEventBus;
    static {
        int threadCount = 15;
        try {
            threadCount = Integer.parseInt(System.getProperty(
                    "eventBusThreadCount", "15"));
        } catch (Exception e) {
            statusHandler
                    .error("Unable to set thread pool size from property eventBusThreadCount; defaulting size to "
                            + threadCount + ".", e);
        }
        asyncEventBus = new AsyncEventBus("EventBus",
                Executors.newFixedThreadPool(threadCount));
    }

    /**
     * Returns the same instance of the data delivery event bus
     * 
     * @return
     */
    public static EventBus getInstance() {
        return instance;
    }

    /**
     * Register an object with the event bus.
     * 
     * @param subscriber
     */
    public void register(Object subscriber) {
        asyncEventBus.register(subscriber);
    }

    /**
     * Publishes events for all subscribers to receive
     * 
     * @param event
     */
    public void publish(Event event) {
        asyncEventBus.post(event);
    }

    /**
     * Unregister an object with the event bus.
     * 
     * @param instance2
     */
    public void unregister(Object subscriber) {
        asyncEventBus.unregister(subscriber);
    }
}
