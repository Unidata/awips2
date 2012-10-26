package com.raytheon.uf.edex.event.handler;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.edex.event.EventBus;

/**
 * 
 * Logs ALL events published on the event bus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class LogHandler {

    private final Log logger;

    private static EventBus eventBus = EventBus.getInstance();

    /**
     * Creates a new object
     */
    public LogHandler() {
        eventBus.register(this);
        logger = LogFactory.getLog("Event");
    }

    /**
     * Listens for any DataDeliveryEvent object published on the event bus
     * 
     * @param event
     */
    @Subscribe
    @AllowConcurrentEvents
    public void eventListener(Event event) {
        logger.info(event.toString());
    }
}
