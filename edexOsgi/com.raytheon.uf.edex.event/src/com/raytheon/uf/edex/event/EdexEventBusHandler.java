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

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.event.IEventBusHandler;

/**
 * EDEX implementation of {@link IEventBusHandler}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2013    1580     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class EdexEventBusHandler implements IEventBusHandler {

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
        this.googleEventBus.post(event);
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

}
