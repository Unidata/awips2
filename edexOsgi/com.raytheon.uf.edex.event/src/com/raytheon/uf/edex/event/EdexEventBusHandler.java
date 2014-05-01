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
import com.google.common.eventbus.EventBus;
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
 * Feb 5, 2013  1580       mpduff      Initial creation.
 * 3/18/2013    1802       bphillip    Modified to use transaction synchronization
 * May 9, 2013  1989       njensen     Spring 3.1.4 compatibility
 * May 28, 2013 1650       djohnson    Simplify and extract out the general event bus handling for reuse.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class EdexEventBusHandler extends BaseEdexEventBusHandler<Event>
        implements IEventBusHandler {

    /**
     * Constructor specifying the event bus factory.
     * 
     * @param eventBusFactory
     */
    public EdexEventBusHandler() {
        this(new AsynchronousEventBusFactory());
    }

    /**
     * Constructor specifying the event bus factory.
     * 
     * @param eventBusFactory
     */
    @VisibleForTesting
    EdexEventBusHandler(GoogleEventBusFactory eventBusFactory) {
        super(eventBusFactory);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void publishInternal(Event event) {
        for (EventBus eventBus : googleEventBuses) {
            eventBus.post(event);
        }
    }

}
