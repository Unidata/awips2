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
package com.raytheon.uf.common.event;

import com.raytheon.uf.common.stats.ProcessEvent;


/**
 * Test {@link EventBus}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2013 1650       djohnson     Initial creation
 * Jun 20, 2013 1802       djohnson     Allow test code to set an explicit event bus handler.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class EventBusTest extends BaseEventBusTest<Event, IEventBusHandler> {

    /**
     * Allows test code to use explicit event bus handlers if the default does
     * not suffice.
     * 
     * @param eventBusHandler
     *            the event bus handler
     */
    public static void useExplicitEventBusHandler(
            IEventBusHandler eventBusHandler) {
        EventBus.handler = eventBusHandler;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected IEventBusHandler getEventBus() {
        return new EventBusBean();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Event getEvent() {
        return new ProcessEvent();
    }

}
