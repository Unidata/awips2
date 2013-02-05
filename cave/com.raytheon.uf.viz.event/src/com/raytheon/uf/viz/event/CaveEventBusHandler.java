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
package com.raytheon.uf.viz.event;

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.event.EventPublishRequest;
import com.raytheon.uf.common.event.IEventBusHandler;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Cave implementation of the {@link IEventBusHandler}
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

public class CaveEventBusHandler implements IEventBusHandler {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CaveEventBusHandler.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public void publish(Event event) {
        EventPublishRequest request = new EventPublishRequest(event);
        try {
            RequestRouter.route(request);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error sending Event", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void register(Object subscriber) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unregister(Object subscriber) {
        throw new UnsupportedOperationException();
    }

}
