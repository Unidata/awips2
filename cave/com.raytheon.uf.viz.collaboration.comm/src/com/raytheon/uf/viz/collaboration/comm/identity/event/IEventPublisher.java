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
package com.raytheon.uf.viz.collaboration.comm.identity.event;

/**
 * Interface for components that publish events to the event bus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 * Jun 12, 2012             njensen      Improved
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IEventPublisher {

    /**
     * Register handler to receive events from this publisher
     * 
     * @param handler
     */
    public void registerEventHandler(Object handler);

    /**
     * Unregister handler to no longer receive events from this publisher
     * 
     * @param handler
     */
    public void unregisterEventHandler(Object handler);

    /**
     * Send event to all handlers registered to this publisher
     * 
     * @param event
     */
    public void postEvent(Object event);
}
