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

import com.google.common.eventbus.EventBus;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface IEventPublisher {

    /**
     * 
     * @param handler
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#registerEventHandler(java.lang.Object)
     */
    void registerEventHandler(Object handler);
    
    /**
     * 
     * @param handler
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISession#unRegisterEventHandler(java.lang.Object)
     */
    void unRegisterEventHandler(Object handler);
    
    /**
     * Get the underlying event publisher. Any class implementing this interface
     * must return a not null event publisher.
     * @return The event publisher.
     */
    EventBus getEventPublisher();
}
