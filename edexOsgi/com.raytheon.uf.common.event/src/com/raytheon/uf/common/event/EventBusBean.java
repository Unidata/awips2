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


/**
 * Convenient Spring Bean class that simplifies access to the {@link EventBus}
 * from Spring.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2013 1650       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EventBusBean implements IEventBusHandler {

    /**
     * {@inheritDoc}
     */
    @Override
    public Object register(Object subscriber) {
        EventBus.register(subscriber);
        return subscriber;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void unregister(Object subscriber) {
        EventBus.unregister(subscriber);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void publish(Event event) {
        EventBus.publish(event);
    }
}
