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
 * IEventBusHandler interface.
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

public interface IEventBusHandler {

    /**
     * Publishes events for all subscribers to receive
     * 
     * @param event
     *            the event
     */
    void publish(Event event);

    /**
     * Register an object with the event bus.
     * 
     * @param subscriber
     *            the subscriber to register
     */
    void register(Object subscriber);

    /**
     * Unregister an object with the event bus.
     * 
     * @param subscriber
     *            the object subscribed to the event buss
     */
    void unregister(Object subscriber);

}
