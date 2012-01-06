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
package com.raytheon.uf.viz.core.notification;

/**
 * 
 * Provides generalized interface into receiving CAVE notification messages
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/08/2008   1127       randerso    Initial Creation.
 * 09/03/2008   1448       chammack    Refactored interface to not use jms directly
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public interface INotificationObserver {

    /**
     * Indicates that a notification message has arrived.
     * 
     * This method will be invoked in thread that has been tied to the listener.
     * Thus, it is possible to perform longer-running operations inside
     * implementations of this method. Note that only one thread will be
     * allocated per registration and throughput is potentially throttled per
     * listener.
     * 
     * @param messages
     *            the messages received
     */
    public void notificationArrived(NotificationMessage[] messages);
}
