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
package com.raytheon.uf.viz.core.notification.observers;

import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationMessage;

/**
 * Simple class for printing debug information on incoming messages to standard
 * out.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 12, 2008				randerso	Initial creation
 * Sep 3, 2008  1448        chammack    Support refactored interface
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class DebugNotificationObserver implements INotificationObserver {

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.alerts.INotificationObserver#notificationArrived(javax.jms.Message[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage message : messages) {
            try {
                String dest = message.getSource();
                Object obj = message.getMessagePayload();

                printMessage(dest, "Message", "\"" + obj.toString() + "\"");

            } catch (Exception e) {
                // TODO: handle exception
                e.printStackTrace();
            }
        }
    }

    private void printMessage(String dest, String messageType, String payload) {
        System.out.println(messageType + " received on " + dest + ": "
                + payload);
    }
}
