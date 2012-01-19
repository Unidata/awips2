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
package com.raytheon.uf.viz.core.localization;

import org.eclipse.core.runtime.Status;
import org.eclipse.ui.statushandlers.StatusManager;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.LocalizationNotificationObserver;
import com.raytheon.uf.viz.core.Activator;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;

/**
 * Provides notification support for Localization
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 20, 2008				randerso	Initial creation
 * Sep 3, 2008  1448        chammack    Support refactored interface
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class CAVELocalizationNotificationObserver implements
        INotificationObserver {
    private static CAVELocalizationNotificationObserver instance = null;

    private LocalizationNotificationObserver observer;

    public static synchronized void register() {
        if (instance == null) {
            instance = new CAVELocalizationNotificationObserver();
            NotificationManagerJob.addObserver(
                    LocalizationNotificationObserver.LOCALIZATION_TOPIC,
                    instance);
        }
    }

    public static synchronized void unregister() {
        if (instance != null) {
            NotificationManagerJob.removeObserver(
                    LocalizationNotificationObserver.LOCALIZATION_TOPIC,
                    instance);
        }
    }

    private CAVELocalizationNotificationObserver() {
        observer = LocalizationNotificationObserver.getInstance();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.alerts.INotificationObserver#notificationArrived(javax
     * .jms.Message[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage message : messages) {
            try {
                FileUpdatedMessage fum = (FileUpdatedMessage) message
                        .getMessagePayload();
                observer.fileUpdateMessageRecieved(fum);
            } catch (NotificationException e) {
                StatusManager.getManager().handle(
                        new Status(Status.ERROR, Activator.PLUGIN_ID,
                                "Error reading incoming notification", e));
            }
        }

    }

}
