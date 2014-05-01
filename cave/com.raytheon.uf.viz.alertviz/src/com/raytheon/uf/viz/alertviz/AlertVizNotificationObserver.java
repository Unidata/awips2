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
package com.raytheon.uf.viz.alertviz;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;

/**
 * Allows for sending messages from server to AlertViz
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2009            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlertVizNotificationObserver implements INotificationObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlertVizNotificationObserver.class, "GDN_ADMIN",
                    "GDN_ADMIN");

    private static final String ALERTVIZ_TOPIC = "edex.alerts.msg";

    private static AlertVizNotificationObserver instance = null;

    private AlertVizNotificationObserver() {
    }

    /**
     * Register the alertviz notification observer for receiving alert messages
     * from edex
     */
    public static synchronized void registerAlertVizNotificationObserver() {
        if (instance == null) {
            instance = new AlertVizNotificationObserver();
            NotificationManagerJob.addObserver(ALERTVIZ_TOPIC, instance);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        StatusMessage ob = null;
        for (NotificationMessage message : messages) {
            try {
                ob = (StatusMessage) message.getMessagePayload();
                VizApp.runAsync(new AlertRun(ob));
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not pass message from server", e);
            }
        }
    }

    private static class AlertRun implements Runnable {
        private StatusMessage sm;

        public AlertRun(StatusMessage sm) {
            this.sm = sm;
        }

        @Override
        public void run() {
            try {
                AlertVizClient.sendMessage(sm);
            } catch (final AlertvizException e) {
                Container.logInternal(Priority.ERROR,
                        "AlertVizNotificationObserver unable to send message through AlertVizClient: "
                                + sm, e);
                // not a good situation, since we can't communicate with the log
                // server properly

                // DO NOT SEND TO LOG HERE OR INFINITE LOOPS MAY OCCUR
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        ErrorDialog.openError(Display.getDefault()
                                .getActiveShell(),
                                "Error communicating with log server",
                                "Error communicating with log server "
                                        + e.getCause().getMessage(),
                                Status.OK_STATUS);
                    }

                });
            }
        }
    }

}
