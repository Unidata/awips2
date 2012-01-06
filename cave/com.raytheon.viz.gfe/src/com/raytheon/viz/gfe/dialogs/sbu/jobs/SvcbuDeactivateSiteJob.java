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
package com.raytheon.viz.gfe.dialogs.sbu.jobs;

import java.util.Date;

import com.raytheon.uf.common.site.notify.ClusterActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.site.requests.DeactivateSiteRequest;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.dialogs.sbu.ServiceBackupDlg;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SvcbuDeactivateSiteJob extends ServiceBackupJob implements
        INotificationObserver {

    private String siteToDeactivate;

    private ClusterActivationNotification notification;

    /**
     * @param name
     */
    public SvcbuDeactivateSiteJob(String name, String primarySite) {
        super("Deactivate Site: " + name, primarySite);
        this.siteToDeactivate = name;
        NotificationManagerJob.addObserver(ServiceBackupDlg.ACTIVATION_TOPIC, this);
    }

    @Override
    public void run() {
        DeactivateSiteRequest request = new DeactivateSiteRequest(
                siteToDeactivate, "gfe");
        try {
            makeRequest(request);
            while (notification == null) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                }
            }
            if (notification.isClusterActive()) {
                statusHandler
                        .info(notification.getModifiedSite()
                                + " has been successfully deactivated on all cluster members");
            } else if (notification.isFailure()) {
                statusHandler
                        .error(notification.getModifiedSite()
                                + " has failed to deactivate on some or all cluster members.  See logs for details");
                this.failed = true;
            }
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SERVICE BACKUP problem: Unable to activate "
                            + siteToDeactivate, e);
        } finally {
            NotificationManagerJob.removeObserver(ServiceBackupDlg.ACTIVATION_TOPIC, this);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage msg : messages) {
            Object obj;
            try {
                obj = msg.getMessagePayload();
                String message = null;
                if (obj instanceof ClusterActivationNotification) {
                    ClusterActivationNotification notify = (ClusterActivationNotification) obj;
                    if (notify.getPluginName().equals("gfe")
                            && notify.isDeactivation()) {
                        this.notification = notify;
                    }
                } else if (obj instanceof SiteActivationNotification) {
                    message = dateFormat.format(new Date()) + "  "
                            + obj.toString();
                }
                statusHandler.info(message);
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to read incoming notification", e);
                continue;
            }
        }
    }
}
