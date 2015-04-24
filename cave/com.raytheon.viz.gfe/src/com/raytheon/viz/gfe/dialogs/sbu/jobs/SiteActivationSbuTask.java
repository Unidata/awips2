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

import com.raytheon.uf.common.dataplugin.gfe.request.AbstractGfeRequest;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.site.notify.ClusterActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONSTATUS;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONTYPE;
import com.raytheon.uf.common.site.requests.ActivateSiteRequest;
import com.raytheon.uf.common.site.requests.DeactivateSiteRequest;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Service Backup Task to activate/deactivate a GFE site
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2015  #4300     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SiteActivationSbuTask extends ServerRequestSbuTask implements
        INotificationObserver {
    private static final String SITE_ACTIVATION_TOPIC = "edex.alerts.siteActivate";

    private JobProgress status;

    private String siteID;

    private ACTIVATIONTYPE type;

    /**
     * @param siteID
     * @param activate
     *            true to activate, false to deactivate
     * @param statusFileName
     * @param guiDescription
     */
    public SiteActivationSbuTask(String siteID, ACTIVATIONTYPE type,
            String statusFileName, String guiDescription) {
        super(statusFileName, guiDescription, (type
                .equals(ACTIVATIONTYPE.ACTIVATE) ? new ActivateSiteRequest(
                siteID, "gfe") : new DeactivateSiteRequest(siteID, "gfe")));
        this.siteID = siteID;
        this.type = type;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.dialogs.sbu.jobs.ServerRequestSbuTask#runTask()
     */
    @Override
    public JobProgress runTask() throws VizException {
        status = JobProgress.FAILED;

        if (request instanceof AbstractGfeRequest) {
            ((AbstractGfeRequest) request).setWorkstationID(VizApp.getWsId());
        }

        NotificationManagerJob.addObserver(SITE_ACTIVATION_TOPIC, this);

        try {
            ThriftClient.sendRequest(request);

            status = JobProgress.IN_PROGRESS;
            while (status.equals(JobProgress.IN_PROGRESS)) {
                // TODO: add timeout

                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    status = JobProgress.FAILED;
                }
            }
        } finally {
            NotificationManagerJob.removeObserver(SITE_ACTIVATION_TOPIC, this);
        }

        return status;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.jms.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.common.jms.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage msg : messages) {
            Object obj;
            try {
                obj = msg.getMessagePayload();
                if (obj instanceof ClusterActivationNotification) {
                    ClusterActivationNotification notif = (ClusterActivationNotification) obj;
                    if (this.siteID.equals(notif.getModifiedSite())) {
                        if (notif.getType().equals(this.type)) {
                            if (notif.getStatus().equals(
                                    ACTIVATIONSTATUS.SUCCESS)) {
                                status = JobProgress.SUCCESS;
                            } else if (notif.getStatus().equals(
                                    ACTIVATIONSTATUS.FAILURE)) {
                                status = JobProgress.FAILED;
                            } else {
                                status = JobProgress.FAILED;
                            }
                        }
                    }
                }
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to read incoming notification", e);
                continue;
            }
        }
    }
}
