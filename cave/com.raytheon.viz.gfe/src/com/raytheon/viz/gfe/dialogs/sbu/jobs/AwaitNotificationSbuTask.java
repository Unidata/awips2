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

import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.ServiceBackupJobStatusNotification;
import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;

/**
 * Service Backup Task to wait for a particular
 * ServiceBackupJobStatusNotification
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

public class AwaitNotificationSbuTask extends AbstractSbuTask implements
        INotificationObserver {

    private static final String NOTIFICATION_TOPIC = "edex.alerts.gfe";

    private String siteID;

    private String name;

    private long timeout;

    private Shell parent;

    private volatile JobProgress status;

    private long startTime;

    /**
     * @param siteID
     *            site ID to wait for
     * @param taskName
     *            taskName to wait for
     * @param timeout
     *            in milliseconds
     * @param statusFileName
     * @param guiDescription
     */
    public AwaitNotificationSbuTask(String siteID, String taskName,
            long timeout, Shell parent, String statusFileName,
            String guiDescription) {
        super(statusFileName, guiDescription);
        this.siteID = siteID;
        this.name = taskName;
        this.timeout = timeout;
        this.parent = parent;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.dialogs.sbu.jobs.AbstractSbuTask#runTask()
     */
    @Override
    protected JobProgress runTask() throws VizException {
        NotificationManagerJob.addObserver(NOTIFICATION_TOPIC, this);

        try {
            status = JobProgress.IN_PROGRESS;
            startTime = System.currentTimeMillis();
            while (status.equals(JobProgress.IN_PROGRESS)) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    status = JobProgress.FAILED;
                }

                if ((this.timeout > 0)
                        && ((System.currentTimeMillis() - startTime) > this.timeout)) {

                    parent.getDisplay().syncExec(new Runnable() {

                        @Override
                        public void run() {
                            String message = "Task " + name
                                    + " has not completed after "
                                    + TimeUtil.prettyDuration(timeout)
                                    + ".\nDo you wish to continue waiting?";
                            if (MessageDialog.openQuestion(parent, "Timeout",
                                    message)) {
                                startTime = System.currentTimeMillis();
                            } else {
                                synchronized (status) {
                                    if (status.equals(JobProgress.IN_PROGRESS)) {
                                        status = JobProgress.FAILED;
                                    }
                                }
                            }
                        }
                    });
                }

            }
        } finally {
            NotificationManagerJob.removeObserver(NOTIFICATION_TOPIC, this);
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
                if (obj instanceof ServiceBackupJobStatusNotification) {
                    handleNotification((ServiceBackupJobStatusNotification) obj);
                } else if (obj instanceof List) {
                    @SuppressWarnings("unchecked")
                    List<GfeNotification> notifList = (List<GfeNotification>) obj;
                    for (GfeNotification notif : notifList) {
                        if (notif instanceof ServiceBackupJobStatusNotification) {
                            handleNotification((ServiceBackupJobStatusNotification) notif);
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

    /**
     * @param notif
     */
    private void handleNotification(ServiceBackupJobStatusNotification notif) {
        if (this.siteID.equals(notif.getSiteID())) {
            if (notif.getName().equals(this.name)) {
                synchronized (status) {
                    status = notif.getState();
                }
            }
        }
    }

}
