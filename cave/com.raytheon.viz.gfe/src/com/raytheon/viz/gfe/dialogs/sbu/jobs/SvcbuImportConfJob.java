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

import java.util.ArrayList;

import org.eclipse.jface.dialogs.MessageDialog;

import com.raytheon.uf.common.dataplugin.gfe.request.AbortOperationRequest;
import com.raytheon.uf.common.dataplugin.gfe.request.ImportConfRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.ServiceBackupMessageNotification;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.NotificationException;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.dialogs.sbu.ProgressDlg;
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
 * Aug 05, 2011            bphillip     Initial creation
 * Mar 20, 2013   1447     dgilling     Add support for service backup
 *                                      troubleshooting mode from A1.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SvcbuImportConfJob extends ServiceBackupJob implements
        INotificationObserver {

    private String failedSite;

    private boolean trMode;

    private ProgressDlg progress;

    private boolean complete;

    private long startTime;

    private String errorMsg;

    public SvcbuImportConfJob(String primarySite, String failedSite,
            boolean trMode, ProgressDlg progress) {
        super("Import Configuration: " + failedSite, primarySite);
        this.failedSite = failedSite;
        this.progress = progress;
        this.trMode = trMode;
        NotificationManagerJob.addObserver(ServiceBackupDlg.NOTIFY_TOPIC, this);
    }

    @Override
    public void run() {
        ImportConfRequest request = new ImportConfRequest(primarySite,
                failedSite, trMode);
        try {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    progress.open();
                }
            });

            startTime = System.currentTimeMillis();
            makeRequest(request);
            while (!complete) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                }

                if (System.currentTimeMillis() - startTime > THIRTY_MINUTES) {
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            boolean wait = MessageDialog.openQuestion(
                                    progress.getShell(),
                                    "Question",
                                    "Configuration has not been imported after 30 minutes.  Do you wish to continue to wait?");
                            if (wait) {
                                startTime = System.currentTimeMillis();
                            } else {
                                complete = true;
                                aborted = true;
                                progress.close();
                            }
                        }
                    });
                }
            }

            if (aborted) {
                AbortOperationRequest abortRequest = new AbortOperationRequest(
                        "importConfiguration");
                try {
                    Object obj = ThriftClient.sendRequest(abortRequest);
                    if (obj instanceof ServerResponse) {
                        @SuppressWarnings("unchecked")
                        ServerResponse<String> rval = (ServerResponse<String>) obj;
                        if (!rval.isOkay()) {
                            errorMsg = rval.message();
                        }
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error processing get abort operation request", e);
                }
            } else if (complete) {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        progress.close();
                    }
                });
            }

            if (errorMsg != null) {
                throw new Exception(errorMsg);
            }

        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SERVICE BACKUP problem: Unable to import digital data for "
                            + failedSite, e);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SERVICE BACKUP: " + e.getLocalizedMessage());
        } finally {
            NotificationManagerJob.removeObserver(
                    ServiceBackupDlg.NOTIFY_TOPIC, this);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        try {
            for (NotificationMessage msg : messages) {
                ArrayList<GfeNotification> notifications = (ArrayList<GfeNotification>) msg
                        .getMessagePayload();
                for (GfeNotification notification : notifications) {
                    if (notification instanceof ServiceBackupMessageNotification) {
                        ServiceBackupMessageNotification notify = (ServiceBackupMessageNotification) notification;
                        if (notify.getMessage().equals(
                                "Import Configuration Complete!")) {
                            complete = true;
                        } else if (notify.getMessage().startsWith(
                                "Error Processing Configuration Data!")) {
                            complete = true;
                            failed = true;
                            errorMsg = notify.getMessage();
                        }
                    }
                }
            }
        } catch (NotificationException e) {
            statusHandler.error("Error processing gfe notifications!!", e);
        }
    }
}
