
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
package com.raytheon.uf.edex.backupsvc.handlers;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.backupsvc.notification.BackupServiceNotification;
import com.raytheon.uf.common.backupsvc.request.RemoteOutgoingBackupJobsRequest;
import com.raytheon.uf.common.backupsvc.response.BackupSvcOutgoingResponse;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcOutgoing;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcIncomingDao;
import com.raytheon.uf.edex.backupsvc.notification.BackupServiceNotificationPublisher;

/**
 * Handles remote requests for Backup Service Outgoing jobs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer     Description
 * ------------ ---------- ------------ --------------------------
 * Jun 08, 2021 92508      Robert.Blum     Initial creation
 * Jun 22, 2021 92788      Gang Chen       Replace the getJobName with
 *                                         getJobFileLocation and getJobFilePath
 * Jun 28, 2021 84643      Gang Chen       Add the module to publish the notification.
 * Jul 07, 2021 84656      Amanuel Challa  Log messages clean up
 * Jul 13, 2021 92922      Robert.Blum     Updates for new DAO method signatures.
 * Aug 13, 2021 93179      Amanuel Challa  Removed unused recipientHostName/blobSize parameter in createIncomingJobs()
 * Jul 26, 2023 2035783    Lisa.Singh      Improved error handling for individual job failures.
 * </pre>
 */
public class RemoteOutgoingBackupJobsHandler
        implements IRequestHandler<RemoteOutgoingBackupJobsRequest> {

    private final BackupSvcIncomingDao dao;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteOutgoingBackupJobsHandler.class);

    public RemoteOutgoingBackupJobsHandler() {
        dao = new BackupSvcIncomingDao();
        statusHandler.info("Initializing RemoteOutgoingBackupJobsHandler.");
    }

    @Override
    public Object handleRequest(RemoteOutgoingBackupJobsRequest request)
            throws Exception {
        BackupSvcOutgoingResponse response = null;

        statusHandler.debug("Handling Remote Outgoing Backup Jobs request from "
                + request.getSite() + ".");

        switch (request.getType()) {
        case SEND:
            /*
             * From an Outgoing jobs perspective we only care when jobs are
             * initially sent. If they are updated to WAIT or DELETED, those are
             * just local changes and do not impact remote recipient sites.
             */
            response = createIncomingJobs(request.getJobs());
            break;
        default:
            String errorMsg = "Unable to process Remote Outgoing Backup Jobs Request with request type: "
                    + request.getType();
            statusHandler.error(errorMsg);
            Exception e = new Exception(errorMsg);
            response = new BackupSvcOutgoingResponse();
            response.setExceptions(e);
            response.addFailedOutgoingJob(request.getJobs(), e);
            return response;
        }
        // Publish the backup service notification
        if (response != null) {
            BackupServiceNotificationPublisher backupSvcNotificationPublisher = new BackupServiceNotificationPublisher();
            statusHandler.info("Send both INCOMING notification - "
                    + BackupServiceNotification.TOPIC + " : "
                    + BackupServiceNotification.NotificationType.INCOMING);
            backupSvcNotificationPublisher.notify(
                    BackupServiceNotification.NotificationType.INCOMING, true);
        }
        return response;

    }

    private BackupSvcOutgoingResponse createIncomingJobs(
            List<BackupSvcOutgoing> jobs) {
        BackupSvcOutgoingResponse response = new BackupSvcOutgoingResponse();
        List<BackupSvcOutgoing> failedJobs = new ArrayList<>();
        boolean successful = true;
        for (BackupSvcOutgoing job : jobs) {
            int daoStatus = dao.createInBSJ(
                    job.getBackupSvcJobInfo().getRequestBlob(),
                    job.getBackupSvcJobInfo().getJobFileLocation(),
                    job.getBackupSvcJobInfo().getJobFilePath(),
                    job.getComponent(), job.getSenderSite(),
                    job.getRecipientSite(), job.getSenderHostName(),
                    job.getSenderPort(), job.getBksvcId(),
                    job.getRecipientVersion(), job.getSystemVersion());
            if (daoStatus != BackupSvcIncomingDao.SUCCESS) {
                // DAO failed to create job
                failedJobs.add(job);
                successful = false;
            }
        }
        if (!successful) {
            String errorMessage = "Failed to create BackupSvcIncoming jobs.";
            statusHandler.error(errorMessage);
            Exception e = new Exception(errorMessage);
            response.setExceptions(e);
            response.addFailedOutgoingJob(failedJobs, e);
        }
        return response;
    }

}