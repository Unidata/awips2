
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
import com.raytheon.uf.common.backupsvc.request.RemoteIncomingBackupJobsRequest;
import com.raytheon.uf.common.backupsvc.response.BackupSvcIncomingResponse;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcOutgoing;
import com.raytheon.uf.common.dataplugin.backupsvc.database.DbStatus;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcIncomingDao;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcOutgoingDao;
import com.raytheon.uf.edex.backupsvc.notification.BackupServiceNotificationPublisher;

/**
 * Handles remote requests for Backup Service Incoming jobs.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer         Description
 * ------------ ---------- ---------------- --------------------------
 * Jun 08, 2021 92508      Amanuel.Challa   Initial creation
 * Jun 28, 2021 84643      Gang Chen        Add the module to publish the notification.
 * Jul 07, 2021 84656      Amanuel Challa   Log messages clean up
 * Aug 13, 2021 92922      Robert.Blum      Cleanup of the job statuses
 *
 * </pre>
 */
public class RemoteIncomingBackupJobsHandler
        implements IRequestHandler<RemoteIncomingBackupJobsRequest> {
    private final BackupSvcOutgoingDao dao;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RemoteIncomingBackupJobsHandler.class);

    public RemoteIncomingBackupJobsHandler() {
        dao = new BackupSvcOutgoingDao();
        statusHandler.info("Initializing RemoteIncomingBackupJobsHandler.");
    }

    @Override
    public Object handleRequest(RemoteIncomingBackupJobsRequest request)
            throws Exception {
        BackupSvcIncomingResponse response = null;
        statusHandler.debug("Handling Remote Incoming Backup Jobs request from "
                + request.getSite() + ".");
        switch (request.getType()) {
        case ACCEPTED:
            response = setOutgoingJobsStatus(request.getJobIds(),
                    DbStatus.ACCEPTED);
            break;
        case REJECTED:
            response = setOutgoingJobsStatus(request.getJobIds(),
                    DbStatus.REJECTED);
            break;
        default:
            statusHandler.error("Unable to process " + request.getType()
                    + " Request from Remote Incoming Backup Jobs Request.");
            return false;
        }
        // Publish the backup service notification
        if (response != null) {
            BackupServiceNotificationPublisher backupSvcNotificationPublisher = new BackupServiceNotificationPublisher();
            statusHandler.info("Send the OUTGOING notification - "
                    + BackupServiceNotification.TOPIC + " : "
                    + BackupServiceNotification.NotificationType.OUTGOING);
            backupSvcNotificationPublisher.notify(
                    BackupServiceNotification.NotificationType.OUTGOING, true);
        }
        return response;
    }

    // Update the Status of OutgoingJobs that were Rejected/Accepted by
    // Recipient site
    private BackupSvcIncomingResponse setOutgoingJobsStatus(List<Long> jobIds,
            DbStatus status) {
        BackupSvcIncomingResponse response = new BackupSvcIncomingResponse();
        List<BackupSvcOutgoing> failedJobs = new ArrayList<>();
        List<BackupSvcOutgoing> outgoingJobs = new ArrayList<>();

        try {
            for (long jobId : jobIds) {
                BackupSvcOutgoing job = dao.getJobForId(jobId);
                outgoingJobs.add(job);
            }
            int daoStatus = dao.updateStatus(outgoingJobs, status);

            if (daoStatus != BackupSvcIncomingDao.SUCCESS) {
                // DAO failed to Update status for OutgoingJobs
                failedJobs.addAll(outgoingJobs);
            }
            // If success return the outgoing jobs in response obj
            response.setOutgoingJobs(outgoingJobs);
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to update the status of Outgoing Backup Job(s) at Remote Sender Site ",
                    e);

            response.setExceptions(new Exception(
                    "Failed to update status of Outgoing Backup Job(s) at Remote Sender Site."));
            response.setFailedOutgoingJobs(failedJobs);
        }
        return response;
    }

}
