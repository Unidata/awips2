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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.backupsvc.request.IncomingBackupJobsRequest;
import com.raytheon.uf.common.backupsvc.request.IncomingOutgoingJobsRequest.RequestType;
import com.raytheon.uf.common.backupsvc.request.RemoteIncomingBackupJobsRequest;
import com.raytheon.uf.common.backupsvc.response.BackupSvcIncomingResponse;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcIncoming;
import com.raytheon.uf.common.dataplugin.backupsvc.database.DbStatus;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.serialization.comm.response.GenericResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcIncomingDao;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Handles requests made in the Incoming Tab on the Backup Services Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer        Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2021 91326      Lisa.Singh       Initial creation
 * May 24, 2021 84473      Amanuel Challa   Modified updateStatus method to return
 *                                          BackupSvcOutgoingResponse response Object
 * Jun 08, 2021 92508      Robert.Blum      Fix successful spelling.
 * Jul 01, 2021 93517      Amanuel Challa   Modified handleRequest's READ to return Filtered Jobs
 * Jul 07, 2021 84656      Amanuel Challa   Log messages clean up
 * Aug 13, 2021 92922      Robert.Blum      Cleanup of the job statuses, apply the blob request to the system, and
 *                                          cleanup BackupHost logic to set sender host and port correctly.
 * </pre>
 */
public class IncomingBackupJobsHandler
        implements IRequestHandler<IncomingBackupJobsRequest> {
    private final BackupSvcIncomingDao dao;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(IncomingBackupJobsHandler.class);

    public IncomingBackupJobsHandler() {
        dao = new BackupSvcIncomingDao();
        statusHandler.info("Initializing IncomingBackupJobsHandler.");
    }

    @Override
    public Object handleRequest(IncomingBackupJobsRequest request)
            throws Exception {
        BackupSvcIncomingResponse response = new BackupSvcIncomingResponse();
        statusHandler.debug("Handling Incoming Backup Jobs request.");
        switch (request.getType()) {
        case READ:
            try {
                String queryStr = request.getFilterQueryStr();
                response.setIncomingJobs(dao.getAllIncomingJobs(queryStr));
            } catch (Exception e) {
                response.setExceptions(e);
            }
            return response;
        case ACCEPTED:
            return sendJobsToSite(request.getJobIds(), request.getOutJobIds(),
                    DbStatus.ACCEPTED);
        case REJECTED:
            return sendJobsToSite(request.getJobIds(), request.getOutJobIds(),
                    DbStatus.REJECTED);
        case WAIT:
            return updateStatus(request.getJobIds(), request.getOutJobIds(),
                    DbStatus.WAIT);
        default:
            statusHandler.error("Unable to process " + request.getType()
                    + " Request for Incoming Backup Jobs Request.");
            return false;
        }
    }

    /**
     * Send Jobs to Sender-site when Incoming Jobs are Rejected/Accepted
     *
     * @param jobIds
     *            Incoming Job Ids
     * @param outjobIds
     *            OutgoingJob Ids
     * @param status
     *            Job status "Accepted" or "Rejected"
     * @return BackupSvcIncomingResponse Object
     */
    private BackupSvcIncomingResponse sendJobsToSite(List<Long> jobIds,
            List<Long> outjobIds, DbStatus status) {
        BackupSvcIncomingResponse responseGen = new BackupSvcIncomingResponse();
        List<BackupSvcIncoming> jobs = new ArrayList<>();
        List<String> siteInfo = new ArrayList<>();
        /*
         * Since Recipient Site don't have hostName,site,port information
         * configured in backupSvc.xml,the information needs to be extracted
         * from each IncomingJobs.
         */
        /*
         * From the given List of IncomingJob Id's query the dao for JobInfo and
         * senderSite name.
         */
        for (Long id : jobIds) {
            BackupSvcIncoming job = dao.getJobForId(id);
            jobs.add(job);
            siteInfo.add(job.getSenderSite());
        }
        // "No backup sites Host information can be found from Incoming Jobs!"
        if (siteInfo.isEmpty()) {
            statusHandler.info(
                    "Could not find Host Information for the Incoming Job with id "
                            + jobIds + " from the backup site!");
            responseGen.setExceptions(new Exception(
                    "Could not find Host Information for the Incoming Job with id "
                            + jobIds + " from the backup site!"));
            responseGen.setFailedIncomingJobs(jobs);
            return responseGen;
        } else {
            // Update the local DB status
            responseGen = updateStatus(jobIds, outjobIds, status);

            if (responseGen.isSuccessful()) {
                List<BackupSvcIncoming> failedJobs = new ArrayList<>();
                if (DbStatus.ACCEPTED.equals(status)) {
                    for (BackupSvcIncoming job : jobs) {
                        if (job.getBackupSvcJobInfo()
                                .getRequestBlob() != null) {
                            try {
                                /*
                                 * Route the IServerRequest (Blob) to the local
                                 * Request so the Localization File can be
                                 * applied.
                                 */
                                IServerRequest request = (IServerRequest) DynamicSerializationManager
                                        .getManager(SerializationType.Thrift)
                                        .deserialize(job.getBackupSvcJobInfo()
                                                .getRequestBlob());
                                GenericResponse response = (GenericResponse) RequestRouter
                                        .route(request);
                                if (response.isSuccess()) {
                                    statusHandler.debug(
                                            "Successly applied BackupSvcJob to system.");
                                } else {
                                    statusHandler.warn(
                                            "Got failure response from attempting to apply BackupSvcJob to system: "
                                                    + response.getMessage());
                                    failedJobs.add(job);
                                }
                            } catch (Exception e) {
                                statusHandler.error(
                                        "Error applying BackupSvcJob to system.",
                                        e);
                                failedJobs.add(job);
                            }
                        }
                    }
                    if (!failedJobs.isEmpty()) {
                        // Remove the failed jobs so we done report back to the
                        // sender
                        jobs.removeAll(failedJobs);
                        updateStatus(
                                failedJobs.stream()
                                        .map(BackupSvcIncoming::getBksvcId)
                                        .collect(Collectors.toList()),
                                failedJobs.stream()
                                        .map(BackupSvcIncoming::getOutBksvcId)
                                        .collect(Collectors.toList()),
                                DbStatus.FAILED);
                    }
                }
                // Create a map of outgoing jobs ids per site
                Map<String, List<Long>> jobsForSite = new HashMap<>();

                // Create a map of hostName per siteName
                Map<String, BackupHost> hostForSite = new HashMap<>();
                for (BackupSvcIncoming job : jobs) {
                    if (!(jobsForSite.containsKey(job.getSenderSite())
                            && hostForSite.containsKey(job.getSenderSite()))) {
                        jobsForSite.put(job.getSenderSite(), new ArrayList<>());
                        // Assuming each site has a unique hostName add it to
                        // the map
                        BackupHost host = new BackupHost();
                        // add the host name for the jobs that needed to be sent
                        host.setHostName(job.getSenderHostName());
                        host.setPort(Integer.parseInt(job.getSenderPort()));
                        hostForSite.put(job.getSenderSite(), host);
                    }
                    jobsForSite.get(job.getSenderSite())
                            .add(job.getOutBksvcId());
                }
                String edexSite = EDEXUtil.getEdexSite();
                for (String siteName : siteInfo) {
                    // Are there any jobs to be sent to this site
                    if (jobsForSite.containsKey(siteName)) {
                        RemoteIncomingBackupJobsRequest remoteRequest = null;
                        switch (status) {
                        case ACCEPTED:
                            remoteRequest = new RemoteIncomingBackupJobsRequest(
                                    RequestType.ACCEPTED, edexSite, null,
                                    jobsForSite.get(siteName));
                            break;
                        case REJECTED:
                            remoteRequest = new RemoteIncomingBackupJobsRequest(
                                    RequestType.REJECTED, edexSite, null,
                                    jobsForSite.get(siteName));
                            break;
                        default:
                            break;

                        }
                        Object remoteResponse = null;
                        try {
                            remoteResponse = hostForSite.get(siteName)
                                    .sendRequest(remoteRequest);
                        } catch (Exception e) {
                            statusHandler.error(
                                    "Error when sending remote Incoming Job request "
                                            + remoteRequest + " to " + siteName,
                                    e);
                        }
                        if (remoteResponse instanceof BackupSvcIncomingResponse) {
                            // Handle BackupSvcIncomingResponse response if we
                            // got one
                            BackupSvcIncomingResponse remoteIncomingResponse = (BackupSvcIncomingResponse) remoteResponse;
                            if (remoteIncomingResponse.isSuccessful()) {
                                statusHandler.debug(
                                        "Got success sending Incoming Job response from "
                                                + siteName);
                            } else {
                                statusHandler.warn(
                                        "Got failure response from sending Incoming Job "
                                                + siteName + ": "
                                                + remoteIncomingResponse
                                                        .getExceptions()
                                                        .getMessage());
                            }
                        } else {
                            statusHandler.info(
                                    "Got unknown type of response from sending Incoming Job "
                                            + siteName + ": " + remoteResponse);
                        }
                    }
                }
            }
        }
        return responseGen;
    }

    /**
     * Updates the status of the backup jobs with the given status
     *
     * @param jobIds
     * @param status
     * @return BackupSvcIncomingResponse Object with exception.
     */
    private BackupSvcIncomingResponse updateStatus(List<Long> jobIds,
            List<Long> outJobIds, DbStatus status) {
        BackupSvcIncomingResponse response = new BackupSvcIncomingResponse();
        List<BackupSvcIncoming> incomingJobs = new ArrayList<>();
        List<BackupSvcIncoming> updatedIncomingJobs = new ArrayList<>();
        int daoStatus;
        try {
            for (long jobId : jobIds) {
                BackupSvcIncoming job = dao.getJobForId(jobId);
                incomingJobs.add(job);
            }
            // capture the response from BackupSvcIncomingDao when calling
            // the updateStatus method
            daoStatus = dao.updateStatus(incomingJobs, status);
            /**
             * Save the newly updated Incoming Jobs in the response object. Will
             * be used by CAVE to Display whether the jobs were updated or
             * failed to be updated with the given new status
             **/
            for (long jobId : jobIds) {
                BackupSvcIncoming job = dao.getJobForId(jobId);
                updatedIncomingJobs.add(job);
            }
            /*
             * TODO: Improve upon site to site communication completion.as his
             * method stands, there is no way to find out the individual jobs
             * that weren't successfully updated by the IncomingDao since each
             * transaction is executed in bulk. However, if there is a failure
             * the dao will return 0, and the jobs are stored in
             * FailedIncomingJobs List.
             */
            if (BackupSvcIncomingDao.SUCCESS == daoStatus) {
                response.setIncomingJobs(updatedIncomingJobs);
            } else {
                response.setFailedIncomingJobs(updatedIncomingJobs);
            }
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to update the status for Incoming Backup Job(s): ",
                    e);
            response.setExceptions(e);
        }
        return response;
    }
}
