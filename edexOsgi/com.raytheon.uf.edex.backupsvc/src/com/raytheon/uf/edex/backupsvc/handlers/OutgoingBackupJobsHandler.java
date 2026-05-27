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
import java.util.Optional;

import com.raytheon.uf.common.backupsvc.BackupHost;
import com.raytheon.uf.common.backupsvc.IRefreshableServerRequest;
import com.raytheon.uf.common.backupsvc.request.IncomingOutgoingJobsRequest.RequestType;
import com.raytheon.uf.common.backupsvc.request.OutgoingBackupJobsRequest;
import com.raytheon.uf.common.backupsvc.request.RemoteOutgoingBackupJobsRequest;
import com.raytheon.uf.common.backupsvc.response.BackupSvcOutgoingResponse;
import com.raytheon.uf.common.dataplugin.backupsvc.database.BackupSvcOutgoing;
import com.raytheon.uf.common.dataplugin.backupsvc.database.DbStatus;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.serialization.comm.response.GenericResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.common.util.rate.TokenBucket;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcOutgoingDao;
import com.raytheon.uf.edex.backupsvc.service.BackupServiceConfigManager;
import com.raytheon.uf.edex.backupsvc.service.EdexVersionChecker;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Handles requests made in the Incoming Tab on the Backup Services Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer      Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2021 91325      Lisa.Singh      Initial creation
 * May 24, 2021 84473      Amanuel Challa  Modified updateStatus/deleteJobs methods to return
 *                                         a BackupSvcOutgoingResponse response Object
 * Jun 08, 2021 92508      Robert.Blum     Implemented site to site communication in sendJobs().
 * Jul 01, 2021 93517      Amanuel Challa  Modified handleRequest's READ to return Filtered Jobs
 * Jul 07, 2021 84656      Amanuel Challa  Log messages clean up
 * Aug 13, 2021 92922      Robert.Blum     Cleanup of the job statuses and setting status of failed
 *                                         if remote request was unsuccessful.
 * Aug 30, 2021 93179      Amanuel Challa  Implemented Backward compatibility of Backup Services
 * Jul 24, 2023 2035783    Lisa.Singh      Improved error handling for individual job failures.
 *
 * </pre>
 */
public class OutgoingBackupJobsHandler
        implements IRequestHandler<OutgoingBackupJobsRequest> {
    private final BackupSvcOutgoingDao dao;

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(OutgoingBackupJobsHandler.class);

    private TokenBucket rateLimiter;

    public OutgoingBackupJobsHandler() {
        dao = new BackupSvcOutgoingDao();
        statusHandler.info("Initializing OutgoingBackupJobsHandler.");
    }

    @Override
    public Object handleRequest(OutgoingBackupJobsRequest request)
            throws Exception {
        BackupSvcOutgoingResponse response = new BackupSvcOutgoingResponse();
        statusHandler.debug("Handling Outgoing Backup Jobs request.");
        switch (request.getType()) {
        case READ:
            try {
                String queryStr = request.getFilterQueryStr();
                response.setOutgoingJobs(dao.getAllOutgoingJobs(queryStr));
            } catch (Exception e) {
                response.setExceptions(e);
            }
            return response;
        case WAIT:
            return updateStatus(request.getJobIds(), DbStatus.WAIT);
        case DELETE:
            return deleteJobs(request.getJobIds());
        case SEND:
            return sendJobs(request.getJobIds());
        default:
            statusHandler.error("Unable to process " + request.getType()
                    + " Request for Outgoing Backup Jobs Request.");
            return false;
        }
    }

    /**
     * Updates the status of the backup jobs with the given status
     *
     * @param jobIds
     * @param status
     * @return BackupSvcOutgoingResponse Object with exception
     */
    private BackupSvcOutgoingResponse updateStatus(List<Long> jobIds,
            DbStatus status) {
        BackupSvcOutgoingResponse response = new BackupSvcOutgoingResponse();
        List<BackupSvcOutgoing> outgoingJobs = new ArrayList<>();
        try {
            for (long jobId : jobIds) {
                BackupSvcOutgoing job = dao.getJobForId(jobId);
                outgoingJobs.add(job);
            }
            dao.updateStatus(outgoingJobs, status);
            /**
             * Save the Outgoing Jobs in the response object. Will be used by
             * CAVE to Display which jobs were updated with a status
             **/
            List<BackupSvcOutgoing> updatedOutgoingJobs = new ArrayList<>();
            for (long jobId : jobIds) {
                BackupSvcOutgoing job = dao.getJobForId(jobId);
                updatedOutgoingJobs.add(job);
            }
            response.setOutgoingJobs(updatedOutgoingJobs);
        } catch (Exception e) {
            statusHandler.error(
                    "Unable to update the status for Outgoing Backup Job(s): ",
                    e);
            response.setExceptions(e);
            response.addFailedOutgoingJob(outgoingJobs, e);
        }
        return response;
    }

    /**
     * Updates the status of the backup jobs with the given status
     *
     * @param jobIds
     * @param status
     * @return BackupSvcOutgoingResponse Object with exception
     */
    /*
     * TODO:Based on the requirements and design, the delete action need to
     * handle the following use cases
     *
     * (1) If a job was sent but not accepted, delete this job record from DB in
     * outgoing and incoming table (TBD)
     *
     * (2) If a job was sent and accepted (TBD)
     */
    private BackupSvcOutgoingResponse deleteJobs(List<Long> jobIds) {
        List<BackupSvcOutgoing> jobs = new ArrayList<>();
        BackupSvcOutgoingResponse response = new BackupSvcOutgoingResponse();
        try {
            for (Long id : jobIds) {
                jobs.add(dao.getJobForId(id));
            }
            dao.deleteAll(jobs);
        } catch (Exception e) {
            statusHandler.error("Unable to delete Outgoing Backup Job(s): ", e);
            response.setExceptions(e);
            response.addFailedOutgoingJob(jobs, e);
        }
        return response;
    }

    private BackupSvcOutgoingResponse sendJobs(List<Long> jobIds) {

        BackupSvcOutgoingResponse response = new BackupSvcOutgoingResponse();
        List<BackupSvcOutgoing> jobs = new ArrayList<>(jobIds.size());

        for (Long id : jobIds) {
            jobs.add(dao.getJobForId(id));
        }
        BackupServiceConfigManager configMgr = BackupServiceConfigManager
                .getInstance();
        configMgr.reload();
        List<BackupHost> hosts = configMgr.getBackupHosts();

        // check if host info is not configured in backupSvc.xml
        if (hosts.isEmpty()) {
            String errorMsg = "No backup sites configured in backupSvc.xml.";
            statusHandler.warn(errorMsg);
            Exception e = new Exception(errorMsg);
            response.setExceptions(e);
            response.addFailedOutgoingJob(jobs, e);
            return response;
        }

        // Update the local db status to send
        response = updateStatus(jobIds, DbStatus.SENT);

        if (!response.isSuccessful()) {
            return response;
        }

        // Create a map of jobs per site
        Map<String, List<BackupSvcOutgoing>> jobsForSite = new HashMap<>();
        // map of jobs per site for sites with previous edex version
        Map<String, List<BackupSvcOutgoing>> jobsForSiteOldVersion = new HashMap<>();

        for (BackupSvcOutgoing job : jobs) {

            /*
             * Check for outgoing jobs with recipient version less than
             * EDEX-cutoff version and store it into a new map
             */
            EdexVersionChecker checkVersion = new EdexVersionChecker();
            String recipientEdexVersion = job.getRecipientVersion();
            String compatibleEdexVersion = configMgr.getEdexCutoffVersion();

            if (recipientEdexVersion == null
                    || recipientEdexVersion.isBlank()) {
                // recipient site is either down or non-existent (eg. a typo in
                // backupSvc.xml)
                String errorMsg = "For Job with ID " + job.getBksvcId()
                        + ", cannot reach recipient site '"
                        + job.getRecipientHostName() + "'.";
                Exception e = new Exception(errorMsg);
                statusHandler.error(errorMsg);
                response.addFailedOutgoingJob(job, e);
                continue;
            }

            boolean isVersionCompatable = checkVersion.isCompatibleEdexVersion(
                    recipientEdexVersion, compatibleEdexVersion);

            if (!isVersionCompatable) {

                if (!jobsForSiteOldVersion
                        .containsKey(job.getRecipientSite())) {
                    jobsForSiteOldVersion.put(job.getRecipientSite(),
                            new ArrayList<>());
                }
                jobsForSiteOldVersion.get(job.getRecipientSite()).add(job);

            } else {
                if (!jobsForSite.containsKey(job.getRecipientSite())) {
                    jobsForSite.put(job.getRecipientSite(), new ArrayList<>());
                }
                jobsForSite.get(job.getRecipientSite()).add(job);
            }
        }

        /*
         * If jobs with EDEX-cutoff version are found send
         * LocalizationSendRequest to HOST
         */
        if (jobsForSiteOldVersion.size() >= 1) {

            if (rateLimiter == null || configMgr.getRateLimitKBps()
                    * SizeUtil.BYTES_PER_KB != rateLimiter.getCapacity()) {
                rateLimiter = new TokenBucket(
                        (int) (configMgr.getRateLimitKBps()
                                * SizeUtil.BYTES_PER_KB));
            }

            sendtoOldVersionEDEX(hosts, jobsForSiteOldVersion, rateLimiter);

        }
        if (jobsForSite != null) {
            sendtoNewVersionEDEX(hosts, jobsForSite);
        }
        return response;
    }

    private void sendtoNewVersionEDEX(List<BackupHost> hosts,
            Map<String, List<BackupSvcOutgoing>> jobsForSite) {
        for (BackupHost host : hosts) {

            // Are there any jobs to be sent to this host
            if (jobsForSite.containsKey(host.getSite())) {
                RemoteOutgoingBackupJobsRequest remoteRequest = new RemoteOutgoingBackupJobsRequest(
                        RequestType.SEND, EDEXUtil.getEdexSite(), null,
                        jobsForSite.get(host.getSite()));
                Object remoteResponse = null;
                try {
                    remoteResponse = host.sendRequest(remoteRequest);
                } catch (Exception e) {
                    statusHandler.error("Error when sending request "
                            + remoteRequest + " to " + host.getHostName(), e);
                }
                if (remoteResponse instanceof BackupSvcOutgoingResponse) {
                    // Handle BackupSvcOutgoingResponse response if we
                    // got one
                    BackupSvcOutgoingResponse remoteOutgoingResponse = (BackupSvcOutgoingResponse) remoteResponse;
                    if (remoteOutgoingResponse.isSuccessful()) {
                        statusHandler
                                .debug("Got success response from " + host);
                    } else {
                        statusHandler.warn("Got failure response from " + host
                                + ": " + remoteOutgoingResponse.getExceptions()
                                        .getMessage());
                        /*
                         * If the remote request failed we should revert the
                         * local db change.
                         */
                        revertFailedJobs(new ArrayList<>(remoteOutgoingResponse
                                .getFailedOutgoingJobs().keySet()));
                    }
                } else {
                    statusHandler.info("Got unknown type of response from "
                            + host.getHostName());
                    /*
                     * If the remote request failed we should revert the local
                     * db change.
                     */
                    revertFailedJobs(remoteRequest.getJobs());
                }
            }
        }
    }

    private void sendtoOldVersionEDEX(List<BackupHost> hosts,
            Map<String, List<BackupSvcOutgoing>> jobsForSiteOldVersion,
            TokenBucket rateLimiter) {
        Object response = null;
        try {
            for (BackupHost host : hosts) {
                if (jobsForSiteOldVersion.containsKey(host.getSite())) {

                    List<BackupSvcOutgoing> outJobsList = jobsForSiteOldVersion
                            .get(host.getSite());

                    for (BackupSvcOutgoing outJob : outJobsList) {

                        Optional<IServerRequest> maybeRequest;

                        maybeRequest = getRequestFromJob(outJob);

                        if (maybeRequest == null || !maybeRequest.isPresent()) {
                            statusHandler.error(
                                    "Error when generating a request for jobs");

                        } else {
                            IServerRequest requestOld = maybeRequest.get();

                            try {

                                response = host.sendRequest(requestOld,
                                        rateLimiter);
                            } catch (Exception e) {

                                revertFailedJobs(outJobsList);

                                statusHandler.error(
                                        "Error when sending IServerRequest request "
                                                + maybeRequest + " to "
                                                + host.getHostName(),
                                        e);

                            }
                            if (response instanceof IServerRequest) {
                                // Route response-request if we got one
                                try {
                                    RequestRouter
                                            .route((IServerRequest) response);
                                } catch (Exception e) {
                                    statusHandler.error(
                                            "Error when handling IServerRequest response"
                                                    + " from "
                                                    + host.getHostName(),
                                            e);
                                }
                            } else if (response instanceof GenericResponse) {
                                // Handle generic response if we got one
                                GenericResponse genericResponse = (GenericResponse) response;
                                if (genericResponse.isSuccess()) {
                                    statusHandler
                                            .debug("Got success response from "
                                                    + host);
                                } else {
                                    statusHandler
                                            .warn("Got failure response from "
                                                    + host.getHostName() + ": "
                                                    + genericResponse
                                                            .getMessage());
                                    /*
                                     * If the LocalizationFileSaveRequest
                                     * request failed we should revert the local
                                     * db change.
                                     */
                                    revertFailedJobs(outJobsList);

                                }
                            } else {
                                statusHandler.warn(
                                        "Got unknown type of response from "
                                                + host.getHostName());
                                /*
                                 * If the LocalizationFileSaveRequest failed we
                                 * should revert the local db change.
                                 */
                                revertFailedJobs(outJobsList);
                            }
                        }
                    }
                }

            }
        } catch (SerializationException e) {
            statusHandler.error(
                    "Error when deserializing stored LocalizationFileSaveRequest",
                    e);
        }
    }

    private void revertFailedJobs(List<BackupSvcOutgoing> failedJobs) {
        List<Long> failedJobIds = new ArrayList<>(failedJobs.size());
        for (BackupSvcOutgoing job : failedJobs) {
            failedJobIds.add(job.getBksvcId());
        }
        BackupSvcOutgoingResponse revertResponse = updateStatus(failedJobIds,
                DbStatus.FAILED);
        if (!revertResponse.isSuccessful()) {
            statusHandler.warn("Failed to revert BackupSvcOutgoing jobs.");
        }
    }

    private Optional<IServerRequest> getRequestFromJob(BackupSvcOutgoing outJob)
            throws SerializationException {
        byte[] requestBlob = outJob.getBackupSvcJobInfo().getRequestBlob();
        IServerRequest request = (IServerRequest) DynamicSerializationManager
                .getManager(SerializationType.Thrift).deserialize(requestBlob);

        if (!(request instanceof IRefreshableServerRequest)) {
            return Optional.of(request);
        }
        IRefreshableServerRequest refreshable = (IRefreshableServerRequest) request;

        Optional<IServerRequest> maybeRequest = refreshable.refresh();

        if (maybeRequest.isPresent() && maybeRequest.get() != request) {
            statusHandler.info("Job " + outJob.getBksvcId() + " was refreshed");
        }

        return maybeRequest;
    }
}
