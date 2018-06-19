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
package com.raytheon.uf.edex.backupsvc.service;

import java.util.List;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationType;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.serialization.comm.response.GenericResponse;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.common.util.rate.TokenBucket;
import com.raytheon.uf.edex.backupsvc.database.BackupJob;
import com.raytheon.uf.edex.backupsvc.database.BackupJobDao;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexTimerBasedThread;

/**
 * Service for pushing data to other EDEX hosts. Designed for pushing
 * localization file updates to backup hosts, but may be used for other
 * inter-site communication purposes.
 *
 * This is a clustered singleton service. It must only be instantiated from
 * Spring and only in one place.
 *
 * USAGE: Submit a BackupEnqueueRequest that contains the request you want to
 * send out to the backup hosts.
 *
 * CONFIGURATION: The service is configured by a site-level localization XML
 * file. That file lists the hosts that all data will be sent to, and specifies
 * the rate limit (in KiB) for sending requests. The config file is reloaded
 * each time the backup service wakes up.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 18, 2016 5937       tgurney     Initial creation
 * Dec  9, 2016 5937       tgurney     Fix initial sleep interval
 * Dec  9, 2016 5937       tgurney     Improve configuration logic
 * Mar 23, 2017 5937       tgurney     Do not process() during EDEX startup
 * Mar 30, 2017 5937       rjpeter     Use EdexTimerBasedThread logger.
 * Jul 20, 2017 6352       tgurney     Add min/maxVersionRequired job parameters
 * Jul 24, 2017 6352       tgurney     Move stats reporting code to new class
 *
 * </pre>
 *
 * @author tgurney
 */

public class BackupService extends EdexTimerBasedThread {
    private final BackupServiceStatsReporter statsReporter;

    private final BackupJobDao dao;

    private TokenBucket rateLimiter;

    /**
     * Public constructor for Spring only; do not try to instantiate this
     * yourself
     */
    public BackupService() {
        long t0 = System.currentTimeMillis();
        threadSleepInterval = (int) (BackupServiceConfigManager.getInstance()
                .getPollIntervalSeconds() * TimeUtil.MILLIS_PER_SECOND);
        dao = new BackupJobDao();
        statsReporter = new BackupServiceStatsReporter();
        long t1 = System.currentTimeMillis();
        logger.info("Initialized in " + TimeUtil.prettyDuration(t1 - t0));
    }

    /**
     * Run a single job for a single host.
     *
     * @param job
     * @param host
     * @return true if the job ran. false if it did not run because of an error
     */
    private boolean runJob(BackupJob job, BackupHost host) {
        try {
            // Send the request
            Object response = null;
            IServerRequest request = (IServerRequest) DynamicSerializationManager
                    .getManager(SerializationType.Thrift)
                    .deserialize(dao.fetchBlob(job));
            try {
                response = host.sendRequest(request, rateLimiter);
            } catch (Exception e) {
                logger.error("Error when sending request " + request + " to "
                        + host + " for job " + job.getJobName(), e);
                return false;
            }
            if (response instanceof IServerRequest) {
                // Route response-request if we got one
                try {
                    RequestRouter.route((IServerRequest) response);
                } catch (Exception e) {
                    logger.error("Error when handling response " + response
                            + " from " + host, e);
                }
            } else if (response instanceof GenericResponse) {
                // Handle generic response if we got one
                GenericResponse genericResponse = (GenericResponse) response;
                if (genericResponse.isSuccess()) {
                    logger.debug("Got success response from " + host);
                } else {
                    logger.warn("Got failure response from " + host + ": "
                            + genericResponse.getMessage());
                }
            } else {
                logger.info("Got unknown type of response from " + host + ": "
                        + response.toString());
            }
            return true;
        } catch (SerializationException e) {
            logger.error("Error when deserializing stored request for job "
                    + job.getJobName(), e);
            return false;
        }
    }

    /**
     * Take a snapshot of enqueued jobs and run them one at a time.
     */
    @Override
    public void process() throws Exception {
        if (!EDEXUtil.isRunning()) {
            return;
        }

        statsReporter.logRun();
        logger.info("Checking for Backup Service jobs...");
        BackupServiceConfigManager configMgr = BackupServiceConfigManager
                .getInstance();
        configMgr.reload();
        threadSleepInterval = (int) (configMgr.getPollIntervalSeconds()
                * TimeUtil.MILLIS_PER_SECOND);
        if (rateLimiter == null || configMgr.getRateLimitKBps()
                * SizeUtil.BYTES_PER_KB != rateLimiter.getCapacity()) {
            rateLimiter = new TokenBucket((int) (configMgr.getRateLimitKBps()
                    * SizeUtil.BYTES_PER_KB));
        }

        List<BackupHost> hosts = configMgr.getBackupHosts();
        if (hosts.isEmpty()) {
            logger.info("No backup sites configured, exiting");
            return;
        }

        long bytesSentThisRun = 0;
        int requestsSentThisRun = 0;
        long t0 = System.currentTimeMillis();
        for (BackupHost host : hosts) {
            if (EDEXUtil.isShuttingDown()) {
                break;
            }
            for (BackupJob job : dao.poll(host.getName())) {
                if (EDEXUtil.isShuttingDown()) {
                    break;
                }
                Integer versionMatches = host.compareVersion(
                        job.getMinVersionRequired(),
                        job.getMaxVersionRequired());
                if (versionMatches == null) {
                    logger.warn("Unable to contact " + job.getHost()
                            + " for version information. Skipping job "
                            + job.getJobName() + " for this host");
                } else if (versionMatches < 0) {
                    /*
                     * Remote server is not yet caught up to the required
                     * version. Do not run the job; keep it around for later
                     */
                    logger.debug("Skipping job " + job.getJobName() + " for "
                            + job.getHost() + ". Job requires version at least "
                            + job.getMaxVersionRequired() + "; " + job.getHost()
                            + " has " + host.getEDEXVersion());
                } else if (versionMatches > 0) {
                    /*
                     * The remote server has a too-new EDEX version. Discard the
                     * job for that server
                     */
                    logger.info("Discarding job " + job.getJobName() + " for "
                            + job.getHost()
                            + ". Job requires version no greater than "
                            + job.getMaxVersionRequired() + "; " + job.getHost()
                            + " has " + host.getEDEXVersion());
                    dao.removeFinishedJob(job);
                } else {
                    if (job.getBlobSize() > configMgr.getBigJobSize()) {
                        logger.warn(String.format(
                                "Job %s includes large request (%s)",
                                job.getJobName(),
                                SizeUtil.prettyByteSize(job.getBlobSize())));
                    }

                    boolean jobSucceeded = runJob(job, host);
                    if (jobSucceeded) {
                        bytesSentThisRun += job.getBlobSize();
                        requestsSentThisRun++;
                        dao.removeFinishedJob(job);
                    }
                    statsReporter.logProcessedJob(job, jobSucceeded);
                }
            }
        }

        long t1 = System.currentTimeMillis();
        if (requestsSentThisRun > 0) {
            logger.info(String.format("Sent %d requests totaling %s in %s.",
                    requestsSentThisRun,
                    SizeUtil.prettyByteSize(bytesSentThisRun),
                    TimeUtil.prettyDuration(t1 - t0)));
        } else {
            logger.info("No jobs to process");
        }

        if (!EDEXUtil.isShuttingDown() && requestsSentThisRun > 0) {
            // This is a maintenance task that need not be performed every run
            dao.cleanUp(configMgr.getHostnamesOnly());
        }
    }

    @Override
    public String getThreadGroupName() {
        return "backupServiceThread";
    }

    @Override
    public void preStop() {
        super.preStop();

        // Stop any network I/O
        synchronized (threads) {
            for (Thread thread : threads) {
                thread.interrupt();
            }
        }
    }

    /** Dump stats report to log. Called via Spring */
    public void reportStats() {
        statsReporter.reportStats();
    }

}
