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

import java.util.EnumMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.edex.backupsvc.database.BackupJob;

/**
 * Collect and report statistics for BackupService. Thread-safe
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2017 6352       tgurney     Initial creation, extracted from
 *                                     BackupService
 * Oct  3, 2019 7929       tgurney     Report stats for deferred jobs
 *
 * </pre>
 *
 * @author tgurney
 */

public class BackupServiceStatsReporter {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private Map<BackupService.JobStatus, Integer> processedJobs = new EnumMap<>(
            BackupService.JobStatus.class);

    private long bytesSent;

    private long lastStatusReportTime;

    private boolean ranSinceLastStatusReport;

    public BackupServiceStatsReporter() {
        resetStats();
    }

    private synchronized void resetStats() {
        bytesSent = 0;
        processedJobs.clear();
        for (BackupService.JobStatus status : BackupService.JobStatus
                .values()) {
            processedJobs.put(status, 0);
        }
        lastStatusReportTime = System.currentTimeMillis();
    }

    public synchronized void logProcessedJob(BackupJob job,
            BackupService.JobStatus status) {
        bytesSent += job.getBlobSize();
        int last = processedJobs.get(status);
        processedJobs.put(status, last + 1);
    }

    public synchronized void logRun() {
        ranSinceLastStatusReport = true;
    }

    public synchronized void reportStats() {
        if (!ranSinceLastStatusReport) {
            return;
        }
        BackupServiceConfigManager configMgr = BackupServiceConfigManager
                .getInstance();
        ranSinceLastStatusReport = false;
        String rateLimitText = SizeUtil.prettyByteSize(
                configMgr.getRateLimitKBps() * SizeUtil.BYTES_PER_KB);
        long now = System.currentTimeMillis();
        String timePeriod = TimeUtil.prettyDuration(now - lastStatusReportTime);
        logger.info(String.format("Backup service activity for the last %s: ",
                timePeriod));
        int totalProcessedJobs = processedJobs.values().stream()
                .mapToInt(Integer::intValue).sum();
        for (BackupService.JobStatus status : BackupService.JobStatus
                .values()) {
            String msg = status + " jobs: " + processedJobs.get(status) + "/"
                    + totalProcessedJobs;
            logger.info(msg);
        }
        logger.info("Data sent: " + SizeUtil.prettyByteSize(bytesSent));
        logger.info("Rate limit: " + rateLimitText + "/s");
        logger.info("Backup hosts configured: "
                + configMgr.getBackupHosts().size());
        resetStats();
    }

}
