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

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

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
 *
 * </pre>
 *
 * @author tgurney
 */

public class BackupServiceStatsReporter {
    private final Logger logger = LoggerFactory.getLogger(getClass());

    private AtomicInteger jobsProcessed = new AtomicInteger();

    private AtomicInteger successfulJobs = new AtomicInteger();

    private AtomicInteger bytesSent = new AtomicInteger();

    private AtomicLong lastStatusReportTime = new AtomicLong(
            System.currentTimeMillis());

    private AtomicBoolean ranSinceLastStatusReport = new AtomicBoolean();

    public void logProcessedJob(BackupJob job, boolean succeeded) {
        bytesSent.addAndGet(job.getBlobSize());
        jobsProcessed.incrementAndGet();
        if (succeeded) {
            successfulJobs.incrementAndGet();
        }
    }

    public void logRun() {
        ranSinceLastStatusReport.set(true);
    }

    public void reportStats() {
        if (!ranSinceLastStatusReport.get()) {
            return;
        }
        BackupServiceConfigManager configMgr = BackupServiceConfigManager
                .getInstance();
        ranSinceLastStatusReport.set(false);
        String rateLimitText = SizeUtil.prettyByteSize(
                configMgr.getRateLimitKBps() * SizeUtil.BYTES_PER_KB);
        long now = System.currentTimeMillis();
        String timePeriod = TimeUtil
                .prettyDuration(now - lastStatusReportTime.get());
        logger.info(String.format("Backup service activity for the last %s: ",
                timePeriod));
        logger.info("Successful jobs: " + successfulJobs.get());
        logger.info(
                "Failed jobs: " + (jobsProcessed.get() - successfulJobs.get()));
        logger.info("Data sent: " + SizeUtil.prettyByteSize(bytesSent.get()));
        logger.info("Rate limit: " + rateLimitText + "/s");
        logger.info("Backup hosts configured: "
                + configMgr.getBackupHosts().size());
        lastStatusReportTime.set(now);
        bytesSent.set(0);
        jobsProcessed.set(0);
        successfulJobs.set(0);
    }

}
