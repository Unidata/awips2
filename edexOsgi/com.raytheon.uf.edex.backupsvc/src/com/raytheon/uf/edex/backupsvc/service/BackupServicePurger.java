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

import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcIncomingDao;
import com.raytheon.uf.edex.backupsvc.database.BackupSvcOutgoingDao;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexTimerBasedThread;

/**
 * Backup service purger will purge the BSJs from DB when their updatetime is
 * over two weeks. The purging operation will be performed once per day.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer        Description
 * ------------ ----------- --------------- --------------------------
 * Jul 15, 2021 88368       Gang Chen       Initial creation
 *
 * </pre>
 *
 * @author gchen
 */

public class BackupServicePurger extends EdexTimerBasedThread {

    private final BackupSvcOutgoingDao backupSvcOutgoingDao;

    private final BackupSvcIncomingDao backupSvcIncomingDao;

    private final BackupServiceConfigManager configMgr = BackupServiceConfigManager
            .getInstance();

    private long purgeTimePeriodInDays;

    private long purgeTimePeriodInMilliseconds;

    /**
     * Public constructor for Spring only in backupsvc-request.xml
     */
    public BackupServicePurger() {
        threadSleepInterval = (int) (configMgr.getPollIntervalSeconds()
                * TimeUtil.MILLIS_PER_SECOND);

        purgeTimePeriodInDays = configMgr.getPurgeTimePeriodDays();

        purgeTimePeriodInMilliseconds = purgeTimePeriodInDays
                * TimeUtil.MILLIS_PER_DAY;

        backupSvcOutgoingDao = new BackupSvcOutgoingDao();
        backupSvcIncomingDao = new BackupSvcIncomingDao();
    }

    /**
     * Purge the BSJs from outgoing and incoming tables over 2 weeks
     */
    @Override
    public void process() throws Exception {
        if (!EDEXUtil.isRunning()) {
            return;
        }

        logger.info("Reload configurations from the backupSvc.xml file.");

        configMgr.reload();
        threadSleepInterval = (int) (configMgr.getPollIntervalSeconds()
                * TimeUtil.MILLIS_PER_SECOND);

        purgeTimePeriodInDays = configMgr.getPurgeTimePeriodDays();
        purgeTimePeriodInMilliseconds = purgeTimePeriodInDays
                * TimeUtil.MILLIS_PER_DAY;

    }

    @Override
    public String getThreadGroupName() {
        return "backupServicePurgerThread";
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

    /**
     * Purge those expired jobs from DB tables when they are over specified days
     */
    public void purgeExpiredJobs() {
        try {
            configMgr.reload();
            purgeTimePeriodInDays = configMgr.getPurgeTimePeriodDays();
            purgeTimePeriodInMilliseconds = purgeTimePeriodInDays
                    * TimeUtil.MILLIS_PER_DAY;
        } catch (Exception e) {
            logger.error(
                    "Reload the purgeTimePeriodDays configuration is failed.");
        }

        logger.info("Purging the backup service jobs are over "
                + purgeTimePeriodInDays + " days");

        backupSvcOutgoingDao
                .purgeExpiredJobs(this.purgeTimePeriodInMilliseconds);
        backupSvcIncomingDao
                .purgeExpiredJobs(this.purgeTimePeriodInMilliseconds);
    }

}
