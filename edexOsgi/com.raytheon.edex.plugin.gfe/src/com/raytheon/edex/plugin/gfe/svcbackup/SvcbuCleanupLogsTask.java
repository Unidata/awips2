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
package com.raytheon.edex.plugin.gfe.svcbackup;

import com.raytheon.uf.common.dataplugin.gfe.util.FilePurger;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Bean to purge outdated log files from service backup logs directory. Expected
 * to be scheduled by camel using a cron job. Can set the number of days to
 * retain logs via the setting purge.svcbu.logs.retention in
 * com.raytheon.edex.plugin.gfe.properties.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 03, 2013            jdynina      Initial creation
 * Feb 24, 2015  #4103     dgilling     Rewrite to use FilePurger.
 * 
 * </pre>
 * 
 * @author jdynina
 * @version 1.0
 */
public final class SvcbuCleanupLogsTask {

    private static final String DEFAULT_LOG_DIR = "/awips2/GFESuite/ServiceBackup/logs";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SvcbuCleanupLogsTask.class);

    private final FilePurger logPurger;

    public SvcbuCleanupLogsTask(int daysToRetain) {
        long purgeAge = daysToRetain * TimeUtil.MILLIS_PER_DAY;
        String logDirectory = SvcBackupUtil.getSvcBackupProperties()
                .getProperty("IFPS_LOG", DEFAULT_LOG_DIR);
        this.logPurger = new FilePurger(logDirectory, purgeAge);
    }

    public void run() {
        statusHandler.info("Cleanup service backup logs cron started.");

        try {
            logPurger.purge();
        } catch (Exception e) {
            statusHandler
                    .error("Cleanup service backup logs cron threw an unhandled exception.",
                            e);
        }
    }
}
