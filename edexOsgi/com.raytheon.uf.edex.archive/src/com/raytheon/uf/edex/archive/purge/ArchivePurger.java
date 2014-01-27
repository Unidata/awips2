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
package com.raytheon.uf.edex.archive.purge;

import java.util.Collection;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;

/**
 * Purge task to purge archived data based on configured expiration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May  6, 2013 1965       bgonzale    Initial creation
 *                                     Added info logging for purge counts.
 * Aug 28, 2013 2299       rferrel     manager.purgeExpiredFromArchive now returns
 *                                      number of files purged.
 * Sep 03, 2013 2224       rferrel     Add check to enable/disable purger.
 * Nov 05, 2013 2499       rjpeter     Repackaged
 * Dec 17, 2013 2603       rjpeter     Reload configuration every run of purge.
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public class ArchivePurger {
    private final static IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchivePurger.class);

    private final static String ENABLE_PROPERTY = "archive.purge.enable";

    /**
     * Purge expired elements from the archives.
     */
    public static void purge() {
        Thread.currentThread().setName("Purge-Archive");
        String enableString = System.getProperty(ENABLE_PROPERTY, "false");
        if (Boolean.parseBoolean(enableString)) {
            ITimer timer = TimeUtil.getTimer();
            timer.start();
            statusHandler.info("Archive Purge started.");
            ArchiveConfigManager manager = ArchiveConfigManager.getInstance();
            manager.reset();
            Collection<ArchiveConfig> archives = manager.getArchives();
            for (ArchiveConfig archive : archives) {
                ITimer archiveTimer = TimeUtil.getTimer();
                archiveTimer.start();
                int purgeCount = manager.purgeExpiredFromArchive(archive);
                if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                    StringBuilder sb = new StringBuilder(archive.getName());
                    sb.append("::Archive Purged ");
                    sb.append(purgeCount);
                    sb.append(" file");
                    if (purgeCount != 1) {
                        sb.append("s");
                    }
                    sb.append(" in ")
                            .append(TimeUtil.prettyDuration(archiveTimer
                                    .getElapsedTime())).append(".");
                    statusHandler.info(sb.toString());
                }
            }
            statusHandler.info("Archive Purge finished.  Time to run: "
                    + TimeUtil.prettyDuration(timer.getElapsedTime()));
        } else {
            statusHandler.info("Archive Purge disabled, exiting");
        }

    }
}
