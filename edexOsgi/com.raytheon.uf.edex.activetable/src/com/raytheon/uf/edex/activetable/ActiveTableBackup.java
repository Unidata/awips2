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
package com.raytheon.uf.edex.activetable;

import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import jep.JepException;

import org.apache.commons.io.filefilter.WildcardFileFilter;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;

/**
 * Perform Active Table Backup
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2014  #3296     randerso    Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ActiveTableBackup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTableBackup.class);

    private static class BackupRequest {
        ActiveTableMode activeTableMode;

        List<ActiveTableRecord> activeTable;

        BackupRequest(ActiveTableMode activeTableMode,
                List<ActiveTableRecord> activeTable) {
            this.activeTableMode = activeTableMode;
            this.activeTable = activeTable;
        }
    }

    private static int QUEUE_LIMIT = 10;

    private static BlockingQueue<BackupRequest> queue = new LinkedBlockingQueue<BackupRequest>(
            QUEUE_LIMIT);

    private static ThreadLocal<PythonScript> threadLocalPythonScript = new ThreadLocal<PythonScript>() {

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.ThreadLocal#initialValue()
         */
        @Override
        protected PythonScript initialValue() {
            try {
                IPathManager pathMgr = PathManagerFactory.getPathManager();
                LocalizationContext commonCx = pathMgr.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
                String filePath = pathMgr.getFile(commonCx,
                        "vtec" + File.separator + "VTECTableUtil.py").getPath();
                String siteId = pathMgr.getContext(
                        LocalizationType.COMMON_STATIC, LocalizationLevel.SITE)
                        .getContextName();
                String includePath = PyUtil.buildJepIncludePath(
                        ActiveTablePyIncludeUtil.getCommonPythonIncludePath(),
                        ActiveTablePyIncludeUtil.getVtecIncludePath(siteId));

                PythonScript python = new PythonScript(filePath, includePath,
                        ActiveTableBackup.class.getClassLoader());
                return python;
            } catch (JepException e) {
                throw new RuntimeException(e);
            }
        }

    };

    private static Runnable target = new Runnable() {

        @Override
        public void run() {
            IPerformanceStatusHandler perfStat = PerformanceStatus
                    .getHandler("ActiveTable");
            ITimer timer = TimeUtil.getTimer();
            long lastPurge = 0;
            while (true) {
                try {
                    BackupRequest request = queue.take();
                    timer.reset();
                    timer.start();
                    IPathManager pathMgr = PathManagerFactory.getPathManager();
                    LocalizationContext ctx = pathMgr.getContext(
                            LocalizationType.EDEX_STATIC,
                            LocalizationLevel.SITE);
                    String siteId = ctx.getContextName();
                    File backupDir = pathMgr.getFile(ctx,
                            FileUtil.join("vtec", "backup")).getAbsoluteFile();

                    try {
                        PythonScript python = threadLocalPythonScript.get();
                        HashMap<String, Object> args = new HashMap<String, Object>(
                                4, 1.0f);
                        args.put("activeTable", request.activeTable);
                        args.put("activeTableMode",
                                request.activeTableMode.toString());
                        args.put("filePath", backupDir.getParent());
                        args.put("siteId", siteId);
                        try {
                            python.execute("backupActiveTable", args);
                        } catch (JepException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    "Error backing up active table", e);
                        }
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error initializing active table python", e);
                    }
                    timer.stop();
                    perfStat.logDuration("backup activeTable",
                            timer.getElapsedTime());

                    long now = System.currentTimeMillis();
                    // don't run purge more than once a day
                    if ((now - lastPurge) > TimeUtil.MILLIS_PER_DAY) {
                        timer.reset();
                        timer.start();
                        lastPurge = now;
                        // get purge age in hours
                        long purgeAge = ((Number) VTECPartners.getInstance(
                                siteId).getattr("VTEC_BACKUP_TABLE_PURGE_TIME",
                                168 * 4)).longValue();

                        // compute purge time
                        long purgeTime = now
                                - (purgeAge * TimeUtil.MILLIS_PER_HOUR);

                        // file filter for backup files for the requested mode
                        FilenameFilter filter = new WildcardFileFilter("*"
                                + request.activeTableMode.toString() + "*.gz");

                        // purge any backup file older than purge time;
                        for (File file : backupDir.listFiles(filter)) {
                            if (file.lastModified() < purgeTime) {
                                file.delete();
                            }
                        }

                        timer.stop();
                        perfStat.logDuration("purge activeTable backups",
                                timer.getElapsedTime());
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }

    };

    private static Thread backupJob = new Thread(target, "activeTableBackup");

    /**
     * Queue an active table backup request
     * 
     * @param activeTableMode
     * @param activeTable
     */
    public static void queue(ActiveTableMode activeTableMode,
            List<ActiveTableRecord> activeTable) {
        BackupRequest req = new BackupRequest(activeTableMode, activeTable);
        try {
            queue.add(req);
        } catch (IllegalStateException e) {
            // discard a backup request due to queue full
            BackupRequest discard = queue.poll();
            queue.add(req);
            if (discard != null) {
                statusHandler
                        .warn("ActiveTable backup request discarded, queue full");
            }
        }
        if (!backupJob.isAlive()) {
            backupJob.start();
        }
    }
}
