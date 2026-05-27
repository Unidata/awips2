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
package com.raytheon.uf.edex.archive;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.TimeZone;
import java.util.Timer;
import java.util.TimerTask;

import com.raytheon.uf.common.archive.config.ArchiveConstants;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.PluginProperties;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.dataplugin.PluginRegistry;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * This class handles saving processed data to the archiver directory.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            rjpeter     Initial creation
 * Jan 18, 2013 1469       bkowal      Removed the hdf5 data directory.
 * Oct 23, 2013 2478       rferrel     Make date format thread safe.
 *                                     Add debug information.
 * Nov 05, 2013 2499       rjpeter     Repackaged, removed config files, always compresses hdf5.
 * Nov 11, 2013 2478       rjpeter     Updated data store copy to always copy hdf5.
 * Dec 13, 2013 2555       rjpeter     Refactored logic into DatabaseArchiveProcessor.
 * Feb 12, 2014 2784       rjpeter     Fixed clusterLock to not update the time by default.
 * Apr 01, 2014 2862       rferrel     Add exclusive lock at plug-in level.
 * Apr 23, 2014 2726       rjpeter     Added shutdown hook for quicker shutdown while archiver is running.
 * Oct 29, 2021 8690       aelgorashi  moved some common code to ArchiveUtil class.
 * </pre>
 *
 * @author rjpeter
 * @version 1.0
 */
public class DatabaseArchiver implements IPluginArchiver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatabaseArchiver.class);

    /** Default batch size for database queries */
    private static final Integer defaultBatchSize = 10000;

    /** Job's name. */
    private static final String TASK_NAME = "DB Archiver";

    /** Cluster time out on lock. */
    private static final long CLUSTER_LOCK_TIMEOUT = 10
            * TimeUtil.MILLIS_PER_MINUTE;

    /** Mapping for plug-in formatters. */
    private final Map<String, IPluginArchiveFileNameFormatter> pluginArchiveFormatters;

    /** Mapping for plug-in fetch size */
    private final Map<String, Integer> pluginBatchSize;

    private final IPluginArchiveFileNameFormatter defaultFormatter = new DefaultPluginArchiveFileNameFormatter();

    /** When true dump the pdos. */
    private final boolean debugArchiver;

    private final boolean compressDatabaseFiles;

    /** Task to update the lock time for the locked plugin cluster task. */
    private static final class LockUpdateTask extends TimerTask {
        /** The locked cluster task's details. */
        private final String details;

        public LockUpdateTask(String details) {
            this.details = details;
        }

        @Override
        public void run() {
            long currentTime = System.currentTimeMillis();
            ClusterLockUtils.updateLockTime(ArchiveConstants.CLUSTER_NAME,
                    details, currentTime);
        }
    }

    /**
     * The constructor.
     */
    public DatabaseArchiver() {
        pluginArchiveFormatters = new HashMap<>();
        pluginBatchSize = new HashMap<>();
        debugArchiver = Boolean.getBoolean("archive.debug.enable");
        compressDatabaseFiles = Boolean
                .getBoolean("archive.compression.enable");
    }

    @Override
    public void archivePlugin(String pluginName, String archivePath) {
        PluginProperties props = PluginRegistry.getInstance()
                .getRegisteredObject(pluginName);
        if ((props != null) && (props.getRecord() != null)
                && (props.getDao() != null)) {
            Class<?> recordClass = props.getRecord();
            if (recordClass != null
                    && PluginDataObject.class.isAssignableFrom(recordClass)) {
                archivePluginData(pluginName, archivePath);
            }
        }
    }

    private void archivePluginData(String pluginName, String archivePath) {
        if (EDEXUtil.isShuttingDown()) {
            return;
        }

        File archiveDir = new File(archivePath);
        File pluginDir = new File(archiveDir, pluginName);
        ClusterTask ctPlugin = ArchiveUtil
                .getWriteLock(pluginDir.getAbsolutePath());

        if (ctPlugin == null) {
            return;
        }

        SimpleDateFormat dateFormat = ArchiveUtil.TL_DATE_FORMAT.get();
        // set archive time
        Calendar runTime = TimeUtil.newGmtCalendar();
        runTime.add(Calendar.MINUTE, -30);

        // cluster lock, grabbing time of last successful archive
        CurrentTimeClusterLockHandler lockHandler = new CurrentTimeClusterLockHandler(
                CLUSTER_LOCK_TIMEOUT, false);
        ClusterTask ct = ClusterLockUtils.lock(TASK_NAME, pluginName,
                lockHandler, false);
        if (!LockState.SUCCESSFUL.equals(ct.getLockState())) {
            ArchiveUtil.releaseWriteLock(ctPlugin);
            return;
        }

        // keep extra info the same until processing updates the time.
        lockHandler.setExtraInfo(ct.getExtraInfo());

        Calendar startTime = null;
        long timimgStartMillis = System.currentTimeMillis();
        int recordCount = 0;
        statusHandler.info(pluginName + ": Archiving plugin");

        Timer lockUpdateTimer = new Timer("Update Shared Lock Time", true);
        TimerTask task = new LockUpdateTask(ctPlugin.getId().getDetails());
        lockUpdateTimer.schedule(task, TimeUtil.MILLIS_PER_MINUTE,
                TimeUtil.MILLIS_PER_MINUTE);

        try {
            // lookup dao
            PluginDao dao = null;
            try {
                dao = PluginFactory.getInstance().getPluginDao(pluginName);
            } catch (PluginException e) {
                statusHandler.error(pluginName
                        + ": Error getting data access object!  Unable to archive data!",
                        e);
                return;
            }

            startTime = determineStartTime(pluginName, ct.getExtraInfo(),
                    runTime, dao);
            Calendar endTime = ArchiveUtil.getEndTime(startTime, runTime);

            IPluginArchiveFileNameFormatter archiveFormatter = pluginArchiveFormatters
                    .get(pluginName);
            if (archiveFormatter == null) {
                archiveFormatter = defaultFormatter;
            }

            Integer batchSize = pluginBatchSize.get(pluginName);

            if (batchSize == null) {
                batchSize = defaultBatchSize;
            }

            DatabaseArchiveProcessor processor = new DatabaseArchiveProcessor(
                    archivePath, pluginName, dao, archiveFormatter);
            processor.setCompressDatabaseFiles(compressDatabaseFiles);
            processor.setDebugArchiver(debugArchiver);
            processor.setBatchSize(batchSize.intValue());

            while (!EDEXUtil.isShuttingDown() && (startTime != null)
                    && (endTime != null) && !processor.isFailed()) {
                statusHandler.info(pluginName + ": Checking for records from "
                        + TimeUtil.formatDate(startTime) + " to "
                        + TimeUtil.formatDate(endTime));

                processor.reset();
                dao.processArchiveRecords(startTime, endTime, processor);
                if (!processor.isFailed()) {
                    recordCount += processor.getRecordsSaved();
                    startTime = endTime;
                    endTime = ArchiveUtil.getEndTime(startTime, runTime);

                    // update the cluster lock with check point details
                    String extraInfo = dateFormat.format(startTime.getTime());
                    lockHandler.setExtraInfo(extraInfo);
                    ClusterLockUtils.updateExtraInfoAndLockTime(TASK_NAME,
                            pluginName, extraInfo, System.currentTimeMillis());
                }
            }

            if (recordCount > 0) {
                statusHandler.info(pluginName + ": archived " + recordCount
                        + " records in "
                        + TimeUtil.prettyDuration(System.currentTimeMillis()
                                - timimgStartMillis));
            } else {
                statusHandler
                        .info(pluginName + ": Found no records to archive");
            }
        } catch (Throwable e) {
            statusHandler.error(pluginName + ": Error occurred archiving data",
                    e);
        } finally {
            if (ct != null) {
                // release lock setting archive time in cluster lock
                ClusterLockUtils.unlock(ct, false);
            }

            /*
             * Stop updating ctPlugin's last execution time before releasing the
             * cluster's lock.
             */
            if (lockUpdateTimer != null) {
                lockUpdateTimer.cancel();
            }

            if (ctPlugin != null) {
                ArchiveUtil.releaseWriteLock(ctPlugin);
            }
        }

        return;
    }

    /**
     * Get the plug-in's start time for a query.
     *
     * @param pluginName
     * @param extraInfo
     * @param runTime
     * @param dao
     * @return startTime
     * @throws DataAccessLayerException
     */
    private Calendar determineStartTime(String pluginName, String extraInfo,
            Calendar runTime, PluginDao dao) throws DataAccessLayerException {
        Calendar startTime = ArchiveUtil.getStartTime(pluginName, extraInfo,
                runTime);
        if (startTime == null) {
            Date minInsert = dao.getMinInsertTime(null);
            if (minInsert != null) {
                startTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                startTime.setTimeInMillis(minInsert.getTime());
            } else {
                // if no data for plugin in db, set startTime to runTime
                startTime = (Calendar) runTime.clone();
            }

        }

        return startTime;
    }

    /**
     * Register archive formatter for a plug-in; and issue a warning if plug-in
     * is already registered.
     *
     * @param pluginName
     * @param archiveFormatter
     * @return databaseArchiver
     */
    public Object registerPluginArchiveFormatter(String pluginName,
            IPluginArchiveFileNameFormatter archiveFormatter) {
        if (!pluginArchiveFormatters.containsKey(pluginName)) {
            pluginArchiveFormatters.put(pluginName, archiveFormatter);
        } else {
            statusHandler
                    .warn("Plugin archive formatter already registered for: "
                            + pluginName);
        }

        return this;
    }

    /**
     * Register batch size for a plug-in.
     *
     * @param pluginName
     * @param batchSize
     *            Batch Size for the plugin. Default is 10000.
     * @return databaseArchiver
     */
    public Object registerPluginBatchSize(String pluginName,
            Integer batchSize) {
        pluginBatchSize.put(pluginName, batchSize);
        return this;
    }
}
