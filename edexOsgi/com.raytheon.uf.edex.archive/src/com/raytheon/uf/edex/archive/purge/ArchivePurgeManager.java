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

import java.io.File;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;

import com.raytheon.uf.common.archive.config.ArchiveConfig;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConstants;
import com.raytheon.uf.common.archive.config.CategoryConfig;
import com.raytheon.uf.common.archive.config.CategoryFileDateHelper;
import com.raytheon.uf.common.archive.config.DataSetStatus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.exception.ShutdownException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.ClusterTaskPK;
import com.raytheon.uf.edex.database.cluster.handler.SharedLockHandler;
import com.raytheon.uf.edex.database.cluster.handler.SharedLockHandler.LockType;

/**
 * This class coordinates cluster locking of plug-in directories while
 * performing the archive purge.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 01, 2014 2862       rferrel     Initial creation
 * Apr 24, 2014 2726       rjpeter     Added shutdown cancel
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class ArchivePurgeManager {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchivePurgeManager.class);

    /** Single instance of the manager. */
    private final static ArchivePurgeManager instance = new ArchivePurgeManager();

    /** Manger to handle archive configuration information. */
    private final ArchiveConfigManager manager;

    /** Limit number of times message is sent. */
    private boolean sentPurgeMessage = false;

    /** Prevent flooding of lock updates on cluster lock. */
    private final ITimer lockUpdateTimer = TimeUtil.getTimer();

    /**
     * Private constructor for singleton.
     */
    private ArchivePurgeManager() {
        manager = ArchiveConfigManager.getInstance();
    }

    /**
     * 
     * @return instance
     */
    public static ArchivePurgeManager getInstance() {
        return instance;
    }

    /**
     * Purge the Files that fall outside of the time frame constraints for the
     * archive. This will always leave the archive's top level directories even
     * when they are empty.
     * 
     * @param archive
     * @return purgeCount
     */
    public int purgeExpiredFromArchive(ArchiveConfig archive)
            throws ShutdownException {
        String archiveRootDirPath = archive.getRootDir();
        File archiveRootDir = new File(archiveRootDirPath);

        int purgeCount = 0;
        sentPurgeMessage = false;

        if (!archiveRootDir.isDirectory()) {
            statusHandler.error(archiveRootDir.getAbsolutePath()
                    + " not a directory.");
            return purgeCount;
        }

        if (statusHandler.isPriorityEnabled(Priority.INFO)) {
            statusHandler.info("Purging directory: \""
                    + archiveRootDir.getAbsolutePath() + "\".");
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            String message = String.format(
                    "Start setup of category date helpers for archive: %s.",
                    archive.getName());
            statusHandler.debug(message);
        }

        Map<CategoryConfig, CategoryFileDateHelper> helperMap = new HashMap<CategoryConfig, CategoryFileDateHelper>();
        for (CategoryConfig category : archive.getCategoryList()) {
            CategoryFileDateHelper helper = new CategoryFileDateHelper(
                    archiveRootDirPath, category);
            helperMap.put(category, helper);
        }

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            String message = String.format(
                    "End setup of category date helpers for archive: %s.",
                    archive.getName());
            statusHandler.debug(message);
        }

        final Calendar minPurgeTime = calculateExpiration(archive, null);

        IOFileFilter defaultTimeFilter = new IOFileFilter() {

            @Override
            public boolean accept(File dir, String name) {
                File file = new File(dir, name);
                return accept(file);
            }

            @Override
            public boolean accept(File file) {
                Calendar time = TimeUtil.newGmtCalendar();
                time.setTimeInMillis(file.lastModified());
                return time.compareTo(minPurgeTime) < 0;
            }
        };

        List<File> topLevelFiles = new LinkedList<File>(
                Arrays.asList(archiveRootDir.listFiles()));

        int previousSize = -1;

        // Keep looping as long as we keep getting locks or list is empty.
        while ((topLevelFiles.size() > 0)
                && (previousSize != topLevelFiles.size())) {
            previousSize = topLevelFiles.size();
            Iterator<File> topLevelIter = topLevelFiles.iterator();

            while (topLevelIter.hasNext()) {
                File topFile = topLevelIter.next();
                /*
                 * In top level directory ignore all hidden files and
                 * directories.
                 */
                ClusterTask ct = null;
                if (topFile.isHidden()) {
                    topLevelIter.remove();
                } else {
                    if (topFile.isDirectory()) {
                        ct = getWriteLock(topFile.getAbsolutePath());
                        if (ct == null) {
                            continue;
                        }
                        topLevelIter.remove();
                        boolean isInCategory = false;
                        for (CategoryConfig category : archive
                                .getCategoryList()) {
                            CategoryFileDateHelper helper = helperMap
                                    .get(category);

                            if (helper.isCategoryDirectory(topFile.getName())) {
                                isInCategory = true;
                                if (statusHandler
                                        .isPriorityEnabled(Priority.INFO)) {
                                    String message = String
                                            .format("Start purge of category %s - %s, directory \"%s\".",
                                                    archive.getName(),
                                                    category.getName(),
                                                    topFile.getAbsolutePath());
                                    statusHandler.info(message);
                                }

                                final Calendar extPurgeTime = calculateExpiration(
                                        archive, category);
                                int pc = purgeDir(topFile, defaultTimeFilter,
                                        minPurgeTime, extPurgeTime, helper,
                                        category, ct);
                                purgeCount += pc;
                                if (statusHandler
                                        .isPriorityEnabled(Priority.INFO)) {
                                    String message = String
                                            .format("End purge of category %s - %s, directory \"%s\", deleted %d files and directories.",
                                                    archive.getName(),
                                                    category.getName(),
                                                    topFile.getAbsolutePath(),
                                                    pc);
                                    statusHandler.info(message);
                                }
                                break;
                            }
                        }
                        if (isInCategory == false) {
                            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                                String message = String.format(
                                        "Start purge of directory: \"%s\".",
                                        topFile.getAbsolutePath());
                                statusHandler.info(message);
                            }
                            int pc = purgeDir(topFile, defaultTimeFilter);
                            purgeCount += pc;
                            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                                String message = String
                                        .format("End purge of directory: \"%s\", deleted %d files and directories.",
                                                topFile.getAbsolutePath(), pc);
                                statusHandler.info(message);
                            }
                        }
                        releaseWriteLock(ct);
                        ct = null;
                    } else {
                        if (defaultTimeFilter.accept(topFile)) {
                            purgeCount += deleteFile(topFile);
                        }
                    }
                }
            }
        }
        return purgeCount;
    }

    /**
     * Attempt to get exclusive write lock.
     * 
     * @param details
     * @return clusterTask when getting lock successful otherwise null
     */
    private ClusterTask getWriteLock(String details) {
        SharedLockHandler lockHandler = new SharedLockHandler(LockType.WRITER);
        ClusterTask ct = ClusterLockUtils.lock(ArchiveConstants.CLUSTER_NAME,
                details, lockHandler, false);
        if (ct.getLockState().equals(LockState.SUCCESSFUL)) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO, String.format(
                        "Locked: \"%s\"", ct.getId().getDetails()));
            }
            lockUpdateTimer.reset();
            lockUpdateTimer.start();
        } else {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO, String.format(
                        "Skip purge unable to lock: \"%s\"", ct.getId()
                                .getDetails()));
            }
            ct = null;
        }
        return ct;
    }

    /**
     * Unlock the consumer's lock.
     */
    private void releaseWriteLock(ClusterTask ct) {
        if (ClusterLockUtils.unlock(ct, false)) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO, String.format(
                        "Unlocked: \"%s\"", ct.getId().getDetails()));
            }
        } else {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM, String.format(
                        "Unable to unlock: \"%s\"", ct.getId().getDetails()));
            }
        }
        lockUpdateTimer.stop();
    }

    private void updateLockTime(ClusterTask ct) {
        ClusterTaskPK id = ct.getId();

        // Slow down the rate at which we hit the database.
        if (lockUpdateTimer.getElapsedTime() >= TimeUtil.MILLIS_PER_MINUTE) {
            lockUpdateTimer.stop();
            lockUpdateTimer.reset();
            lockUpdateTimer.start();
            ClusterLockUtils.updateLockTime(id.getName(), id.getDetails(),
                    System.currentTimeMillis());
        }
    }

    /**
     * Purge the contents of a directory of expired data leaving a possibly
     * empty directory.
     * 
     * @param dir
     * @param defaultTimeFilter
     * @param minPurgeTime
     * @param extPurgeTime
     * @param helper
     * @param category
     * @param ct
     * @return purgeCount
     */
    private int purgeDir(File dir, IOFileFilter defaultTimeFilter,
            Calendar minPurgeTime, Calendar extPurgeTime,
            CategoryFileDateHelper helper, CategoryConfig category,
            ClusterTask ct) throws ShutdownException {
        EDEXUtil.checkShuttingDown();

        int purgeCount = 0;

        File[] dirFiles = dir.listFiles();
        if (dirFiles == null) {
            sendPurgeMessage();
            return purgeCount;
        }

        for (File file : dirFiles) {
            EDEXUtil.checkShuttingDown();
            updateLockTime(ct);

            if (!file.isHidden()) {
                DataSetStatus status = helper.getFileDate(file);
                if (status.isInDataSet()) {
                    Collection<String> labels = category
                            .getSelectedDisplayNames();
                    boolean isSelected = false;
                    for (String label : status.getDisplayLabels()) {
                        if (labels.contains(label)) {
                            isSelected = true;
                            break;
                        }
                    }

                    Calendar checkTime = (isSelected ? extPurgeTime
                            : minPurgeTime);
                    Calendar fileTime = status.getTime();
                    boolean purge = fileTime.compareTo(checkTime) < 0;

                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        String message = String
                                .format("%s [%s] category [%s] %s retention [%s] checkTime [%s] = %s.",
                                        (file.isDirectory() ? "Directory"
                                                : "File"), file
                                                .getAbsoluteFile(), category
                                                .getName(), (isSelected ? "ext"
                                                : "min"), TimeUtil
                                                .formatCalendar(checkTime),
                                        TimeUtil.formatCalendar(fileTime),
                                        (purge ? "purge" : "retain"));
                        statusHandler.debug(message);
                    }

                    if (purge) {
                        if (file.isDirectory()) {
                            purgeCount += purgeDir(file,
                                    FileFilterUtils.trueFileFilter());
                        }
                        purgeCount += deleteFile(file);
                    }
                } else if (file.isDirectory()) {
                    purgeCount += purgeDir(file, defaultTimeFilter,
                            minPurgeTime, extPurgeTime, helper, category, ct);
                    if (file.list().length == 0) {
                        purgeCount += deleteFile(file);
                    }
                } else if (defaultTimeFilter.accept(file)) {
                    purgeCount += deleteFile(file);
                }
            }
        }

        return purgeCount;
    }

    /**
     * Recursively purge the contents of a directory based on the filter. The
     * directory in the initial call is not deleted. This may result in an empty
     * directory which is the desired result for top level directories.
     * 
     * 
     * @param dir
     * @param fileDataFilter
     * @return purgeCount
     */
    private int purgeDir(File dir, IOFileFilter fileDataFilter)
            throws ShutdownException {
        EDEXUtil.checkShuttingDown();

        int purgeCount = 0;
        File[] dirFiles = dir.listFiles();
        if (dirFiles == null) {
            sendPurgeMessage();
        } else {
            for (File file : dirFiles) {
                EDEXUtil.checkShuttingDown();

                if (!file.isHidden()) {
                    if (file.isDirectory()) {
                        purgeCount += purgeDir(file, fileDataFilter);
                        if (file.list().length == 0) {
                            purgeCount += deleteFile(file);
                        }
                    } else if (fileDataFilter.accept(file)) {
                        purgeCount += deleteFile(file);
                    }
                }
            }
        }
        return purgeCount;
    }

    /**
     * Delete a file or directory.
     * 
     * @param file
     * @return purgeCount
     */
    private int deleteFile(File file) {
        int purgeCount = 0;
        boolean isDir = file.isDirectory();
        if (file.delete()) {
            ++purgeCount;
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler
                        .debug(String.format("Purged %s: \"%s\"",
                                (isDir ? "directory" : "file"),
                                file.getAbsolutePath()));
            }
        } else {
            statusHandler.warn(String.format("Failed to purge %s: \"%s\"",
                    (isDir ? "directory" : "file"), file.getAbsolutePath()));
        }
        return purgeCount;
    }

    /**
     * Get expiration time for the category.
     * 
     * @param archive
     * @param category
     * @return expireCal
     */
    private Calendar calculateExpiration(ArchiveConfig archive,
            CategoryConfig category) {
        Calendar expireCal = TimeUtil.newGmtCalendar();
        int retHours = (category == null)
                || (category.getRetentionHours() == 0) ? archive
                .getRetentionHours() : category.getRetentionHours();
        if (retHours != 0) {
            expireCal.add(Calendar.HOUR, (-1) * retHours);
        }
        return expireCal;
    }

    /**
     * Send race condition message out only one time per purge request.
     */
    private void sendPurgeMessage() {
        if (!sentPurgeMessage) {
            sentPurgeMessage = true;
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                String message = "Archive purge finding missing directory. Purge may be running on more then one EDEX server";
                statusHandler.handle(Priority.PROBLEM, message);
            }
        }
    }

    /**
     * Get archives from the archive configuration manager.
     * 
     * @return archives
     */
    public Collection<ArchiveConfig> getArchives() {
        return manager.getArchives();
    }

    /**
     * Perform the archive configuration manager's reset.
     */
    public void reset() {
        manager.reset();
    }
}
