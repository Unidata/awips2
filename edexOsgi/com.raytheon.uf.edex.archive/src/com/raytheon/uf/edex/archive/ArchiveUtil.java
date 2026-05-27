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

import java.nio.file.attribute.PosixFilePermission;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.EnumSet;
import java.util.Set;

import com.raytheon.uf.common.archive.config.ArchiveConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.SharedLockHandler;
import com.raytheon.uf.edex.database.cluster.handler.SharedLockHandler.LockType;

/**
 * This is a utility class that provide common API for archiving
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2021 8690      aelgorashi   Initial creation.
 * Mar 03, 2022 8690      lsingh   Added common static fields/methods to be used in multiple Archive classes
 * </pre>
 *
 * @author aelgorashi
 */
public class ArchiveUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveUtil.class);

    /** Maximum time increment to archive, note based off of insertTime. */
    protected static final long MAX_DURATION_MILLIS = 60
            * TimeUtil.MILLIS_PER_MINUTE;

    /** Minimum time increment to archive, note based off of insertTime. */
    protected static final long MIN_DURATION_MILLIS = 30
            * TimeUtil.MILLIS_PER_MINUTE;;

    public static final Set<PosixFilePermission> FILE_PERMISSIONS = Collections
            .unmodifiableSet(EnumSet.of(PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.GROUP_READ));

    public static final Set<PosixFilePermission> DIR_PERMISSIONS = Collections
            .unmodifiableSet(EnumSet.of(PosixFilePermission.OWNER_READ,
                    PosixFilePermission.OWNER_WRITE,
                    PosixFilePermission.GROUP_READ,
                    PosixFilePermission.OWNER_EXECUTE,
                    PosixFilePermission.GROUP_EXECUTE));

    /** Thread safe date format. */
    public static final ThreadLocal<SimpleDateFormat> TL_DATE_FORMAT = new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat df = new SimpleDateFormat(
                    "yyyy-MM-dd HH:mm:ss.SSS");
            df.setTimeZone(TimeUtil.GMT_TIME_ZONE);
            return df;
        }
    };

    /**
     * Determine the start of the archiving date range
     *
     * @param pluginName
     * @param extraInfo
     * @param runTime
     * @return the start time of the data to be retrieved and archived
     * @throws DataAccessLayerException
     */
    public static Calendar getStartTime(String pluginName, String extraInfo,
            Calendar runTime) throws DataAccessLayerException {
        Calendar startTime = null;
        SimpleDateFormat dateFormat = TL_DATE_FORMAT.get();

        // get previous run time
        if ((extraInfo != null) && !extraInfo.isEmpty()) {
            try {
                Date prevDate = dateFormat.parse(extraInfo);

                // cloning runTime as it already has the correct time zone
                startTime = (Calendar) runTime.clone();
                startTime.setTimeInMillis(prevDate.getTime());
            } catch (ParseException e) {
                statusHandler.error(pluginName
                        + ": Unable to parse last run time [" + extraInfo
                        + "], will . all data up to current time", e);
                startTime = null;
            }
        }
        // protect against time failure where startTime is more than
        // MIN_DURATION in the future
        if (startTime != null) {
            if ((startTime.getTimeInMillis()
                    - runTime.getTimeInMillis()) > MIN_DURATION_MILLIS) {
                statusHandler.warn(pluginName
                        + ": Previous run time is a future time, reseting to current time.  Check server times");
                startTime = (Calendar) runTime.clone();
            }
        }
        return startTime;
    }

    /**
     * Determine the end of the archiving date range
     *
     * @param startTime
     * @param runTime
     * @return the end time of the data to be retrieved and archived
     */
    public static Calendar getEndTime(Calendar startTime, Calendar runTime) {
        Calendar endTime = null;
        long timeDiff = runTime.getTimeInMillis() - startTime.getTimeInMillis();

        if (timeDiff > MAX_DURATION_MILLIS) {
            endTime = (Calendar) startTime.clone();
            endTime.setTimeInMillis(
                    endTime.getTimeInMillis() + MAX_DURATION_MILLIS);
        } else if (timeDiff > MIN_DURATION_MILLIS) {
            endTime = (Calendar) runTime.clone();
        }

        return endTime;

    }

    /**
     * Attempt to get exclusive consumer's writer lock.
     *
     * @param details
     * @return clusterTask when getting lock successful otherwise null
     */
    public static ClusterTask getWriteLock(String details) {
        SharedLockHandler lockHandler = new SharedLockHandler(LockType.WRITER);
        ClusterTask ct = ClusterLockUtils.lock(ArchiveConstants.CLUSTER_NAME,
                details, lockHandler, false);
        if (LockState.SUCCESSFUL.equals(ct.getLockState())) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO, String
                        .format("Locked: \"%s\"", ct.getId().getDetails()));
            }
        } else {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO,
                        String.format(
                                "Skip database Archive unable to lock: \"%s\"",
                                ct.getId().getDetails()));
            }
            ct = null;
        }

        return ct;
    }

    /**
     * Unlock the consumer's lock.
     *
     * @param ct
     */
    public static void releaseWriteLock(ClusterTask ct) {
        if (ClusterLockUtils.unlock(ct, false)) {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(Priority.INFO, String
                        .format("Unlocked: \"%s\"", ct.getId().getDetails()));
            }
        } else {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM, String.format(
                        "Unable to unlock: \"%s\"", ct.getId().getDetails()));
            }
        }
    }
}
