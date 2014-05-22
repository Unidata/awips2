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
package com.raytheon.uf.edex.database.cluster.handler;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Class to limit a cluster task to either a writer client or reader shared
 * clients. When unlocking a lock with a shared reader a client must use the
 * ClusterLockUtils.getLocks() to find the desired cluster tasks with the latest
 * updates to extrainfo field. When getting the cluster task for an exclusive
 * writer client the returned cluster task may be used since no other client
 * will be associated with the cluster.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2014 2862       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public final class SharedLockHandler extends CurrentTimeClusterLockHandler {

    /** Client Lock type placed in cluster tasks' extrainfo column. */
    public static enum LockType {
        /** Shared reader lock. */
        READER,
        /** Exclusive writer lock. */
        WRITER
    };

    /**
     * Common override time out. Clients need to rely on this value in order to
     * update last execution time to maintain the lock.
     */
    private static final long OVERRIDE_TIMEOUT = 5 * TimeUtil.MILLIS_PER_MINUTE;

    /** Used to split type and count. */
    private final String SPLIT_DELIMITER = ":";

    /** The handler's current lock time. */
    private LockType type;

    /** The lock type handler wants to obtain. */
    private LockType wantedType;

    /** The number of clients associated with the cluster task's lock. */
    private int lockCount;

    /**
     * Constructor using default time out override.
     * 
     * @param wantedType
     */
    public SharedLockHandler(LockType wantedType) {
        super(OVERRIDE_TIMEOUT, true);
        this.wantedType = wantedType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler
     * #setTimeOutOverride(long)
     */
    @Override
    public void setTimeOutOverride(long timeOutOverride) {
        /*
         * Do not allow the override time out to be changed. If changed may
         * allowed the lock to be taken from an active client.
         */
    }

    /**
     * Set the count and lock type from the string.
     * 
     * @param extraInfo
     */
    public void parseExtraInfoString(String extraInfo) {
        if ((extraInfo == null) || (extraInfo.length() == 0)) {
            // Creating new entry,
            type = null;
            lockCount = 0;
        } else {
            StringBuilder errorMessage = null;
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                errorMessage = new StringBuilder();
            }

            String val[] = extraInfo.split(SPLIT_DELIMITER);
            if (val.length == 2) {
                if (LockType.READER.name().equals(val[0])) {
                    type = LockType.READER;
                } else if (LockType.WRITER.name().equals(val[0])) {
                    type = LockType.WRITER;
                } else if (errorMessage != null) {
                    errorMessage.append("has invalid lock type");
                }
                try {
                    lockCount = Integer.parseInt(val[1]);
                } catch (NumberFormatException ex) {
                    if (errorMessage != null) {
                        if (errorMessage.length() > 0) {
                            errorMessage.append(" and");
                        } else {
                            errorMessage.append("has");
                        }
                        errorMessage.append(" invalid count value");
                    }
                }
            } else if (errorMessage != null) {
                errorMessage.append("is a corrupted value");
            }

            if ((errorMessage != null) && (errorMessage.length() > 0)) {
                statusHandler.handle(Priority.PROBLEM, String.format(
                        " The extrainfo column value: \"%s\" %s.", extraInfo,
                        errorMessage.toString()));
            }
        }
    }

    /**
     * 
     * @return true when wanted lock and cluster lock are the same
     */
    public boolean locksMatch() {
        return wantedType.equals(type);
    }

    /**
     * 
     * @return lockCount
     */
    public int getLockCount() {
        return lockCount;
    }

    /**
     * Convert the current lock type and count into an information string.
     * 
     * @return extraInfo
     */
    private String createExtraInfoString() {
        return String.format("%s%s%d", type.name(), SPLIT_DELIMITER, lockCount);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler
     * #handleLock(com.raytheon.uf.edex.database.cluster.ClusterTask)
     */
    @Override
    public LockState handleLock(ClusterTask ct) {
        parseExtraInfoString(ct.getExtraInfo());
        LockState ls = super.handleLock(ct);

        if (ct.isRunning()) {
            if (LockState.SUCCESSFUL.equals(ls)) {
                // Assume locked timed out.
                lockCount = 0;
            } else if (LockType.READER.equals(wantedType) && locksMatch()) {
                // Allow shared reader.
                ls = LockState.SUCCESSFUL;
            }
        } else if (LockState.SUCCESSFUL.equals(ls)) {
            // non-running lock play it safe and reset the count.
            lockCount = 0;
        }

        if (LockState.SUCCESSFUL.equals(ls)) {
            type = wantedType;
            lockCount++;
            extraInfo = createExtraInfoString();
        }

        return ls;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler
     * #unlock(com.raytheon.uf.edex.database.cluster.ClusterTask, boolean)
     */
    @Override
    public void unlock(ClusterTask ct, boolean clearTime) {
        parseExtraInfoString(ct.getExtraInfo());

        if (LockType.WRITER.equals(type)) {
            lockCount = 0;
        } else {
            /*
             * For LockType.READER this assumes ClusterLockUtils.getLocks(name)
             * is used to find the cluster task in order to get latest extra
             * info; otherwise the count may be wrong.
             */
            --lockCount;
        }

        // Update to reflect count change for the lock.
        ct.setExtraInfo(createExtraInfoString());

        if (lockCount == 0) {
            ct.setRunning(false);
            if (clearTime) {
                ct.setLastExecution(0L);
            }
        }
    }
}
