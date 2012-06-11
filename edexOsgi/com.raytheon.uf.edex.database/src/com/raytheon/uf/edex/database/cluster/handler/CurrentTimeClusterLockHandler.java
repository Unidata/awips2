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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class CurrentTimeClusterLockHandler implements IClusterLockHandler {
    private static final transient IUFStatusHandler handler = UFStatus
            .getHandler(CurrentTimeClusterLockHandler.class);

    protected long checkTime;

    protected long timeOutOverride;

    protected String extraInfo;

    protected boolean saveExtraInfoOnLock = true;

    /**
     * Gets a lock based on current time. When lock is acquired the extraInfo
     * field will be set.
     * 
     * @param timeOutOverride
     * @param extraInfo
     */
    public CurrentTimeClusterLockHandler(long timeOutOverride, String extraInfo) {
        this.timeOutOverride = timeOutOverride;
        this.extraInfo = extraInfo;
    }

    /**
     * Gets a lock based on current time. If saveExtraInfoOnLock is true, the
     * extraInfo field will be set when lock is received, otherwise extraInfo
     * will be set on unlock.
     * 
     * @param timeOutOverride
     * @param saveExtraInfoOnLock
     */
    public CurrentTimeClusterLockHandler(long timeOutOverride,
            boolean saveExtraInfoOnLock) {
        this.timeOutOverride = timeOutOverride;
        this.saveExtraInfoOnLock = saveExtraInfoOnLock;
    }

    /**
     * Gets a lock based on current time. If saveExtraInfoOnLock is true, the
     * extraInfo field will be set when lock is received, otherwise extraInfo
     * will be set on unlock.
     * 
     * @param timeOutOverride
     * @param extraInfo
     * @param saveExtraInfoOnLock
     */
    public CurrentTimeClusterLockHandler(long timeOutOverride,
            String extraInfo, boolean saveExtraInfoOnLock) {
        this.timeOutOverride = timeOutOverride;
        this.extraInfo = extraInfo;
        this.saveExtraInfoOnLock = saveExtraInfoOnLock;
    }

    @Override
    public LockState handleLock(ClusterTask ct) {
        LockState ls = null;
        checkTime = System.currentTimeMillis();

        if (ct.isRunning()) {
            ls = LockState.ALREADY_RUNNING;
            // Override
            if (checkTime > ct.getLastExecution() + timeOutOverride) {
                if (handler.isPriorityEnabled(Priority.INFO)) {
                    handler.handle(
                            Priority.INFO,
                            "Overriding lock for cluster task ["
                                    + ct.getId().getName()
                                    + "/"
                                    + ct.getId().getDetails()
                                    + "] time out ["
                                    + timeOutOverride
                                    + "] exceeded by "
                                    + (checkTime - (ct.getLastExecution() + timeOutOverride))
                                    + " ms.");
                }
                ls = LockState.SUCCESSFUL;
            }
        } else {
            if (checkTime > ct.getLastExecution()) {
                ls = LockState.SUCCESSFUL;
            } else {
                ls = LockState.OLD;
            }
        }
        return ls;
    }

    @Override
    public boolean updateLock(ClusterTask ct) {
        ct.setLastExecution(checkTime);
        ct.setRunning(true);
        if (saveExtraInfoOnLock) {
            ct.setExtraInfo(extraInfo);
        }
        return true;
    }

    @Override
    public void unlock(ClusterTask ct, boolean clearTime) {
        ct.setRunning(false);
        if (!saveExtraInfoOnLock) {
            ct.setExtraInfo(extraInfo);
        }
        if (clearTime) {
            ct.setLastExecution(0);
        }
    }

    public long getCheckTime() {
        return checkTime;
    }

    public long getTimeOutOverride() {
        return timeOutOverride;
    }

    public void setTimeOutOverride(long timeOutOverride) {
        this.timeOutOverride = timeOutOverride;
    }

    public String getExtraInfo() {
        return extraInfo;
    }

    public void setExtraInfo(String extraInfo) {
        this.extraInfo = extraInfo;
    }
}
