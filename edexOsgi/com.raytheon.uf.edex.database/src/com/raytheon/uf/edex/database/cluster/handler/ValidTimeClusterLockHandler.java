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
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;

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

public class ValidTimeClusterLockHandler implements IClusterLockHandler {
    private static final transient IUFStatusHandler handler = UFStatus.getHandler(ValidTimeClusterLockHandler.class);
    protected long checkTime;

    protected long validTime;

    protected long timeOutOverride;

    public ValidTimeClusterLockHandler(long validTime, long timeOutOverride) {
        this.validTime = validTime;
        this.timeOutOverride = timeOutOverride;
    }

    @Override
    public LockState handleLock(ClusterTask ct) {
        LockState ls = null;
        checkTime = System.currentTimeMillis();

        if (ct.isRunning()) {
            if (checkTime > ct.getLastExecution() + timeOutOverride) {
                // override
                
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
            } else {
                ls = LockState.ALREADY_RUNNING;
            }
        } else {
            // not currently running
            // using extraInfo to track previous valid time
            long previousValidTime = 0;

            if (ct.getExtraInfo() != null) {
                try {
                    previousValidTime = Long.parseLong(ct.getExtraInfo());
                } catch (NumberFormatException e) {
                    handler.handle(
                            Priority.INFO,
                            "Invalid valid time in cluster lock definition, expected epoch millis in extraInfo, received ["
                                    + ct.getExtraInfo() + "]");
                }
            }

            if (validTime > previousValidTime) {
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
        ct.setExtraInfo("" + validTime);
        return true;
    }

    @Override
    public void unlock(ClusterTask ct, boolean clearTime) {
        ct.setRunning(false);
        if (clearTime) {
            ct.setLastExecution(0);
            ct.setExtraInfo("0");
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

    public long getValidTime() {
        return validTime;
    }

    public void setValidTime(long validTime) {
        this.validTime = validTime;
    }
}
