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
package com.raytheon.uf.edex.database.handlers;

import java.util.List;

import com.raytheon.uf.common.dataquery.requests.SharedLockRequest;
import com.raytheon.uf.common.dataquery.requests.SharedLockRequest.RequestType;
import com.raytheon.uf.common.dataquery.responses.SharedLockResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.SharedLockHandler;
import com.raytheon.uf.edex.database.cluster.handler.SharedLockHandler.LockType;

/**
 * This is the handler class for a shared lock request. It coordinates with the
 * shared lock handler to perform the desired request.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 2, 2014  2862       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class SharedLockRequestHandler implements
        IRequestHandler<SharedLockRequest> {

    private final IUFStatusHandler statusHander = UFStatus
            .getHandler(SharedLockRequestHandler.class);

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public SharedLockResponse handleRequest(SharedLockRequest request)
            throws Exception {

        SharedLockResponse response = new SharedLockResponse();
        String name = request.getName();
        String details = request.getDetails();
        RequestType type = request.getRequestType();
        response.setSucessful(false);

        try {
            switch (type) {
            case READER_LOCK:
                if (lock(name, details, LockType.READER)) {
                    response.setSucessful(true);
                } else {
                    response.setSucessful(false);
                    response.setErrorMessage(String.format(
                            "Unable to obtain %s lock.", LockType.READER));
                }
                break;
            case READER_UNLOCK:
                if (unlock(name, details, LockType.READER)) {
                    response.setSucessful(true);
                } else {
                    response.setSucessful(false);
                    response.setErrorMessage(String.format(
                            "Unable to unlock %s.", LockType.READER));
                }
                break;
            case READER_UPDATE_TIME:
                if (updateTime(name, details, LockType.READER)) {
                    response.setSucessful(true);
                } else {
                    response.setSucessful(false);
                    response.setErrorMessage(String.format(
                            "Unable to update %s last exection time.",
                            LockType.READER));
                }
                break;
            case WRITER_LOCK:
                if (lock(name, details, LockType.WRITER)) {
                    response.setSucessful(true);
                } else {
                    response.setSucessful(false);
                    response.setErrorMessage(String.format(
                            "Unable to obtain %s lock.", LockType.WRITER));
                }
                break;
            case WRITER_UNLOCK:
                if (unlock(name, details, LockType.WRITER)) {
                    response.setSucessful(true);
                } else {
                    response.setSucessful(false);
                    response.setErrorMessage(String.format(
                            "Unable to unlock %s.", LockType.WRITER));
                }
                break;
            case WRITER_UPDATE_TIME:
                if (updateTime(name, details, LockType.WRITER)) {
                    response.setSucessful(false);
                    response.setErrorMessage(String.format(
                            "Unable to update %s last execution time.",
                            LockType.WRITER));
                }
                break;
            default:
                String message = "Unimplemented request type: " + type;
                statusHander.error(message);
                response.setErrorMessage(message);
                response.setSucessful(false);
            }
        } catch (Exception ex) {
            response.setSucessful(false);
            String message = String.format(
                    "Request type %s for details %s failed %s", type, details,
                    ex.getMessage());
            response.setErrorMessage(message);
            if (statusHander.isPriorityEnabled(Priority.PROBLEM)) {
                statusHander.handle(Priority.PROBLEM, message, ex);
            }
        }
        return response;
    }

    /**
     * Request details lock of the desired lock type.
     * 
     * @param details
     * @param lockType
     * @return true when obtaining lock is successful
     */
    private boolean lock(String name, String details, LockType lockType) {
        SharedLockHandler lockHandler = new SharedLockHandler(lockType);
        ClusterTask ct = ClusterLockUtils.lock(name, details, lockHandler,
                false);
        return LockState.SUCCESSFUL.equals(ct.getLockState());
    }

    /**
     * Release lock for given details. The unlock request is only attempted when
     * the details' lock type matches and the count is a positive number.
     * 
     * @param details
     * @param lockType
     * @return true when successful
     */
    private boolean unlock(String name, String details, LockType lockType) {
        ClusterTask ct = findCluster(name, details, lockType);
        if (ct != null) {
            SharedLockHandler handler = (SharedLockHandler) ct.getLockHandler();
            if (handler.getLockCount() > 0) {
                return ClusterLockUtils.unlock(ct, false);
            }
        }
        return false;
    }

    /**
     * Find the details' cluster task with latest extrainfo and set up its
     * handler. The found cluster task is only returned if its lock type matches
     * the requested type and it is in the run state.
     * 
     * @param details
     *            - Whose cluster task to find
     * @param lockType
     *            - Expected lock type for the cluster
     * @return ct when found, matches lockType, and is running else null.
     */
    private ClusterTask findCluster(String name, String details,
            LockType lockType) {
        ClusterTask ct = null;
        List<ClusterTask> cts = ClusterLockUtils.getLocks(name);

        if ((cts != null) && (cts.size() > 0)) {
            for (ClusterTask tmpCt : cts) {
                if (details.equals(tmpCt.getId().getDetails())) {
                    if (tmpCt.isRunning()) {
                        SharedLockHandler handler = new SharedLockHandler(
                                lockType);
                        handler.parseExtraInfoString(tmpCt.getExtraInfo());
                        if (handler.locksMatch()
                                && (handler.getLockCount() > 0)) {
                            ct = tmpCt;
                            ct.setLockHandler(handler);
                        }
                    }
                    break;
                }
            }
        }
        return ct;
    }

    /**
     * Update the last execution time for the detail's lock.
     * 
     * @param details
     * @param lockType
     * @return true when update successful
     */
    private boolean updateTime(String name, String details, LockType lockType) {
        ClusterTask ct = findCluster(name, details, lockType);
        if (ct != null) {
            if (ClusterLockUtils.updateLockTime(ct.getId().getName(), ct
                    .getId().getDetails(), System.currentTimeMillis())) {
                return true;
            }
        }
        return false;
    }
}
