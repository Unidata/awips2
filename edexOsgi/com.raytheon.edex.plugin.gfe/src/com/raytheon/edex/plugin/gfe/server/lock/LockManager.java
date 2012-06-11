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

package com.raytheon.edex.plugin.gfe.server.lock;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfigManager;
import com.raytheon.edex.plugin.gfe.db.dao.GFELockDao;
import com.raytheon.edex.plugin.gfe.exception.GfeLockException;
import com.raytheon.edex.plugin.gfe.server.GridParmManager;
import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockTableRequest;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Manages lock operations on the IFP Server
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class LockManager {

    /** The logger */
    private Log logger = LogFactory.getLog(getClass());

    private LockComparator startTimeComparator = new LockComparator();

    /** The singleton instance of the LockManager */
    private static LockManager instance;

    /**
     * Gets the singleton instance of the LockManager
     * 
     * @return The singleton instance of the LockManager
     */
    public synchronized static LockManager getInstance() {
        if (instance == null) {
            instance = new LockManager();
        }
        return instance;
    }

    /**
     * Creates a new LockManager
     */
    private LockManager() {

    }

    /**
     * Returns a list of lock tables requests by LockTableRequest
     * 
     * @param request
     *            The list of lock table requests
     * @param wsId
     *            The workstation ID of the requestor
     * @return The list of lock tables
     * @throws GfeException
     *             If errors occur while querying the database
     */
    public ServerResponse<List<LockTable>> getLockTables(
            List<LockTableRequest> request, WsId requestor, String siteID) {
        ServerResponse<List<LockTable>> sr = new ServerResponse<List<LockTable>>();

        if (request.size() == 0) {
            sr.addMessage("No Lock Table Requests");
            return sr;
        }

        // extract the ParmIds from the request list
        List<ParmID> parmIds = new ArrayList<ParmID>();
        sr.addMessages(extractParmIds(request, parmIds, siteID));

        try {
            sr.setPayload(new ArrayList<LockTable>(new GFELockDao().getLocks(
                    parmIds, requestor).values()));
        } catch (DataAccessLayerException e) {
            sr.addMessage("Error getting lock tables for " + parmIds);
            sr.setPayload(new ArrayList<LockTable>());
        }

        return sr;
    }

    /**
     * Gets the lock tables based on the LockTableRequests
     * 
     * @param request
     *            The lock table request
     * @param wsId
     *            The workstation ID of the requestor
     * @return The lock table specified in the LockTableRequest
     * @throws GfeException
     *             If errors occur while retrieving locks
     */
    public ServerResponse<List<LockTable>> getLockTables(
            LockTableRequest request, WsId wsId, String siteID) {
        List<LockTableRequest> requests = new ArrayList<LockTableRequest>();
        requests.add(request);
        return getLockTables(requests, wsId, siteID);
    }

    public ServerResponse<List<LockTable>> requestLockChange(
            LockRequest request, WsId requestor, String siteID)
            throws GfeLockException {
        return requestLockChange(request, requestor, siteID, true);
    }

    /**
     * Makes a change to a lock in the database.
     * 
     * @param request
     *            The lock request
     * @param requestor
     *            The workstationID of the requestor
     * @throws GfeException
     *             If errors occur during database interaction
     */
    public ServerResponse<List<LockTable>> requestLockChange(
            LockRequest request, WsId requestor, String siteID,
            boolean combineLocks) throws GfeLockException {
        List<LockRequest> requests = new ArrayList<LockRequest>();
        requests.add(request);
        return requestLockChange(requests, requestor, siteID, combineLocks);
    }

    public ServerResponse<List<LockTable>> requestLockChange(
            List<LockRequest> requests, WsId requestor, String siteID) {
        return requestLockChange(requests, requestor, siteID, true);
    }

    /**
     * Makes a change to a lock in the database.
     * 
     * @param requests
     *            The lock requests
     * @param requestor
     *            The workstationID of the requestor
     * @throws GfeException
     *             If errors occur during database interaction
     */
    public ServerResponse<List<LockTable>> requestLockChange(
            List<LockRequest> requests, WsId requestor, String siteID,
            boolean combineLocks) {

        List<LockTable> lockTablesAffected = new ArrayList<LockTable>();
        List<GridUpdateNotification> gridUpdatesAffected = new ArrayList<GridUpdateNotification>();
        ServerResponse<List<LockTable>> sr = new ServerResponse<List<LockTable>>();
        sr.setPayload(lockTablesAffected);

        // check for official database locks (which are not allowed)
        sr.addMessages(officialDbLockCheck(requests, siteID));

        if (!sr.isOkay()) {
            return sr;
        }

        // expand the request as appropriate to the time boundary requirements
        // and convert to all parm-type requests
        List<LockRequest> req = new ArrayList<LockRequest>();

        sr.addMessages(adjustLockToTimeBoundaries(requests, req));
        if (!sr.isOkay()) {
            sr.addMessage("Request Lock change failed");
            return sr;
        }

        // extract the ParmIds from the requests
        List<ParmID> parmIds = new ArrayList<ParmID>();
        sr.addMessages(extractParmIdsFromLockReq(req, parmIds, siteID));

        // get the lock tables specific to the extracted parmIds
        Map<ParmID, LockTable> lockTableMap;
        try {
            lockTableMap = new GFELockDao().getLocks(parmIds, requestor);
        } catch (DataAccessLayerException e) {
            sr.addMessage("Error getting lock tables for " + parmIds);
            return sr;
        }

        // process each modified lock request, these are all parm-type requests
        ParmID currentParmId = null;
        for (LockRequest currentRequest : req) {
            currentParmId = currentRequest.getParmId();
            // get table from sequence
            LockTable lt = lockTableMap.get(currentParmId);
            LockTable prevLT = lt.clone();

            try {
                // Change Lock
                if (!changeLock(lt, currentRequest.getTimeRange(), requestor,
                        currentRequest.getMode(), combineLocks)) {
                    sr.addMessage("Requested change lock failed - Lock is owned by another user - "
                            + currentRequest + " LockTable=" + lt);
                    lockTablesAffected.clear();
                    gridUpdatesAffected.clear();
                    return sr;

                }
            } catch (Exception e) {
                sr.addMessage("Requested change lock failed - Exception thrown - "
                        + currentRequest
                        + " LockTable="
                        + lt
                        + " Exception: "
                        + e.getLocalizedMessage());
                lockTablesAffected.clear();
                gridUpdatesAffected.clear();
                return sr;
            }

            // the change lock worked, but resulted in the same lock situation
            if (prevLT.equals(lt)) {
                continue;
            }

            // add the lock table to the lockTablesAffected" if it already
            // doesn't exist -- if it does exist, then replace it but don't add
            // it if it really didn't change

            LockTable tableToRemove = null;
            for (int j = 0; j < lockTablesAffected.size(); j++) {
                if (lockTablesAffected.get(j).getParmId().equals(currentParmId)) {
                    tableToRemove = lockTablesAffected.get(j);
                    break;
                }
            }

            if (tableToRemove != null) {
                lockTablesAffected.remove(tableToRemove);
            }
            lockTablesAffected.add(lt);

            // assemble a grid update notification since the lock table has
            // changed - IF this is BREAK LOCK request
            if (currentRequest.getMode().equals(LockTable.LockMode.BREAK_LOCK)) {
                List<TimeRange> trs = new ArrayList<TimeRange>();
                ServerResponse<List<TimeRange>> ssr = GridParmManager
                        .getGridInventory(currentParmId);
                sr.addMessages(ssr);
                trs = ssr.getPayload();
                if (!sr.isOkay()) {
                    lockTablesAffected.clear();
                    gridUpdatesAffected.clear();
                    return sr;
                }
                List<TimeRange> updatedGridsTR = new ArrayList<TimeRange>();
                for (int p = 0; p < trs.size(); p++) {
                    if (trs.get(p).overlaps(currentRequest.getTimeRange())) {
                        updatedGridsTR.add(trs.get(p));
                    }
                }

                ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr1 = GridParmManager
                        .getGridHistory(currentParmId, updatedGridsTR);
                Map<TimeRange, List<GridDataHistory>> histories = null;
                if (sr1.isOkay()) {
                    histories = sr1.getPayload();
                }

                gridUpdatesAffected.add(new GridUpdateNotification(
                        currentParmId, currentRequest.getTimeRange(),
                        histories, requestor, siteID));

            }
        }
        // if we made it here, then all lock requests were successful
        for (int k = 0; k < lockTablesAffected.size(); k++) {
            lockTablesAffected.get(k).resetWsId(requestor);
        }

        for (GridUpdateNotification notify : gridUpdatesAffected) {
            sr.addNotifications(notify);
        }
        return sr;
    }

    /**
     * Changes the lock over the specified time range to the given requestorId
     * 
     * @param timeRange
     *            The time range to change the lock for
     * @param requestorId
     *            The requestor ID to change the lock for
     * @param lockMode
     *            The lockMode to change the lock to
     * @return True if successful, else false
     * @throws GfeLockException
     *             If the lock could not be changed
     */
    private boolean changeLock(LockTable lt, TimeRange timeRange,
            WsId requestorId, LockMode lockMode, boolean combineLocks)
            throws GfeLockException {

        LockTable.LockStatus ls = lt.checkLock(timeRange, requestorId);

        if (lockMode.equals(LockTable.LockMode.LOCK)) {
            if (ls.equals(LockTable.LockStatus.LOCKED_BY_OTHER)) {
                return false;
            } else if (ls.equals(LockTable.LockStatus.LOCKED_BY_ME)) {
                return true;
            } else if (ls.equals(LockTable.LockStatus.LOCKABLE)) {
                GFELockDao dao = new GFELockDao();
                List<Lock> existingLocks = new ArrayList<Lock>();
                try {
                    existingLocks = dao.getLocksInRange(lt.getParmId(),
                            timeRange);
                    if (existingLocks != null && !existingLocks.isEmpty()) {
                        for (Lock lock : existingLocks) {
                            dao.delete(lock);
                            lt.removeLock(lock);
                        }
                    }
                } catch (DataAccessLayerException e) {
                    logger.error("Error changing locks", e);
                }

                Lock newLock = new Lock(timeRange, requestorId);
                newLock.setParmId(lt.getParmId());
                dao.persist(newLock);
                try {
                    newLock = dao.getLock(newLock.getParmId(),
                            newLock.getTimeRange(), newLock.getWsId());
                } catch (DataAccessLayerException e) {
                    throw new GfeLockException("Unable to update new lock", e);
                }
                lt.addLock(newLock);
            }

            if (combineLocks) {
                combineLocks(lt);
            }
        }

        else if (lockMode.equals(LockTable.LockMode.UNLOCK)) {
            if (ls.equals(LockTable.LockStatus.LOCKED_BY_ME)) {
                try {
                    GFELockDao dao = new GFELockDao();
                    Lock newLock = dao.getLock(lt.getParmId(), timeRange,
                            requestorId);
                    if (newLock != null) {
                        dao.delete(newLock);
                        lt.removeLock(newLock);
                    }
                } catch (DataAccessLayerException e) {
                    throw new GfeLockException(
                            "Unable to retrieve lock information for: "
                                    + lt.getParmId() + " TimeRange: "
                                    + timeRange + " WorkstationID: "
                                    + requestorId);
                }

            } else if (ls.equals(LockTable.LockStatus.LOCKED_BY_OTHER)) {
                logger.warn("Lock for time range: " + timeRange
                        + " already owned");
            } else {
                // Record already unlocked
            }
        }

        else if (lockMode.equals(LockTable.LockMode.BREAK_LOCK)) {
            try {
                GFELockDao dao = new GFELockDao();
                Lock newLock = dao.getLock(lt.getParmId(), timeRange,
                        requestorId);
                if (newLock != null) {
                    dao.delete(newLock);
                    lt.removeLock(newLock);
                }
            } catch (DataAccessLayerException e) {
                throw new GfeLockException(
                        "Unable to retrieve lock information for: "
                                + lt.getParmId() + " TimeRange: " + timeRange
                                + " WorkstationID: " + requestorId);
            }
        }
        return true;
    }

    /**
     * Examines the locks contained in a given lock table and combines locks if
     * possible.
     * 
     * @param lt
     *            The lock table to examine
     * @throws GfeLockException
     *             If errors occur when updating the locks in the database
     */
    private void combineLocks(final LockTable lt) throws GfeLockException {
        Set<Lock> added = new HashSet<Lock>();
        Set<Lock> deleted = new HashSet<Lock>();
        List<Lock> locks = null;
        Lock currentLock = null;
        Lock nextLock = null;
        boolean lockCombined = true;
        while (lockCombined) {
            lockCombined = false;
            lt.addLocks(added);
            lt.removeLocks(deleted);
            Collections.sort(lt.getLocks(), startTimeComparator);
            locks = lt.getLocks();
            for (int i = 0; i < locks.size() - 1; i++) {
                currentLock = locks.get(i);
                nextLock = locks.get(i + 1);
                if (currentLock.getEndTime() >= nextLock.getStartTime() && currentLock.getWsId().equals(nextLock.getWsId())) {
                    lockCombined = true;
                    deleted.add(currentLock);
                    deleted.add(nextLock);
                    Lock newLock = new Lock(new TimeRange(
                            currentLock.getStartTime(), nextLock.getEndTime()),
                            lt.getWsId());
                    newLock.setParmId(lt.getParmId());
                    added.add(newLock);
                    break;
                }
            }
        }
        try {
            new GFELockDao().updateCombinedLocks(deleted, added);
        } catch (DataAccessLayerException e) {
            throw new GfeLockException("Error combining locks", e);
        }
    }

    /**
     * Notification that one or more databases were deleted. No lock change
     * notifications are generated since all clients will already know that the
     * databases have been removed.
     * 
     * Asks the LockDatabase for the list of LockTables. Eliminate entries in
     * LockTables that no longer should exist based on the sequence of database
     * identifiers that were deleted. Restore the LockTables by passing them to
     * the LockDatabase.
     * 
     * @param deletions
     */
    public void databaseDeleted(List<DatabaseID> deletions) {
        // TODO: Implement database deletion
    }

    /**
     * Routine to modify the requested lock time to the time constaint
     * boundaries. A sequence of lock requests are input and a modified sequence
     * of lock requests returned. The number of returned lock requests may not
     * be equal to the number of input lock requests. Converts all lock requests
     * into parm Id type lock requests.
     * 
     * Processes each LockRequest sequentially. Converts each request into a
     * parm-type request (database-type requests are expanded into multiple
     * parm-type requests). Then expands each request to a lock boundary if
     * necessary.
     * 
     * @param requestsIn
     *            The input lock requests
     * @param requestsOut
     *            the output lock requests
     * @return The adjusted lock requests
     */
    private ServerResponse<?> adjustLockToTimeBoundaries(
            List<LockRequest> requestsIn, List<LockRequest> requestsOut) {
        ServerResponse<?> sr = new ServerResponse<String>();

        // Convert into parm requests
        for (LockRequest req : requestsIn) {
            List<LockRequest> tempRequests = new ArrayList<LockRequest>();
            sr.addMessages(translateIntoParms(req, tempRequests));
            requestsOut.addAll(tempRequests);
            if (!sr.isOkay()) {
                return sr;
            }
        }

        for (LockRequest req : requestsOut) {
            List<LockRequest> reqList = new ArrayList<LockRequest>();
            reqList.add(req);
            sr.addMessages(expandRequestToBoundary(reqList));
            req = reqList.get(0);
            if (!sr.isOkay()) {
                return sr;
            }
        }

        return sr;
    }

    /**
     * Utility routine to translate the given lock request into a series of
     * LockRequests. This is used to convert a database request type lock
     * request into multiple parameter-type lock requests.
     * 
     * Validates that we have a LockDatabase, then gets the tables.
     * 
     * @param req
     *            The lock request
     * @param parmReq
     *            The parm requests out
     * @return
     */
    private ServerResponse<?> translateIntoParms(LockRequest req,
            List<LockRequest> parmReq) {
        ServerResponse<?> sr = new ServerResponse<String>();

        // if simple parm-type request, simply return the same request
        if (req.isParmRequest()) {
            parmReq.add(req);
            return sr;
        }

        if (!req.isDatabaseRequest()) {
            sr.addMessage("Invalid LockRequest( not parm or database type): "
                    + req);
            return sr;
        }

        DatabaseID dbid = req.getDbId();

        List<ParmID> parmList = null;

        if (dbid.getFormat().equals(DatabaseID.DataType.GRID)) {
            parmList = GridParmManager.getParmList(dbid).getPayload();
        } else {
            sr.addMessage("Invalid LockRequest (not GRID type): " + req);
            return sr;
        }

        // make list of lock requests from the list of parms and original
        // request
        for (ParmID parm : parmList) {
            parmReq.add(new LockRequest(parm, req.getTimeRange(), req.getMode()));
        }

        return sr;
    }

    /**
     * Expands the request to the time constraint or projection time boundary.
     * It assumes that the lock request is a parm-id type request. --
     * implementation ---------------------------------------------------------
     * Determines whether it is a GRID request based on the DatabaseID. Retreive
     * the TimeRange entries.
     * 
     * For GRID types, get the GridParmInfo for that parameter from the
     * GridParmMgr. Use the TimeContraints class to expand the time request to
     * the time contraints.
     * 
     * @param req
     *            The lock requst
     * @return
     */
    private ServerResponse<?> expandRequestToBoundary(List<LockRequest> req) {

        ServerResponse<?> sr = new ServerResponse<String>();
        if (!req.get(0).isParmRequest()) {
            logger.error("Expected parm-type request in expandRequestToBoundary");
        }

        // If this is a break-lock request, then do not expand to time constrts
        if (req.get(0).getMode().equals(LockTable.LockMode.BREAK_LOCK)) {
            return sr;
        }

        // Only expand for request lock and unlock
        DatabaseID dbid = req.get(0).getParmId().getDbId();

        TimeRange tr = null;

        switch (dbid.getFormat()) {
        case GRID:
            ServerResponse<GridParmInfo> ssr = GridParmManager
                    .getGridParmInfo(req.get(0).getParmId());
            GridParmInfo gpi = ssr.getPayload();
            sr.addMessages(ssr);
            if (!sr.isOkay()) {
                sr.addMessage("Unable to expand lock request to boundary since unable to get GridParmInfo for parameter");
                return sr;
            }
            // calculate the expanded time
            tr = gpi.getTimeConstraints().expandTRToQuantum(
                    req.get(0).getTimeRange());
            if (!tr.isValid()) {
                sr.addMessage("Request does not match TimeConstraints "
                        + gpi.getTimeConstraints() + " ParmReq: " + req);
            }
            break;
        default:
            sr.addMessage("Invalid LockRequest (not GRID type): " + req);
            return sr;
        }

        // Update the lock request
        if (tr.isValid()) {
            req.add(new LockRequest(req.get(0).getParmId(), tr, req.get(0)
                    .getMode()));
            req.remove(0);
        } else {
            sr.addMessage("TimeRange not valid in LockRequest");
        }

        return sr;
    }

    /**
     * * Overloaded utility routine to extract the ParmIds for a given
     * SeqOf<LockTableRequest>. If the request is a parm request, the ParmId
     * will be appended to the list ParmIds to be returned. If the request is a
     * database request, all the ParmIds for the given databaseId wil be
     * appended to the list of ParmIds. If the request is neither a parm nor a
     * database request, all the parmId for all the databaseIds are appended to
     * the list of ParmIds to be returned.
     * 
     * @param ltr
     *            The input LockTableRequests
     * @return The ParmIDs contained in the requests
     * @throws GfeException
     */
    private ServerResponse<?> extractParmIds(List<LockTableRequest> ltr,
            List<ParmID> parmIds, String siteID) {

        ServerResponse<?> sr = new ServerResponse<String>();
        // process each request
        for (int i = 0; i < ltr.size(); i++) {
            if (ltr.get(i).isParmRequest()) {
                // append parm (if not already in the list)
                if (!parmIds.contains(ltr.get(i).getParmId())) {
                    parmIds.add(ltr.get(i).getParmId());
                }
            } else if (ltr.get(i).isDatabaseRequest()) {
                // get all the parmIds for that databaseId

                List<ParmID> pids = GridParmManager.getParmList(
                        ltr.get(i).getDbId()).getPayload();
                for (ParmID id : pids) {
                    if (!parmIds.contains(id)) {
                        parmIds.add(id);
                    }
                }
            } else {
                // get all the parms for all the databases
                List<DatabaseID> dbids = GridParmManager.getDbInventory(siteID)
                        .getPayload();
                for (int j = 0; j < dbids.size(); j++) {
                    List<ParmID> pids = GridParmManager.getParmList(
                            dbids.get(j)).getPayload();
                    for (ParmID id : pids) {
                        if (!parmIds.contains(id)) {
                            parmIds.add(id);
                        }
                    }
                }
                // this only needs to be done once
            }
        }
        return sr;
    }

    /**
     * Overloaded utility routine to extract the ParmIds for a given
     * SeqOf<LockRequest>.
     * 
     * If the request is a parm request, the ParmId will be appended to the list
     * ParmIds to be returned. If the request is a database request, all the
     * ParmIds for the given databaseId wil be appended to the list of ParmIds.
     * If the request is neither a parm nor a database request, all the parmId
     * for all the databaseIds are appended to the list of ParmIds to be
     * returned.
     * 
     * @param lr
     *            The input LockRequests
     * @return The list of parmIDs contained in the lock requests
     * @throws GfeException
     *             If errors occur
     */
    private ServerResponse<?> extractParmIdsFromLockReq(List<LockRequest> lr,
            List<ParmID> parmIds, String siteID) {
        ServerResponse<?> sr = new ServerResponse<String>();

        // process each request
        for (int i = 0; i < lr.size(); i++) {
            if (lr.get(i).isParmRequest()) {
                // append parm (if not already in the list)
                if (!parmIds.contains(lr.get(i).getParmId())) {
                    parmIds.add(lr.get(i).getParmId());
                }
            } else if (lr.get(i).isDatabaseRequest()) {
                List<ParmID> pids = new ArrayList<ParmID>();
                ServerResponse<List<ParmID>> ssr = GridParmManager
                        .getParmList(lr.get(i).getDbId());
                sr.addMessages(ssr);
                pids = ssr.getPayload();
                if (!sr.isOkay()) {
                    return sr;
                }
                for (ParmID id : pids) {
                    if (!parmIds.contains(id)) {
                        parmIds.add(id);
                    }
                }
            } else {
                // get all the parms for all the databases
                List<DatabaseID> dbids = new ArrayList<DatabaseID>();
                ServerResponse<List<DatabaseID>> ssr = GridParmManager
                        .getDbInventory(siteID);
                dbids = ssr.getPayload();
                sr.addMessages(ssr);
                if (!sr.isOkay()) {
                    return sr;
                }
                for (int j = 0; j < dbids.size(); j++) {
                    List<ParmID> pids = new ArrayList<ParmID>();
                    ServerResponse<List<ParmID>> ssr1 = GridParmManager
                            .getParmList(dbids.get(j));
                    sr.addMessages(ssr1);
                    pids = ssr1.getPayload();
                    if (!sr.isOkay()) {
                        return sr;
                    }
                    for (ParmID id : pids) {
                        if (!parmIds.contains(id)) {
                            parmIds.add(id);
                        }
                    }
                }
                // this only needs to be done once
            }
        }

        return sr;
    }

    /**
     * Checks for lock requests containing official database locks. The requests
     * may be a mix of parm and database type
     * 
     * @param req
     *            The lock requests
     * @return The server status
     */
    private ServerResponse<?> officialDbLockCheck(final List<LockRequest> req,
            String siteID) {

        ServerResponse<?> sr = new ServerResponse<String>();

        List<DatabaseID> official = new ArrayList<DatabaseID>();
        List<DatabaseID> officialDbs = null;
        try {
            officialDbs = IFPServerConfigManager.getServerConfig(siteID)
                    .getOfficialDatabases();

            for (int j = 0; j < officialDbs.size(); j++) {
                official.add(officialDbs.get(j).stripModelTime());
            }
        } catch (GfeException e) {
            sr.addMessage("Unable to get official databases from IFPServer config");
            logger.error(
                    "Unable to get official database from IFPServer config", e);
            return sr;
        }

        // process each request - extracting out the databse id w/o modeltime
        for (int i = 0; i < req.size(); i++) {
            DatabaseID requestDB = null;
            if (req.get(i).isDatabaseRequest()) {
                requestDB = req.get(i).getDbId();
            } else if (req.get(i).isParmRequest()) {
                requestDB = req.get(i).getParmId().getDbId();
            } else {
                sr.addMessage("Invalid Lock Request (not parm or databse type): "
                        + req.get(i));
                return sr;
            }
            requestDB = requestDB.stripModelTime();

            // look for a match
            if (official.contains(requestDB)) {
                sr.addMessage("Request locks on official database not permitted.  Request="
                        + req.get(i));
                return sr;
            }

        }
        return sr;
    }

    private class LockComparator implements Comparator<Lock> {
        @Override
        public int compare(Lock o1, Lock o2) {
            if (o1.getStartTime() < o2.getStartTime()) {
                return -1;
            }
            if (o1.getStartTime() > o2.getStartTime()) {
                return 1;
            }
            return 0;
        }
    }
}
