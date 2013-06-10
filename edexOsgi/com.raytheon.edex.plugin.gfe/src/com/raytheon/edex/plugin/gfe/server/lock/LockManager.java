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
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
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
 * 04/23/13     #1949      rjpeter     Updated to work with Normalized Database,
 *                                     fixed inefficiencies in querying/merging
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class LockManager {

    /** The logger */
    private final Log logger = LogFactory.getLog(getClass());

    private final LockComparator startTimeComparator = new LockComparator();

    private final GFELockDao dao = new GFELockDao();

    /** The singleton instance of the LockManager */
    private static LockManager instance = new LockManager();

    /**
     * Gets the singleton instance of the LockManager
     * 
     * @return The singleton instance of the LockManager
     */
    public static LockManager getInstance() {
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
        Set<ParmID> parmIds = new HashSet<ParmID>();
        try {
            sr.addMessages(extractParmIds(request, parmIds, siteID));
            List<ParmID> nonIfpParmIds = new LinkedList<ParmID>();

            // remove parm IDs that are not persisted to database
            Iterator<ParmID> iter = parmIds.iterator();
            while (iter.hasNext()) {
                ParmID id = iter.next();
                if (id.getId() == 0) {
                    nonIfpParmIds.add(id);
                    iter.remove();
                }
            }

            List<LockTable> payLoad = null;

            if (!parmIds.isEmpty()) {
                Map<ParmID, LockTable> lockMap = dao.getLocks(parmIds,
                        requestor);
                payLoad = new ArrayList<LockTable>(lockMap.size()
                        + nonIfpParmIds.size());
                payLoad.addAll(lockMap.values());
            } else {
                payLoad = new ArrayList<LockTable>(nonIfpParmIds.size());
            }

            if (!nonIfpParmIds.isEmpty()) {
                for (ParmID id : nonIfpParmIds) {
                    payLoad.add(new LockTable(id, new ArrayList<Lock>(0),
                            requestor));
                }
            }

            sr.setPayload(payLoad);
        } catch (Exception e) {
            logger.error("Error getting lock tables for " + parmIds, e);
            sr.addMessage("Error getting lock tables for " + parmIds);
            sr.setPayload(new ArrayList<LockTable>(0));
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
        return getLockTables(Arrays.asList(request), wsId, siteID);
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
        return requestLockChange(Arrays.asList(request), requestor, siteID,
                combineLocks);
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

        List<LockTable> lockTablesAffected = new LinkedList<LockTable>();
        List<GridUpdateNotification> gridUpdatesAffected = new LinkedList<GridUpdateNotification>();
        ServerResponse<List<LockTable>> sr = new ServerResponse<List<LockTable>>();
        sr.setPayload(lockTablesAffected);

        // check for official database locks (which are not allowed)
        sr.addMessages(officialDbLockCheck(requests, siteID));

        if (!sr.isOkay()) {
            return sr;
        }

        // expand the request as appropriate to the time boundary requirements
        // and convert to all parm-type requests
        List<LockRequest> req = new ArrayList<LockRequest>(requests.size());

        sr.addMessages(adjustLockToTimeBoundaries(requests, req));
        if (!sr.isOkay()) {
            sr.addMessage("Request Lock change failed");
            return sr;
        }

        Set<ParmID> parmIds = new HashSet<ParmID>();
        Map<ParmID, LockTable> lockTableMap;
        try {
            // extract the ParmIds from the requests
            sr.addMessages(extractParmIdsFromLockReq(req, parmIds, siteID));

            Iterator<ParmID> iter = parmIds.iterator();
            while (iter.hasNext()) {
                ParmID id = iter.next();
                // non persisted parm IDs cannot be locked
                if (id.getId() == 0) {
                    sr.addMessage("ParmID " + id + " is not a lockable parm");
                    iter.remove();
                }
            }

            // get the lock tables specific to the extracted parmIds
            lockTableMap = dao.getLocks(parmIds, requestor);
        } catch (Exception e) {
            logger.error("Error getting lock tables for " + parmIds, e);
            sr.addMessage("Error getting lock tables for " + parmIds);
            return sr;
        }

        // process each modified lock request, these are all parm-type requests
        ParmID currentParmId = null;
        TimeRange currentTimeRange = null;
        for (LockRequest currentRequest : req) {
            currentParmId = currentRequest.getParmId();
            currentTimeRange = currentRequest.getTimeRange();

            // get table from sequence
            LockTable lt = lockTableMap.get(currentParmId);
            LockTable prevLT = lt.clone();

            try {
                // Change Lock
                if (!changeLock(lt, currentTimeRange, requestor,
                        currentRequest.getMode(), combineLocks)) {
                    sr.addMessage("Requested change lock failed - Lock is owned by another user - "
                            + currentRequest + " LockTable=" + lt);
                    continue;
                }
            } catch (Exception e) {
                logger.error("Error changing lock", e);
                sr.addMessage("Requested change lock failed - Exception thrown - "
                        + currentRequest
                        + " LockTable="
                        + lt
                        + " Exception: "
                        + e.getLocalizedMessage());
                continue;
            }

            // the change lock worked, but resulted in the same lock situation
            if (prevLT.equals(lt)) {
                // TODO: Equals not implemented, this is dead code due to clone
                continue;
            }

            // add the lock table to the lockTablesAffected if it already
            // doesn't exist

            boolean addTable = true;
            for (LockTable ltAffected : lockTablesAffected) {
                if (ltAffected.getParmId().equals(currentParmId)) {
                    addTable = false;
                    break;
                }
            }

            if (addTable) {
                lockTablesAffected.add(lt);
            }

            if (currentRequest.getMode().equals(LockTable.LockMode.BREAK_LOCK)) {
                // TODO: Should be able to do in a single look up that retrieves
                // the histories that intersect the time ranges instead of the
                // current two stage query
                List<TimeRange> trs = new ArrayList<TimeRange>();
                ServerResponse<List<TimeRange>> ssr = GridParmManager
                        .getGridInventory(currentParmId, currentTimeRange);
                sr.addMessages(ssr);
                trs = ssr.getPayload();
                if (!sr.isOkay()) {
                    // unable to get payload, can't reverse the break lock, add
                    // a new message to our current response and keep going
                    sr.addMessages(ssr);
                    continue;
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

        // update the lockTables that were successful
        for (LockTable lt : lockTablesAffected) {
            lt.resetWsId(requestor);
        }

        // process the break lock notifications that were successful
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
                try {
                    Lock newLock = new Lock(lt.getParmId(), timeRange,
                            requestorId);
                    replaceLocks(lt, newLock, combineLocks);
                } catch (DataAccessLayerException e) {
                    logger.error("Error adding lock", e);
                    throw new GfeLockException("Unable add new lock", e);
                }
            }
        } else if (lockMode.equals(LockTable.LockMode.UNLOCK)) {
            if (ls.equals(LockTable.LockStatus.LOCKED_BY_ME)) {
                try {
                    deleteLocks(lt, timeRange);
                } catch (DataAccessLayerException e) {
                    throw new GfeLockException("Unable to delete locks for: "
                            + lt.getParmId() + " TimeRange: " + timeRange
                            + " WorkstationID: " + requestorId);
                }
            } else if (ls.equals(LockTable.LockStatus.LOCKED_BY_OTHER)) {
                logger.warn("Lock for time range: " + timeRange
                        + " already owned");
            } else {
                // Record already unlocked
            }
        } else if (lockMode.equals(LockTable.LockMode.BREAK_LOCK)) {
            try {
                deleteLocks(lt, timeRange);
            } catch (DataAccessLayerException e) {
                throw new GfeLockException("Unable to delete locks for: "
                        + lt.getParmId() + " TimeRange: " + timeRange
                        + " WorkstationID: " + requestorId);
            }
        }

        return true;
    }

    /**
     * Replaces locks in the given time range with the passed lock.
     * 
     * @param lt
     *            The lock table to examine
     * @param newLock
     *            The lock to add
     * @throws GfeLockException
     *             If errors occur when updating the locks in the database
     */
    private void replaceLocks(final LockTable lt, final Lock newLock,
            boolean combineLocks) throws DataAccessLayerException {
        // update the locks in memory
        Set<Lock> added = new HashSet<Lock>();
        List<Integer> removed = new ArrayList<Integer>();

        List<Lock> locks = lt.getLocks();
        Collections.sort(locks, startTimeComparator);
        long start = newLock.getStartTime();
        long end = newLock.getEndTime();
        Iterator<Lock> iter = locks.iterator();

        while (iter.hasNext()) {
            Lock lock = iter.next();
            if (start <= lock.getStartTime()) {
                if (end >= lock.getEndTime()) {
                    removed.add(lock.getId());
                    iter.remove();
                } else {
                    // list was sorted by start time, not need to go any further
                    break;
                }
            }
        }

        added.add(newLock);
        locks.add(newLock);

        // if combineLocks, do the combine before storing the initial change to
        // the db
        if (combineLocks) {
            Lock prevLock = null;
            Lock currentLock = null;
            Collections.sort(locks, startTimeComparator);
            prevLock = locks.get(0);
            for (int i = 1; i < locks.size(); i++) {
                currentLock = locks.get(i);
                if ((prevLock.getEndTime() >= currentLock.getStartTime())
                        && prevLock.getWsId().equals(currentLock.getWsId())) {
                    // remove previous lock, checking if it was a new lock first
                    if (!added.remove(prevLock)) {
                        removed.add(prevLock.getId());
                    }

                    // remove currentLock, checking if it was a new lock first
                    if (!added.remove(currentLock)) {
                        removed.add(currentLock.getId());
                    }
                    locks.remove(i);

                    // replace prevLock with new Lock
                    prevLock = new Lock(lt.getParmId(), new TimeRange(
                            prevLock.getStartTime(), Math.max(
                                    prevLock.getEndTime(),
                                    currentLock.getEndTime())), lt.getWsId());

                    added.add(prevLock);
                    locks.set(i - 1, prevLock);

                    // keep current position
                    i--;
                } else {
                    prevLock = currentLock;
                }
            }
        }

        // update the database
        if (!added.isEmpty() || !removed.isEmpty()) {
            dao.addRemoveLocks(added, removed);
        }
    }

    /**
     * Deletes locks in the given time range
     * 
     * @param lt
     *            The lock table to examine
     * @param tr
     *            The TimeRange to delete
     * @throws GfeLockException
     *             If errors occur when updating the locks in the database
     */
    private void deleteLocks(final LockTable lt, final TimeRange tr)
            throws DataAccessLayerException {
        // update the locks in memory
        List<Lock> locks = lt.getLocks();
        Collections.sort(locks, startTimeComparator);
        long start = tr.getStart().getTime();
        long end = tr.getEnd().getTime();
        Iterator<Lock> iter = locks.iterator();
        List<Integer> locksToRemove = new LinkedList<Integer>();

        while (iter.hasNext()) {
            Lock lock = iter.next();
            if (start <= lock.getStartTime()) {
                if (end >= lock.getEndTime()) {
                    locksToRemove.add(lock.getId());
                    iter.remove();
                } else {
                    // list was sorted by start time, not need to go any further
                    break;
                }
            }
        }

        // update the database
        dao.addRemoveLocks(null, locksToRemove);
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
            sr.addMessages(expandRequestToBoundary(req));
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
    private ServerResponse<?> expandRequestToBoundary(LockRequest req) {

        ServerResponse<?> sr = new ServerResponse<String>();
        if (!req.isParmRequest()) {
            logger.error("Expected parm-type request in expandRequestToBoundary");
        }

        // If this is a break-lock request, then do not expand to time constrts
        if (req.getMode().equals(LockTable.LockMode.BREAK_LOCK)) {
            return sr;
        }

        // Only expand for request lock and unlock
        DatabaseID dbid = req.getParmId().getDbId();

        TimeRange tr = null;

        switch (dbid.getFormat()) {
        case GRID:
            ServerResponse<GridParmInfo> ssr = GridParmManager
                    .getGridParmInfo(req.getParmId());
            GridParmInfo gpi = ssr.getPayload();
            sr.addMessages(ssr);
            if (!sr.isOkay()) {
                sr.addMessage("Unable to expand lock request to boundary since unable to get GridParmInfo for parameter");
                return sr;
            }
            // calculate the expanded time
            tr = gpi.getTimeConstraints().expandTRToQuantum(req.getTimeRange());
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
            req.setTimeRange(tr);
        } else {
            sr.addMessage("TimeRange not valid in LockRequest");
        }

        return sr;
    }

    /**
     * Overloaded utility routine to extract the ParmIds for a given
     * SeqOf<LockTableRequest>. If the request is a parm request, the ParmId
     * will be appended to the list ParmIds to be returned. If the request is a
     * database request, all the ParmIds for the given databaseId will be
     * appended to the list of ParmIds. If the request is neither a parm nor a
     * database request, all the parmId for all the databaseIds are appended to
     * the list of ParmIds to be returned.
     * 
     * @param ltr
     *            The input LockTableRequests
     * @return The ParmIDs contained in the requests
     * @throws GfeException
     */
    private ServerResponse<?> extractParmIds(List<LockTableRequest> ltrList,
            Set<ParmID> parmIds, String siteID) throws GfeException {

        ServerResponse<?> sr = new ServerResponse<String>();
        // process each request
        for (LockTableRequest ltr : ltrList) {
            if (ltr.isParmRequest()) {
                ParmID parmId = ltr.getParmId();
                // append parm (if not already in the set)
                if (!parmIds.contains(parmId)) {
                    parmIds.add(GridParmManager.getDb(parmId.getDbId())
                            .getCachedParmID(parmId));
                }
            } else if (ltr.isDatabaseRequest()) {
                // get all the parmIds for that databaseId
                List<ParmID> pids = GridParmManager.getParmList(ltr.getDbId())
                        .getPayload();
                parmIds.addAll(pids);
            } else {
                // get all the parms for all the databases
                List<DatabaseID> dbids = GridParmManager.getDbInventory(siteID)
                        .getPayload();
                for (int j = 0; j < dbids.size(); j++) {
                    List<ParmID> pids = GridParmManager.getParmList(
                            dbids.get(j)).getPayload();
                    parmIds.addAll(pids);
                }
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
    private ServerResponse<?> extractParmIdsFromLockReq(List<LockRequest> lrs,
            Set<ParmID> parmIds, String siteID) throws GfeException {
        ServerResponse<?> sr = new ServerResponse<String>();

        // process each request
        for (LockRequest lr : lrs) {
            if (lr.isParmRequest()) {
                ParmID parmId = lr.getParmId();
                // append parm (if not already in the list)
                if (!parmIds.contains(parmId)) {
                    parmIds.add(GridParmManager.getDb(parmId.getDbId())
                            .getCachedParmID(parmId));
                }
            } else if (lr.isDatabaseRequest()) {
                ServerResponse<List<ParmID>> ssr = GridParmManager
                        .getParmList(lr.getDbId());
                sr.addMessages(ssr);
                List<ParmID> pids = ssr.getPayload();
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
                    ServerResponse<List<ParmID>> ssr1 = GridParmManager
                            .getParmList(dbids.get(j));
                    sr.addMessages(ssr1);
                    List<ParmID> pids = ssr1.getPayload();
                    if (!sr.isOkay()) {
                        return sr;
                    }
                    for (ParmID id : pids) {
                        if (!parmIds.contains(id)) {
                            parmIds.add(id);
                        }
                    }
                }
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
        Set<DatabaseID> official = null;

        try {
            List<DatabaseID> officialDbs = IFPServerConfigManager
                    .getServerConfig(siteID).getOfficialDatabases();
            official = new HashSet<DatabaseID>(officialDbs.size(), 1);

            for (DatabaseID offDbId : officialDbs) {
                official.add(offDbId.stripModelTime());
            }
        } catch (GfeException e) {
            sr.addMessage("Unable to get official databases from IFPServer config");
            logger.error(
                    "Unable to get official database from IFPServer config", e);
            return sr;
        }

        // process each request - extracting out the databse id w/o modeltime
        for (LockRequest lr : req) {
            DatabaseID requestDB = null;
            if (lr.isDatabaseRequest()) {
                requestDB = lr.getDbId();
            } else if (lr.isParmRequest()) {
                requestDB = lr.getParmId().getDbId();
            } else {
                sr.addMessage("Invalid Lock Request (not parm or databse type): "
                        + lr);
                return sr;
            }
            requestDB = requestDB.stripModelTime();

            // look for a match
            if (official.contains(requestDB)) {
                sr.addMessage("Request locks on official database not permitted.  Request="
                        + lr);
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
