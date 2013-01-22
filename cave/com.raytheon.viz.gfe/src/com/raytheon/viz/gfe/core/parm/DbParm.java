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
package com.raytheon.viz.gfe.core.parm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockStatus;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;

/**
 * Placeholder for DbParm
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 01/29/08     #1271      njensen     Rewrote populateGridFromData()
 *                                      to use IFPClient
 * 02/23/12     #346       dgilling    Implement a dispose method.
 * 03/01/12     #346       dgilling    Re-order dispose method.
 * 01/21/12     #1504      randerso    Cleaned up old debug logging to improve performance
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DbParm extends Parm {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbParm.class);

    // save if we accumulate more than 16 MB
    private static final long MAX_SAVE_SIZE = 16 * 1024 * 1024;

    public DbParm(ParmID parmID, GridParmInfo gridInfo, boolean mutable,
            boolean displayable, DataManager dataMgr) throws GFEServerException {
        super(parmID, gridInfo, mutable, displayable, dataMgr);

        TimeRange tr = TimeRange.allTimes();
        try {
            List<IGridData> newGrids = Arrays.asList(getGridsFromDb(tr, false));
            this.grids.acquireWriteLock();
            try {
                this.grids.addAll(newGrids);
            } finally {
                this.grids.releaseWriteLock();
            }
        } catch (GFEOperationFailedException e) {
            statusHandler.handle(Priority.PROBLEM, "Error populating parm: "
                    + parmID + "attempting recovery", e);
            boolean success = recoverGetGridFailure(true);
            if (!success) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve gridded data for " + parmID
                                + " during loading");
            }
        }

        if (this.dataManager != null && this.dataManager.getClient() != null) {
            this.lockTable = this.dataManager.getClient().getLockTable(
                    this.getParmID());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.Parm#dispose()
     */
    @Override
    public void dispose() {
        looseLocks();
        super.dispose();
    }

    // -- private
    // ----------------------------------------------------------------
    // DbParm::getGridsFromDb()
    // Gets a series of grids from the database. Returns true if successful.
    // The grids are those defined by overlapping time range. The data is
    // returned as GridData*. The user is responsible for memory management
    // of these grids. In the event of a failure, then a sequence of
    // zero GridData*s are returned and false is returned. Populate flag
    // indicates whether grids should contains grids, or just a shell.
    // -- implementation
    // ---------------------------------------------------------
    // Gets the database inventory. Figures out the overlapping times. Then
    // uses the getGridsFromDb() that takes a sequence of time ranges.
    // ---------------------------------------------------------------------------
    private IGridData[] getGridsFromDb(final TimeRange timeRange,
            boolean populate) throws GFEOperationFailedException {

        List<TimeRange> invTR;
        try {
            invTR = this.dataManager.getClient().getGridInventory(getParmID());
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve gridded data [inventory] for "
                            + getParmID(), e);
            return new IGridData[0]; // empty sequence
        }

        // now filter out those grids which are not of interest
        List<TimeRange> desiredTR = new ArrayList<TimeRange>();
        for (TimeRange tr : invTR) {
            if (tr.overlaps(timeRange)) {
                desiredTR.add(tr);
            }
            if (tr.getEnd().after(timeRange.getEnd())) {
                break;
            } // efficiency so we don't have to go through everything
        }

        return getGridsFromDb(desiredTR, populate, null);
    }

    // -- private
    // ----------------------------------------------------------------
    // DbParm::getGridsFromDb()
    // Gets a series of grids from the database. Returns true if successful.
    // The grids are those defined by the seq of time ranges. The data is
    // returned as GridData*. The user is responsible for memory management
    // of these grids. In the event of a failure, then a sequence of
    // zero GridData*s are returned and false is returned. Populate indicates
    // whether grids should be populated or just a shell.
    // -- implementation
    // ---------------------------------------------------------
    // Makes a request for the data grids, converts the data slices into
    // GridData*.
    // ---------------------------------------------------------------------------
    @SuppressWarnings("unchecked")
    private IGridData[] getGridsFromDb(List<TimeRange> gridTimes,
            boolean populate, Map<TimeRange, List<GridDataHistory>> histories) {
        // success = true;
        List<IGridSlice> dataSlices = null;
        // want populated
        if (populate) {
            if (gridTimes.size() == 0) {
                return new IGridData[0]; // nothing to do
            }

            try {
                dataSlices = this.dataManager.getClient().getGridData(
                        getParmID(), gridTimes);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve gridded data [get data] for "
                                + getParmID(), e);
                // success = false;
                return new IGridData[0]; // empty sequence
            }

            // want empty shell
        } else {
            if (histories == null) {
                try {
                    ServerResponse<?> sr = this.dataManager.getClient()
                            .getGridHistory(getParmID(), gridTimes);
                    histories = (Map<TimeRange, List<GridDataHistory>>) sr
                            .getPayload();
                    if (!sr.isOkay() || histories.size() != gridTimes.size()) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Unable to retrieve gridded data [history] for "
                                        + getParmID() + sr);
                        // success = false;
                        return new IGridData[0]; // empty sequence
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to retrieve gridded data [history] for "
                                    + getParmID(), e);
                    // success = false;
                    return new IGridData[0]; // empty sequence
                }
            }

            dataSlices = new ArrayList<IGridSlice>();
            for (TimeRange tr : gridTimes) {
                dataSlices.add(getGridSlice(tr, histories.get(tr)));
            }
        }

        // convert the data slices to grids
        List<IGridData> grids = new ArrayList<IGridData>(dataSlices.size());
        for (int i = 0; i < dataSlices.size(); i++) {
            IGridData g;
            try {
                g = AbstractGridData.makeGridData(this, dataSlices.get(i));
                grids.add(g);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        return grids.toArray(new IGridData[grids.size()]);
    }

    // -- private
    // ----------------------------------------------------------------
    // DbParm::getGridSlice()
    // Creates and returns a GridSlice without data (unpopulated), given the
    // grid valid time range and grid data history.
    // ---------------------------------------------------------------------------
    private IGridSlice getGridSlice(final TimeRange timeRange,
            final Collection<GridDataHistory> hist) {
        IGridSlice gs = null;
        switch (this.gridInfo.getGridType()) {
        case SCALAR:
            gs = new ScalarGridSlice(timeRange, this.gridInfo,
                    hist.toArray(new GridDataHistory[hist.size()]), null);
            break;
        case VECTOR:
            gs = new VectorGridSlice(timeRange, this.gridInfo,
                    hist.toArray(new GridDataHistory[hist.size()]), null, null);
            break;
        case WEATHER:
            gs = new WeatherGridSlice(timeRange, this.gridInfo,
                    hist.toArray(new GridDataHistory[hist.size()]), null,
                    new WeatherKey[0]);
            break;
        case DISCRETE:
            gs = new DiscreteGridSlice(timeRange, this.gridInfo,
                    hist.toArray(new GridDataHistory[hist.size()]), null,
                    new DiscreteKey[0]);
            break;
        default:
            throw new IllegalArgumentException("Unknown grid type: "
                    + this.gridInfo.getGridType());
        }
        return gs;
    }

    @Override
    public void populateGrid(IGridData grid) {

        IGridData[] grids = getGridsFromDb(Arrays.asList(grid.getGridTime()),
                true, null);

        // replace the data portion of the input "grid" with what was retrieved
        // from the network
        if (grids.length == 1) {
            // logDebug << "GRID LOAD " << parmID() << ' '
            // << grid->gridTime() << std::endl;
            grid.replace(grids[0]);
            // delete grids[0]; // deallocate the memory
        }

        // failure
        else {
            statusHandler.handle(Priority.EVENTA, "Unable to get grid for "
                    + getParmID() + " tr=" + grid.getGridTime()
                    + ". Temporarily using default data");
            IGridData g = makeEmptyGrid();
            g.changeValidTime(grid.getGridTime(), false);
            g.updateHistory(grid.getHistory());
            grid.replace(g);
        }

    }

    @Override
    public void populateGrids(List<IGridData> grids) {
        List<TimeRange> gridTimes = new ArrayList<TimeRange>(grids.size());
        for (IGridData grid : grids) {
            gridTimes.add(grid.getGridTime());
        }

        IGridData[] newGrids = getGridsFromDb(gridTimes, true, null);

        // replace the data portion of the input "grid" with what was retrieved
        // from the network
        if (grids.size() == newGrids.length) {
            int index = 0;
            for (IGridData grid : grids) {
                grid.replace(newGrids[index++]);
            }
        }

        // failure
        else {
            List<IGridData> gridsNotReplaced = new ArrayList<IGridData>(grids);
            Iterator<IGridData> iter = gridsNotReplaced.iterator();
            while (iter.hasNext()) {
                IGridData grid = iter.next();
                for (IGridData nGrid : newGrids) {
                    if (grid.getGridTime().equals(nGrid.getGridTime())) {
                        grid.replace(nGrid);
                        iter.remove();
                        break;
                    }
                }
            }
            for (IGridData grid : gridsNotReplaced) {
                statusHandler.handle(Priority.EVENTA, "Unable to get grid for "
                        + getParmID() + " tr=" + grid.getGridTime()
                        + ". Temporarily using default data");
                IGridData g = makeEmptyGrid();
                g.changeValidTime(grid.getGridTime(), false);
                g.updateHistory(grid.getHistory());
                grid.replace(g);
            }
        }

    }

    // -- public
    // -----------------------------------------------------------------
    // DbParm::inventoryArrived()
    // Notification received from the NetworkMgr when the parm's inventory
    // has been changed in the database server. The affected time range of the
    // inventory and a list of the new time ranges are provided.
    // -- implementation
    // ---------------------------------------------------------
    // Get the data slices from the network and convert them into
    // grids. Then use replaceGrids() to insert the grids. Use
    // sendInvChangedNotification to notify all parm clients of the change.
    // ---------------------------------------------------------------------------
    @Override
    public void inventoryArrived(final TimeRange affectedTimeRange,
            Map<TimeRange, List<GridDataHistory>> histories) {

        // Old code also rejected when newTimeRanges.length was 0.
        // Length 0 is valid for deletions over whole time range.
        if (histories == null) {
            return;
        }

        List<TimeRange> newTimeRanges = new ArrayList<TimeRange>(
                histories.keySet());

        // Find the full range of data affected
        Collections.sort(newTimeRanges);

        // First get the new grids from the network

        // direct access to the grids with standard technique
        IGridData[] grids = this
                .getGridsFromDb(newTimeRanges, false, histories);

        boolean normal;
        if (grids.length != newTimeRanges.size()) {
            normal = false;
            // failure occurred - get the entire inventory
            boolean success = recoverGetGridFailure(false);
            if (!success) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve latest grids from server for "
                                + getParmID() + " during automatic update");
                return;
            }
        } else {
            normal = true;
        }

        // normal mode
        if (normal) {
            Arrays.sort(grids);

            // Now replace the existing grids with the new ones
            replaceGrids(affectedTimeRange, grids);

            // Notify ParmClients of new grid inventory
            sendInvChangedNotification(affectedTimeRange);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.Parm#historyUpdateArrived(java.util.Map)
     */
    @Override
    public void historyUpdateArrived(
            Map<TimeRange, List<GridDataHistory>> histories) {

        // Old code also rejected when newTimeRanges.length was 0.
        // Length 0 is valid for deletions over whole time range.
        if (histories == null) {
            return;
        }

        List<TimeRange> newTimeRanges = new ArrayList<TimeRange>(
                histories.keySet());

        // Find the full range of data affected
        Collections.sort(newTimeRanges);

        // process history updates
        for (TimeRange tr : newTimeRanges) {
            IGridData[] grids = this.getGridInventory(tr);

            // if only a single unmodified grid exactly matches the time range
            if (grids.length == 1 && !this.isLocked(tr)
                    && grids[0].getGridTime().equals(tr)) {
                List<GridDataHistory> newHist = histories.get(tr);
                GridDataHistory[] currentHist = grids[0].getHistory();

                // if current history exists and has a matching update time
                if (currentHist != null
                        && currentHist[0].getUpdateTime().equals(
                                newHist.get(0).getUpdateTime())) {
                    // update last sent time
                    currentHist[0].setLastSentTime(newHist.get(0)
                            .getLastSentTime());
                }
            }

            // notify parm clients of history update
            sendGridHistoryUpdatedNotification(tr);
        }
    }

    @Override
    protected boolean requestLock(List<LockRequest> lreq) {

        if (ignoreLocks()) {
            return true;
        }

        ServerResponse<List<LockTable>> sr;
        try {
            sr = this.dataManager.getClient().requestLockChange(lreq);
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error requesting lock change", e);
            return false;
        }

        List<LockTable> lockTableList = sr.getPayload();
        for (LockTable lt : lockTableList) {
            // it is for our parm
            if (lt.getParmId().equals(this.getParmID())) {
                lockTableArrived(lt); // treat as a notification
            }
            // it is for another parm
            else {
                Parm otherParm = this.dataManager.getParmManager().getParm(
                        lt.getParmId());
                if (otherParm != null) {
                    otherParm.lockTableArrived(lt);
                }
            }
        }
        for (ServerMsg msg : sr.getMessages()) {
            statusHandler.error(msg.getMessage());
        }

        return sr.isOkay();
    }

    @Override
    protected boolean saveParameterSubClass(List<TimeRange> trs) {

        List<TimeRange> myLocks = lockTable.lockedByMe();
        if (myLocks.isEmpty()) {
            return true;
        }

        // FIXME: Purge functionality
        purgeUndoGrids();

        // assemble the save grid request and lock requests
        // List<IGridData> gridsSaved = new ArrayList<IGridData>();
        List<SaveGridRequest> sgr = new ArrayList<SaveGridRequest>();
        List<LockRequest> lreq = new ArrayList<LockRequest>();
        List<TimeRange> pendingUnlocks = new ArrayList<TimeRange>();

        GridLocation gloc = this.getGridInfo().getGridLoc();
        int gridSize = gloc.getNx() * gloc.getNy();
        switch (this.getGridInfo().getGridType()) {
        case SCALAR:
            gridSize *= 4; // 4 bytes per grid cell
            break;
        case VECTOR:
            gridSize *= 8; // 2 floats (8 bytes) per grid cell
            break;
        case WEATHER:
        case DISCRETE:
            // do nothing since 1 byte per grid cell and
            // ignoring size of keys for now
        }

        boolean success = true;
        long size = 0;
        for (int i = 0; i < trs.size(); i++) {
            // ensure we have a lock for the time period
            TimeRange lockTime = new TimeRange();
            for (int j = 0; j < myLocks.size(); j++) {
                if (myLocks.get(j).overlaps(trs.get(i))) {
                    lockTime = trs.get(i).intersection(myLocks.get(j));
                    break;
                }
            }

            // not valid save time, ignore it
            if (!lockTime.isValid()) {
                continue;
            }

            // List<IGridSlice> dataSlices = new ArrayList<IGridSlice>();
            IGridData[] grids = this.getGridInventory(lockTime);

            List<GFERecord> records = new ArrayList<GFERecord>();
            boolean allSaved = true;

            // time range remaining to be saved
            TimeRange saveTime = new TimeRange(lockTime.getStart(),
                    lockTime.getEnd());

            for (IGridData data : grids) {
                IGridSlice collapsedGrid;
                try {
                    collapsedGrid = data.getGridSlice().clone();
                    collapsedGrid.collapse();
                    data.setGridSlice(collapsedGrid);
                } catch (CloneNotSupportedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                    return false;
                }
                GFERecord record = new GFERecord(this.getParmID(),
                        data.getGridTime());
                data.setSaveHistory();
                record.setGridHistory(data.getHistory());
                record.setMessageData(data.getGridSlice());
                records.add(record);
                size += gridSize;

                if (size > MAX_SAVE_SIZE) {
                    // time range being saved in this chunk
                    TimeRange tr = new TimeRange(saveTime.getStart(), data
                            .getGridTime().getEnd());
                    sgr.add(new SaveGridRequest(getParmID(), tr, records,
                            dataManager.clientISCSendStatus()));

                    // save this batch of grids
                    if (doSave(sgr)) {
                        // if successful add pending locks to unlock requests
                        for (TimeRange t : pendingUnlocks) {
                            lreq.add(new LockRequest(getParmID(), t,
                                    LockMode.UNLOCK));
                        }
                    } else {
                        allSaved = false;
                    }

                    pendingUnlocks.clear();
                    sgr.clear();
                    records.clear();
                    size = 0;
                    saveTime.setStart(tr.getEnd());
                }
            }

            // if any grids or any time not saved
            if (size > 0 || saveTime.getDuration() > 0) {
                sgr.add(new SaveGridRequest(getParmID(), saveTime, records,
                        dataManager.clientISCSendStatus()));
            }

            // if we haven't had a failure yet add to pending locks
            if (allSaved) {
                pendingUnlocks.add(lockTime);
            }

            success &= allSaved;
        }
        // if any pending saves
        if (sgr.size() > 0) {
            if (doSave(sgr)) {
                for (TimeRange t : pendingUnlocks) {
                    lreq.add(new LockRequest(getParmID(), t, LockMode.UNLOCK));
                }
            } else {
                success = false;
            }
        }

        // if any pending saves
        if (sgr.size() > 0) {
            if (!doSave(sgr)) {
                success = false;
            }
        }

        if (success) {
            for (TimeRange t : pendingUnlocks) {
                lreq.add(new LockRequest(getParmID(), t, LockMode.UNLOCK));
            }
        }

        if (lreq.size() > 0) {
            success &= requestLock(lreq);
        }

        if (success) {
            purgeUndoGrids();
        }

        return success;
    }

    private boolean doSave(List<SaveGridRequest> sgr) {
        boolean success = true;
        ServerResponse<?> sr = null;
        try {
            sr = dataManager.getClient().saveGrids(sgr);
        } catch (GFEServerException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Save data request not granted for " + getParmID()
                                    + ". \n", e);
            return false;
        }
        if (!sr.isOkay()) {
            StringBuilder sb = new StringBuilder();
            sb.append("Save data request not granted for ");
            sb.append(getParmID().toString());

            for (ServerMsg msg : sr.getMessages()) {
                sb.append('\n').append(msg.getMessage());
            }
            statusHandler.handle(Priority.PROBLEM, sb.toString());
            success = false;
        }

        return success;
    }

    // -- public
    // -----------------------------------------------------------------
    // DbParm::deallocateUnusedGrids()
    // Deallocates grids that haven't been used in awhile.
    // -- implementation
    // ---------------------------------------------------------
    // never deallocate grids within a lock (my lock)
    // ---------------------------------------------------------------------------
    @Override
    public void deallocateUnusedGrids(int seconds) {
        Date time = this.dataManager.getSpatialDisplayManager()
                .getSpatialEditorTime();
        IGridData se = null;

        if (time != null) {
            se = overlappingGrid(time);
        }

        long milliseconds = 1000L * seconds;

        // go through each grid in existence
        // must use for i loop to avoid concurrentModification exception
        long now = System.currentTimeMillis();
        this.grids.acquireReadLock();
        try {
            for (IGridData grid : this.grids) {
                if (!grid.isPopulated()) {
                    continue;
                }

                if (grid == se) {
                    continue; // grid overlaps spatial editor time -- skip it
                }

                long lastAccess = grid.getLastAccessTime();

                long delta = now - lastAccess;
                if (delta < milliseconds) {
                    continue; // recently accessed
                }

                // grid is populated, is it modified?
                final TimeRange gTime = grid.getGridTime();
                boolean locked = this.getLockTable().checkLock(gTime)
                        .equals(LockStatus.LOCKED_BY_ME);

                // only deallocate unlocked grids
                if (!locked) {
                    // String msg = "Deallocating " + getParmID() + " tr=" +
                    // gTime;
                    // statusHandler.handle(Priority.DEBUG, msg, new Exception(
                    // "Debug: " + msg));

                    grid.depopulate();
                }
            }
        } finally {
            this.grids.releaseReadLock();
        }
    }

    @Override
    public boolean revertParameter() {

        boolean retVal = true;

        List<TimeRange> timesToSave = this.getLockTable().lockedByMe();

        if (!timesToSave.isEmpty()) {
            purgeUndoGrids();
        }

        List<LockRequest> lreq = new ArrayList<LockRequest>(timesToSave.size());
        for (int i = 0; i < timesToSave.size(); i++) {
            // String msg = "Reverting " + getParmID() + " tr="
            // + timesToSave.get(i);
            // statusHandler.handle(Priority.DEBUG, msg, new Exception("Debug: "
            // + msg));

            boolean success = true;
            IGridData[] grids = null;
            try {
                grids = this.getGridsFromDb(timesToSave.get(i), false);
            } catch (GFEOperationFailedException e) {
                e.printStackTrace();
                success = false;
            }

            if (!success) {
                retVal = false;
                this.recoverGetGridFailure(true);
                Status statusMessage = new Status(
                        IStatus.ERROR,
                        Activator.PLUGIN_ID,
                        "Cannot revert parameter "
                                + getParmID()
                                + " completely due to problem getting data from servers");
                Activator.getDefault().getLog().log(statusMessage);
            } else {
                this.replaceGrids(timesToSave.get(i), grids);
            }

            LockRequest lr = new LockRequest(this.getParmID(),
                    timesToSave.get(i), LockMode.UNLOCK);
            lreq.add(lr);
        }
        if (!requestLock(lreq)) {
            retVal = false;
        }

        this.sendInvChangedNotification(TimeRange.allTimes());

        return retVal;
    }

    public boolean recoverGetGridFailure(boolean allGrids) {

        List<TimeRange> notMyLocks = new ArrayList<TimeRange>();
        if (!allGrids) {
            notMyLocks = calcNonLocks();
        } else {
            notMyLocks.add(new TimeRange(0, (long) Integer.MAX_VALUE * 1000));
        }

        // failure occurred - get the entire inventory
        IGridData[] grids = null;
        int ntrys = 20;
        boolean success = true;

        while (ntrys-- > 0) {
            statusHandler.handle(Priority.PROBLEM,
                    "Attempting recovery in getGrids for " + getParmID()
                            + " trys remaining#=" + ntrys);
            TimeRange allTimes = new TimeRange(0,
                    (long) Integer.MAX_VALUE * 1000);
            try {
                grids = getGridsFromDb(allTimes, false);
            } catch (GFEOperationFailedException e) {
                success = false;
            }

            if (success) {
                break;
            }
        }

        // were we successful?
        if (!success) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve latest grids from " + getParmID()
                            + " during automatic update");
            return false;
        }

        // Now replace the existing grids with the new ones
        for (int i = 0; i < notMyLocks.size(); i++) {
            // just get the ones that are covered by "notmylock"
            List<IGridData> tGrids = new ArrayList<IGridData>();
            for (int j = 0; j < grids.length; j++) {
                if (grids[j].getGridTime().overlaps(notMyLocks.get(i))) {
                    tGrids.add(grids[j]);
                    grids[j] = null;
                }
            }

            // insert the grids into the parm's inventory
            replaceGrids(notMyLocks.get(i), tGrids.toArray(new IGridData[] {}));

            // notify parmClients of new grid inventory
            this.sendInvChangedNotification(notMyLocks.get(i));
        }

        return true;

    }

    private List<TimeRange> calcNonLocks() {
        // create inverse of the lock table
        List<TimeRange> myLocks = lockTable.lockedByMe();
        List<TimeRange> notMyLocks = new ArrayList<TimeRange>();

        Collections.sort(myLocks);

        for (int i = 0; i < myLocks.size(); i++) {
            if (i == 0) {
                notMyLocks.add(new TimeRange(0, myLocks.get(0).getStart()
                        .getTime()));
            } else {
                Date startTime = myLocks.get(i - 1).getEnd();
                Date endTime = myLocks.get(i).getStart();
                if (!startTime.equals(endTime)) {
                    notMyLocks.add(new TimeRange(startTime, endTime));
                }
            }
        }

        if (notMyLocks.size() == 0) {
            notMyLocks.add(new TimeRange(0, (long) Integer.MAX_VALUE * 1000));
        } else {
            notMyLocks.add(new TimeRange(myLocks.get(myLocks.size() - 1)
                    .getEnd().getTime(), (long) Integer.MAX_VALUE * 1000));
        }
        return notMyLocks;
    }

    @Override
    public void looseLocks() {
        // if we are ignoring locks?
        if (ignoreLocks()) {
            return;
        }

        // unlock any existing locks
        List<TimeRange> timesToUnlock = getLockTable().lockedByMe();
        if (timesToUnlock.isEmpty()) {
            return; // nothing to do
        }

        List<LockRequest> lreq = new ArrayList<LockRequest>();
        for (int i = 0; i < timesToUnlock.size(); i++) {
            lreq.add(new LockRequest(getParmID(), timesToUnlock.get(i),
                    LockMode.UNLOCK));
            requestLock(lreq);
        }

    }

}
