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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerMsg;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.GetGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SaveGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.GfeClientConfig;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.IDataObject;
import com.raytheon.viz.gfe.core.griddata.IGridData;

/**
 * DbParm is a concrete-class of the Parm hierarchy and is used to handle
 * parameters that come directly from the IFPGridDatabase on the server.
 *
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 17, 2008  940      bphillip  Implemented GFE Locking
 * Jan 29, 2008  1271     njensen   Rewrote populateGridFromData() to use
 *                                  IFPClient
 * Feb 23, 2012  346      dgilling  Implement a dispose method.
 * Mar 01, 2012  346      dgilling  Re-order dispose method.
 * Jan 21, 2012  1504     randerso  Cleaned up old debug logging to improve
 *                                  performance
 * Feb 12, 2013  1597     randerso  Made save threshold a configurable value.
 *                                  Added detailed logging for save performance
 * Apr 23, 2013  1949     rjpeter   Added logging of number of records.
 * Jun 26, 2013  2044     randerso  Fixed error message priority
 * Apr 03, 2014  2737     randerso  Moved clientSendStatus from SaveGridRequest
 *                                  to SaveGFEGridRequest
 * Nov 17, 2015  5129     dgilling  Support new IFPClient.
 * Apr 28, 2016  5618     randerso  Changed "Unable to get grid" message from
 *                                  INFO to ERROR
 * Dec 14, 2017  7178     randerso  Code formatting and cleanup
 * Jan 04, 2018  7178     randerso  Changes to support IDataObject. Code cleanup
 *
 * </pre>
 *
 * @author chammack
 */

public class DbParm extends Parm {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbParm.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    /**
     * Constructor
     *
     * @param parmID
     * @param gridInfo
     * @param mutable
     * @param displayable
     * @param dataMgr
     * @param lt
     */
    public DbParm(ParmID parmID, GridParmInfo gridInfo, boolean mutable,
            boolean displayable, DataManager dataMgr, LockTable lt) {
        super(parmID, gridInfo, mutable, displayable, dataMgr);
        this.lockTable = lt;

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
            statusHandler.handle(Priority.PROBLEM,
                    "Error populating parm: " + parmID + "attempting recovery",
                    e);
            boolean success = recoverGetGridFailure(true);
            if (!success) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve gridded data for " + parmID
                                + " during loading");
            }
        }
    }

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
        ServerResponse<List<TimeRange>> sr = dataManager.getClient()
                .getGridInventory(getParmID());
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Unable to retrieve gridded data [inventory] for %s: %s",
                    getParmID(), sr.message()));
            // empty array
            return new IGridData[0];
        }

        // now filter out those grids which are not of interest
        List<TimeRange> invTR = sr.getPayload();
        List<TimeRange> desiredTR = new ArrayList<>();
        for (TimeRange tr : invTR) {
            if (tr.overlaps(timeRange)) {
                desiredTR.add(tr);
            }
            if (tr.getEnd().after(timeRange.getEnd())) {
                // efficiency so we don't have to go through everything
                break;
            }
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
    private IGridData[] getGridsFromDb(List<TimeRange> gridTimes,
            boolean populate, Map<TimeRange, List<GridDataHistory>> histories) {
        List<IGridSlice> dataSlices = null;
        if (populate) {
            // want populated

            if (gridTimes.isEmpty()) {
                // nothing to do
                return new IGridData[0];
            }

            GetGridRequest ggr = new GetGridRequest(getParmID(), gridTimes);
            ServerResponse<List<IGridSlice>> sr = dataManager.getClient()
                    .getGridData(ggr);
            if (!sr.isOkay()) {
                statusHandler.error(String.format(
                        "Unable to retrieve gridded data [get data] for %s: %s",
                        getParmID(), sr.message()));
                // empty array
                return new IGridData[0];
            }

            dataSlices = sr.getPayload();

        } else {
            // want empty shell

            if (histories == null) {
                ServerResponse<Map<TimeRange, List<GridDataHistory>>> sr = dataManager
                        .getClient().getGridHistory(getParmID(), gridTimes);
                histories = sr.getPayload();
                if ((!sr.isOkay()) || (histories.size() != gridTimes.size())) {
                    statusHandler.error(String.format(
                            "Unable to retrieve gridded data [history] for %s: %s",
                            getParmID(), sr.message()));
                    // empty array
                    return new IGridData[0];
                }
            }

            dataSlices = new ArrayList<>();
            for (TimeRange tr : gridTimes) {
                dataSlices.add(getGridSlice(tr, histories.get(tr)));
            }
        }

        // convert the data slices to grids
        List<IGridData> grids = new ArrayList<>(dataSlices.size());
        for (IGridSlice slice : dataSlices) {
            IGridData g;
            try {
                g = AbstractGridData.makeGridData(this, slice, false);
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
            throw new IllegalArgumentException(
                    "Unknown grid type: " + this.gridInfo.getGridType());
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
            grid.replace(grids[0]);
        }

        else {
            // failure
            statusHandler.error("Unable to get grid for " + getParmID() + " tr="
                    + grid.getGridTime() + ". Temporarily using default data");
            IGridData g = makeEmptyGrid();
            g.changeValidTime(grid.getGridTime(), false);
            g.updateHistory(grid.getHistory());
            grid.replace(g);
        }

    }

    @Override
    public void populateGrids(List<IGridData> grids) {
        List<TimeRange> gridTimes = new ArrayList<>(grids.size());
        for (IGridData grid : grids) {
            gridTimes.add(grid.getGridTime());
        }

        IGridData[] newGrids = getGridsFromDb(gridTimes, true, null);

        // replace the data portion of the input "grid" with what was retrieved
        // from the network
        if (grids.size() == newGrids.length) {
            int index = 0;
            for (IGridData grid : grids) {
                grid.replace(newGrids[index]);
                index++;
            }
        }

        // failure
        else {
            List<IGridData> gridsNotReplaced = new ArrayList<>(grids);
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
                statusHandler.error("Unable to get grid for " + getParmID()
                        + " tr=" + grid.getGridTime()
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

        List<TimeRange> newTimeRanges = new ArrayList<>(histories.keySet());

        // Find the full range of data affected
        Collections.sort(newTimeRanges);

        // First get the new grids from the network

        // direct access to the grids with standard technique
        IGridData[] grids = this.getGridsFromDb(newTimeRanges, false,
                histories);

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

    @Override
    public void historyUpdateArrived(
            Map<TimeRange, List<GridDataHistory>> histories) {

        // Old code also rejected when newTimeRanges.length was 0.
        // Length 0 is valid for deletions over whole time range.
        if (histories == null) {
            return;
        }

        List<TimeRange> newTimeRanges = new ArrayList<>(histories.keySet());

        // Find the full range of data affected
        Collections.sort(newTimeRanges);

        // process history updates
        for (TimeRange tr : newTimeRanges) {
            IGridData[] grids = this.getGridInventory(tr);

            // if only a single unmodified grid exactly matches the time range
            if ((grids.length == 1) && !this.isLocked(tr)
                    && grids[0].getGridTime().equals(tr)) {
                List<GridDataHistory> newHist = histories.get(tr);
                GridDataHistory[] currentHist = grids[0].getHistory();

                // if current history exists and has a matching update time
                if ((currentHist != null) && currentHist[0].getUpdateTime()
                        .equals(newHist.get(0).getUpdateTime())) {
                    // update last sent time
                    currentHist[0]
                            .setLastSentTime(newHist.get(0).getLastSentTime());
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

        ITimer timer = TimeUtil.getTimer();
        timer.start();
        ServerResponse<List<LockTable>> sr = this.dataManager.getClient()
                .requestLockChange(lreq);
        timer.stop();
        perfLog.logDuration("Server lock change for " + this.getParmID() + " "
                + lreq.size() + " time ranges", timer.getElapsedTime());

        if (sr.isOkay()) {
            timer.reset();
            timer.start();
            List<LockTable> lockTableList = sr.getPayload();
            for (LockTable lt : lockTableList) {
                // it is for our parm
                if (lt.getParmId().equals(getParmID())) {
                    // treat as a notification
                    lockTableArrived(lt);
                }
                // it is for another parm
                else {
                    Parm otherParm = dataManager.getParmManager()
                            .getParm(lt.getParmId());
                    if (otherParm != null) {
                        otherParm.lockTableArrived(lt);
                    }
                }
            }
            timer.stop();
            perfLog.logDuration("Processing lock tables",
                    timer.getElapsedTime());
        } else {
            statusHandler.error(String.format(
                    "Failed to process lock request: %s", sr.message()));
        }

        return sr.isOkay();
    }

    @Override
    protected boolean saveParameterSubClass(List<TimeRange> trs) {

        ITimer timer = TimeUtil.getTimer();
        timer.start();

        List<TimeRange> myLocks = lockTable.lockedByMe();
        if (myLocks.isEmpty()) {
            timer.stop();
            perfLog.logDuration("Saving " + getParmID().getParmName() + ": "
                    + " no grids to save ", timer.getElapsedTime());
            return true;
        }

        // this operation cannot be undone
        purgeUndoGrids();

        // compute grid size in bytes
        GridLocation gloc = this.getGridInfo().getGridLoc();
        GridType gridType = this.getGridInfo().getGridType();
        int gridSize = gloc.getNx() * gloc.getNy();
        switch (gridType) {
        case SCALAR:
            // 4 bytes per grid cell
            gridSize *= 4;
            break;

        case VECTOR:
            // 2 floats (8 bytes) per grid cell
            gridSize *= 8;
            break;

        case WEATHER:
        case DISCRETE:
        default:
            // do nothing since 1 byte per grid cell and
            // ignoring size of keys for now
        }

        // assemble the save grid request and lock requests
        List<SaveGridRequest> sgr = new ArrayList<>();
        List<LockRequest> lreq = new ArrayList<>();
        List<TimeRange> pendingUnlocks = new ArrayList<>();

        boolean success = true;
        int gridCount = 0;
        int totalGrids = 0;
        long totalSize = 0;
        int totalRecords = 0;
        long size = 0;
        int recordCount = 0;
        List<IGridData> savedGrids = new LinkedList<>();
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

            List<GFERecord> records = new ArrayList<>();
            boolean allSaved = true;

            // time range remaining to be saved
            TimeRange saveTime = new TimeRange(lockTime.getStart(),
                    lockTime.getEnd());

            for (IGridData data : grids) {
                IDataObject collapsedGrid;
                collapsedGrid = data.getDataObject().copy();
                collapsedGrid.collapse();
                data.setDataObject(collapsedGrid);
                GFERecord record = new GFERecord(this.getParmID(),
                        data.getGridTime());
                data.setSaveHistory();
                record.setGridHistory(data.getHistory());
                record.setMessageData(data.getGridSlice());
                records.add(record);
                savedGrids.add(data);
                gridCount += (gridType.equals(GridType.VECTOR) ? 2 : 1);
                size += gridSize;

                if (size > GfeClientConfig.getInstance()
                        .getGridSaveThreshold()) {
                    TimeRange tr = new TimeRange(saveTime.getStart(),
                            data.getGridTime().getEnd());
                    sgr.add(new SaveGridRequest(getParmID(), tr, records));

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

                    totalGrids += gridCount;
                    totalRecords += records.size();
                    totalSize += size;

                    pendingUnlocks.clear();
                    sgr.clear();
                    gridCount = 0;
                    records.clear();
                    size = 0;
                    saveTime.setStart(tr.getEnd());
                }
            }

            // if any grids or any time not saved
            if ((size > 0) || (saveTime.getDuration() > 0)) {
                sgr.add(new SaveGridRequest(getParmID(), saveTime, records));
                recordCount = records.size();
            }

            // if we haven't had a failure yet add to pending locks
            if (allSaved) {
                pendingUnlocks.add(lockTime);
            }

            success &= allSaved;
        }

        // if any pending saves
        if (!sgr.isEmpty()) {
            if (doSave(sgr)) {
                for (TimeRange t : pendingUnlocks) {
                    lreq.add(new LockRequest(getParmID(), t, LockMode.UNLOCK));
                }
            } else {
                success = false;
            }

            totalSize += size;
            totalGrids += gridCount;
            totalRecords += recordCount;
            pendingUnlocks.clear();
        }

        if (success) {
            for (TimeRange t : pendingUnlocks) {
                lreq.add(new LockRequest(getParmID(), t, LockMode.UNLOCK));
            }
        }

        if (!lreq.isEmpty()) {
            success &= requestLock(lreq);
        }

        if (success) {
            for (IGridData grid : savedGrids) {
                grid.successfullySaved();
            }
            purgeUndoGrids();
        }

        timer.stop();
        perfLog.logDuration("Save Grids " + getParmID().getParmName() + ": "
                + totalRecords + " records, " + totalGrids + " grids ("
                + totalSize + " bytes) ", timer.getElapsedTime());

        return success;
    }

    private boolean doSave(List<SaveGridRequest> sgr) {
        boolean success = true;

        ServerResponse<?> sr = dataManager.getClient().saveGridData(sgr,
                dataManager.clientISCSendStatus());
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

    @Override
    public boolean revertParameter() {

        boolean retVal = true;

        List<TimeRange> timesToSave = this.getLockTable().lockedByMe();

        if (!timesToSave.isEmpty()) {
            // this operation cannot be undone
            purgeUndoGrids();
        }

        List<LockRequest> lreq = new ArrayList<>(timesToSave.size());
        for (TimeRange tr : timesToSave) {
            boolean success = false;
            IGridData[] grids = null;
            try {
                grids = this.getGridsFromDb(tr, false);
                success = true;
            } catch (GFEOperationFailedException e) {
                statusHandler.debug(
                        "Exception getting grids from db for " + getParmID(),
                        e);
            }

            if (!success) {
                success = recoverGetGridFailure(true);
                if (!success) {
                    statusHandler.error("Cannot revert parameter " + getParmID()
                            + " completely due to problem getting data from servers");
                    retVal = false;
                }
            } else {
                this.replaceGrids(tr, grids);
            }

            LockRequest lr = new LockRequest(this.getParmID(), tr,
                    LockMode.UNLOCK);
            lreq.add(lr);
        }
        if (!requestLock(lreq)) {
            retVal = false;
        }

        this.sendInvChangedNotification(TimeRange.allTimes());

        return retVal;
    }

    /**
     * @param allGrids
     *            true if all grids should be recovered
     * @return true if successful
     */
    public boolean recoverGetGridFailure(boolean allGrids) {
        TimeRange allTimes = TimeRange.allTimes();

        List<TimeRange> notMyLocks = new ArrayList<>();
        if (!allGrids) {
            notMyLocks = calcNonLocks();
        } else {
            notMyLocks.add(allTimes);
        }

        // failure occurred - get the entire inventory
        IGridData[] grids = null;
        int ntrys = 20;
        boolean success = false;
        while (ntrys > 0) {
            ntrys--;

            statusHandler.debug("Attempting recovery in getGrids for "
                    + getParmID() + " trys remaining#=" + ntrys);
            try {
                grids = getGridsFromDb(allTimes, false);
                success = true;
                break;
            } catch (GFEOperationFailedException e) {
                Priority priority = (ntrys > 0 ? Priority.DEBUG
                        : Priority.PROBLEM);
                statusHandler.handle(priority,
                        "Exception getting grids from db for " + getParmID(),
                        e);
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
            List<IGridData> tGrids = new ArrayList<>();
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
        List<TimeRange> notMyLocks = new ArrayList<>();

        Collections.sort(myLocks);

        for (int i = 0; i < myLocks.size(); i++) {
            if (i == 0) {
                notMyLocks.add(new TimeRange(TimeRange.MIN_TIME,
                        myLocks.get(0).getStart().getTime()));
            } else {
                Date startTime = myLocks.get(i - 1).getEnd();
                Date endTime = myLocks.get(i).getStart();
                if (!startTime.equals(endTime)) {
                    notMyLocks.add(new TimeRange(startTime, endTime));
                }
            }
        }

        if (notMyLocks.isEmpty()) {
            notMyLocks.add(TimeRange.allTimes());
        } else {
            notMyLocks.add(new TimeRange(
                    myLocks.get(myLocks.size() - 1).getEnd().getTime(),
                    TimeRange.MAX_TIME));
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
            // nothing to do
            return;
        }

        List<LockRequest> lreq = new ArrayList<>();
        for (int i = 0; i < timesToUnlock.size(); i++) {
            lreq.add(new LockRequest(getParmID(), timesToUnlock.get(i),
                    LockMode.UNLOCK));
            requestLock(lreq);
        }

    }

}
