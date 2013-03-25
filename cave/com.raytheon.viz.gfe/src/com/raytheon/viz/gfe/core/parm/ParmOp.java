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
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.request.CommitGridRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.server.request.SendISCRequest;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.internal.IFPClient;
import com.raytheon.viz.gfe.core.parm.Parm.InterpState;
import com.raytheon.viz.gfe.core.parm.ParmState.InterpMode;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;

/**
 * A ParmOp provides global functions to affect all parms.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/04/2008   1066       Dan Fitch   Initial Creation 
 * 11/11/2008   1666       njensen     Added procedure cmds
 * 06/24/2009   1876       njensen     Publish updates inventory
 * 02/23/2012   1876       dgilling    Implement missing clearUndoParmList
 *                                     function.
 * 02/13/2013   #1597      randerso    Added logging to support GFE Performance metrics
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1.0
 */

public class ParmOp {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParmOp.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    private static final int MAX_CONCURRENT_JOBS = 5;

    private IGridData copiedGrid;

    private final Set<Parm> undoParmList;

    private final DataManager dataManager;

    private TimeRange selectionTimeRange;

    /**
     * Constructor
     * 
     * @param dataManager
     */
    public ParmOp(DataManager dataManager) {
        this.undoParmList = new HashSet<Parm>();
        this.dataManager = dataManager;
    }

    /**
     * Removes a parm from the list that can be undone.
     * 
     * @param p
     *            <code>Parm</code> to remove from the undo list.
     */
    public void clearUndoParmList(Parm p) {
        undoParmList.remove(p);
    }

    /**
     * Resets the undo parm list. This is normally commanded by the edit tools
     * prior to an edit operation.
     * 
     */
    public void clearUndoParmList() {
        this.undoParmList.clear();
    }

    /**
     * Called when the user selects undo. Commands an undo to each parm in the
     * undo list.
     */
    public void undo() {
        Iterator<Parm> parmIterator = new ArrayList<Parm>(this.undoParmList)
                .iterator();
        while (parmIterator.hasNext()) {
            Parm p = parmIterator.next();
            p.undo();
        }
        return;
    }

    /**
     * Called when the user changes the selected VectorMode.
     */
    public void setVectorMode(ParmState.VectorMode mode) {
        Iterator<Parm> parmIterator = new ArrayList<Parm>(
                Arrays.asList(this.dataManager.getParmManager().getAllParms()))
                .iterator();
        while (parmIterator.hasNext()) {
            Parm p = parmIterator.next();
            p.getParmState().setVectorMode(mode);
        }
        return;
    }

    /**
     * Adds the parm to the list that can be undone. This is normally commanded
     * by the edit tools prior to an edit operation.
     * 
     * @param p
     *            the parm
     */
    public void setUndoParmList(Parm p) {
        this.undoParmList.add(p);
    }

    /**
     * Copies the specified grid into the copy/paste buffer.
     */
    public boolean copyGrid(Parm parm, Date date) {

        IGridData sourceGrid = parm.overlappingGrid(date);
        if (sourceGrid == null) {
            return false; // no grid at this spot
        }

        // deallocate any existing copied grid
        if (copiedGrid != null) {
            copiedGrid = null;
        }

        // make a new copy and store it.
        try {
            copiedGrid = sourceGrid.clone();
        } catch (CloneNotSupportedException e) {
            copiedGrid = null;
        }
        copiedGrid.resetSavePublishHistory();

        return true;
    }

    /**
     * Pastes the grid in the paste buffer to the specified parm/tr identified
     * by the GridID.
     * 
     * @param parm
     * @param date
     */
    public boolean pasteGrid(Parm parm, Date date) {
        // is it a compatible grid?
        if (!okToPasteGrid(parm, date)) {
            statusHandler.handle(Priority.PROBLEM,
                    "pasteGrid called when not ok to paste grid");
            return false;
        }

        // calculate the destination time range which is either 1 quantum,
        // if there isn't a grid there already, or the same size of the existing
        // grid if there is a grid there already.
        TimeRange tr;
        IGridData gd = parm.overlappingGrid(date);
        if (gd != null) {
            tr = gd.getGridTime();
        } else {
            tr = parm.getGridInfo().getTimeConstraints().constraintTime(date);
        }

        // we know it is okay, now we need to prepare the destination grid
        IGridData newGrid;
        try {
            newGrid = copiedGrid.clone();
        } catch (CloneNotSupportedException e1) {
            newGrid = null;
        }
        newGrid.changeValidTime(tr, false); // IGNORE RATE CHG, keep same data

        newGrid.updateHistoryToModified(this.dataManager.getWsId());

        IGridData tmpGridArray[] = new IGridData[1];
        tmpGridArray[0] = newGrid;
        // replace the grid (note newGrid's memory management is taken over
        // by the parm. Don't deallocate it here!
        if (!parm.replaceGriddedData(tr, tmpGridArray)) {
            statusHandler.handle(Priority.PROBLEM,
                    "pasteGrid failed in replaceGriddedData");
            return false;
        }

        return true;
    }

    /**
     * Checks if it is ok to paste the grid in the paste buffer into the
     * location identified by the GridID. Return true if it is okay to paste.
     * 
     * @param parm
     *            The location the grid will be pasted into.
     * @param date
     *            The time the grid will be pasted into.
     * @return True, if the grid can be pasted in the designated location and
     *         time. Else, false.
     */
    public boolean okToPasteGrid(Parm parm, Date date) {
        // verify we have a valid source grid
        if (copiedGrid == null || parm == null) {
            return false;
        }
        GridParmInfo sourceGPI = copiedGrid.getParm().getGridInfo();
        GridParmInfo destGPI = parm.getGridInfo();

        // System.out.println("Unit statement? "
        // + (!sourceGPI.getUnitObject().isCompatible(
        // destGPI.getUnitObject()) && !sourceGPI.getUnitObject()
        // .equals(destGPI.getUnitObject())));

        // verify compatible parms (types and units).
        if (!sourceGPI.getGridType().equals(destGPI.getGridType())
                || (!(sourceGPI.getUnitObject().isCompatible(
                        destGPI.getUnitObject()) && sourceGPI.getUnitObject()
                        .equals(destGPI.getUnitObject())))) {
            return false;// not compatible
        }

        // special check for DISCRETE, ok if discreteKeys match, and overlap
        // matches
        // TODO: When DISCRETE is supported port this code.
        // if (sourceGPI.getGridType() == destGPI.getGridType()
        // && sourceGPI.getGridType() == GridType.DISCRETE
        // && (sourceGPI.discreteKeys() != destGPI.discreteKeys()
        // || sourceGPI.overlapAllowedForDiscrete() !=
        // destGPI.overlapAllowedForDiscrete()))
        // return false;

        // calculate the destination time range which is either 1 quantum,
        // if there isn't a grid there already, or the same size of the existing
        // grid if there is a grid there already.
        TimeRange tr;
        IGridData gd = parm.overlappingGrid(date);
        if (gd != null) {
            tr = gd.getGridTime();
        } else {
            tr = parm.getGridInfo().getTimeConstraints().constraintTime(date);
        }

        // ok to edit?
        if (parm.isOkToEdit(tr)) {
            return true;
        } else {
            return false;
        }
    }

    private Parm mutableParmFromName(String parmNameAndLevel) {
        Parm p = this.dataManager.getParmManager().getParmInExpr(
                parmNameAndLevel, false,
                dataManager.getSpatialDisplayManager().getActivatedParm());

        return p;
    }

    /**
     * Create a grid from scratch
     * 
     * @param parmNameAndLevel
     * @param tr
     * @param repeatInterval
     * @param duration
     * @throws GFEOperationFailedException
     */
    public void createFromScratchCmd(String parmNameAndLevel, TimeRange tr,
            int repeatInterval, int duration)
            throws GFEOperationFailedException {

        Parm p = mutableParmFromName(parmNameAndLevel);

        if (p != null) {
            clearUndoParmList();
            p.createFromScratchTR(tr, Parm.CreateFromScratchMode.DEFAULT,
                    repeatInterval, duration);
        } else {
            String msg = "Procedure create from scratch failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }

    }

    /**
     * Create the selected grid from scratch
     * 
     * @param mode
     * @param repeatInterval
     * @param duration
     * @throws GFEOperationFailedException
     */
    public void createFromScratchSelected(Parm.CreateFromScratchMode mode,
            int repeatInterval, int duration)
            throws GFEOperationFailedException {
        Parm[] allParms = this.dataManager.getParmManager().getSelectedParms();

        for (Parm parm : allParms) {
            if (parm.isMutable()) {
                if (repeatInterval == 0 || duration == 0) {
                    TimeConstraints tc = parm.getGridInfo()
                            .getTimeConstraints();
                    parm.createFromScratchSelectedTR(mode,
                            tc.getRepeatInterval(), tc.getDuration());
                } else {
                    parm.createFromScratchSelectedTR(mode, repeatInterval,
                            duration);
                }
            }

        }
    }

    /**
     * This function is called when the interpolate command is selected. The
     * type of interpolation is passed to this routine as well as the async/sync
     * mode. This operation cannot be undone.
     * 
     * For each displayed parm that is selected and mutable call
     * interpolateSelectedTR. Sets the undo parm list to NULL. Does not affect
     * temporary parms.
     * 
     * @param interpMode
     * @param interpState
     * @param interval
     * @param duration
     */
    public void interpolateSelected(InterpMode interpMode,
            InterpState interpState, int interval, int duration) {
        perfLog.log("Interpolation started");
        Parm[] allParms = this.dataManager.getParmManager().getAllParms();
        for (Parm parm : allParms) {
            if (parm.getParmState().isSelected() && parm.isMutable()) {
                try {
                    parm.interpolateSelectedTR(interpMode, interpState,
                            interval, duration);
                } catch (GFEOperationFailedException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error replacing Gridded Data.", e);
                }
            }
        }
        clearUndoParmList();
        return;
    }

    /**
     * Break locks
     * 
     * @param parmId
     * @param times
     * @return if the lock was broken
     */
    public boolean breakLock(final ParmID parmId, final TimeRange... times) {
        // parm exists?
        Parm p = this.dataManager.getParmManager().getParm(parmId);
        if (p != null) {
            p.breakLock(times);
        }

        // parm not in existence
        else {
            List<LockRequest> lreq = new ArrayList<LockRequest>();
            for (TimeRange tr : times) {
                lreq.add(new LockRequest(parmId, tr, LockMode.BREAK_LOCK));
            }
            try {
                this.dataManager.getClient().requestLockChange(lreq);
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error attempting to break locks", e);
                return false;
            }
        }
        return true;
    }

    /**
     * Get the lock table for mutable db
     * 
     * @return the lock table
     * @throws GFEServerException
     */
    public List<LockTable> mutableDbLockTable() throws GFEServerException {
        if (!this.dataManager.getParmManager().getMutableDatabase().isValid()) {
            return null;
        }

        return this.dataManager.getClient().getLockTable(
                this.dataManager.getParmManager().getMutableDatabase());

    }

    /**
     * Publish
     * 
     * @param req
     */
    public void publish(List<CommitGridRequest> req) {
        CAVEMode mode = CAVEMode.getMode();
        if (mode.equals(CAVEMode.PRACTICE) || mode.equals(CAVEMode.TEST)) {
            statusHandler.handle(Priority.EVENTA, "PUBLISH Simulated. ");
            return;
        }

        if (req.size() == 0) {
            statusHandler
                    .handle(Priority.EVENTA, "PUBLISH: Nothing to publish");
            return;
        }

        // filter out some of the requests that are not desired
        final ConcurrentLinkedQueue<CommitGridRequest> requests = new ConcurrentLinkedQueue<CommitGridRequest>();
        for (CommitGridRequest curReq : req) {
            // adjust for server's grid inventory and expand tr to include the
            // entire grid
            TimeRange tr = curReq.getTimeRange();
            ParmID id = curReq.getParmId();
            List<TimeRange> inv;
            try {
                inv = dataManager.serverParmInventory(id);
                for (TimeRange invTR : inv) {
                    if (invTR != null && tr != null && invTR.overlaps(tr)) {
                        tr = tr.combineWith(invTR);
                    } else if (invTR.getStart().after(tr.getEnd())) { // efficiency
                        break;
                    }
                }
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to get server parm inventory", e);
                continue;
            }

            requests.add(new CommitGridRequest(id, tr, dataManager
                    .clientISCSendStatus()));
        }

        final ConcurrentLinkedQueue<ServerResponse<?>> okSrs = new ConcurrentLinkedQueue<ServerResponse<?>>();
        final AtomicBoolean allOk = new AtomicBoolean(true);
        final IFPClient client = this.dataManager.getClient();

        // spawn separate jobs
        final CountDownLatch latch = new CountDownLatch(MAX_CONCURRENT_JOBS);
        for (int i = 0; i < MAX_CONCURRENT_JOBS; i++) {
            new Job("Publishing Parms") {
                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    try {
                        CommitGridRequest req = null;
                        while ((req = requests.poll()) != null) {
                            try {
                                ServerResponse<?> sr = client
                                        .commitGrid(Arrays
                                                .asList(new CommitGridRequest[] { req }));
                                if (sr.isOkay()) {
                                    okSrs.add(sr);
                                } else {
                                    allOk.set(false);
                                    statusHandler.handle(Priority.PROBLEM,
                                            "PUBLISH problem: Unable to publish grid "
                                                    + sr.toString());
                                }
                            } catch (GFEServerException e) {
                                allOk.set(false);
                                statusHandler.handle(Priority.PROBLEM,
                                        "PUBLISH problem: Unable to publish grids. "
                                                + e.getLocalizedMessage(), e);
                            }
                        }
                    } finally {
                        latch.countDown();
                    }

                    return Status.OK_STATUS;
                }
            }.schedule();
        }

        try {
            latch.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        if (allOk.get()) {
            statusHandler.handle(Priority.EVENTA,
                    "PUBLISH: Publish operation completed " + okSrs);
        } else {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "PUBLISH: Publish operation completed with errors.  See previous message for errors.  Successful grids: "
                                    + okSrs);
        }
    }

    /**
     * Selects all of the displayed parms
     */
    public void selectAll() {
        Parm parms[] = dataManager.getParmManager().getDisplayedParms();
        for (Parm parm : parms) {
            parm.getParmState().setSelected(true);
        }
    }

    /**
     * Deselects all of the displayed parms
     */
    public void deselectAll() {
        Parm parms[] = dataManager.getParmManager().getDisplayedParms();
        for (Parm parm : parms) {
            parm.getParmState().setSelected(false);
        }
        setSelectionTimeRange(new TimeRange());
    }

    /**
     * Sets the selected time range on all of the parms.
     * 
     * @param timeRange
     */
    public void setSelectionTimeRange(TimeRange timeRange) {
        selectionTimeRange = timeRange;
        Parm[] allParms = dataManager.getParmManager().getAllParms();
        for (Parm parm : allParms) {
            parm.getParmState().updateSelectedTimeRange(timeRange);
        }
        dataManager.getSpatialDisplayManager().setGlobalTimeRange(
                selectionTimeRange);
    }

    /**
     * Sets the combine mode for all parms.
     * 
     * @param editMode
     */
    public void setCombineMode(ParmState.CombineMode editMode) {
        Parm allParms[] = dataManager.getParmManager().getAllParms();
        for (Parm parm : allParms) {
            parm.getParmState().setCombineMode(editMode);
        }

        // WeatherCombineModeChangedMsg::send(_msgHand, editMode);
        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);

        service.refreshElements("com.raytheon.viz.gfe.actions.setCombineMode",
                null);

        return;
    }

    /**
     * Get selection time range
     * 
     * @return the selection time range
     */
    public TimeRange getSelectionTimeRange() {
        return this.selectionTimeRange;
    }

    /**
     * Copy all the parms from another database into the mutable database
     * 
     * @param model
     *            the model to copy from
     */
    public void copyEverythingFrom(final DatabaseID model) {
        clearUndoParmList();
        Parm[] allParms = this.dataManager.getParmManager().getAllParms();

        // Get the parms that are mutable.
        final ConcurrentLinkedQueue<Parm> mutableParms = new ConcurrentLinkedQueue<Parm>();
        for (Parm parm : allParms) {
            if (parm.isMutable()) {
                mutableParms.add(parm);
            }
        }

        // spawn separate jobs
        final CountDownLatch latch = new CountDownLatch(MAX_CONCURRENT_JOBS);
        final IParmManager parmMgr = this.dataManager.getParmManager();
        for (int i = 0; i < MAX_CONCURRENT_JOBS; i++) {
            new Job("Populating Parms") {
                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    try {
                        // Now find the parms that match the specified
                        // DatabaseID, determine if they exist, or create them
                        // if necessary. It is possible to attempt to create a
                        // parm that doesn't exist.
                        Parm parm = null;
                        while ((parm = mutableParms.poll()) != null) {
                            // attempt a lookup, or create one if necessary
                            ParmID sourceId = new ParmID(parm.getParmID()
                                    .getParmName(), model, parm.getParmID()
                                    .getParmLevel());
                            Parm source = parmMgr.getParm(sourceId);

                            if (source == null) {
                                source = parmMgr.createParm(sourceId, false,
                                        false);
                            }

                            if (source != null) {
                                parm.copyEverythingFrom(source);
                            }
                        }
                    } finally {
                        latch.countDown();
                    }

                    return Status.OK_STATUS;
                }
            }.schedule();
        }

        try {
            latch.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // reset the selection state and selection time ranges for each
        // successful copy. Assumes that the undo list has been set by
        // each parm.
        deselectAll();
        for (Parm parm : undoParmList) {
            parm.getParmState().setSelected(true);
            UndoBuffer[] undoBuf = parm.getUndoBuffer();
            if (undoBuf.length > 0) {
                parm.getParmState().updateSelectedTimeRange(
                        undoBuf[0].getUndoTimeRange());
            }
        }
    }

    /**
     * Copy selected grids from a specified database to the mutable database
     * 
     * @param model
     *            the model to copy from
     */
    public void copySelectedFrom(final DatabaseID model) {
        clearUndoParmList();
        Parm[] allParms = this.dataManager.getParmManager().getAllParms();

        // Get the parms that are mutable.
        final ConcurrentLinkedQueue<Parm> parms = new ConcurrentLinkedQueue<Parm>();

        for (Parm parm : allParms) {
            if (parm.isMutable() && parm.getParmState().isSelected()) {
                parms.add(parm);
            }
        }

        // spawn separate jobs
        final CountDownLatch latch = new CountDownLatch(MAX_CONCURRENT_JOBS);
        final IParmManager parmMgr = this.dataManager.getParmManager();
        for (int i = 0; i < MAX_CONCURRENT_JOBS; i++) {
            new Job("Populating Parms") {
                @Override
                protected IStatus run(IProgressMonitor monitor) {
                    try {
                        // Now find the parms that match the specified
                        // DatabaseID, determine
                        // if they exist, or create them if necessary. It is
                        // possible
                        // to attempt to create a parm that doesn't exist.
                        // mutableParms and sourceParmIDs are parallel arrays
                        Parm parm = null;
                        while ((parm = parms.poll()) != null) {
                            // attempt a lookup, or create one if necessary
                            ParmID sourceId = new ParmID(parm.getParmID()
                                    .getParmName(), model, parm.getParmID()
                                    .getParmLevel());
                            Parm source = parmMgr.getParm(sourceId);

                            boolean created = false;
                            if (source == null) {
                                source = parmMgr
                                        .addParm(sourceId, false, false);
                                created = true;
                            }

                            if (source != null) {
                                parm.copySelectedTRFrom(source);
                            }
                            if (created && source != null) {
                                parmMgr.deleteParm(source);
                            }
                        }
                    } finally {
                        latch.countDown();
                    }

                    return Status.OK_STATUS;
                }
            }.schedule();
        }

        try {
            latch.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    /**
     * Copies the parm identified by parmNameAndLevel and id into the mutable
     * database over the given time range.
     * 
     * @param parmNameAndLevel
     * @param id
     * @param tr
     * @throws GFEOperationFailedException
     */
    public void copyCmd(String parmNameAndLevel, DatabaseID id, TimeRange tr)
            throws GFEOperationFailedException {
        copyToCmd(parmNameAndLevel, parmNameAndLevel, id, tr);
    }

    /**
     * Copies the parm identified by srcParmName and id into the mutable
     * database and parm specified by dstParmName over the given time range.
     * 
     * @param srcParmNameAndLevel
     * @param dstParmNameAndLevel
     * @param id
     * @param tr
     * @throws GFEOperationFailedException
     */
    public void copyToCmd(String srcParmNameAndLevel,
            String dstParmNameAndLevel, DatabaseID id, TimeRange tr)
            throws GFEOperationFailedException {
        Parm p = mutableParmFromName(dstParmNameAndLevel);
        if (p == null) {
            String msg = "Procedure copy failed. Unknown name="
                    + dstParmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
        // boolean addedToCache = false;
        IParmManager parmMgr = dataManager.getParmManager();
        // if
        // (parmMgr.getcachedParmList().find(p.getParmID().getCompositeName())
        // == -1)
        // {
        // addedToCache = true;
        // _parmMgr->addCachedParmList(p->parmID().compositeName());
        // }

        // determine the source parm and create it if necessary
        String[] composite = ParmID.parmNameAndLevel(srcParmNameAndLevel);
        ParmID sourceID = new ParmID(composite[0], id, composite[1]);
        Parm source = parmMgr.getParm(sourceID);
        boolean created = false;
        if (source == null) {
            source = parmMgr.addParm(sourceID, false, false);
            created = true;
        }
        if (source != null) {
            clearUndoParmList();
            p.copyTRFrom(source, tr); // perform the copy
        }
        if (created && source != null) {
            parmMgr.deleteParm(source);
        }
        // if (addedToCache)
        // {
        // parmMgr.removeCachedParmList(p.getParmID().getCompositeName());
        // }
    }

    /**
     * Deletes grids from the mutable database's parm identified by
     * parmNameAndLevel over the given time range.
     * 
     * @param parmNameAndLevel
     * @param tr
     * @throws GFEOperationFailedException
     */
    public void deleteCmd(String parmNameAndLevel, TimeRange tr)
            throws GFEOperationFailedException {
        Parm p = mutableParmFromName(parmNameAndLevel);
        if (p != null) {
            clearUndoParmList();
            p.deleteTR(tr);
        } else {
            String msg = "Procedure delete failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
    }

    /**
     * Zeros out grids from the mutable database's parm identified by
     * parmNameAndLevel over the given time range.
     * 
     * @param parmNameAndLevel
     * @param tr
     * @throws GFEException
     */
    public void zeroCmd(String parmNameAndLevel, TimeRange tr)
            throws GFEException {
        Parm p = mutableParmFromName(parmNameAndLevel);
        if (p != null) {
            clearUndoParmList();
            p.zeroTR(tr);
        } else {
            String msg = "Procedure zero failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
    }

    /**
     * Performs an interpolation for the mutable databases's parmNameAndLevel
     * over the specified time range. The interpolation mode, interpolation
     * state, and interpolation interval control the type of interpolation.
     * 
     * @param parmNameAndLevel
     * @param tr
     * @param interpMode
     * @param interpState
     * @param interval
     * @param duration
     * @throws GFEOperationFailedException
     */
    public void interpolateCmd(String parmNameAndLevel, TimeRange tr,
            String interpMode, String interpState, int interval, int duration)
            throws GFEOperationFailedException {
        Parm p = mutableParmFromName(parmNameAndLevel);
        if (p != null) {
            clearUndoParmList();
            InterpMode mode = InterpMode.valueOf(interpMode);
            InterpState state = InterpState.valueOf(interpState);
            p.interpolateTR(tr, mode, state, interval, duration);
        } else {
            String msg = "Procedure interpolate failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
    }

    /**
     * Performs a time shift on the set of mutable and selected parameters. The
     * secondsToShift must be a multiple of the split boundary repeat interval.
     * If copyOnly is true, then a copy is made of the data before shifting. If
     * copyOnly is false, then the original data is moved.
     * 
     * Call timeShiftSelectedTR for each displayed and mutable parm that is
     * selected. Does not affect temporary parms.
     * 
     * @param secondsToShift
     * @param copyOnly
     */
    public void timeShift(int secondsToShift, boolean copyOnly) {
        clearUndoParmList();
        Parm[] allParms = this.dataManager.getParmManager().getAllParms();
        for (Parm parm : allParms) {
            if (parm.getParmState().isSelected() && parm.isMutable()) {
                parm.timeShiftSelectedTR(secondsToShift, copyOnly);
            }
        }
    }

    /**
     * Time shifts grids from the mutable database identified by
     * parmNameAndLevel over the source time range. Shifts "secondsToShift". If
     * copyOnly is true, then this is a copy command, else it is a move
     * operation.
     * 
     * @param parmNameAndLevel
     * @param tr
     * @param copyOnly
     * @param secondsToShift
     * @throws GFEOperationFailedException
     */
    public void timeShiftCmd(String parmNameAndLevel, TimeRange tr,
            boolean copyOnly, int secondsToShift)
            throws GFEOperationFailedException {
        Parm p = mutableParmFromName(parmNameAndLevel);
        if (p != null) {
            clearUndoParmList();
            p.timeShiftTR(tr, secondsToShift, copyOnly);
        } else {
            String msg = "Procedure timeShift failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
    }

    /**
     * Split grids from the mutable database's parm identified by
     * parmNameAndLevel over the given time range.
     * 
     * @param parmNameAndLevel
     * @param tr
     * @throws GFEOperationFailedException
     */
    public void splitCmd(String parmNameAndLevel, TimeRange tr)
            throws GFEOperationFailedException {
        Parm p = mutableParmFromName(parmNameAndLevel);
        if (p != null) {
            clearUndoParmList();
            p.splitTR(tr);
        } else {
            String msg = "Procedure split failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
    }

    /**
     * Fragments grids from the mutable database's parm identified by
     * parmNameAndLevel over the given time range.
     * 
     * @param parmNameAndLevel
     * @param tr
     * @throws GFEOperationFailedException
     */
    public void fragmentCmd(String parmNameAndLevel, TimeRange tr)
            throws GFEOperationFailedException {
        Parm p = mutableParmFromName(parmNameAndLevel);
        if (p != null) {
            clearUndoParmList();
            p.fragmentTR(tr);
        } else {
            String msg = "Procedure fragment failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
    }

    /**
     * This function is called whenever the user selects the fragment command
     * from the menu. Call fragmentSelectedTR for every parm that is selected.
     * 
     */
    public void fragmentSelected() {
        clearUndoParmList();
        Parm[] allParms = this.dataManager.getParmManager().getAllParms();
        for (Parm parm : allParms) {
            if (parm.getParmState().isSelected() && parm.isMutable()) {
                parm.fragmentSelectedTR();
            }
        }
    }

    /**
     * This function is called whenever the user selects the split command from
     * the edit menu. Call splitSelectedTR for every parm that is selected.
     * 
     */
    public void splitSelected() {
        clearUndoParmList();
        Parm[] allParms = this.dataManager.getParmManager().getAllParms();
        for (Parm parm : allParms) {
            if (parm.getParmState().isSelected() && parm.isMutable()) {
                parm.splitSelectedTR();
            }
        }
    }

    /**
     * Deletes grids from the mutable database's parm identified by
     * parmNameAndLevel over the given time range. The value assigned is
     * specified as a WxValue.
     * 
     * @param parmNameAndLevel
     * @param tr
     * @param wxValue
     * @throws GFEOperationFailedException
     */
    public void assignValueCmd(String parmNameAndLevel, TimeRange tr,
            WxValue wxValue) throws GFEOperationFailedException {
        Parm p = mutableParmFromName(parmNameAndLevel);
        if (p != null) {
            clearUndoParmList();
            p.assignValueTR(tr, wxValue);
        } else {
            String msg = "Procedure assignValue failed. Unknown name="
                    + parmNameAndLevel;
            throw new GFEOperationFailedException(msg);
        }
    }

    /**
     * This function is called to send grids.
     * 
     * @param req
     */
    public void sendISC(List<SendISCRequest> req) {
        CAVEMode mode = CAVEMode.getMode();

        if (mode.equals(CAVEMode.PRACTICE) || mode.equals(CAVEMode.TEST)) {
            statusHandler.handle(Priority.EVENTA, "SEND ISC Simulated. ");
            return;
        }

        List<SendISCRequest> requests = new ArrayList<SendISCRequest>();

        // check for auto - mode, single req with NULL values.
        if (req.size() == 1 && req.get(0).getTimeRange() == null
                && req.get(0).getParmId() == null) {
            requests = req;
        }

        // manual mode -- lots of checking required
        else {
            PythonPreferenceStore store = Activator.getDefault()
                    .getPreferenceStore();
            // determine time limits for skipping, round to the hour
            long current = (Util.getUnixTime(SimulatedTime.getSystemTime()
                    .getTime()) / 3600) * 3600;
            TimeRange limitTime = TimeRange.allTimes();

            int pastHours = 0;
            if (store.getInt("ISC_sendLimitBeforeCurrentTime") != 0) {
                limitTime = new TimeRange(current - (pastHours * 3600),
                        limitTime.getEnd().getTime());
            }

            int futureHours = 7 * 24;
            if (store.getInt("ISC_sendLimitAfterCurrentTime") != 0) {
                limitTime = new TimeRange(limitTime.getStart().getTime(),
                        current + (futureHours * 3600));
            }

            // determine parms to skip
            List<String> skipParms = Arrays.asList(store
                    .getStringArray("ISC_neverSendParms"));

            // filter out some of the requests that are not desired
            for (int i = 0; i < req.size(); i++) {
                // check parm name
                if (skipParms.contains(req.get(i).getParmId().getParmName())) {
                    continue;
                }

                // adjust time limits
                TimeRange tr = req.get(i).getTimeRange()
                        .intersection(limitTime);
                if (tr.equals(new TimeRange())) {
                    continue;
                }

                // now adjust for server's grid inventory and expand tr to
                // include the entire grid
                List<TimeRange> inv = new ArrayList<TimeRange>();
                try {
                    inv = dataManager.serverParmInventory(req.get(i)
                            .getParmId());
                } catch (GFEServerException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Unable to get server parm inventory: "
                                    + e.getLocalizedMessage(), e);
                }
                for (int j = 0; j < inv.size(); j++) {
                    if (inv.get(j).overlaps(tr)) {
                        tr = tr.combineWith(inv.get(j));
                    } else if (inv.get(j).getStart().after(tr.getEnd())) { // efficienty
                        continue;
                    }
                }

                requests.add(new SendISCRequest(req.get(i).getParmId(), tr));
            }

        }
        ServerResponse<?> sr = null;
        try {
            sr = dataManager.getClient().sendISC(requests);
            if (!sr.isOkay()) {
                statusHandler.handle(Priority.PROBLEM,
                        "SEND ISC problem: Unable to send ISC grids. " + sr);
            }
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "SEND ISC problem: Unable to send ISC grids. " + sr);
        }
    }

    public void setSmoothSize(int smoothSize) {
        Parm[] allParms = this.dataManager.getParmManager().getAllParms();

        for (Parm parm : allParms) {
            parm.getParmState().setSmoothSize(smoothSize);
        }
    }

    public void saveAllParameters(boolean undisplayed, boolean displayed) {
        if (displayed) {
            Parm[] parms = dataManager.getParmManager().getDisplayedParms();
            for (Parm p : parms) {
                if (p.isModified()) {
                    p.saveParameter(true);
                }
            }
        }

        if (undisplayed) {
            Parm[] parms = dataManager.getParmManager().getUndisplayedParms();
            for (Parm p : parms) {
                if (p.isModified()) {
                    p.saveParameter(true);
                }
            }
        }
    }
}
