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

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteDefinition;
import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DByte;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.grid.Op;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.Lock;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockMode;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable.LockStatus;
import com.raytheon.uf.common.dataplugin.gfe.server.request.LockRequest;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IContinuousSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherKey;
import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.util.RWLArrayList;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEException;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.parm.ParmSaveJob.ParmSaveStatus;
import com.raytheon.viz.gfe.core.parm.ParmState.InterpMode;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.interpolation.Interpolator;
import com.raytheon.viz.gfe.types.MutableInteger;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * The Parm class is the fundamental object of the entire data subsystem.
 * 
 * All meteorological data for a particular weather element are contain within
 * the parm along with many other attributes such as VisualType, graphic color,
 * edit modes, selected state, and color tables. Most of this information is
 * available via the public interface.
 * 
 * Parms are created and managed by the ParmManager. Objects outside the data
 * subsystem get at parm information through the ParmManager.
 * 
 * There are three levels of editing on this system:
 * <OL>
 * <LI>Selected state grid manipulation
 * <LI>Single Parm Grid manipulation
 * <LI>Direct Grid Editing
 * </OL>
 * 
 * 
 * 
 * Selected state grid manipulation:
 * 
 * The user must use the functions in ParmManager to perform the selected state
 * grid manipulation tasks. The user must NOT use the parm functions to directly
 * perform this since a) selection state is not checked, b) the undo list of
 * parms in ParmManager will not be updated properly. These functions are
 * deleteSelected(), splitSelected(), copySelectedFrom(), copyEverythingFrom(),
 * timeShift(), fragmentSelected(), zeroSelected(), and interpolateSelected().
 * The user simply calls these functions in the ParmManager to perform the
 * operation. Note that when not using the gfe and undo is not required, the
 * deleteSelectedTR(), splitSelectedTR(), and timeShiftSelectedTR() routines in
 * parm may be called directly; however, the user must first check for the
 * selected state. The data manager's setUndoParmList() is handled within the
 * data manager.
 * 
 * 
 * 
 * Single Parm Grid manipulation
 * 
 * The user must use the functions in Parm to perform the single parm grid
 * manipulation tasks. There are no equivalent functions in ParmManager. The
 * user should NOT call startParmEdit() or endParmEdit() in conjunction with
 * this call. The function is: replaceGriddedData(). replaceGriddedData() can be
 * used in two ways: intra-parm copies, and inter-parm copies. Intra-parm copies
 * simply have the grid data pointers all from "this" parm. Inter-parm copies
 * have grid pointers from another parm. The units are checked for validity. The
 * undo capability is built-into this command, i.e., there is no need to call
 * the data manager's setUndoParmList(), or clearUndoParmList().
 * 
 * 
 * 
 * Direct Grid Editing
 * 
 * Before the set of grids are to be edited, the data manager's
 * clearUndoParmList() should be called to reset the undo list. For each parm to
 * be edited, call its startParmEdit() function. This function returns a pointer
 * to the GridData to be edited. If NULL is returned, then do NOT continue the
 * edit. Call the appropriate editing function in GridData or TimeSeriesData.
 * When finished editing each parm, call endParmEdit(). There is no need to call
 * the data manager's setUndoParmList() as this is implicit in the
 * startParmEdit().
 * 
 * Note that only ONE startParmEdit() should be called without a corresponding
 * endParmEdit(). Also extendParmEdit() should not be called unless a
 * startParmEdit() was called first.
 * 
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/29/2008              chammack    Initial creation of skeleton.
 * 04/01/2008   879        rbell       Implement TR functions
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 05Aug2008    #1383       ebabin      Fix for time shift not working.
 * 12Sep2008    #1332      wdougherty  Added deleteLockedTR() for editing hazards.
 * 04/16/2009   #2262      rjpeter     Updated pencilStretch to return Grid2DBit.
 * 06/08/2009   #2159      rjpeter     Fixed undo.
 * 02/23/2012   #346       dgilling    Implement a dispose method to mimic 
 *                                     AWIPS1 use of C++ destructor.
 * 03/02/2012   #346       dgilling    Create a disposed flag to help ensure
 *                                     no interaction with Parms after dispose
 *                                     is called.
 * 02/13/13     #1597      randerso    Removed debug logging to improve performance
 * Mar 13, 2013 1792       bsteffen    Improve performance of gfe parm average
 *                                     ant time weighted average.
 * Apr 02, 2013 #1774      randerso    Fixed a possible deadlock issue.
 * Aug 27, 2013 #2302      randerso    Fix simultaneous save issue
 * Oct 31, 2013 #2508      randerso    Change to use DiscreteGridSlice.getKeys()
 * Jun 30, 2014 #3332      randerso    Kept local reference to lock table to avoid 
 *                                     race conditions with asynchronous updates
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public abstract class Parm implements Comparable<Parm> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(Parm.class);

    private final IPerformanceStatusHandler perfLog = PerformanceStatus
            .getHandler("GFE:");

    protected RWLArrayList<IGridData> grids;

    protected ParmState parmState;

    protected boolean mutable;

    private boolean ignoreLocks;

    protected boolean hidden;

    protected List<Date> timesBeingEdited;

    protected GridParmInfo gridInfo;

    protected boolean inParmEdit;

    protected boolean inTSEdit;

    protected ParmDisplayAttributes displayAttributes;

    protected List<UndoBuffer> undoBuffers;

    protected DataManager dataManager;

    protected ParmListeners parmListeners;

    private Interpolator interpolator;

    protected LockTable lockTable;

    protected String officeType;

    protected boolean disposed;

    private ParmSaveJob saveJob;

    /**
     * The create from scratch mode
     */
    public static enum CreateFromScratchMode {

        DEFAULT, PICKUP
    };

    /**
     * The interpolation state
     */
    public static enum InterpState {
        ASYNC, SYNC
    };

    private static class InterpolateJob extends Job {
        private static final int MAX_JOBS = 2;

        private static class Request {
            Parm parm;

            public Request(Parm parm) {
                this.parm = parm;
            }
        }

        private static BlockingQueue<Request> requestQueue = new ArrayBlockingQueue<Parm.InterpolateJob.Request>(
                50);

        private static List<InterpolateJob> jobs;
        static {
            jobs = new ArrayList<Parm.InterpolateJob>(MAX_JOBS);
            for (int i = 0; i < MAX_JOBS; i++) {
                jobs.add(new InterpolateJob());
            }
        }

        public InterpolateJob() {
            super("");
            setSystem(false);
        }

        public static void request(Parm parm) {
            Request req = new Request(parm);
            try {
                requestQueue.put(req);
                for (InterpolateJob job : jobs) {
                    if (job.getState() == Job.NONE) {
                        job.schedule();
                        break;
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Exception queuing interpolation request for " + parm,
                        e);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            Request req = null;
            try {
                req = requestQueue.poll();
                if (req != null) {
                    this.setName("Interpolating "
                            + req.parm.getParmID().getParmName() + "...");
                    req.parm.doInterpolation(false);
                    this.schedule();
                }
                return Status.OK_STATUS;
            } catch (Exception e) {
                String msg = "Exception during interpolation";
                if (req != null) {
                    msg += "of " + req.parm;
                }
                return new Status(IStatus.ERROR, Activator.PLUGIN_ID, msg, e);
            }
        }
    }

    /**
     * Constructor
     * 
     * @param parmID
     * @param gridInfo
     * @param mutable
     * @param displayable
     * @param manager
     */
    protected Parm(ParmID parmID, GridParmInfo gridInfo, boolean mutable,
            boolean displayable, DataManager manager) {
        this.gridInfo = gridInfo;
        this.gridInfo.setParmID(parmID);
        this.mutable = mutable;
        this.ignoreLocks = false;
        this.dataManager = manager;
        this.grids = new RWLArrayList<IGridData>();
        this.timesBeingEdited = new ArrayList<Date>();
        this.undoBuffers = new ArrayList<UndoBuffer>();
        this.parmState = new ParmState(this);

        this.displayAttributes = new ParmDisplayAttributes(displayable, this);
        this.parmListeners = new ParmListeners(this.dataManager
                .getParmManager().getNotificationPool());

        // Construct an empty lock table
        // Subclasses who utilize locks will override this
        this.lockTable = new LockTable();

        // determine the office type appropriate for this Parm
        String parmName = parmID.getParmName();
        List<String> knownOfficeTypes = dataManager.knownOfficeTypes();
        for (String ot : knownOfficeTypes) {
            if (parmName.endsWith(ot)) {
                this.officeType = ot; // match found
                break;
            }
        }
        // assume our own office type
        if (this.officeType == null) {
            this.officeType = dataManager.getOfficeType();
        }

        this.disposed = false;
        this.saveJob = new ParmSaveJob(this);
    }

    public void dispose() {
        synchronized (this) {
            this.disposed = true;
        }

        if (isModified()) {
            statusHandler.warn("Destroying parm " + getParmID().toString()
                    + " with modified data.");
        }

        if (dataManager.getParmOp() != null) {
            dataManager.getParmOp().clearUndoParmList(this);
        }

        // cleanup interpolator stuff
        finishInterpolation(true);

        // Once this is done we had better not talk to any of the parm clients
        // again...
        parmListeners.clearParmListeners();

        // Remove the undo grids
        purgeUndoGrids();

        // remove all grids
        grids.acquireWriteLock();
        try {
            grids.clear();
        } finally {
            grids.releaseWriteLock();
        }

        return;
    }

    /**
     * Return the associated data manager
     * 
     * @return the datamanager
     */
    public DataManager getDataManager() {
        return this.dataManager;
    }

    /**
     * Set the grid mutable/immutable
     * 
     * TODO: this really shouldn't be done, but is here for legacy reasons.
     * 
     * @param isMutable
     *            the mutable flag
     */
    public void setMutable(boolean isMutable) {
        this.mutable = isMutable;
    }

    /**
     * Locks the particular grid, returns true if successful.
     * 
     * Checks the lock status for the grid's time range. We request a lock if we
     * don't already have one. If we cannot get a lock, then we return false.
     * 
     * @param tr
     * @return
     */
    public boolean forceLockTR(final TimeRange tr) {
        LockStatus lockStatus = this.lockTable.checkLock(tr);
        // request a lock
        if (!lockStatus.equals(LockStatus.LOCKED_BY_ME)) {
            LockRequest lr = new LockRequest(getParmID(), tr, LockMode.LOCK);
            if (!requestLock(lr)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to grant lock request for "
                                + getParmID().getShortParmId() + ' ' + tr);
                return false;
            }
        }

        return true;
    }

    /**
     * Request a lock
     * 
     * @param lreq
     * @return
     */
    protected abstract boolean requestLock(List<LockRequest> lreq);

    /**
     * Request a lock
     * 
     * @param lr
     * @return
     */
    protected boolean requestLock(LockRequest lr) {
        List<LockRequest> lreq = new ArrayList<LockRequest>();
        lreq.add(lr);
        return requestLock(lreq);
    }

    /**
     * NOTE: Formerly isModified().
     * 
     * @return true, if is locked
     */
    public boolean isLocked() {
        if (lockTable.lockedByMe().size() != 0) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * NOTE: Formerly isModified(TimeRange).
     * 
     * @param timeRange
     *            the time range
     * 
     * @return true, if is locked
     */
    public boolean isLocked(TimeRange timeRange) {
        List<TimeRange> locks = lockTable.lockedByMe();
        for (int i = 0; i < locks.size(); i++) {
            if (locks.get(i).overlaps(timeRange)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Is ISC Parm?
     * 
     * @return the iscParm
     */
    public boolean isIscParm() {
        return this.getParmState().isIscParm();
    }

    /**
     * Gets the parm state.
     * 
     * @return the parm state
     */
    public ParmState getParmState() {
        return this.parmState;
    }

    /**
     * Is Mutable?
     * 
     * @return the mutable
     */
    public boolean isMutable() {
        return this.mutable;
    }

    public boolean ignoreLocks() {
        return this.ignoreLocks;
    }

    public void setIgnoreLocks(boolean ignoreLocks) {
        this.ignoreLocks = ignoreLocks;
    }

    /**
     * Is Hidden?
     * 
     * @return the hidden
     */
    public boolean isHidden() {
        return hidden;
    }

    /**
     * Gets the grid info.
     * 
     * @return the grid info
     */
    public GridParmInfo getGridInfo() {
        return this.gridInfo;
    }

    /**
     * @return the displayAttributes
     */
    public ParmDisplayAttributes getDisplayAttributes() {
        return displayAttributes;
    }

    /**
     * @return the parmID
     */
    public ParmID getParmID() {
        return gridInfo.getParmID();
    }

    /**
     * Gets the grid inventory.
     * 
     * @return the grid inventory
     */
    public IGridData[] getGridInventory() {

        this.grids.acquireReadLock();
        try {
            return grids.toArray(new IGridData[grids.size()]);
        } finally {
            this.grids.releaseReadLock();
        }
    }

    /**
     * Returns the time range associated with the entire set of grids
     * 
     * @return time range
     */
    public TimeRange getInventorySpan() {
        // Returns the time range calculated from the start time of grid#0 and
        // the ending time of the last grid.

        this.grids.acquireReadLock();
        try {
            if ((this.grids.size() > 0)
                    && !getGridInfo().isTimeIndependentParm()) {
                return new TimeRange(
                        this.grids.get(0).getGridTime().getStart(), this.grids
                                .get(this.grids.size() - 1).getGridTime()
                                .getEnd());
            }
        } finally {
            this.grids.releaseReadLock();
        }

        return new TimeRange();
    }

    /**
     * Returns the time range that encompasses the data inventory plus the
     * current set of locks. The locks are only considered for mutable parms.
     * 
     * @return the parm time range
     */
    public TimeRange getParmTimeRange() {
        if (this.getGridInfo().isTimeIndependentParm()) {
            return new TimeRange();
        }

        TimeRange tr = getInventorySpan();

        if (isMutable()) {
            LockTable lt = this.lockTable;
            if (lt != null) {
                List<Lock> locks = lt.getLocks();
                for (Lock lock : locks) {
                    tr = tr.combineWith(lock.getTimeRange());
                }
            }
        }

        return tr;
    }

    /**
     * Routine to break the specified locks (identified by time and owner)
     * 
     * @param lockTimes
     *            The times for which to break the locks
     * @return
     */
    public boolean breakLock(TimeRange... lockTimes) {

        if (lockTimes.length == 0) {
            return true; // nothing to do
        }

        // assemble the list of locks to break
        List<LockRequest> lreq = new ArrayList<LockRequest>();
        for (TimeRange tr : lockTimes) {
            lreq.add(new LockRequest(getParmID(), tr, LockMode.BREAK_LOCK));
        }

        // break them
        if (requestLock(lreq)) {
            return true;
        }
        return false;
    }

    /**
     * Creates a corresponding grid (interpolated) for the given time range.
     * Returns the pointer(s) to the newly created grids. If a grid already
     * exists over the time range, then the existing inventory for the time
     * range is returned instead of a newly created grid.
     * 
     * @param timeRange
     * @return
     * @throws GFEOperationFailedException
     */
    public IGridData[] createCorrespondingGrids(TimeRange timeRange)
            throws GFEOperationFailedException {
        IGridData[] newGrid = getGridInventory(timeRange);
        if (newGrid.length > 0) {
            return newGrid;
        }

        // set up an interpolation request, make this a mutable parm, and
        // save the current grid parm info.
        boolean mutableP = this.isMutable();
        GridParmInfo gpi = this.gridInfo;
        if (!mutableP) {
            setMutable(true);
            // allow every hour
            TimeConstraints tc = new TimeConstraints(3600, 3600, 0);
            gridInfo = new GridParmInfo(getParmID(),
                    getGridInfo().getGridLoc(), gpi.getGridType(),
                    gpi.getUnitString(), gpi.getDescriptiveName(),
                    gpi.getMinValue(), gpi.getMaxValue(), gpi.getPrecision(),
                    gpi.isTimeIndependentParm(), tc, gpi.isRateParm());
            setIgnoreLocks(true);
        }

        // interpolate quietly
        interpolateTRInternal(timeRange, InterpMode.GAPS, InterpState.SYNC, 0,
                0, true);

        // reset the mutable flag if necessary, and cancel the lock we
        // created if we are on an immutable database
        if (!mutableP) {
            looseLocks(); // drop all locks
            gridInfo = gpi; // reset the grid parm info
            setMutable(false);
            setIgnoreLocks(false);
        }

        // now get the grids that were created and return the inventory
        return getGridInventory(timeRange);
    }

    /**
     * Save a parameter
     * 
     * @param all
     * @return true if save successful
     */
    public boolean saveParameter(boolean all) {
        if (inParmEdit) {
            return false;
        } else {
            List<TimeRange> myLocks = lockTable.lockedByMe();
            if (all) {
                return saveParameter(myLocks);
            } else if (!myLocks.isEmpty()) {
                return saveParameter(myLocks.get(0));
            }

        }

        return true;
    }

    /**
     * Save a parameter
     * 
     * @param times
     *            TimeRanges to be saved
     * @return true if save successful
     */
    public boolean saveParameter(List<TimeRange> times) {
        if (inParmEdit) {
            return false;
        } else {
            ParmSaveStatus status = this.saveJob.requestSave(times);
            return status.isSuccessful();
        }
    }

    /**
     * Save a parameter
     * 
     * @param tr
     * @return true if save successful
     */
    public boolean saveParameter(TimeRange tr) {
        ArrayList<TimeRange> trs = new ArrayList<TimeRange>();
        trs.add(tr);
        return saveParameter(trs);
    }

    /**
     * Subclass specific save implementation
     * 
     * @param tr
     *            TimeRanges to be saved
     * @return true if save successful
     */
    protected abstract boolean saveParameterSubClass(List<TimeRange> tr);

    /**
     * Returns a list of grids that overlap the given time range.
     * 
     * @param timeRange
     *            the range to search
     * 
     * @return an array of grid data
     */
    public IGridData[] getGridInventory(TimeRange timeRange) {
        List<IGridData> gridPtrs = new ArrayList<IGridData>();
        boolean found = false;
        this.grids.acquireReadLock();
        try {
            for (IGridData grid : this.grids) {
                if (grid.getGridTime().overlaps(timeRange)) {
                    gridPtrs.add(grid);
                    found = true;
                } else if (found) {
                    break;
                }
            }
        } finally {
            this.grids.releaseReadLock();
        }

        return gridPtrs.toArray(new IGridData[gridPtrs.size()]);
    }

    /**
     * Creates a new grid(s) over the specified TimeRanges with a default value.
     * 
     * @param timeRanges
     *            the time ranges
     * @param mode
     *            the mode
     * 
     * @throws GFEOperationFailedException
     *             the GFE operation failed exception
     */
    public void insertNewGrid(TimeRange[] timeRanges, CreateFromScratchMode mode)
            throws GFEOperationFailedException {
        // Makes an empty grid, sets the valid time appropriately, make a
        // Grid2DBit with all bits set (for the data change notification), put
        // the grid into the inventory, set all values in the grid to the
        // default value. History is set to SCRATCH. The mode determines whether
        // the grid is created with the default value, or the current pickup
        // value.

        // create the grids
        List<IGridData> tmpGrids = new ArrayList<IGridData>();
        TimeRange overallTR = new TimeRange();
        for (int i = 0; i < timeRanges.length; i++) {
            // Make a GridData
            IGridData grid;
            grid = makeEmptyGrid(mode);

            // expand the timeRange to match the time constraints
            TimeRange expandedTR = getGridInfo().getTimeConstraints()
                    .expandTRToQuantum(timeRanges[i]);
            if (!expandedTR.isValid()) {
                continue;
            }

            overallTR = overallTR.combineWith(expandedTR);

            // Set to the proper valid time and update the history
            grid.changeValidTime(expandedTR, false);
            GridDataHistory hist = new GridDataHistory(
                    GridDataHistory.OriginType.SCRATCH, getParmID(), expandedTR);

            grid.updateHistory(hist);

            tmpGrids.add(grid);
        }

        // Insert the grids into the parm's inventory (also sends notification)
        replaceGriddedData(overallTR,
                tmpGrids.toArray(new IGridData[tmpGrids.size()]));
    }

    /**
     * Grid editing routine to replace all gridded data contained within the
     * replace range with the given set of the grids. The time affected is the
     * replaceRange expanded to the split boundary. This routine will cause any
     * existing grids entirely contained within replaceRange to be removed and
     * any existing grids partially contained within replaceRange to be split.
     * This routine may be used to copy data from one parameter to another.. The
     * origins of the "grids" are preserved, therefore the user must properly
     * set them before calling this routine.
     * 
     * @param replaceRange
     *            the replace range
     * @param grids
     *            the grids
     * @return true if successful
     */
    public boolean replaceGriddedData(final TimeRange replaceRange,
            IGridData... grids) {
        // Since the replaceRange is not guaranteed to be a split boundary, we
        // must first expand it to the split boundary. Call start parm edit to
        // begin the edit operation, split on the replace range time range, and
        // then call replaceGrids() to remove the old and insert the new grids.
        // If the operation was successul, then send out a notification and
        // return true.

        for (IGridData grid : grids) {
            String errMsg = grid.getGridSlice().isValid();
            if (errMsg != null) {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "ReplaceGriddedData aborted for " + getParmID() + ' '
                                + grid.getGridTime() + ' ' + errMsg);
                return false;
            }
            grid.resetSavePublishHistory();
        }

        TimeRange replaceTR = getGridInfo().getTimeConstraints()
                .expandTRToQuantum(replaceRange);
        if (!replaceTR.isValid()) {
            return false;
        }

        // affectedTR is the actual parts of the inventory that will be modified
        TimeRange affectedTR = expandToGridTimes(replaceTR);

        // check if it is okay to edit over the affectedTR
        this.dataManager.getParmOp().clearUndoParmList();
        if (!startParmEditInternal(affectedTR)) {
            return false;
        }

        boolean invChanged = false;
        boolean endOkay;
        try {
            // split on the edit boundaries
            invChanged = split(replaceTR);

            // now remove the old and insert the new grids
            invChanged = replaceGrids(replaceTR, grids) || invChanged;
        } finally {
            // finish up the parm edit
            endOkay = endParmEdit();
        }

        // send out notifications if anything has changed
        if (invChanged) {
            sendInvChangedNotification(affectedTR);
        }

        return endOkay;
    }

    /**
     * Sends an inventory changed notification to all parm clients. The time
     * range of the changed inventory is given.
     */
    protected void sendInvChangedNotification(final TimeRange tr) {
        if (this.dataManager != null) {
            this.dataManager.getParmManager().parmInventoryChanged(this, tr);
        }

        this.parmListeners.fireParmInventoryChangedListener(this, tr);

        IGridData overlappingGrid = overlappingGrid(tr.getCenterTime());
        if (overlappingGrid == null) {
            this.parmListeners.fireGridChangedListener(this.getParmID(), tr);
        } else {
            this.parmListeners.fireGridChangedListener(this.getParmID(),
                    overlappingGrid.getGridTime());
        }
    }

    /**
     * Sends a grid history updated notification to all parm clients. The time
     * range of the updated history is given.
     */
    protected void sendGridHistoryUpdatedNotification(final TimeRange tr) {
        this.parmListeners.fireGridHistoryUpdatedListener(this, tr);
    }

    /**
     * Command to start a parm edit sequence for the given time. This routine is
     * used for grid and point modification routines (GridData and PointData).
     * The programmer must call this routine before any modifications are made
     * to the data.
     * 
     * @param absTimes
     *            the times
     * @return the grid data
     * @throws GFEOperationFailedException
     */
    public IGridData startParmEdit(final Date absTime)
            throws GFEOperationFailedException {
        Date[] dateArray = { absTime };
        IGridData[] gridData = startParmEdit(dateArray);

        if ((gridData != null) && (gridData.length == 1)) {
            return gridData[0];
        }

        return null;
    }

    /**
     * Command to start a parm edit sequence for the given time. This routine is
     * used for grid and point modification routines (GridData and PointData).
     * The programmer must call this routine before any modifications are made
     * to the data.
     * 
     * @param absTimes
     *            the times
     * @return the grid data
     * @throws GFEOperationFailedException
     */
    public IGridData[] startParmEdit(final Date[] absTimes)
            throws GFEOperationFailedException {
        return setupParmEdit(absTimes, true);
    }

    // Parm::setupParmEdit()
    // Routine to setup a parm edit sequence for the given set of times.
    // This routine is used for multiple grid and point modification routines
    // (GridData and PointData). The length of the return sequence
    // is the same as the length of the abs time sequence, unless the entire
    // sequence is invalid. In this case, the returned sequence is of
    // length 0. The first flag is true if this is the initial parm edit
    // (in a sequence of parm edits caused by a single drag operation). The
    // first flag is false if this is a continuation of a parm edit that
    // is occurring from a drag event.
    // -- implementation
    // ---------------------------------------------------------
    // Figures out the overlapping grid for the given times, and checks if the
    // grids are available for edit. If it is okay to continue editing,
    // the undo buffer is initialized or extended, and the point-to-area
    // converter disabled.
    //
    // The point-area converter and network notifications are only
    // suspended if "first" is true.
    // ---------------------------------------------------------------------------
    private IGridData[] setupParmEdit(final Date[] absTimes, boolean first)
            throws GFEOperationFailedException {
        if (this.inParmEdit && first) {
            throw new GFEOperationFailedException(
                    "startParmEdit() called when already in an edit");
        } else if (!this.inParmEdit && !first) {
            throw new GFEOperationFailedException(
                    "extendParmEdit() called without calling startParmEdit() first");
        }
        // check for grids and ok to edit
        List<IGridData> retGrids = new ArrayList<IGridData>();
        List<TimeRange> affectedTimes = new ArrayList<TimeRange>();
        List<TimeRange> saveUndoTimes = new ArrayList<TimeRange>();
        int i;
        for (i = 0; i < absTimes.length; i++) {
            Date absTime = absTimes[i]; // extract from sequence

            // Get the overlapping grid and see if it's ok to edit.
            IGridData grid = null;
            grid = overlappingGrid(absTime);

            if ((grid != null) && isOkToEdit(grid.getGridTime())) {
                retGrids.add(grid);
                affectedTimes.add(grid.getGridTime());
                saveUndoTimes.add(grid.getGridTime());
            } else {
                retGrids.add(null);
                affectedTimes.add(new TimeRange());
            }
        }

        // any grids at all?
        if (saveUndoTimes.size() == 0) {
            return new IGridData[0]; // empty list
        }

        // Save the current grids in the undo buffer
        saveUndo(saveUndoTimes.toArray(new TimeRange[saveUndoTimes.size()]),
                first); // clear
        // if the first parm edit set the in edit flag, suspend notifications
        if (first) {
            inParmEdit = true;
            // TODO!!!
            // _dataMgr.networkMgr().suspendNotifications();
        }

        // since inventory may have changed, go back and get a new
        // set of grid pointers
        for (i = 0; i < absTimes.length; i++) {
            if (retGrids.get(i) != null) {
                retGrids.set(i, overlappingGrid(absTimes[i]));
            }
        }

        // tell each of the grids that we are about to edit them
        // also save the times of the grids so we can tell them later to
        // endGridEdit.
        for (i = 0; i < retGrids.size(); i++) {
            if (retGrids.get(i) != null) {
                retGrids.get(i).startGridEdit();
            }
        }
        timesBeingEdited.addAll(Arrays.asList(absTimes));

        return retGrids.toArray(new IGridData[retGrids.size()]);
    }

    /**
     * Command to start a parm edit for selected time range or entire grid
     * manipulations. The time range affected is given. Returns true if it is
     * okay to continue to edit.
     * 
     * @param timeRange
     *            the time range to look at
     * @return true if okay to continue editing
     * 
     */
    private boolean startParmEditInternal(final TimeRange timeRange) {
        // Checks if it is okay to edit this. Saves a copy of the data to
        // be edited using saveUndo(). Tells the network manager to
        // suspend notifications.
        if (inParmEdit || inTSEdit) {
            throw new IllegalStateException(
                    "startParmEdit called while editing already started");
        }

        if (!isOkToEdit(timeRange)) {
            return false;
        }

        saveUndo(new TimeRange[] { timeRange }, true);

        // TODO: Fix this
        // _dataMgr.networkMgr().suspendNotifications();

        inParmEdit = true;
        return true;

    }

    /**
     * Routine called by edit functions after the parm has been edited.
     * 
     * @return true, if end parm edit succeeded
     */
    public boolean endParmEdit() {
        // ---------------------------------------------------------
        // Checks the lock status for each undo buffer time range. We request a
        // lock if we don't already have one. If we cannot get a lock, then we
        // call forceUndo() to roll back the changes we just made.
        // Resumes notifications.
        //
        // If any of the required locks fail, then the entire operation is
        // undone.

        if (!inParmEdit || inTSEdit) {
            throw new IllegalStateException(
                    "endParmEdit() called when not in a parm edit");
        }

        boolean returnVal = true;

        if (this.lockTable != null) {
            for (int i = 0; i < this.undoBuffers.size(); i++) {
                LockStatus lockStatus = lockTable.checkLock(undoBuffers.get(i)
                        .getUndoTimeRange());
                if (!lockStatus.equals(LockStatus.LOCKED_BY_ME)) {
                    LockRequest lr = new LockRequest(getParmID(), undoBuffers
                            .get(i).getUndoTimeRange(), LockMode.LOCK);
                    if (!requestLock(lr)) {
                        this.forceUndo();
                        returnVal = false;
                        break;
                    }
                }
            }
        }

        // tell all of the grids that we are done editing. This may send
        // grid change notifications
        for (Date time : this.timesBeingEdited) {
            IGridData grid = overlappingGrid(time);
            if (grid != null) {
                grid.resetSavePublishHistory();
                gridDataHasChanged(grid, grid.endGridEdit(), true);
            }
        }
        timesBeingEdited.clear();

        inParmEdit = false;

        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        // TODO: FIXME
        // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        // dataMgr.networkMgr().resumeNotifications();

        return returnVal;
    }

    /**
     * Notification that grid data has been modified. This is called by
     * endParmEdit() after an edit operation has occurred. Also called by
     * virtual calculated parms. The isHistoryModified indicates whether the the
     * grid data history should be set to modified.
     * 
     * @param gridData
     *            the grid data
     * @param changedPoints
     *            the changed points
     * @param isHistoryModified
     *            flag if history is modified
     */
    protected void gridDataHasChanged(IGridData gridData,
            final Grid2DBit changedPoints, boolean isHistoryModified) {
        // ---------------------------------------------------------
        // We figure out whether there is an overlap between the changed points
        // and the active reference set. If so, then we run the converter to
        // recalculate the new representative values.
        //
        // Changes the source grid to modified.
        // ---------------------------------------------------------------------------.

        // if no changed points, then don't do anything.
        if (changedPoints == null) {
            return;
        }
        if (!changedPoints.isValid() || !changedPoints.isAnyBitsSet()) {
            return;
        }

        if (isHistoryModified) {
            gridData.updateHistoryToModified(this.dataManager.getWsId());
        }

        this.grids.acquireReadLock();
        try {
            if (this.grids.indexOf(gridData) == -1) {
                throw new IllegalArgumentException(
                        "GridDataChanged received on unknown grid for "
                                + getParmID().toString() + " and "
                                + gridData.getGridTime());
            }
        } finally {
            this.grids.releaseReadLock();
        }

        // notify parm clients that the grid data has changed.
        this.parmListeners.fireGridChangedListener(this.getParmID(),
                gridData.getGridTime());

    }

    /**
     * Splits the grids at the boundaries of the given time range. Returns true
     * if the inventory was changed. Assumes that the starting and ending times
     * of the split time range match possible split boundaries. No notifications
     * are made in this routine. Splits at the starting and ending time of the
     * time range provided that the grids aren't already split at those points.
     * 
     * @param splitTimeRange
     *            the split time range
     * 
     * @return true, if split
     */
    private boolean split(final TimeRange splitTimeRange) {
        // If the user wants to know the affected time range, then a call to
        // expandToGridTimes() can be used.
        //
        // Uses overlappingGrid() to find the affected grid. Then if a split is
        // called for, a copy is made, and the times set on the original and
        // copy of the grid. Then replaceGrids() is used to insert the changes
        // into the inventory. The operation is repeated for the right grid. It
        // is possible that the left and right grids end up being the same grid.

        boolean invChanged = false;
        IGridData leftGrid = overlappingGrid(splitTimeRange.getStart());
        if ((leftGrid != null)
                && !leftGrid.getGridTime().getStart()
                        .equals(splitTimeRange.getStart())) {
            IGridData newGrid = null;
            try {
                newGrid = leftGrid.clone();
            } catch (CloneNotSupportedException e) {
                newGrid = null;
            }
            newGrid.updateHistoryToModified(this.dataManager.getWsId());
            newGrid.changeValidTime(new TimeRange(leftGrid.getGridTime()
                    .getStart(), splitTimeRange.getStart()), true);
            leftGrid.changeValidTime(new TimeRange(splitTimeRange.getStart(),
                    leftGrid.getGridTime().getEnd()), true);
            leftGrid.updateHistoryToModified(this.dataManager.getWsId());
            replaceGrids(newGrid.getGridTime(), new IGridData[] { newGrid });
            invChanged = true;
        }

        IGridData rightGrid = overlappingGrid(new Date(splitTimeRange.getEnd()
                .getTime() - 1000));
        if ((rightGrid != null)
                && !rightGrid.getGridTime().getEnd()
                        .equals(splitTimeRange.getEnd())) {
            IGridData newGrid;
            try {
                newGrid = rightGrid.clone();
            } catch (CloneNotSupportedException e) {
                newGrid = null;
            }

            newGrid.updateHistoryToModified(this.dataManager.getWsId());
            newGrid.changeValidTime(new TimeRange(rightGrid.getGridTime()
                    .getStart(), splitTimeRange.getEnd()), true);
            rightGrid.changeValidTime(new TimeRange(splitTimeRange.getEnd(),
                    rightGrid.getGridTime().getEnd()), true);
            rightGrid.updateHistoryToModified(this.dataManager.getWsId());
            replaceGrids(newGrid.getGridTime(), new IGridData[] { newGrid });
            invChanged = true;
        }

        return invChanged;
    }

    /**
     * Utility routine to remove grids in the inventory that fall within the
     * time range specified. Returns true if the inventory was modified. This
     * routine assumes that the time range must encompass an entire grid or
     * grids, i.e., the time range must not start or end within a grid. No
     * splitting of grids will occur. No notifications are made. Assumes that
     * locks have already been checked and that it is okay to edit over the time
     * range.
     * 
     * @param timeRange
     *            the time range
     * 
     * @return true, if action occurred
     */
    private boolean removeGrids(final TimeRange timeRange) {
        // find the start and end indexes of the grids to be removed.
        IGridData[] grids = getGridInventory(timeRange);
        if (grids.length != 0) {
            this.grids.acquireWriteLock();
            try {
                this.grids.removeAll(Arrays.asList(grids));
            } finally {
                this.grids.releaseWriteLock();
            }
            return true;
        }
        return false;
    }

    /**
     * Replace Grids is a utility routine used to manipulate the grid inventory.
     * Returns true if the grid inventory was modified. All previously existing
     * grids contained in time range will be replaced by those grids in grids.
     * The time range (replaceRange) must encompass entire grids (e.g., no
     * splitting is allowed), the grids identified must lie within the
     * replaceRange. GridData pointers passed into this routine have their
     * ownership transferred thus the caller MUST NOT deallocate their memory or
     * use these pointers again. No notifications are made from this routine.
     * Assumes that the user has already verified that it is okay to edit over
     * the replaceRange time range. The grid sources are checked. If the grid
     * has come from a different parm, then data translation will occur.
     * 
     * @param replaceRange
     * @param gGrids
     * @return
     */
    protected boolean replaceGrids(final TimeRange replaceRange,
            final IGridData[] orgGrids) {
        if (!validateUnits(orgGrids)) {
            return false;
        }

        boolean invChanged = removeGrids(replaceRange);

        // these will be the actual ones stored
        List<IGridData> grids = new ArrayList<IGridData>(orgGrids.length);

        for (int i = 0; i < orgGrids.length; i++) {
            // ensure that the grid valid times are expanded to this parm's
            // quantum requirements
            TimeRange orgTR = orgGrids[i].getGridTime();
            TimeRange expTR = getGridInfo().getTimeConstraints()
                    .expandTRToQuantum(orgTR);

            // in some cases, you can end up with an invalid TR
            if (!expTR.isValid()
                    || (orgTR.getStart().getTime() < replaceRange.getStart()
                            .getTime())
                    || (orgTR.getEnd().getTime() > replaceRange.getEnd()
                            .getTime())) {
                continue;
            }

            try {
                orgGrids[i] = copyGrid(orgGrids[i]);
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to copy grid for parm " + this.getParmID(), e);
                continue;
            }

            // normal weather element, or input grids not-populated
            if (!getGridInfo().isRateParm() || !orgGrids[i].isPopulated()) {
                if (!addNonOverlap(grids, orgGrids[i], orgTR, expTR)) {
                    continue;
                }
            } else { // rate-dependent weather element with populated grids
                adjustIntersects(grids, orgGrids[i], orgTR);
                TimeRange newTR = new TimeRange();
                TimeRange[] retVal = getNewTimeRange(
                        grids.toArray(new IGridData[grids.size()]), orgTR,
                        newTR);

                TimeRange newTRExp = retVal[0];
                newTR = retVal[1];
                if (!newTRExp.isValid()) {
                    continue;
                }

                try {
                    createGrids(grids, orgGrids[i], orgTR, newTRExp, newTR);
                } catch (GFEServerException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to create grids", e);
                }
            }
        }

        return insertGrids(grids) || invChanged;
    }

    /**
     * Returns an expanded new TimeRange and sets newTR based on grids and
     * orgTR.
     * 
     * @param grids
     * @param orgTR
     * @param newTR
     * @returns an array of time ranges, the expanded time range is index zero,
     *          and the modified newTR is index 1
     */
    private TimeRange[] getNewTimeRange(IGridData[] grids, TimeRange orgTR,
            TimeRange newTR) {
        // after all intersecting grids, handled, are there any
        // other pieces remaining to be processed (i.e., create new grid?)
        // figure out last intersection found, and calculate remainder

        if (grids.length == 0) {
            newTR = orgTR;
        } else {
            // check intersection of last one in file
            TimeRange interTR = orgTR.intersection(grids[grids.length - 1]
                    .getGridTime());

            // exact match -- so already handled
            if (interTR == orgTR) {
                return new TimeRange[] { new TimeRange(), newTR };
            } else if (!interTR.isValid()) {
                newTR = orgTR;
            } else {
                newTR = new TimeRange(interTR.getEnd(), orgTR.getEnd());
            }
        }

        // newTR contains the desired TR of the grid to create, but
        // it hasn't been expanded to this parm's time constraints

        // create the new grid(s)
        return new TimeRange[] {
                getGridInfo().getTimeConstraints().expandTRToQuantum(newTR),
                newTR };
    }

    /**
     * Adjusts the values of the grids which may intersect orgTR using the
     * supplied grid.
     * 
     * @param grids
     * @param grid
     * @param orgTR
     */
    private void adjustIntersects(List<IGridData> grids, final IGridData grid,
            final TimeRange orgTR) {
        // first see if there is an intersecting grid
        // and add adjusted values to it
        for (int j = 0; j < grids.size(); j++) {
            TimeRange intersect = orgTR
                    .intersection(grids.get(j).getGridTime());
            if (intersect.isValid()) {
                // add proportional value to existing grid
                float timeRatio = (float) intersect.getDuration()
                        / (float) orgTR.getDuration();
                if ((grids.get(j).getGridSlice() instanceof IContinuousSlice)
                        && (grid.getGridSlice() instanceof IContinuousSlice)) {

                    IGridSlice copy;
                    try {
                        copy = grid.getGridSlice().clone();
                    } catch (CloneNotSupportedException e) {
                        copy = null;
                    }

                    ((IContinuousSlice) copy).operateEquals(Op.MULTIPLY,
                            timeRatio);

                    ((IContinuousSlice) grids.get(j).getGridSlice())
                            .operateEquals(Op.ADD, (IContinuousSlice) copy);

                }
                // update history
                GridDataHistory[] his = grids.get(j).getHistory();
                List<GridDataHistory> historyAsList = new ArrayList<GridDataHistory>(
                        Arrays.asList(his));
                historyAsList.addAll(Arrays.asList(grid.getHistory()));

                grids.get(j).updateHistory(
                        historyAsList.toArray(new GridDataHistory[historyAsList
                                .size()]));
                // updated history

            }
        }
    }

    /**
     * Inserts the grids in 'grids' into this Parm.
     * 
     * @param grids
     *            the grids to insert
     */
    private boolean insertGrids(List<IGridData> grids) {
        if (grids.size() != 0) {
            ArrayList<IGridData> newGrids = new ArrayList<IGridData>(grids);
            Collections.sort(newGrids);

            // need to insert the set of new grids before an existing grid
            Date gridsEndTime = grids.get(grids.size() - 1).getGridTime()
                    .getEnd();

            boolean inserted = false;
            this.grids.acquireWriteLock();
            try {
                this.grids.ensureCapacity(this.grids.size() + newGrids.size());
                for (int i = 0; i < this.grids.size(); i++) {
                    if (this.grids.get(i).getGridTime().getStart().getTime() >= gridsEndTime
                            .getTime()) {
                        this.grids.addAll(i, grids);
                        inserted = true;
                        break;
                    }
                }

                // If we got this far without inserting then we append
                if (!inserted) {
                    this.grids.addAll(grids);
                }
            } finally {
                this.grids.releaseWriteLock();
            }
        }
        return grids.size() != 0;
    }

    /**
     * Returns a copy of the supplied grid. The copy may be translated to work
     * with this parms units, etc. So... The copy may either be the same pointer
     * passed in or a new copy. If it is a new grid then the old one will be
     * deleted!
     * 
     * @param originalGrid
     * @return
     * @throws GFEServerException
     */
    private IGridData copyGrid(IGridData originalGrid)
            throws GFEServerException {
        // did this grid come from a different parm? If so,
        // then we have to translate the data before inserting the set
        // of grids. Copy grids will also change units if necessary.
        if (!originalGrid.getParm().getParmID().equals(getParmID())) {
            // create a dummy one
            IGridData grid = makeEmptyGrid();

            grid.copyGridValues(originalGrid); // translate the grid

            // make a new his if ISC, if not keep the rest
            if (originalGrid.getParm().isIscParm()) {
                GridDataHistory iscHis = new GridDataHistory(
                        GridDataHistory.OriginType.CALCULATED, originalGrid
                                .getParm().getParmID(),
                        originalGrid.getGridTime(), null, (WsId) null);
                grid.updateHistory(iscHis);
            } else {
                grid.updateHistory(originalGrid.getHistory()); // history
            }
            grid.resetSavePublishHistory();

            grid.changeValidTime(originalGrid.getGridTime(), false);
            originalGrid = grid; // replace it with the translated one
        }

        return originalGrid;
    }

    /**
     * Adds grid to grids if it does not overlap. Returns true if a grid was
     * added and false if not.
     * 
     * @param grids
     *            the grids
     * @param grid
     *            the grid
     * @param orgTR
     *            the org tr
     * @param expTR
     *            the exp tr
     * 
     * @return true, if success
     */
    private boolean addNonOverlap(List<IGridData> grids, IGridData grid,
            final TimeRange orgTR, final TimeRange expTR) {
        for (int j = 0; j < grids.size(); j++) {
            if (grids.get(j).getGridTime().overlaps(expTR)) {
                return false;
            }
        }

        if (!orgTR.equals(expTR)) {
            grid.changeValidTime(expTR, false); // match time constraints

        }

        grids.add(grid); // add grid to list
        return true;
    }

    /**
     * May add new grids to grids based on the supplied grid and time ranges.
     * This function is used for rate-dependent parameters.
     * 
     * @param retvalList
     * @param grid
     *            the grid
     * @param orgTR
     *            the org tr
     * @param newTRExp
     *            the new tr exp
     * @param newTR
     *            the new tr
     * @return the grid result
     * @throws GFEServerException
     */
    private IGridData[] createGrids(List<IGridData> retvalList, IGridData grid,
            final TimeRange orgTR, final TimeRange newTRExp,
            final TimeRange newTR) throws GFEServerException {
        // get possible destination grids, combine where possible
        List<TimeRange> ctimes = new ArrayList<TimeRange>(
                Arrays.asList(getGridInfo().getTimeConstraints()
                        .constraintTimes(newTRExp)));
        for (int k = ctimes.size() - 1; k > 0; k--) {
            if (newTR.contains(ctimes.get(k))
                    && newTR.contains(ctimes.get(k - 1))
                    && ctimes.get(k - 1).isAdjacentTo(ctimes.get(k))) {
                ctimes.set(k - 1, new TimeRange(ctimes.get(k - 1).getStart(),
                        ctimes.get(k).getEnd()));
                ctimes.remove(k);
            }
        }

        // List<IGridData> retvalList = new ArrayList<IGridData>(Arrays
        // .asList(grids));
        // create the grids
        for (int k = 0; k < ctimes.size(); k++) {
            TimeRange intersect = orgTR.intersection(ctimes.get(k));
            float timeRatio = (float) intersect.getDuration()
                    / (float) orgTR.getDuration();
            // create a dummy one
            IGridData newgrid = makeEmptyGrid(CreateFromScratchMode.DEFAULT);

            IGridSlice copy;
            try {
                copy = grid.getGridSlice().clone();
            } catch (CloneNotSupportedException e) {
                throw new GFEServerException(
                        "Failed to clone the grid's underlying grid gridSlice.",
                        e);
            }

            if ((copy instanceof IContinuousSlice)
                    && (newgrid.getGridSlice() instanceof IContinuousSlice)) {
                ((IContinuousSlice) copy).operateEquals(Op.MULTIPLY, timeRatio,
                        dataManager.getRefManager().fullRefSet().getGrid());
                ((IContinuousSlice) newgrid.getGridSlice()).operateEquals(
                        Op.ADD, (IContinuousSlice) copy, dataManager
                                .getRefManager().fullRefSet().getGrid());
            }

            newgrid.updateHistory(grid.getHistory()); // history
            // rate-accum grids that are time shifted are not considered
            // modified

            newgrid.changeValidTime(ctimes.get(k), false);
            retvalList.add(newgrid); // add grid to list
        }

        return retvalList.toArray(new IGridData[retvalList.size()]);
    }

    /**
     * Returns true if all of the grids in 'grids' match the units (or can be
     * converted to the units) of this Parm.
     * 
     * @param grids
     * @return
     */
    private boolean validateUnits(final IGridData[] grids) {
        for (int i = 0; i < grids.length; i++) {
            if (!grids[i].getParm().getGridInfo().getUnitObject()
                    .isCompatible(this.getGridInfo().getUnitObject())) {
                statusHandler.handle(Priority.PROBLEM,
                        "replaceGrids() with grids from "
                                + grids[i].getParm().getParmID()
                                + " that do not share units ["
                                + grids[i].getParm().getGridInfo()
                                        .getUnitString() + " with this parm: "
                                + this.getParmID() + " units: "
                                + this.getGridInfo().getUnitString());
                return false;
            }
        }
        return true;
    }

    /**
     * Utility routine to save (create) the undo grids over the given undo time
     * range. If clearFirst is true, then any existing undo buffer is purged. If
     * clearFirst is false, then the new saveUndo() request will be merged with
     * the previous saveUndo() requests.
     * 
     * @param undoRanges
     * @param clearFirst
     */
    private void saveUndo(TimeRange[] undoRanges, boolean clearFirst) {
        if (clearFirst) {
            purgeUndoGrids();
        }

        for (int i = 0; i < undoRanges.length; i++) {
            // is the time range already represented in the UndoBuffer?
            boolean found = false;
            for (int j = 0; j < this.undoBuffers.size(); j++) {
                if (this.undoBuffers.get(j).getUndoTimeRange()
                        .contains(undoRanges[i])) {
                    found = true;
                    break;
                }
            }

            // if not found, then add it to the undo buffer.
            if (!found) {
                IGridData[] grids = getGridInventory(undoRanges[i]);
                this.undoBuffers.add(new UndoBuffer(undoRanges[i], grids));
            }
        }

        // notify the data manager that this parm can be undone
        if (this.dataManager != null) {
            this.dataManager.getParmOp().setUndoParmList(this);
        }
    }

    /**
     * Returns true if it is okay to edit over the given time range.
     * 
     * @param timeRange
     *            the time range to check
     * @return true if ok to edit
     */
    public boolean isOkToEdit(final TimeRange timeRange) {

        // It is okay to edit if this is a mutable parm and the time range is
        // not locked by someone else. The input time range is expanded to the
        // quantum boundary and the grid boundary if a grid exists.

        // check for invalid time range
        if (!timeRange.isValid()) {
            return false;
        }

        if (!isMutable()) {
            return false;
        }

        TimeRange expandedTR = this.getGridInfo().getTimeConstraints()
                .expandTRToQuantum(timeRange);

        if (!expandedTR.isValid()) {
            return false;
        }

        expandedTR = expandToGridTimes(expandedTR);

        if (!expandedTR.isValid()) {
            return false;
        }

        LockStatus lockStatus;
        if (lockTable != null) {
            try {
                lockStatus = lockTable.checkLock(expandedTR);
            } catch (Exception e) {
                return false;
            }
            if (lockStatus.equals(LockStatus.LOCKED_BY_OTHER)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Populate a grid with data. This is implemented but subclasses of Parm
     * 
     * @param grid
     */
    public abstract void populateGrid(IGridData grid);

    /**
     * Populate a grid with data. This is implemented but subclasses of Parm
     * 
     * @param grid
     */
    public abstract void populateGrids(List<IGridData> grids);

    /**
     * Expand the input TimeRange so that it matches the time boundaries for the
     * grids identified by the specified TimeRange. If a grid doesn't exist on
     * one end of the time range, then that time isn't modified.
     * 
     * @param timeRange
     *            the time range to expand
     * 
     * @return the expanded range
     */
    protected TimeRange expandToGridTimes(final TimeRange timeRange) {
        // Get the overlapping grid for the starting time and one second less
        // than the ending time. If the grids are defined then expand to use
        // their times, if not, use the passed in time range. The start and
        // ending times are considered separately.
        final IGridData leftGrid = overlappingGrid(timeRange.getStart());
        final IGridData rightGrid = overlappingGrid(new Date(timeRange.getEnd()
                .getTime() - 1000));

        Date startTime = new Date(timeRange.getStart().getTime());
        Date endTime = new Date(timeRange.getEnd().getTime());

        if (leftGrid != null) {
            startTime = leftGrid.getGridTime().getStart();
        }
        if (rightGrid != null) {
            endTime = rightGrid.getGridTime().getEnd();
        }

        return new TimeRange(startTime, endTime);
    }

    /**
     * Grid history changed
     * 
     * @param gridData
     */
    public void gridHistoryChanged(IGridData gridData) {
        // TODO: Implement
    }

    protected IGridData makeEmptyGrid() {
        return makeEmptyGrid(CreateFromScratchMode.DEFAULT);
    }

    protected IGridData makeEmptyGrid(CreateFromScratchMode mode) {
        // get grid sizes
        int x = this.gridInfo.getGridLoc().getNx();
        int y = this.gridInfo.getGridLoc().getNy();
        IGridSlice gs = null;

        GridDataHistory gdh = new GridDataHistory(
                GridDataHistory.OriginType.SCRATCH, getParmID(), new TimeRange(
                        new Date(0l), new Date(1l)), null, (WsId) null);

        switch (getGridInfo().getGridType()) {
        case SCALAR: {
            float value = 0.0f;
            WxValue wxValue = null;
            // default or pickup
            if (mode == CreateFromScratchMode.DEFAULT) {
                wxValue = WxValue.defaultValue(this);
            } else {
                wxValue = this.parmState.getPickUpValue();
            }

            value = ((ScalarWxValue) wxValue).getValue();
            Grid2DFloat g2d = new Grid2DFloat(x, y, value);

            gs = new ScalarGridSlice(new TimeRange(), getGridInfo(),
                    new GridDataHistory[] { gdh }, g2d);
            break;
        }
        case VECTOR: {
            // default or pickup
            WxValue value = mode == CreateFromScratchMode.DEFAULT ? WxValue
                    .defaultValue(this) : this.parmState.getPickUpValue();

            Grid2DFloat mag = new Grid2DFloat(x, y,
                    ((VectorWxValue) value).getMag());

            Grid2DFloat dir = new Grid2DFloat(x, y,
                    ((VectorWxValue) value).getDir());

            gs = new VectorGridSlice(new TimeRange(), getGridInfo(),
                    new GridDataHistory[] { gdh }, mag, dir);
            break;
        }
        case WEATHER: {
            WeatherWxValue value = (WeatherWxValue) (mode == CreateFromScratchMode.DEFAULT ? WxValue
                    .defaultValue(this) : this.parmState.getPickUpValue());

            Grid2DByte grid = new Grid2DByte(x, y, (byte) 0);

            gs = new WeatherGridSlice(new TimeRange(), getGridInfo(),
                    new GridDataHistory[] { gdh }, grid,
                    new WeatherKey[] { value.getWeatherKey() });
            break;
        }
        case DISCRETE: {
            DiscreteWxValue value = (DiscreteWxValue) (mode == CreateFromScratchMode.DEFAULT ? WxValue
                    .defaultValue(this) : this.parmState.getPickUpValue());

            Grid2DByte grid = new Grid2DByte(x, y, (byte) 0);

            gs = new DiscreteGridSlice(new TimeRange(), getGridInfo(),
                    new GridDataHistory[] { gdh }, grid,
                    new DiscreteKey[] { value.getDiscreteKey() });
            break;
        }
        default:
            throw new IllegalArgumentException("Unsupported Grid type: "
                    + getGridInfo().getGridType());
        }

        // now create the grid data
        return AbstractGridData.makeGridData(this, gs);
    }

    /**
     * Set the underlying grids
     * <p>
     * <b>Note: this function is for testing only, not for general use.</b>
     * 
     * @param grids
     */
    public void setGrids(List<IGridData> grids) {
        this.grids.acquireWriteLock();
        try {
            this.grids.clear();
            this.grids.ensureCapacity(grids.size());
            this.grids.addAll(grids);
            this.grids.trimToSize();
        } finally {
            this.grids.releaseWriteLock();
        }
    }

    /**
     * Undo the last edit.
     */
    public void undo() {
        // Ensures that there is data in the undo buffer, checks that we can
        // perform the undo (check locks) using okToEdit(), and then calls
        // forceUndo() to perform the undo operation.
        // Check for invalid undo buffer
        if (this.undoBuffers.size() == 0) {
            return;
        }

        // Check for locks for undo buffer TimeRange
        for (UndoBuffer undoBuffer : this.undoBuffers) {
            if (!isOkToEdit(undoBuffer.getUndoTimeRange())) {
                return;
            }
        }

        forceUndo();

    }

    /**
     * Routine to perform the undo operation. No checking of current lock states
     * is performed.
     */
    @SuppressWarnings("unchecked")
    private void forceUndo() {
        // The system state is restored after an edit where a lock could not
        // be granted by this routine -- thus no checking of locks can be done.
        // Sends out the appropriate notification that data has changed.

        // make change to inventory
        boolean invChanged = false;
        List<IGridData>[] bgridCopies = new ArrayList[this.undoBuffers.size()];
        TimeRange[] baffectedTR = new TimeRange[this.undoBuffers.size()];

        int i;
        for (i = 0; i < this.undoBuffers.size(); i++) {
            UndoBuffer undoBuffer = this.undoBuffers.get(i);

            // String msg = "Undoing " + getParmID() + " tr="
            // + undoBuffer.getUndoTimeRange();
            // statusHandler.handle(Priority.DEBUG, msg, new Exception("Debug: "
            // + msg));

            baffectedTR[i] = undoBuffer.getUndoTimeRange();
            bgridCopies[i] = new ArrayList<IGridData>();

            for (IGridData gridData : undoBuffer.getUndoGrids()) {
                try {
                    bgridCopies[i].add(gridData.clone());
                } catch (CloneNotSupportedException e) {
                    // don't add this undo grid
                }
            }
        }

        saveUndo(baffectedTR, true); // Save the current set of grids

        for (i = 0; i < bgridCopies.length; i++) {
            invChanged |= replaceGrids(
                    baffectedTR[i],
                    bgridCopies[i].toArray(new IGridData[bgridCopies[i].size()]));
        }

        // send out notification if anything has changed
        if (invChanged) {
            for (TimeRange tr : baffectedTR) {
                sendInvChangedNotification(tr);
            }
        }
    }

    /**
     * Return the undo buffer
     * 
     * @return
     */
    public UndoBuffer[] getUndoBuffer() {
        return this.undoBuffers
                .toArray(new UndoBuffer[this.undoBuffers.size()]);
    }

    /**
     * Calculates the time-weighted average at each gridPoint for all the grids
     * that are contained within the specified TimeRange.
     * 
     * @param timeRange
     *            the timerange to average over
     * @return the weighted average parm
     */
    public Parm twavg(final TimeRange timeRange) {
        // Get the list of grids that are found inside the specified timeRange
        IGridData[] grids = getGridInventory(timeRange);

        // See if we have any grids first
        if (grids.length == 0) {
            return null;
        }

        // calculate the time ranges of the grids that overlap
        List<TimeRange> gridTR = new ArrayList<TimeRange>();
        for (int i = 0; i < grids.length; i++) {
            gridTR.add(timeRange.intersection(grids[i].getGridTime()));
        }

        // calculate the total duration of all grids, intersect with input tr
        float totalDuration = 0.0f;
        for (int i = 0; i < gridTR.size(); i++) {
            totalDuration += gridTR.get(i).getDuration();
        }

        // get some stuff for later use
        // CartCoord2D<int> gridSize = gridLocation().gridSize();
        int gridCount = grids.length;
        GridParmInfo gridInfo = getGridInfo();

        // Make a new GridSlice into which the result will go
        IGridSlice gridSlice = null;
        GridEnvelope ge = MapUtil.getGridGeometry(this.gridInfo.getGridLoc())
                .getGridRange();

        ParmID parmId = getParmID();
        String siteId = parmId.getDbId().getSiteId();

        if ((grids.length == 1)
                && (getGridInfo().getGridType() != GridType.WEATHER)) {
            // nothing to average so we're done, except in the weather case
            try {
                gridSlice = grids[0].getGridSlice().clone();
            } catch (CloneNotSupportedException e) {
                // This should never happen.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else if (getGridInfo().getGridType() == GridType.SCALAR) {
            Grid2DFloat grid = new Grid2DFloat(ge.getSpan(0), ge.getSpan(1));
            Grid2DFloat[] scalarGrids = new Grid2DFloat[gridCount];
            for (int c = 0; c < gridCount; c++) {
                scalarGrids[c] = ((ScalarGridSlice) grids[c].getGridSlice())
                        .getScalarGrid();
            }
            for (int i = 0; i < ge.getSpan(0); i++) {
                for (int j = 0; j < ge.getSpan(1); j++) {
                    float value = 0.0f;
                    for (int k = 0; k < gridCount; k++) {
                        value += scalarGrids[k].get(i, j)
                                * gridTR.get(k).getDuration();
                    }
                    grid.set(i, j, value / totalDuration);
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, getParmID(),
                    timeRange, null, (WsId) null);
            gridSlice = new ScalarGridSlice(timeRange, getGridInfo(),
                    new GridDataHistory[] { gridHistory }, grid);
        } else if (getGridInfo().getGridType() == GridType.VECTOR)
        // Do average using UV to MagDir conversions
        {
            // Get the mag and dir grids
            Grid2DFloat uGrid;
            Grid2DFloat vGrid;
            Grid2DFloat uSum = new Grid2DFloat(ge.getSpan(0), ge.getSpan(1));
            Grid2DFloat vSum = new Grid2DFloat(ge.getSpan(0), ge.getSpan(1));
            // sum
            for (int k = 0; k < gridCount; k++) {
                VectorGridSlice vgs = (VectorGridSlice) grids[k].getGridSlice();

                uGrid = vgs.vectorUGrid();
                vGrid = vgs.vectorVGrid();

                float dur = gridTR.get(k).getDuration();
                for (int i = 0; i < ge.getSpan(0); i++) {
                    for (int j = 0; j < ge.getSpan(1); j++) {
                        uSum.set(i, j, uSum.get(i, j) + (uGrid.get(i, j) * dur));
                        vSum.set(i, j, vSum.get(i, j) + (vGrid.get(i, j) * dur));
                    }
                }
            }
            // average
            for (int i = 0; i < ge.getSpan(0); i++) {
                for (int j = 0; j < ge.getSpan(1); j++) {
                    uSum.set(i, j, uSum.get(i, j) / totalDuration);
                    vSum.set(i, j, vSum.get(i, j) / totalDuration);
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, getParmID(),
                    timeRange, null, (WsId) null);
            gridSlice = VectorGridSlice.makeGridSliceFromUV(uSum, vSum,
                    timeRange, gridInfo, new GridDataHistory[] { gridHistory });
        } else if (getGridInfo().getGridType() == GridType.WEATHER) {
            // Time Weighted Average will a combination of all subKeys that
            // exist in over x% of the time periods. The percentage is defined
            // in the gfe configuration file entry:
            // SignificantWeatherTimeWeightAverage_percent.
            // If there aren't any over the threshold, then the most predominent
            // type is chosen.

            int configP = GFEPreference
                    .getIntPreference("SignificantWeatherTimeWeightAverage_percent");
            configP = (configP >= 1) ? configP : 40;
            configP = (configP <= 100) ? configP : 40;

            float significantPercent = (float) (configP / 100.0);
            Point gridSize = this.getGridInfo().getGridLoc().gridSize();

            Grid2DByte grid = new Grid2DByte(gridSize.x, gridSize.y);
            ArrayList<WeatherKey> key = new ArrayList<WeatherKey>();

            for (int i = 0; i < gridSize.x; i++) {
                for (int j = 0; j < gridSize.y; j++) {
                    ArrayList<WeatherSubKey> subKeys = new ArrayList<WeatherSubKey>();
                    ArrayList<Integer> wcount = new ArrayList<Integer>();
                    for (int k = 0; k < gridCount; k++) {
                        WeatherKey key1[] = ((WeatherGridSlice) grids[k]
                                .getGridSlice()).getKeys();
                        Grid2DByte grid1 = ((WeatherGridSlice) grids[k]
                                .getGridSlice()).getWeatherGrid();
                        WeatherKey tmpKey = key1[grid1.get(i, j)];
                        WeatherSubKey gpkeys[] = tmpKey.getSubKeys().toArray(
                                new WeatherSubKey[tmpKey.getSubKeys().size()]);
                        // key1[grid1(i, j)].subKeys();
                        for (int m = 0; m < gpkeys.length; m++) {
                            int index = subKeys.indexOf(gpkeys[m]);
                            if (index == -1) {
                                subKeys.add(gpkeys[m]);
                                wcount.add(0);
                                index = subKeys.size() - 1;
                            }
                            wcount.set(index, wcount.get(index)
                                    + (int) gridTR.get(k).getDuration());
                        }
                    }
                    // Make the combined weather sub key, but only with those
                    // values that exceed the configured percentage
                    ArrayList<WeatherSubKey> weightedKeys = new ArrayList<WeatherSubKey>();
                    for (int k = 0; k < subKeys.size(); k++) {
                        if (((float) wcount.get(k) / totalDuration) > significantPercent) {
                            weightedKeys.add(subKeys.get(k));
                        }
                    }

                    // if none exceed the percentage, then find the most predom.
                    if (weightedKeys.size() == 0) {
                        // find the subkey that is most predominent
                        int maxCount = wcount.get(0);
                        for (int k = 1; k < subKeys.size(); k++) {
                            if (wcount.get(k) > maxCount) {
                                maxCount = wcount.get(k);
                            }
                        }
                        for (int k = 0; k < subKeys.size(); k++) {
                            if (wcount.get(k) == maxCount) {
                                weightedKeys.add(subKeys.get(k));
                            }
                        }
                    }

                    WeatherKey wxkey = new WeatherKey(siteId, weightedKeys);

                    // attempt lookup for existing
                    int index = key.indexOf(wxkey);
                    if (index != -1) {
                        grid.set(i, j, (byte) index);
                    } else {
                        key.add(wxkey);
                        grid.set(i, j, (byte) (key.size() - 1));
                    }
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, getParmID(),
                    timeRange);
            gridSlice = new WeatherGridSlice(timeRange, getGridInfo(),
                    new GridDataHistory[] { gridHistory }, grid,
                    key.toArray(new WeatherKey[key.size()]));
        } else if (getGridInfo().getGridType() == GridType.DISCRETE)
        // TW Average is the time weighted most common value.
        {
            Point gridSize = this.getGridInfo().getGridLoc().gridSize();
            Grid2DByte grid = new Grid2DByte(gridSize.x, gridSize.y);
            ArrayList<DiscreteKey> key = new ArrayList<DiscreteKey>();

            for (int i = 0; i < gridSize.x; i++) {
                for (int j = 0; j < gridSize.y; j++) {
                    // Dict<TextString, int> values;
                    Map<DiscreteKey, MutableInteger> values = new HashMap<DiscreteKey, MutableInteger>();
                    for (int k = 0; k < gridCount; k++) {
                        DiscreteKey key1[] = ((DiscreteGridSlice) grids[k]
                                .getGridSlice()).getKeys();
                        Grid2DByte grid1 = ((DiscreteGridSlice) grids[k]
                                .getGridSlice()).getDiscreteGrid();
                        // TextString kv = key1[grid1(i, j)].keyAsString();
                        DiscreteKey kv = key1[grid1.get(i, j)];
                        // add it to the dictionary
                        MutableInteger cnt = values.get(kv);
                        if (cnt == null) {
                            cnt = new MutableInteger();
                            values.put(kv, cnt);
                        }
                        cnt.add((int) (gridTR.get(k).getDuration() / 1000));
                    }

                    // Find the one with the highest occurance
                    DiscreteKey highestKey = null;
                    int highestCount = 0;
                    for (DiscreteKey k : values.keySet()) {
                        if (values.get(k).getValue() > highestCount) {
                            highestCount = values.get(k).getValue();
                            highestKey = k;
                        }
                    }

                    DiscreteKey newkey = new DiscreteKey(highestKey);

                    // attempt lookup for existing
                    int index = key.indexOf(newkey);
                    if (index != -1) {
                        grid.set(i, j, (byte) index);
                    } else {
                        key.add(newkey);
                        grid.set(i, j, (byte) (key.size() - 1));
                    }
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, getParmID(),
                    timeRange);
            gridSlice = new DiscreteGridSlice(timeRange, getGridInfo(),
                    new GridDataHistory[] { gridHistory }, grid,
                    key.toArray(new DiscreteKey[key.size()]));
        }

        // create the virtual parm
        ParmID pid = this.dataManager.getParmManager().getUniqueParmID(
                getParmID(), "TWAVG", "TMP");
        GridParmInfo gpi = this.gridInfo.clone();
        gpi.resetParmID(pid);

        // FIXME: LOCKING
        // LockTable lt(pid, SeqOf<Lock>(), dataMgr().myWsId());
        updateDiscreteVParmKeys(getParmID(), pid);
        Parm newParm = this.dataManager.getParmManager().createVirtualParm(pid,
                gpi, new IGridSlice[] { gridSlice }, true, false);
        newParm.getParmState().setTemporary(true);

        return newParm;
    }

    private void updateDiscreteVParmKeys(ParmID sourceid, ParmID newvpid) {
        if (getGridInfo().getGridType().equals(GridType.DISCRETE)) {
            String siteId = sourceid.getDbId().getSiteId();
            DiscreteDefinition dd = DiscreteKey.discreteDefinition(siteId);
            dd.addDefinition(newvpid.getCompositeName(), true, 0,
                    dd.keys(sourceid));
            DiscreteKey.setDiscreteDefinition(siteId, dd);
        }
    }

    /**
     * Performs an adjust operation over the specified ReferenceData area at the
     * specified time. Used by smart tools. User must use a startParmEdit and
     * endParmEdit.
     * 
     * @param deltaValue
     *            the delta value
     * @param time
     *            the time
     * @param refData
     *            the ref data
     */
    public void adjust(float deltaValue, final Date time,
            final ReferenceData refData) {
        // Can't adjust weather or discrete
        if ((getGridInfo().getGridType() == GridType.WEATHER)
                || (getGridInfo().getGridType() == GridType.DISCRETE)) {
            return;
        }

        // Get the grid
        IGridData grid = overlappingGrid(time);

        // Make sure there is a grid
        if (grid == null) {
            return;
        }

        // Convert the referenceData to a Grid2DBit
        final Grid2DBit bits = refData.getGrid();

        // Smooth the grid over the specified area
        grid.applyDelta(time, deltaValue, false, bits);
    }

    public IGridData extendParmEdit(Date absTime)
            throws GFEOperationFailedException {
        Date[] dateArray = { absTime };
        IGridData[] gridData = extendParmEdit(dateArray);

        if ((gridData != null) && (gridData.length == 1)) {
            return gridData[0];
        }

        return null;
    }

    public IGridData[] extendParmEdit(Date[] absTimes)
            throws GFEOperationFailedException {
        return setupParmEdit(absTimes, false);
    }

    /**
     * Return the set of listeners for this parm
     * 
     * @return
     */
    public ParmListeners getListeners() {
        return this.parmListeners;
    }

    public boolean isModified() {

        if (!lockTable.lockedByMe().isEmpty()) {
            return true;
        }
        return false;
    }

    /**
     * Returns a set of GridData pointers that are identical copies to the grids
     * contained in this parm's inventory. Only the grids that overlap the given
     * timeRange will be copied and returned. The user of this routine is
     * responsible for deallocating the gridData pointers returned from this
     * function.
     * 
     * Gets the grid pointers that overlap timeRange using gridInventory(). Then
     * uses GridData::makeCopy() to make an identical copy of each grid. Returns
     * the set of copied grids. The origin of the source grids are copied to the
     * returned grids.
     * 
     * @param timeRange
     * @return
     */
    protected IGridData[] getGridCopies(TimeRange timeRange) {
        IGridData[] rVal = this.getGridInventory(timeRange);
        for (int i = 0; i < rVal.length; i++) {
            try {
                rVal[i] = rVal[i].clone();
            } catch (CloneNotSupportedException e) {
                rVal[i] = null;
            }
        }

        return rVal;
    }

    /**
     * Command to shift a parameter over the given time range by the amount
     * given. If copyOnly is true, then a copy is made of the data before
     * shifting. If copyOnly is false, then the original data is moved. The
     * secondsToShift must be a multiple of the split boundary interval. Returns
     * true if this operation was valid and can be undone.
     * 
     * Ensure that the secondsToShift matches a grid interval allowed by the
     * split boundary. Calculates the destination, source, and affected time
     * ranges. Starts the parm edit, performs a split on the destination time
     * range and perhaps the given time range. Copy all of the grids, shift
     * their times. Delete the source data (for move only). Use replaceGrids()
     * to insert the new grids. Notify parm clients that the inventory has
     * changed.
     * 
     * Ideally it would be nice to just lock the source and destination time
     * ranges, rather than the entire span between them. This is impractical in
     * the current design since the undo buffers and the startParmEdit() can
     * only accept a single time range.
     * 
     * The origins of the new grids are set to modified.
     * 
     * @param tr
     * @param secondsToShift
     * @param copyOnly
     * @return
     */
    public boolean timeShiftTR(TimeRange tr, int secondsToShift,
            boolean copyOnly) {
        if ((Math.abs(secondsToShift) % this.gridInfo.getTimeConstraints()
                .getRepeatInterval()) != 0) {
            throw new IllegalArgumentException("timeShiftSelectedTR of "
                    + secondsToShift + " not an interval of "
                    + this.gridInfo.getTimeConstraints().getRepeatInterval()
                    + " for " + this.getParmID());
        }

        // Make sure the given timeRange is valid and there is something to do
        if (!tr.isValid() || (secondsToShift == 0)) {
            return false;
        }

        // calculate source and destination timeRanges
        TimeRange sourceTR = new TimeRange(tr.getStart(), tr.getEnd());
        TimeRange destinationTR = new TimeRange(tr.getStart().getTime()
                + (secondsToShift * 1000), tr.getEnd().getTime()
                + (secondsToShift * 1000));

        // Create a combinedTR that includes the all blocks it touches
        TimeRange combinedTR = destinationTR;
        IGridData targetGrids[] = this.getGridInventory(destinationTR);
        IGridData firstGrid = null;
        IGridData lastGrid = null;
        if (targetGrids.length > 0) {
            firstGrid = targetGrids[0];
            lastGrid = targetGrids[targetGrids.length - 1];
        }
        if (firstGrid != null) {
            combinedTR = combinedTR.combineWith(firstGrid.getGridTime());
        }
        if (lastGrid != null) {
            combinedTR = combinedTR.combineWith(lastGrid.getGridTime());
        }

        // If we're moving then add the sourceTR to the combindTR
        // after adjusting for the edges of the grids that overlap
        if (!copyOnly) {
            // expand the combinedTR to include blocks sourceTR touches
            IGridData sourceGrids[] = this.getGridInventory(sourceTR);
            if (sourceGrids.length == 0) {
                // USER_ALERT(_msgHand, "No grids to shift for "
                // << parmID().shortParmIdentifier() << std::endl, 'S', "GFE");
                return false; // nothing to do
            }

            firstGrid = sourceGrids[0];
            lastGrid = sourceGrids[sourceGrids.length - 1];
            if (firstGrid != null) {
                combinedTR = combinedTR.combineWith(firstGrid.getGridTime());
            }
            if (lastGrid != null) {
                combinedTR = combinedTR.combineWith(lastGrid.getGridTime());
            }
        }

        // make a copy of all of the grids now in the selected time range
        // and adjust their valid time by secondsToShift
        IGridData destGrids[] = getGridCopies(tr);
        if (destGrids.length == 0) {
            // USER_ALERT(_msgHand, "No grids to shift for "
            // << parmID().shortParmIdentifier() << std::endl,
            // 'S', "GFE");
            return false; // nothing to do
        }

        for (int i = 0; i < destGrids.length; i++) {
            TimeRange tr2 = destGrids[i].getGridTime();
            tr2 = new TimeRange(tr2.getStart().getTime()
                    + (secondsToShift * 1000), tr2.getEnd().getTime()
                    + (secondsToShift * 1000));
            destGrids[i].changeValidTime(tr2, true);
            destGrids[i].updateHistoryToModified(this.dataManager.getWsId());
        }

        // Trim the start of the first copied grid, if necessary
        if (destGrids[0].getGridTime().getStart().getTime() < (sourceTR
                .getStart().getTime() + (secondsToShift * 1000))) {
            TimeRange newTR = new TimeRange(sourceTR.getStart().getTime()
                    + (secondsToShift * 1000), destGrids[0].getGridTime()
                    .getEnd().getTime());
            destGrids[0].changeValidTime(newTR, true);
        }

        // Trim the end of the last copied grid, if necessary
        if (destGrids[destGrids.length - 1].getGridTime().getEnd().getTime() > (sourceTR
                .getEnd().getTime() + (secondsToShift * 1000))) {
            TimeRange newTR = new TimeRange(destGrids[destGrids.length - 1]
                    .getGridTime().getStart().getTime(), sourceTR.getEnd()
                    .getTime() + (secondsToShift * 1000));
            destGrids[destGrids.length - 1].changeValidTime(newTR, true);
        }

        // ok to edit?
        if (!startParmEditInternal(combinedTR)) {
            return false;
        }

        // Always split at the destinationTR and remove those grids
        boolean invChanged = split(destinationTR);

        invChanged = removeGrids(destinationTR) || invChanged;

        // If we're moving, split along the sourceTR and remove those grids
        if (!copyOnly) {
            invChanged = split(sourceTR) || invChanged;
            invChanged = removeGrids(sourceTR) || invChanged;
        }

        // now replace the destinationTR with the shifted grids
        invChanged = replaceGrids(destinationTR, destGrids) || invChanged;

        // finish up the parm edit
        boolean endOkay = endParmEdit();

        // Notify ParmClients that grids were moved.
        if (invChanged) {
            sendInvChangedNotification(combinedTR);
            // else
            // USER_ALERT(_msgHand, "No grids shifted for "
            // << parmID().shortParmIdentifier() << std::endl, 'S', "GFE");
        }

        return endOkay;
    }

    /**
     * Command to shift a parameter over its selected time range by the amount
     * given. If copyOnly is true, then a copy is made of the data before
     * shifting. If copyOnly is false, then the original data is moved. The
     * secondsToShift must be a multiple of the split boundary interval. This
     * function will perform time shifting even if the parm is not selected.
     * Returns true if this operation was valid and can be undone. Note that
     * this parm does not need to be in the selected state for this operation to
     * work.
     * 
     * Calls timeShiftTR().
     * 
     * @param secondsToShift
     * @param copyOnly
     * @return
     */
    public boolean timeShiftSelectedTR(int secondsToShift, boolean copyOnly) {
        return timeShiftTR(this.parmState.getSelectedTimeRange(),
                secondsToShift, copyOnly);
    }

    /**
     * Request to delete selected portions of the parm's inventory. Any existing
     * grids that are entirely contained within the parm's selection time range
     * will be removed. Any existing grids that are partially contained within
     * the parm's selection time range will be split. Returns true if this
     * operation was successful and the operation can be undone. Note that this
     * parm does not have to be in the selected state for this routine to
     * function.
     * 
     * Calls deleteTR() with the selected time range.
     * 
     * @return
     */
    public boolean deleteSelectedTR() {
        return deleteTR(this.parmState.getSelectedTimeRange());
    }

    /**
     * Request to delete selected portions of the parm's inventory. Any existing
     * grids that are entirely contained within the given time range will be
     * removed. Any existing grids that are partially contained within the time
     * range will be split. Returns true if this operation was successful and
     * the operation can be undone.
     * 
     * Checks to see if there is anything to delete by using gridInventory().
     * Expands out the time range to include the complete grids -- this is the
     * required lock state. Then calls startParmEdit() to validate the complete
     * editing time. Commands a split at the time range boundaries and then
     * calls remove grids over the time range. If the inventory has changed,
     * then we notify parm clients of its change.
     * 
     * @param tr
     * @return
     */
    public boolean deleteTR(TimeRange tr) {
        if (this.getGridInventory(tr).length == 0) {
            return false; // nothing to delete
        }

        // expand the tr to the time constraints (so split will work)
        TimeRange expandTR = this.getGridInfo().getTimeConstraints()
                .expandTRToQuantum(tr);

        // affectedTR is the locking time range
        TimeRange affectedTR = expandToGridTimes(expandTR);

        // Check if it's O.K. to edit
        if (!startParmEditInternal(affectedTR)) {
            return false;
        }

        // do a split if necessary at the boundaries of the time range
        boolean invChanged = split(expandTR);

        invChanged = removeGrids(expandTR) || invChanged;

        // finish up the parm edit
        boolean endOkay = endParmEdit();

        // Notify ParmClients that grids were deleted.
        if (invChanged) {
            sendInvChangedNotification(affectedTR);
        }

        return endOkay;
    }

    /**
     * Request to delete selected portions of the parm's inventory. Any existing
     * grids that are entirely contained within the given time range will be
     * removed. Any existing grids that are partially contained within the time
     * range will be split. Returns true if this operation was successful and
     * the operation can be undone.
     * 
     * Checks to see if there is anything to delete by using gridInventory().
     * Expands out the time range to include the complete grids -- this is the
     * required lock state. Checks the lock table to be sure the lock state is
     * held by the current workstation. Commands a split at the time range
     * boundaries and then calls remove grids over the time range. If the
     * inventory has changed, then we notify parm clients of its change.
     * 
     * This method is the same as deleteTR(TimeRange), except that the
     * workstation must already own the lock to the affected time range, and the
     * lock is not released when it completes.
     * 
     * @param tr
     *            The time range
     * @return true if grids in tr were deleted, false if there were no grids or
     *         the workstation does not hold the lock to the affected time
     *         range.
     */
    public boolean deleteLockedTR(TimeRange tr) {
        if (this.getGridInventory(tr).length == 0) {
            return false; // nothing to delete
        }

        // expand the tr to the time constraints (so split will work)
        TimeRange expandTR = this.getGridInfo().getTimeConstraints()
                .expandTRToQuantum(tr);

        // affectedTR is the locking time range
        TimeRange affectedTR = expandToGridTimes(expandTR);

        // Check if it's O.K. to edit
        if (LockTable.LockStatus.LOCKED_BY_ME != lockTable
                .checkLock(affectedTR)) {
            return false;
        }

        // do a split if necessary at the boundaries of the time range
        boolean invChanged = split(expandTR);

        invChanged = removeGrids(expandTR) || invChanged;

        // Notify ParmClients that grids were deleted.
        if (invChanged) {
            sendInvChangedNotification(affectedTR);
        }

        return true;

    }

    /**
     * Request to split selected portions of the parm's inventory. Any existing
     * grids that are entirely contained within the parm's selection time range
     * will not be affected. Any existing grids that are partially contained
     * within the parm's selection time range will be split. Returns true if the
     * operation was successful and it can be undone. Note that this routine
     * will operate even if the parm is not selected.
     * 
     * @return
     */
    public boolean splitSelectedTR() {
        return splitTR(this.parmState.getSelectedTimeRange());
    }

    /**
     * Returns true if a call to split() with the specified time range will
     * modify the inventory.
     * 
     * Kinda ugly. This has the same logic in it as split()...
     * 
     * @param timeRange
     * @return
     */
    public boolean willSplit(TimeRange timeRange) {
        IGridData leftGrid = overlappingGrid(timeRange.getStart());
        IGridData rightGrid = overlappingGrid(new Date(timeRange.getEnd()
                .getTime() - 1000));
        if (((leftGrid == null) || leftGrid.getGridTime().getStart()
                .equals(timeRange.getStart()))
                && ((rightGrid == null) || rightGrid.getGridTime().getEnd()
                        .equals(timeRange.getEnd()))) {
            return false; // nothing to split
        }
        return true;
    }

    /**
     * Request to split portions of the parm's inventory. Any existing grids
     * that are entirely contained within the given time range will not be
     * affected. Any existing grids that are partially contained within the
     * given time range will be split. Returns true if the operation was
     * successful and it can be undone. Note that this routine will operate even
     * if the parm is not selected.
     * 
     * Checks to see if there is anything to split by using gridInventory().
     * Expands out the given time range to include the complete grids. Commands
     * a split at the selected time range boundaries. Then we notify parm
     * clients of its change.
     * 
     * NOTE: this function is similar to deleteSelected with the exception that
     * removeGrids() is not called after the split occurs.
     * 
     * @param timeRange
     * @return
     */
    public boolean splitTR(TimeRange timeRange) {
        if (this.getGridInventory(timeRange).length == 0) {
            return false; // nothing to delete
        }

        if (!willSplit(timeRange)) {
            return false; // nothing to split
        }

        // affectedTR is the locking time range
        TimeRange affectedTR = expandToGridTimes(timeRange);

        // Check if it's O.K. to edit
        if (!startParmEditInternal(affectedTR)) {
            return false;
        }

        // do a split if necessary at the boundaries of the given time range
        boolean invChanged = split(timeRange);

        // finish up the parm edit
        // finish up the parm edit
        boolean endOkay = endParmEdit();

        // Notify ParmClients that grids were split.

        if (invChanged) {
            sendInvChangedNotification(affectedTR);
        }

        return endOkay;
    }

    /**
     * Command to copy grids from the identified source parm to this parm. The
     * time range affected is based on the selection time range of this parm. If
     * necessary, the source grid(s) will be translated into the geographical
     * coordinate system of this parm. Weighting algorithms may also be applied
     * if the split boundary information for the source and destination parms
     * differ. Note that this function will operate even if this parm is not
     * selected. Returns true if the operation was successful and it can be
     * undone.
     * 
     * Uses copyTRFrom().
     * 
     * @param sourceParm
     * @return
     */
    public boolean copySelectedTRFrom(Parm sourceParm) {
        return copyTRFrom(sourceParm, this.parmState.getSelectedTimeRange());
    }

    /**
     * Copies data from another parm and stores it into this parm. The time
     * range of interest and the source parm are given. Returns true if
     * successful. This routine is primarily used for the set of copySelected
     * To/From routines. If there aren't any grids in the source, then nothing
     * is copied to the destination.
     * 
     * The grids sent to replaceGrids() are set to INITIALIZED.
     * 
     * @param timeRange
     * @param sourceParm
     * @return
     */
    private boolean copyFromParm(TimeRange timeRange, Parm sourceParm) {

        // expand the time range to the quantum boundary if necessary
        TimeRange copyTR = this.getGridInfo().getTimeConstraints()
                .expandTRToQuantum(timeRange);

        // figure out how much will be affected
        TimeRange affectedTR = expandToGridTimes(copyTR);

        // Get the source grids and make a copy of them
        IGridData newGrids[] = sourceParm.getGridCopies(copyTR);

        // these will be the actual ones stored
        List<IGridData> gridsToPopulate = new ArrayList<IGridData>(
                newGrids.length);

        // populate grids all at once from db
        for (IGridData grid : newGrids) {
            if (!grid.isPopulated()) {
                gridsToPopulate.add(grid);
            }
        }

        // this will update the references on the array and populate all grids
        // in one call to server
        sourceParm.populateGrids(gridsToPopulate);

        // any grids within the copy time range?, if not, then nothing to do
        if (newGrids.length == 0) {
            statusHandler.handle(Priority.EVENTA, "No grids to copy for "
                    + getParmID().getShortParmId() + ' ' + affectedTR);
            // USER_ALERT(_msgHand, "No grids to copy for "
            // << parmID().shortParmIdentifier() << std::endl, 'R', "GFE");
            return false; // nothing to do
        }

        // Start the edit operation
        this.inParmEdit = false;
        if (!this.startParmEditInternal(affectedTR)) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Unable to copy grids into " + getParmID().getShortParmId()
                            + " since there is a lock owned by someone else");
            // USER_ALERT(_msgHand, "Unable to copy grids into "
            // << parmID().shortParmIdentifier()
            // << " since there is a lock owned by someone else" << std::endl,
            // 'S',
            // "GFE");
            return false;
        }

        // split this parm based on the copyTR
        boolean invChanged = split(copyTR);

        // now check the time ranges against the selection time range
        if (newGrids.length > 0) {
            TimeRange tr;
            if (newGrids[0].getGridTime().getStart().getTime() < copyTR
                    .getStart().getTime()) {
                tr = new TimeRange(copyTR.getStart(), newGrids[0].getGridTime()
                        .getEnd());
                newGrids[0].changeValidTime(tr, true);
                newGrids[0].updateHistoryToModified(this.dataManager.getWsId());
            }
            if (newGrids[newGrids.length - 1].getGridTime().getEnd().getTime() > copyTR
                    .getEnd().getTime()) {
                tr = new TimeRange(newGrids[newGrids.length - 1].getGridTime()
                        .getStart(), copyTR.getEnd());
                newGrids[newGrids.length - 1].changeValidTime(tr, true);
                newGrids[newGrids.length - 1]
                        .updateHistoryToModified(this.dataManager.getWsId());
            }
        }

        // now replace the grids
        invChanged = replaceGrids(copyTR, newGrids) || invChanged;

        // finish up the parm edit
        boolean endOkay = endParmEdit();

        // Notify ParmClients of new data.
        if (invChanged) {
            sendInvChangedNotification(affectedTR);
        }

        return endOkay;
    }

    /**
     * Command to copy grids from the identified source parm to this parm. The
     * time range affected is based on the given time range. If necessary, the
     * source grid(s) will be translated into the geographical coordinate system
     * of this parm. Weighting algorithms may also be applied if the split
     * boundary information for the source and destination parms differ. Note
     * that this function will operate even if this parm is not selected.
     * Returns true if the operation was successful and it can be undone.
     * 
     * Uses copyFromParm().
     * 
     * @param sourceParm
     * @param tr
     * @return
     */
    public boolean copyTRFrom(Parm sourceParm, TimeRange tr) {
        return copyFromParm(tr, sourceParm);
    }

    /**
     * Command to copy grids from the identified source parm to this parm. The
     * time range affected is based on the inventory span of the source parm. If
     * necessary, the source grid(s) will be translated into the geographical
     * coordinate system of this parm. Weighting algorithms may also be applied
     * if the split boundary information for the source and destination parms
     * differ. Note that the selection time range for this parm will be changed.
     * Note that this function will operate even if this parm is not selected.
     * Returns true if the operation was successful and it can be undone.
     * 
     * Uses copyFromParm() to perform the copies.
     * 
     * @param sourceParm
     * @return
     */
    public boolean copyEverythingFrom(Parm sourceParm) {
        boolean retVal = false;

        TimeRange affectedTR = sourceParm.getInventorySpan();
        retVal = copyFromParm(affectedTR, sourceParm);
        if (retVal) {
            // change the selection TR
            this.parmState.updateSelectedTimeRange(affectedTR);
        }

        return retVal;
    }

    /**
     * Utility routine to deallocate any outstanding undo grids.
     */
    protected void purgeUndoGrids() {
        this.undoBuffers.clear();
    }

    /**
     * SwapParm() swaps the given parm's contents with this parm's contents. The
     * parmID is swapped also. Note that this routine only works for parms with
     * the same parmname, model source, type, and siteID. Only the model time
     * may be different. Returns true if successful.
     * 
     * The important aspect of this routine is to swap this and the other parm
     * without changing the parm pointers. The following private data gets
     * swapped: _gridParmInfo, _grids, _lockTable. Notifications are sent out
     * for this and the other parm for gridInventoryChanged, lockTableChanged,
     * and parm id changed. Many of the private data does not get swapped since
     * it is assumed that we are swapping parms of the same name and model
     * source, and hence they have the same attributes.
     * 
     * Note that the pointers for the grid data are also swapped, no grid copies
     * are made. This is a safe operation since no one caches grid data
     * pointers.
     * 
     * Purges the undo grids since effectively the contents of the data
     * inventory have been completely replaced.
     * 
     * @param otherParm
     * @return
     */
    public boolean swapParm(Parm otherParm) {
        // verify that we can perform the swap by comparing aspects of the
        // Parmids
        if (!this.getParmID().getDbId().stripModelTime()
                .equals(otherParm.getParmID().getDbId().stripModelTime())) {
            throw new IllegalArgumentException(
                    "Attempt to swapParms with incompatible databaseIDs: "
                            + this.getParmID() + ' ' + otherParm.getParmID());
        }
        if (this.getParmID().getDbId().getModelTime()
                .equals(otherParm.getParmID().getDbId().getModelTime())) {
            throw new IllegalArgumentException(
                    "Attempt to swapParms with same modeltime: "
                            + this.getParmID() + ' ' + otherParm.getParmID());
        }

        statusHandler.handle(Priority.VERBOSE, "Swapping " + getParmID()
                + " with " + otherParm.getParmID());

        // purge the undo grids for both this and the other parm
        purgeUndoGrids();
        otherParm.purgeUndoGrids();

        // copy some of the private data from the other parm
        this.grids.acquireWriteLock();
        otherParm.grids.acquireWriteLock();
        try {
            RWLArrayList<IGridData> otherGrids = otherParm.grids;
            GridParmInfo otherParmInfo = otherParm.getGridInfo();
            LockTable otherLockTable = otherParm.getLockTable();

            // store this parm's information into the other parm's data members
            otherParm.grids = this.grids;
            otherParm.gridInfo = this.gridInfo;
            otherParm.lockTable = getLockTable();

            // now store the copies of the other's parm info into this parm
            this.grids = otherGrids;
            this.gridInfo = otherParmInfo;
            this.lockTable = otherLockTable;

            // modify the parm association for the GridSlices
            for (IGridData grid : this.grids) {
                grid.changeParmAssociation(this);
            }
            for (IGridData grid : otherParm.grids) {
                grid.changeParmAssociation(otherParm);
            }
        } finally {
            // always release locks in reverse order of acquiring to prevent
            // deadlock (note that the grids were swapped inside this try block)
            this.grids.releaseWriteLock();
            otherParm.grids.releaseWriteLock();
        }

        // the swap is now complete, send out the parm id changed notifications
        sendParmIdChangedNotification(getParmID());
        otherParm.sendParmIdChangedNotification(getParmID());

        otherParm.sendLockTableChangedNotification(otherParm.getLockTable());
        otherParm.sendInvChangedNotification(TimeRange.allTimes());

        // now send out lock table changed and inv changed notifications
        // for this parm
        sendLockTableChangedNotification(getLockTable());
        sendInvChangedNotification(TimeRange.allTimes());

        return true;
    }

    /**
     * Sends a ParmID changed notification to all parm clients. The new ParmID
     * is given.
     * 
     * @param newParmId
     *            The new ParmID for this Parm.
     */
    protected void sendParmIdChangedNotification(final ParmID newParmId) {
        if (this.dataManager != null) {
            this.dataManager.getParmManager().parmIDChanged(this, newParmId);
        }

        this.parmListeners.fireParmIDChangedListener(this, newParmId);
    }

    /**
     * Request to zero grids in selected portions of the parm's inventory. Any
     * existing grids that overlap the parm's selection time range will be
     * zeroed. time range will be zeroed. Returns true if this operation was
     * successful and the operation can be undone. Note that this parm does not
     * have to be in the selected state for this routine to function.
     * 
     * Calls zeroTR(selected time range)
     * 
     * @return
     */
    public boolean zeroSelectedTR() throws GFEException {
        return zeroTR(this.parmState.getSelectedTimeRange());
    }

    /**
     * Request to zero grids in selected portions of the parm's inventory. Any
     * existing grids that overlap the time range will be zeroed. Returns true
     * if this operation was successful and the operation can be undone. Note
     * that this parm does not have to be in the selected state for this routine
     * to function.
     * 
     * Calls assignValueTR().
     * 
     * @param tr
     * @return
     * @throws GFEServerException
     */
    public boolean zeroTR(TimeRange tr) throws GFEException {
        WxValue wxValue = WxValue.defaultValue(this);
        return assignValueTR(tr, wxValue);
    }

    /**
     * Request to assign a value to the entire grid over the selection tr.
     * Existing grids that overlap the parm's selection time range will be
     * zeroed. time range will be modified. Returns true if this operation was
     * successful and the operation can be undone. Note that this parm does not
     * have to be in the selected state for this routine to function.
     * 
     * Calls assignValueTR(selected time range, wxvalue)
     * 
     * @param wxValue
     * @return
     */
    public boolean assignValueSelectedTR(WxValue wxValue)
            throws GFEOperationFailedException {
        return assignValueTR(this.parmState.getSelectedTimeRange(), wxValue);
    }

    /**
     * Request to assign value to grids in the given time range. Existing grids
     * that overlap the time range will be modified. Returns true if this
     * operation was successful and the operation can be undone. Note that this
     * parm does not have to be in the selected state for this routine to
     * function.
     * 
     * Expands the time range to the grid times. Gets the grid inventory to
     * ensure there is something to do. Extracts the grid times. Starts a parm
     * edit with all grids. Verifies the value. Sets the entire grid (each grid)
     * to the specified value. Ends parm edit. Routine resets the VECTOR or
     * WEATHER mode if appropriate to really set the data.
     * 
     * @param tr
     * @param wxValue
     * @return
     */
    public boolean assignValueTR(TimeRange tr, WxValue wxValue)
            throws GFEOperationFailedException {
        // ensure wxValue is correct type
        if (wxValue.getParm() != this) {
            throw new IllegalArgumentException(
                    "assignValueTR with invalid wxValue [" + wxValue
                            + "] for this parm [" + this.getParmID());
        }

        // affectedTR is the locking time range
        TimeRange affectedTR = expandToGridTimes(tr);

        // get the inventory
        IGridData inv[] = this.getGridInventory(affectedTR);
        if (inv.length == 0) {
            return false; // nothing to zero
        }
        Date invTimes[] = new Date[inv.length];
        int i;
        for (i = 0; i < inv.length; i++) {
            invTimes[i] = inv[i].getGridTime().getStart();
        }

        // start parm edit
        inv = startParmEdit(invTimes);
        if (inv.length == 0) {
            return false; // no changes;
        }

        // calculate the default value
        Point gridSize = this.getGridInfo().getGridLoc().gridSize();
        Grid2DBit grid2DBit = new Grid2DBit(gridSize.x, gridSize.y);
        grid2DBit.negate(); // turn all bits on

        // temporarily change the WEATHER COMBINE or VECTOR MODE to ensure
        // that our setValue really zeros the grid.
        ParmState.VectorMode cVectorMode = this.parmState.getVectorMode();
        ParmState.CombineMode cCombineMode = this.parmState.getCombineMode();
        if (this.gridInfo.getGridType() == GFERecord.GridType.VECTOR) {
            this.parmState.setVectorMode(ParmState.VectorMode.BOTH);
        } else if ((this.gridInfo.getGridType() == GFERecord.GridType.WEATHER)
                || (this.gridInfo.getGridType() == GFERecord.GridType.DISCRETE)) {
            this.parmState.setCombineMode(ParmState.CombineMode.REPLACE);
        }

        // process each grid
        for (i = 0; i < inv.length; i++) {
            if (inv[i] != null) {
                inv[i].setValue(wxValue, grid2DBit);
            }
        }

        // finish up the parm edit
        boolean endOkay = endParmEdit();

        // restore the edit modes
        if (this.gridInfo.getGridType() == GFERecord.GridType.VECTOR) {
            this.parmState.setVectorMode(cVectorMode);
        } else if ((this.gridInfo.getGridType() == GFERecord.GridType.WEATHER)
                || (this.gridInfo.getGridType() == GFERecord.GridType.DISCRETE)) {
            this.parmState.setCombineMode(cCombineMode);
        }

        return endOkay;
    }

    /**
     * Request to fragment grids in selected portions of the parm's inventory.
     * Any existing grids that overlap the selected time range will be replaced
     * with grids of 1 quantum duration with the same data. Returns true if this
     * operation was successful and the operation can be undone. Note that this
     * parm does not have to be in the selected state for this routine to
     * function.
     * 
     * Uses fragmentTR().
     * 
     * @return
     */
    public boolean fragmentSelectedTR() {
        return fragmentTR(this.parmState.getSelectedTimeRange());
    }

    /**
     * Request to fragment grids in selected portions of the parm's inventory.
     * Any existing grids that overlap the time range will be replaced with
     * grids of 1 quantum duration with the same data. Returns true if this
     * operation was successful and the operation can be undone. Note that this
     * parm does not have to be in the selected state for this routine to
     * function.
     * 
     * Fragments 1 grid at a time and uses replaceGriddedData().
     * 
     * @param tr
     * @return
     */
    public boolean fragmentTR(TimeRange tr) {
        if (this.getGridInventory(tr).length == 0) {
            return false; // nothing to fragment
        }

        if (!willSplit(tr)) {
            TimeRange expandTR = this.getGridInfo().getTimeConstraints()
                    .expandTRToQuantum(tr);
            IGridData[] inv = this.getGridInventory(expandTR);
            boolean found = false;
            for (int i = 0; i < inv.length; i++) {
                if (inv[i] == null) {
                    continue;
                }
                if (this.getGridInfo().getTimeConstraints()
                        .constraintTimes(inv[i].getGridTime()).length != 0) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }

        // expand the tr to the time constraints (so split will work)
        TimeRange expandTR = this.getGridInfo().getTimeConstraints()
                .expandTRToQuantum(tr);

        // affectedTR is the locking time range
        TimeRange affectedTR = expandToGridTimes(expandTR);

        // Check if it's O.K. to edit
        if (!startParmEditInternal(affectedTR)) {
            return false;
        }

        // do a split if necessary at the boundaries of the time range
        boolean invChanged = split(expandTR);

        // now get the primary grid data that remains within expandTR
        // Couldn't do this before since split may have changed GridData
        // pointers
        IGridData[] inv = this.getGridInventory(expandTR);

        // get each grid from the range and see if it needs fragmenting
        ArrayList<IGridData> newGrids = new ArrayList<IGridData>();
        for (int i = 0; i < inv.length; i++) {
            if (inv[i] == null) {
                continue; // skip this one
            }
            TimeRange times[] = this.getGridInfo().getTimeConstraints()
                    .constraintTimes(inv[i].getGridTime());
            for (int j = 0; j < times.length; j++) {
                IGridData copy;
                try {
                    copy = inv[i].clone();
                } catch (CloneNotSupportedException e) {
                    copy = null;
                }
                copy.changeValidTime(times[j], true);
                newGrids.add(copy);
            }
        }

        invChanged = this.replaceGrids(expandTR,
                newGrids.toArray(new IGridData[newGrids.size()]))
                || invChanged;

        // finish up the parm edit
        boolean endOkay = endParmEdit();

        // Notify ParmClients that grids were deleted.
        if (invChanged) {
            sendInvChangedNotification(affectedTR);
        }

        return endOkay;
    }

    /**
     * Command to create data from scratch for the selected time range for this
     * parameter. Interval specifies the create interval. Duration specifies the
     * duration of each grid gridSlice to be created. Returns true if the
     * request was handled.
     * 
     * Uses createFromScratchTR().
     * 
     * @param mode
     * @param interval
     * @param duration
     * @return
     * @throws GFEOperationFailedException
     */
    public boolean createFromScratchSelectedTR(CreateFromScratchMode mode,
            int interval, int duration) throws GFEOperationFailedException {
        return createFromScratchTR(this.parmState.getSelectedTimeRange(), mode,
                interval, duration);
    }

    /**
     * Command to create data from scratch for the given time range for this
     * parameter. Interval specifies the create interval. Duration specifies the
     * duration of each created grid. Returns true if the request was handled.
     * 
     * @param tr
     * @param mode
     * @param interval
     * @param duration
     * @return
     * @throws GFEOperationFailedException
     */
    public boolean createFromScratchTR(TimeRange tr,
            CreateFromScratchMode mode, int interval, int duration)
            throws GFEOperationFailedException {
        // Check if it's O.K. to edit
        if (!isOkToEdit(tr)) {
            return false;
        }

        List<TimeRange> gridTimes = new ArrayList<TimeRange>();

        TimeConstraints tc = gridInfo.getTimeConstraints();

        boolean gaps = tc.getDuration() == tc.getRepeatInterval() ? false
                : true;

        if (gaps) {
            duration = tc.getDuration();
            if (interval < tc.getRepeatInterval()) {
                interval = tc.getRepeatInterval();
            }
            if ((interval % tc.getRepeatInterval()) != 0) {
                interval = ((interval / tc.getRepeatInterval()) + 1)
                        * tc.getRepeatInterval();
            }
        }

        if ((interval == 0) || (duration == 0)) {
            gridTimes.add(tr);
        }

        else if (gaps) {
            TimeRange[] times = tc.constraintTimes(tr);
            int increment = interval / tc.getRepeatInterval();
            if (increment < 1) {
                increment = 1;
            }
            for (TimeRange time : times) {
                gridTimes.add(time);
            }
        }

        else {
            Date t = tr.getStart();
            TimeRange gridTR = new TimeRange(t, duration * 1000);
            Calendar cal = new GregorianCalendar();
            cal.setTime(gridTR.getStart());
            while (tr.contains(gridTR)) {
                gridTimes.add(gridTR);
                cal.add(Calendar.SECOND, interval);
                gridTR = new TimeRange(cal.getTime(), duration * 1000);
            }
        }

        if (gridTimes.size() > 0) {
            insertNewGrid(gridTimes.toArray(new TimeRange[] {}), mode);
        }
        return true;
    }

    /**
     * Calculates the minimum value at each gridPoint for all the grids that are
     * contained within the specified TimeRange.
     * 
     * @param timeRange
     * @return
     */
    public Parm min(TimeRange timeRange) {
        // Can't take the minimum of a weather or discrete type
        if ((this.gridInfo.getGridType() == GridType.WEATHER)
                || (this.gridInfo.getGridType() == GridType.DISCRETE)) {
            return null;
        }

        // Get the list of grids that are found inside the specified timeRange
        IGridData grids[] = this.getGridInventory(timeRange);

        // See if we have any grids first
        if (grids.length == 0) {
            return null;
        }

        TimeRange vtime = new TimeRange(grids[0].getGridTime().getStart()
                .getTime(), grids[grids.length - 1].getGridTime().getEnd()
                .getTime());

        // Make a new GridSlice into which the result will go

        IGridSlice gridSlice = null;
        try {
            gridSlice = grids[0].getGridSlice().clone();
        } catch (CloneNotSupportedException e) {
            // This should never happen.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        gridSlice
                .setHistory(new GridDataHistory[] { new GridDataHistory(
                        GridDataHistory.OriginType.CALCULATED,
                        this.getParmID(), vtime) });
        gridSlice.setValidTime(vtime);

        // loop through each grid starting at 1 since we copied the 0th
        for (int i = 1; i < grids.length; i++) {
            gridSlice = grids[i].gridMin(gridSlice);
        }

        // create the virtual parm
        ParmID pid = this.dataManager.getParmManager().getUniqueParmID(
                this.getParmID(), "MIN", "TMP");
        GridParmInfo gpi = this.gridInfo.clone();
        gpi.resetParmID(pid);
        // TODO
        // LockTable lt = new LockTable(pid, SeqOf<Lock>(),
        // this.dataManager.getWsId());
        updateDiscreteVParmKeys(getParmID(), pid);
        Parm newParm = this.dataManager.getParmManager().createVirtualParm(pid,
                gpi, new IGridSlice[] { gridSlice }, true, false);
        newParm.parmState.setTemporary(true);

        return newParm;
    }

    /**
     * Calculates the maximum value at each gridPoint for all the grids that are
     * contained within the specified TimeRange.
     * 
     * @param timeRange
     * @return
     */
    public Parm max(TimeRange timeRange) {
        // Can't take the minimum of a weather or discrete type
        if ((this.gridInfo.getGridType() == GridType.WEATHER)
                || (this.gridInfo.getGridType() == GridType.DISCRETE)) {
            return null;
        }

        // Get the list of grids that are found inside the specified timeRange
        IGridData grids[] = this.getGridInventory(timeRange);

        // See if we have any grids first
        if (grids.length == 0) {
            return null;
        }

        TimeRange vtime = new TimeRange(grids[0].getGridTime().getStart()
                .getTime(), grids[grids.length - 1].getGridTime().getEnd()
                .getTime());

        // Make a new GridSlice into which the result will go

        IGridSlice gridSlice = null;
        try {
            gridSlice = grids[0].getGridSlice().clone();
        } catch (CloneNotSupportedException e) {
            // This should never happen.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        gridSlice
                .setHistory(new GridDataHistory[] { new GridDataHistory(
                        GridDataHistory.OriginType.CALCULATED,
                        this.getParmID(), vtime) });
        gridSlice.setValidTime(vtime);

        // loop through each grid starting at 1 since we copied the 0th
        for (int i = 1; i < grids.length; i++) {
            gridSlice = grids[i].gridMax(gridSlice);
        }

        // create the virtual parm
        ParmID pid = this.dataManager.getParmManager().getUniqueParmID(
                this.getParmID(), "MAX", "TMP");
        GridParmInfo gpi = this.gridInfo.clone();
        gpi.resetParmID(pid);
        // TODO
        // LockTable lt = new LockTable(pid, SeqOf<Lock>(),
        // this.dataManager.getWsId());
        updateDiscreteVParmKeys(getParmID(), pid);
        Parm newParm = this.dataManager.getParmManager().createVirtualParm(pid,
                gpi, new IGridSlice[] { gridSlice }, true, false);
        newParm.parmState.setTemporary(true);

        return newParm;
    }

    /**
     * Calculates the sum at each gridPoint for all the grids that are contained
     * within the specified TimeRange.
     * 
     * @param timeRange
     * @return
     */
    public Parm sum(TimeRange timeRange) {
        // Can't take the minimum of a weather or discrete type
        if ((this.gridInfo.getGridType() == GridType.WEATHER)
                || (this.gridInfo.getGridType() == GridType.DISCRETE)) {
            return null;
        }

        // Get the list of grids that are found inside the specified timeRange
        IGridData grids[] = this.getGridInventory(timeRange);

        // See if we have any grids first
        if (grids.length == 0) {
            return null;
        }

        TimeRange vtime = new TimeRange(grids[0].getGridTime().getStart()
                .getTime(), grids[grids.length - 1].getGridTime().getEnd()
                .getTime());

        // Make a new GridSlice into which the result will go

        IGridSlice gridSlice = null;
        try {
            gridSlice = grids[0].getGridSlice().clone();
        } catch (CloneNotSupportedException e) {
            // This should never happen.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        gridSlice
                .setHistory(new GridDataHistory[] { new GridDataHistory(
                        GridDataHistory.OriginType.CALCULATED,
                        this.getParmID(), vtime) });
        gridSlice.setValidTime(vtime);

        // loop through each grid starting at 1 since we copied the 0th
        for (int i = 1; i < grids.length; i++) {
            gridSlice = grids[i].gridSum(gridSlice);
        }

        // create the virtual parm
        ParmID pid = this.dataManager.getParmManager().getUniqueParmID(
                this.getParmID(), "SUM", "TMP");
        GridParmInfo gpi = this.gridInfo.clone();
        gpi.resetParmID(pid);
        // TODO
        // LockTable lt = new LockTable(pid, SeqOf<Lock>(),
        // this.dataManager.getWsId());
        updateDiscreteVParmKeys(getParmID(), pid);
        Parm newParm = this.dataManager.getParmManager().createVirtualParm(pid,
                gpi, new IGridSlice[] { gridSlice }, true, false);
        newParm.parmState.setTemporary(true);

        return newParm;
    }

    /**
     * Calculates the average at each gridPoint for all the grids that are
     * contained within the specified TimeRange.
     * 
     * @param timeRange
     * @return
     */
    public Parm avg(TimeRange timeRange) {
        // Get the list of grids that are found inside the specified timeRange
        IGridData grids[] = this.getGridInventory(timeRange);

        // See if we have any grids first
        if (grids.length == 0) {
            return null;
        }

        // get some stuff for later use
        Point gridSize = this.getGridInfo().getGridLoc().gridSize();
        int gridCount = grids.length;
        GridParmInfo thisGridInfo = grids[0].getParm().getGridInfo();

        // Make a new GridSlice into which the result will go
        IGridSlice gridSlice = null;

        int i, j, k;

        // ServerResponse sr;
        if (grids.length == 1) {
            // nothing to average so we're done
            try {
                gridSlice = grids[0].getGridSlice().clone();
            } catch (CloneNotSupportedException e) {
                // This should never happen.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        } else if (this.gridInfo.getGridType() == GridType.SCALAR) {
            Grid2DFloat grid = new Grid2DFloat(gridSize.x, gridSize.y);
            Grid2DFloat[] scalarGrids = new Grid2DFloat[gridCount];
            for (int c = 0; c < gridCount; c++) {
                scalarGrids[c] = ((ScalarGridSlice) grids[c].getGridSlice())
                        .getScalarGrid();
            }
            for (i = 0; i < gridSize.x; i++) {
                for (j = 0; j < gridSize.y; j++) {
                    float value = 0.0f;
                    for (k = 0; k < gridCount; k++) {
                        value += scalarGrids[k].get(i, j);
                    }
                    grid.set(i, j, value / gridCount);
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, this.getParmID(),
                    timeRange);
            gridSlice = new ScalarGridSlice(timeRange, this.gridInfo,
                    new GridDataHistory[] { gridHistory }, grid);
        } else if (this.gridInfo.getGridType() == GridType.VECTOR)
        // Do average using UV to MagDir conversions
        {
            // Get the mag and dir grids
            Grid2DFloat uGrid, vGrid;
            Grid2DFloat uSum = new Grid2DFloat(gridSize.x, gridSize.y, 0.0f);
            Grid2DFloat vSum = new Grid2DFloat(gridSize.x, gridSize.y, 0.0f);
            // sum
            for (k = 0; k < gridCount; k++) {
                uGrid = ((VectorGridSlice) grids[k].getGridSlice())
                        .vectorUGrid();
                vGrid = ((VectorGridSlice) grids[k].getGridSlice())
                        .vectorVGrid();

                for (i = 0; i < gridSize.x; i++) {
                    for (j = 0; j < gridSize.y; j++) {
                        uSum.set(i, j, uSum.get(i, j) + uGrid.get(i, j));
                        vSum.set(i, j, vSum.get(i, j) + vGrid.get(i, j));
                    }
                }
            }
            // average
            for (i = 0; i < gridSize.x; i++) {
                for (j = 0; j < gridSize.y; j++) {
                    uSum.set(i, j, uSum.get(i, j) / gridCount);
                    vSum.set(i, j, vSum.get(i, j) / gridCount);
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, this.getParmID(),
                    timeRange);
            gridSlice = VectorGridSlice.makeGridSliceFromUV(uSum, vSum,
                    timeRange, thisGridInfo,
                    new GridDataHistory[] { gridHistory });
        }

        else if (this.gridInfo.getGridType() == GridType.WEATHER)
        // Average will be a combination of all weatherKeys
        {
            Grid2DByte grid = new Grid2DByte(gridSize.x, gridSize.y);
            List<WeatherKey> key = new ArrayList<WeatherKey>();
            Map<String, Byte> idxMap = new HashMap<String, Byte>();

            for (j = 0; j < gridSize.y; j++) {
                for (i = 0; i < gridSize.x; i++) {
                    // Build an index sequence string.
                    StringBuilder idxSb = new StringBuilder();
                    for (k = 0; k < gridCount; k++) {
                        DiscreteGridSlice slice = ((DiscreteGridSlice) grids[k]
                                .getGridSlice());
                        byte inIdx = slice.getDiscreteGrid().get(i, j);
                        idxSb.append(inIdx).append(":");
                    }
                    String idxSeq = idxSb.toString();

                    // See if we know the weather key for this index sequence.
                    Byte idx = idxMap.get(idxSeq);
                    if (idx == null) {
                        // new idxSeq. Build a weather key for it.
                        ArrayList<WeatherSubKey> subkeys = new ArrayList<WeatherSubKey>();
                        for (k = 0; k < gridCount; k++) {
                            WeatherGridSlice slice = ((WeatherGridSlice) grids[k]
                                    .getGridSlice());
                            WeatherKey[] key1 = slice.getKeys();
                            Grid2DByte grid1 = slice.getWeatherGrid();
                            WeatherKey key1ij = key1[grid1.get(i, j)];
                            subkeys.addAll(key1ij.getSubKeys());
                        }

                        WeatherKey wxkey = new WeatherKey(getParmID().getDbId()
                                .getSiteId(), subkeys);

                        // Keys for different idxSeqs might be equivalent.
                        // Look for identical WeatherKey in key.
                        int index = key.indexOf(wxkey);
                        if (index < 0) {
                            // No equivalent, so add it and use new index
                            idx = Byte.valueOf((byte) key.size());
                            key.add(wxkey);
                        } else {
                            // found an equivalent key so use its index
                            idx = Byte.valueOf((byte) index);
                        }
                        // Store idx for idxSeq for lookup on later i,j
                        idxMap.put(idxSeq, idx);
                    }

                    grid.set(i, j, idx.intValue());
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, this.getParmID(),
                    timeRange, SimulatedTime.getSystemTime().getTime(),
                    dataManager.getWsId());
            gridSlice = new DiscreteGridSlice(timeRange, this.gridInfo,
                    new GridDataHistory[] { gridHistory }, grid,
                    key.toArray(new DiscreteKey[0]));
        }

        else if (this.gridInfo.getGridType() == GridType.DISCRETE)
        // Average is the most common value.
        // If two are equally common (i.e., only 2 grids), the earliest wins.
        {
            Grid2DByte grid = new Grid2DByte(gridSize.x, gridSize.y);
            ArrayList<DiscreteKey> key = new ArrayList<DiscreteKey>();

            for (j = 0; j < gridSize.y; j++) {
                for (i = 0; i < gridSize.x; i++) {

                    // DiscreteKey.compareTo() => 0; use Strings.
                    Map<String, Integer> values = new TreeMap<String, Integer>();
                    Map<String, DiscreteKey> keys = new TreeMap<String, DiscreteKey>();

                    for (k = 0; k < gridCount; k++) {
                        DiscreteKey[] key1 = ((DiscreteGridSlice) grids[k]
                                .getGridSlice()).getKeys();
                        Grid2DByte grid1 = ((DiscreteGridSlice) grids[k]
                                .getGridSlice()).getDiscreteGrid();
                        DiscreteKey dkv = key1[grid1.get(i, j)];
                        String dks = dkv.toString();
                        // add it to the dictionary
                        Integer count = values.get(dks);
                        if (count == null) {
                            count = Integer.valueOf(0);
                            keys.put(dks, dkv);
                        }
                        count += 1;
                        values.put(dks, count);
                    }

                    // Find the one with the highest occurance
                    String keyOfHighest = null;
                    Integer highestCount = Integer.valueOf(0);
                    for (Entry<String, Integer> entry : values.entrySet()) {
                        if (entry.getValue() > highestCount) {
                            keyOfHighest = entry.getKey();
                            highestCount = entry.getValue();
                        }
                    }

                    DiscreteKey newkey = new DiscreteKey(keys.get(keyOfHighest));

                    // attempt lookup for existing
                    int index = key.indexOf(newkey);
                    if (index == -1) {
                        index = key.size();
                        key.add(newkey);
                    }
                    grid.set(i, j, index);
                }
            }

            GridDataHistory gridHistory = new GridDataHistory(
                    GridDataHistory.OriginType.CALCULATED, this.getParmID(),
                    timeRange, SimulatedTime.getSystemTime().getTime(),
                    dataManager.getWsId());
            gridSlice = new DiscreteGridSlice(timeRange, this.gridInfo,
                    new GridDataHistory[] { gridHistory }, grid,
                    key.toArray(new DiscreteKey[0]));
        }

        // create the virtual parm
        ParmID pid = this.dataManager.getParmManager().getUniqueParmID(
                this.getParmID(), "AVG", "TMP");
        GridParmInfo gpi = this.gridInfo.clone();
        gpi.resetParmID(pid);
        // TODO
        // LockTable lt = new LockTable(pid, SeqOf<Lock>(),
        // this.dataManager.getWsId());
        updateDiscreteVParmKeys(getParmID(), pid);
        Parm newParm = this.dataManager.getParmManager().createVirtualParm(pid,
                gpi, new IGridSlice[] { gridSlice }, true, false);
        newParm.parmState.setTemporary(true);
        return newParm;
    }

    public void smooth(Date time, ReferenceData refData) {
        IGridData grid = overlappingGrid(time);

        // Make sure there is a grid
        if (grid == null) {
            return;
        }

        // Convert the referenceData to a Grid2DBit
        Grid2DBit bits = refData.getGrid();

        // Smooth the grid over the specified area
        grid.smooth(time, bits);

    }

    /**
     * Performs a pencil stretch operation on the grid. Provided the value to be
     * assigned, and the path in AWIPS world coordintes. User must call
     * startParmEdit() and endParmEdit(). The limitToEditArea indicates whether
     * the pencil stretch operation should be limited to just the active edit
     * area.
     * 
     * @param time
     * @param value
     * @param path
     * @return
     */
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate path[]) {
        return pencilStretch(time, value, path, true);
    }

    /**
     * Performs a pencil stretch operation on the grid. Provided the value to be
     * assigned, and the path in AWIPS world coordintes. User must call
     * startParmEdit() and endParmEdit(). The limitToEditArea indicates whether
     * the pencil stretch operation should be limited to just the active edit
     * area.
     * 
     * @param time
     * @param value
     * @param path
     * @param limitToEditArea
     * @return
     */
    public Grid2DBit pencilStretch(Date time, WxValue value, Coordinate path[],
            boolean limitToEditArea) {
        // Get the grid
        IGridData grid = overlappingGrid(time);

        // Make sure there is a grid
        if (grid == null) {
            return null;
        }

        // do the pencil stretch
        return grid.pencilStretch(time, value, path, limitToEditArea);
    }

    /**
     * Returns the one grid that overlaps the input time. In the event that no
     * grid overlaps the input time, then NULL is returned. The pointer
     * represents the actual grid within this parm. The user must not deallocate
     * it.
     * 
     * Calls getOverlappingGrid();
     * 
     * @param time
     * @return
     */
    public IGridData overlappingGrid(Date time) {
        this.grids.acquireReadLock();
        try {
            int begin = 0;
            int end = this.grids.size();
            int mid = end / 2;

            while (begin < end) {
                // Did we find it?
                if (this.grids.get(mid).getGridTime().contains(time)) {
                    return this.grids.get(mid);
                }

                if (time.getTime() < this.grids.get(mid).getGridTime().getEnd()
                        .getTime()) // on
                // the
                // left
                {
                    end = mid - 1;
                    mid = ((end - begin) / 2) + begin;
                } else // on the right
                {
                    begin = mid + 1;
                    mid = ((end - begin) / 2) + begin;
                }
            }

            // Did we find it? Last chance...
            if ((mid >= 0) && (mid < this.grids.size())
                    && this.grids.get(mid).getGridTime().contains(time)) {
                return this.grids.get(mid);
            }
        } finally {
            this.grids.releaseReadLock();
        }

        // If we got this far, we didn't find it.
        return null;
    }

    /**
     * Performs a move/copy area edit on a grid for the given time, points to
     * move or copy, the amount to move, and whether this is a copy or a move
     * operation. User must call startParmEdit() and endParmEdit().
     * 
     * @param time
     * @param pointsToMove
     * @param delta
     * @parm copyOp
     */
    public void moveCopyArea(final Date time, final Grid2DBit pointsToMove,
            final Point delta, boolean copyOp) {
        // Get the grid
        IGridData grid = overlappingGrid(time);

        // Make sure there is a grid
        if (grid == null) {
            return;
        }

        // move/copy the grid
        grid.moveCopyArea(time, pointsToMove, delta, copyOp);
    }

    /**
     * Command to interpolate data for the selected time range for this
     * parameter. Interval specifies the interpolation interval. Duration
     * specifies the duration of each grid gridSlice to be interpolated. Returns
     * true if the interpolation request was made.
     * 
     * @param interpMode
     * @param state
     * @param interval
     * @param duration
     * @return
     * @throws GFEOperationFailedException
     */
    public boolean interpolateSelectedTR(InterpMode interpMode,
            InterpState state, int interval, int duration)
            throws GFEOperationFailedException {
        return interpolateTR(this.parmState.getSelectedTimeRange(), interpMode,
                state, interval, duration);
    }

    /**
     * Command to interpolate data for the given time range for this parameter.
     * Interval specifies the interpolation interval. Duration specifies the
     * duration of each interpolated grid. Returns true if the interpolation
     * request was made.
     * 
     * Calls interpolateTRInternal() with the quiet flag set to false.
     * 
     * @param tr
     * @param interpMode
     * @param state
     * @param interval
     * @param duration
     * @return
     * @throws GFEOperationFailedException
     */
    public boolean interpolateTR(final TimeRange tr, InterpMode interpMode,
            InterpState state, int interval, int duration)
            throws GFEOperationFailedException {
        return interpolateTRInternal(tr, interpMode, state, interval, duration,
                false);
    }

    /**
     * Command to interpolate data for the given time range for this parameter.
     * Interval specifies the interpolation interval. Duration specifies the
     * duration of each interpolated grid. The quiet flag indicates whether any
     * logging messages (to USER_ALERT) should be made on errors or finish.
     * Returns true if the interpolation request was made.
     * 
     * Get the grid inventory and addd grids on either side to get a complete
     * set. then using splitBoundary, make empty dataSlices with appropriate
     * TimeRanges. Create an interpolator object, request interpolation (creates
     * an interp request) and call for interpolation.
     * 
     * base grids are ones with data already established, and are used as the
     * control values for interpolation.
     * 
     * @param tr
     * @param interpMode
     * @param state
     * @param interval
     * @param duration
     * @param quietMode
     * @return
     * @throws GFEOperationFailedException
     */
    private boolean interpolateTRInternal(final TimeRange tr,
            InterpMode interpMode, InterpState state, int interval,
            int duration, boolean quietMode) throws GFEOperationFailedException {
        int i, j;

        // if interval is not set, then set it to the time constraints
        if (interval == 0) {
            interval = this.gridInfo.getTimeConstraints().getRepeatInterval();
        }

        // set duration to interval if needed
        if ((interval != 0) && (duration == 0)) {
            duration = interval;
        }

        // Check if it's O.K. to edit
        if (!isOkToEdit(tr)) {
            return false;
        }

        // Select the base grids.

        // copy the inventory
        List<IGridData> baseGrids;
        this.grids.acquireReadLock();
        try {
            baseGrids = new ArrayList<IGridData>(this.grids);
        } finally {
            this.grids.releaseReadLock();
        }

        // if in NON_EDITED mode, reject all base grids that were
        // edited.
        if (interpMode.equals(InterpMode.EDITED)) {
            for (i = baseGrids.size() - 1; i >= 0; i--) {
                for (j = 0; j < baseGrids.get(i).getHistory().length; j++) {
                    if (!dataManager.getWsId().equals(
                            baseGrids.get(i).getHistory()[j].getWhoModified())) {
                        baseGrids.remove(i); // remove it from
                        // consideration
                        break; // out of the j loop
                    }
                }
            }
        }

        if (baseGrids.size() < 2) {
            if (quietMode) {
                Activator
                        .getDefault()
                        .getLog()
                        .log(new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                                "Interpolation requires at least two base grids for "
                                        + getParmID().getShortParmId()));
            } else {
                statusHandler.handle(Priority.EVENTA,
                        "Interpolation requires at least two base grids for "
                                + getParmID().getShortParmId());
            }
            return false;
        }

        // Adjust array of base grids to best match the selection time
        // range.

        // find the first base grid that begins After the
        // selection start time, and then set index to previous base
        // grid.
        int startIndex = -1;
        for (i = 0; i < baseGrids.size(); i++) {
            if (baseGrids.get(i).getGridTime().getStart()
                    .compareTo(tr.getStart()) > 0) {
                startIndex = i - 1;
                break;
            }
        }

        // find the base grid that has end time >= the selection end
        // time
        int endIndex = -1;
        for (i = startIndex + 1; i < baseGrids.size(); i++) {
            if (baseGrids.get(i).getGridTime().getEnd().compareTo(tr.getEnd()) >= 0) {
                endIndex = i;
                break;
            }
        }

        // eliminate all of the earlier and the later grids from
        // basegrids
        if ((endIndex != -1) && (endIndex < (baseGrids.size() - 1))) {
            baseGrids.subList(endIndex + 1, baseGrids.size()).clear();
        }
        if (startIndex >= 1) {
            baseGrids.subList(0, startIndex).clear();
        }

        // find all the possible gaps within the selection time range,
        // then
        // eliminate the ones that overlap the base grids
        TimeRange[] times = gridInfo.getTimeConstraints().constraintTimes(tr);
        if (times.length == 0) {
            return false;
        }

        int estimatedGrids = (int) (tr.getDuration() / 1000 / interval);

        List<TimeRange> temp = new ArrayList<TimeRange>(estimatedGrids);
        Date st = times[0].getStart();
        TimeRange gTR;
        while (true) {
            // calculate primary tr of grid w/o regard to base grids
            gTR = new TimeRange(st, duration * 1000);
            if (gridInfo.getTimeConstraints().getRepeatInterval() != gridInfo
                    .getTimeConstraints().getDuration()) {
                gTR = gridInfo.getTimeConstraints().constraintTime(
                        gTR.getStart());
            } else {
                gTR = gridInfo.getTimeConstraints().expandTRToQuantum(gTR);
            }
            if (gTR.isValid()) {
                if (gTR.getStart().compareTo(tr.getEnd()) < 0) {
                    if (gTR.getEnd().compareTo(tr.getEnd()) > 0) {
                        gTR = new TimeRange(gTR.getStart(), tr.getEnd());
                    }
                    temp.add(gTR);
                } else {
                    break;
                }
            }
            st.setTime(st.getTime() + (interval * 1000));
        }

        // now split grids if necessary due to interference with base grids
        List<TimeRange> newDSTR = new ArrayList<TimeRange>(estimatedGrids);
        for (TimeRange t : temp) {
            boolean overlaps = false;
            for (i = 0; i < baseGrids.size(); i++) {
                final TimeRange bgTime = baseGrids.get(i).getGridTime();
                if (t.overlaps(bgTime)) {
                    if (bgTime.contains(t)) {
                        break; // can't salvage this one
                    }
                    if ((bgTime.getStart().compareTo(t.getStart()) <= 0)
                            && (bgTime.getEnd().compareTo(t.getEnd()) < 0)) {
                        newDSTR.add(new TimeRange(bgTime.getEnd(), t.getEnd()));
                        overlaps = true;
                    } else if ((bgTime.getStart().compareTo(t.getStart()) > 0)
                            && (bgTime.getEnd().compareTo(t.getEnd()) >= 0)) {
                        newDSTR.add(new TimeRange(t.getStart(), bgTime
                                .getStart()));
                        overlaps = true;
                    } else if (t.contains(bgTime)) {
                        // split into 2 - left part
                        newDSTR.add(new TimeRange(t.getStart(), bgTime
                                .getStart()));
                        newDSTR.add(new TimeRange(bgTime.getEnd(), t.getEnd()));
                        overlaps = true;
                    }
                }
            }
            if (!overlaps) {
                newDSTR.add(t);
            }
        }

        // almost done - now eliminate any calculated ones that still
        // overlap
        // this will catch any base grids that are together.
        for (j = 0; j < baseGrids.size(); j++) {
            for (i = newDSTR.size() - 1; i >= 0; i--) {
                if (baseGrids.get(j).getGridTime().overlaps(newDSTR.get(i))) {
                    newDSTR.remove(i);
                }
            }
        }

        // last pass - eliminate any entries that overlap other entries
        for (i = 0; i < newDSTR.size(); i++) {
            for (j = newDSTR.size() - 1; j > i; j--) {
                if (newDSTR.get(i).overlaps(newDSTR.get(j))) {
                    newDSTR.remove(j);
                }
            }
        }

        // Make the gridslice and timerange arrays required as input
        // for interpolation.
        List<IGridSlice> dataSlices = new ArrayList<IGridSlice>(estimatedGrids);
        List<TimeRange> interptimes = new ArrayList<TimeRange>(estimatedGrids);

        for (i = 0; i < baseGrids.size(); i++) {
            IGridSlice bs = baseGrids.get(i).getGridSlice();
            dataSlices.add(bs);
            interptimes.add(bs.getValidTime());

            // if a gap's time range fits in here, append to arrays.
            if (i < (baseGrids.size() - 1)) {
                IGridSlice nbs = baseGrids.get(i + 1).getGridSlice();
                for (j = 0; j < newDSTR.size(); j++) {
                    TimeRange ntr = newDSTR.get(j);
                    if ((ntr.getStart().compareTo(bs.getValidTime().getEnd()) >= 0)
                            && (ntr.getEnd().compareTo(
                                    nbs.getValidTime().getStart()) <= 0)) {
                        // make an empty NONE type GridSlice for the
                        // ones to
                        // be interpolated, and append the time for it.
                        IGridSlice ds1 = null;
                        dataSlices.add(ds1);
                        interptimes.add(ntr);
                    }
                }
            }
        }

        // Final check to see if there is anything to do
        int count = 0;
        for (i = 0; i < dataSlices.size(); i++) {
            if (dataSlices.get(i) == null) {
                count++;
                break;
            }
        }
        if (count == 0) {
            if (quietMode) {
                Activator
                        .getDefault()
                        .getLog()
                        .log(new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                                "No grids need interpolating for "
                                        + getParmID().getShortParmId()));
            } else {
                statusHandler.handle(Priority.EVENTA,
                        "No grids need interpolating for "
                                + getParmID().getShortParmId());
            }
            return false;
        }

        // Make Interpolator object
        // use default cstr; no args
        boolean newInterp = false;
        if (this.interpolator == null) {
            this.interpolator = new Interpolator();
            newInterp = true;
        }

        // make Interpolator requests
        if (!this.interpolator.requestInterpolation(getParmID(), gridInfo,
                interptimes.toArray(new TimeRange[interptimes.size()]),
                dataSlices.toArray(new IGridSlice[dataSlices.size()]),
                this.parmState.getInterpolateAlgorithm())) {
            if (quietMode) {
                Activator
                        .getDefault()
                        .getLog()
                        .log(new Status(IStatus.WARNING, Activator.PLUGIN_ID,
                                "Non-valid interpolation request was made for "
                                        + getParmID().getShortParmId()));
            } else {
                statusHandler.handle(Priority.EVENTA,
                        "Non-valid interpolation request was made for "
                                + getParmID().getShortParmId());
            }

            if (newInterp) {
                this.interpolator = null;
            }
            return false;
        }

        // if we are in SYNC mode, then we interpolate() until there is no more
        // work to do.
        if (state == InterpState.SYNC) {
            doInterpolation(quietMode);
        }

        // We will do the real work in a job for ASYNC mode
        else {
            InterpolateJob.request(this);
        }
        return true;
    }

    /**
     * Actually performs the interpolation
     * 
     * @param quietMode
     * @throws GFEOperationFailedException
     */
    private void doInterpolation(boolean quietMode)
            throws GFEOperationFailedException {
        IGridSlice newSlice = null;
        while ((newSlice = this.interpolator.interpolate()) != null) {
            interpolatedDataArrived(newSlice);
        }
        finishInterpolation(quietMode);
    }

    /**
     * Handles cleanup and logging when interpolation is finished. The quiet
     * flag indicates whether USER_ALERT messages should be sent.
     * 
     * @param quietMode
     */
    private void finishInterpolation(boolean quietMode) {

        if (this.interpolator != null) {
            if (statusHandler.isPriorityEnabled(Priority.VERBOSE)) {
                String stats = this.interpolator.outputStatistics();
                statusHandler.handle(Priority.VERBOSE, stats);
            }

            this.interpolator.dispose();
            this.interpolator = null;

            if (!quietMode) {
                statusHandler.handle(Priority.EVENTA, "Interpolation for "
                        + getParmID().getShortParmId() + " finished.");
                perfLog.log("Interpolation for " + getParmID().getShortParmId()
                        + " finished.");
            }
        }
    }

    /**
     * Call this routine when interpolated data arrives from interpolation. The
     * data gridSlice that has been interpolated is specified in arg list.
     * 
     * Convert the data gridSlice into a GridData. Adjust the grid time to match
     * split boundaries if necessary. Use replaceGriddedData() to handle the
     * insertion and notifications.
     * 
     * @param newslice
     * @throws GFEOperationFailedException
     */
    private void interpolatedDataArrived(IGridSlice dataSlice)
            throws GFEOperationFailedException {
        IGridData grid = AbstractGridData.makeGridData(this, dataSlice);
        if (grid == null) {
            return; // received bad grid (or dataslice)
        }

        // validate the time
        TimeRange origGridTR = grid.getGridTime();
        TimeRange expandedTR = getGridInfo().getTimeConstraints()
                .expandTRToQuantum(origGridTR);
        if (!expandedTR.equals(origGridTR)) {
            Activator
                    .getDefault()
                    .getLog()
                    .log(new Status(IStatus.ERROR, Activator.PLUGIN_ID,
                            "interpolated data does not match quantum. gridTR="
                                    + origGridTR + " expandedTR=" + expandedTR));
            grid.changeValidTime(expandedTR, true);
        }

        replaceGriddedData(grid.getGridTime(), new IGridData[] { grid });
        this.dataManager.getParmOp().clearUndoParmList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(Parm o) {

        int cmp = 0;
        for (char sortChar : ParmSortPreference.getParmSortAlgorithm()) {
            switch (sortChar) {
            case 'm': // mutable/immutable
                if (this.mutable && !o.mutable) {
                    cmp = -1;
                } else if (!this.mutable && o.mutable) {
                    cmp = 1;
                }
                break;
            case 'M': // model
                String model1 = this.getParmID().getDbId().getModelName();
                String model2 = o.getParmID().getDbId().getModelName();
                cmp = model1.compareTo(model2);
                break;
            case 'N': // name
                cmp = compareName(o);
                break;
            case 't': // time
                cmp = getParmID()
                        .getDbId()
                        .getModelTimeAsDate()
                        .compareTo(o.getParmID().getDbId().getModelTimeAsDate());

                break;
            case 'o': // "other"- type
                String type1 = getParmID().getDbId().getDbType();
                String type2 = o.getParmID().getDbId().getDbType();
                cmp = type1.compareTo(type2);
                break;
            case 'l':
                String level1 = this.getParmID().getParmLevel();
                String level2 = o.getParmID().getParmLevel();
                cmp = level1.compareTo(level2);
                break;
            default:
                break;
            }

            if (cmp != 0) {
                return cmp;
            }
        }

        return 0;

    }

    /**
     * Compare two strings by name. This isn't just a simple comparison; there's
     * a config setting involved. Parm names that are listed in the setting are
     * sorted before other parms. When both are in the list, their position in
     * the list determines which comes first. If neither is in the list, then
     * the comparison reverts to a string comparison between the two names.
     * 
     * @param o
     *            The other parm to compare to by name
     * @return -1, 1, or 0
     */
    protected int compareName(Parm o) {
        int cmp;
        String parm1 = this.getParmID().getParmName();
        String parm2 = o.getParmID().getParmName();

        List<String> strings = Arrays.asList(ParmSortPreference
                .getParmSortPreference());
        int idx1 = strings.indexOf(parm1);
        int idx2 = strings.indexOf(parm2);

        // parms in strings come before those that aren't
        if ((idx1 != -1) && (idx2 == -1)) {
            cmp = -1;
        } else if ((idx1 == -1) && (idx2 != -1)) {
            cmp = 1;
        } else if ((idx1 != -1) && (idx2 != -1) && (idx1 != idx2)) {
            // both are in strings at different indices.
            // compare their position in strings.
            if (idx1 > idx2) {
                cmp = 1;
            } else {
                cmp = -1;
            }
        } else {
            // neither parm is in strings or they are identical
            cmp = parm1.compareTo(parm2);
        }
        return cmp;
    }

    /**
     * Displays a formatted string of parm: ex. ABR IFP ISC ----- MinT SFC
     * 
     * @return a formmated string
     */
    public String getFormattedString() {
        String singleTime = "-------";
        String formattedStr = "";
        String typeStr = "";
        String timeStr = "";
        String modelStr = "";
        String parmStr = "";

        if (this.getParmID().getDbId().getDbType() != null) {
            typeStr = this.getParmID().getDbId().getDbType();
        } else {
            typeStr = "IFP";
        }

        if (this.getParmID().getDbId().getModelTime() == null) {
            timeStr = singleTime;
        } else {
            if (this.getParmID().getDbId().getModelTime()
                    .equalsIgnoreCase(DatabaseID.NO_MODEL_TIME)) {
                timeStr = singleTime;
            } else {
                timeStr = this.getParmID().getDbId().getModelTime();
            }
        }

        modelStr = this.getParmID().getDbId().getModelName();
        parmStr = this.getParmID().getParmName();

        formattedStr = this.getParmID().getDbId().getSiteId() + " " + typeStr
                + "  " + modelStr + " " + timeStr + "  " + parmStr + "  "
                + this.getParmID().getParmLevel();

        return formattedStr;

    }

    /**
     * Method called when notification of new inventory arrives
     * 
     * @param affectedTimeRange
     * @param histories
     */
    public abstract void inventoryArrived(final TimeRange affectedTimeRange,
            final Map<TimeRange, List<GridDataHistory>> histories);

    /**
     * Method called when notification of updated history arrives
     * 
     * @param histories
     */
    public void historyUpdateArrived(
            Map<TimeRange, List<GridDataHistory>> histories) {
        // do nothing default implementation
    }

    /**
     * Updates the lock table with an updated version due to notifications
     * 
     * @param table
     *            The updated lock table
     */
    public void lockTableArrived(LockTable table) {
        this.lockTable = table;

        sendLockTableChangedNotification(table);
    }

    protected void sendLockTableChangedNotification(LockTable lockTable) {
        if (this.dataManager != null) {
            this.dataManager.getParmManager().lockTableChanged(this, lockTable);
        }

        getListeners().fireLockTableChanged(this, lockTable);
    }

    /**
     * Return the lock table
     * 
     * @return
     */
    public LockTable getLockTable() {
        return lockTable;
    }

    /**
     * Parm specific implementation of deallocation of grids
     * 
     * @param seconds
     */
    public abstract void deallocateUnusedGrids(int seconds);

    /**
     * Build the expression name
     * 
     * @return
     */
    public String expressionName() {
        // Build the expression name for the parm
        ParmID topoID = dataManager.getTopoManager().getCompositeParmID();

        return getParmID().expressionName(topoID,
                dataManager.getParmManager().getMutableDatabase(), true);
    }

    /**
     * Revert parameter replaces all currently modified gridded data with the
     * data contained in the database. Returns true if the parameter was
     * successfully reverted. Purge the undo grids since this operation cannot
     * be undone. For each lock owned by me, we get replacement grids from the
     * database, convert the data slices into grids, and then call
     * replaceGrids() to insert them into the inventory. Notification of changes
     * to the inventory are sent.
     * 
     * @return true if the operation succeeded
     */
    public abstract boolean revertParameter();

    public abstract void looseLocks();

    public String getOfficeType() {
        return officeType;
    }

    public void setOfficeType(String officeType) {
        this.officeType = officeType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return getParmID().toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return getParmID().hashCode();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Parm) {
            return getParmID().equals(((Parm) obj).getParmID());
        }
        return false;
    }
}
