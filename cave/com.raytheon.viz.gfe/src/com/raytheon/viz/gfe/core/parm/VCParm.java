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
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.slice.DiscreteGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.ScalarGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.VectorGridSlice;
import com.raytheon.uf.common.dataplugin.gfe.slice.WeatherGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmListChangedListener;
import com.raytheon.viz.gfe.core.parm.VCModule.DepParmInv;
import com.raytheon.viz.gfe.core.parm.VCModule.VCInventory;

/**
 * A VC (virtual calculated) parm is virtual (non-database ifpServer) and is
 * automatically defined by a calculation written in Python.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 17, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class VCParm extends VParm implements IParmListChangedListener,
        IParmInventoryChangedListener, IGridDataChangedListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VCParm.class);

    private VCModule mod;

    private Set<Parm> registeredParms = new HashSet<Parm>();

    private List<VCInventory> vcInventory;

    /**
     * Constructor for Virtual Calculated Parm.
     * 
     * @param dataMgr
     *            <code>DataManager</code> reference
     * @param displayable
     *            Whether or not this <code>VCParm</code> should be displayable.
     * @param mod
     *            The <code>VCModule</code> that defines this
     *            <code>VCParm</code>.
     */
    public VCParm(DataManager dataMgr, boolean displayable, VCModule mod) {
        super(mod.getGpi().getParmID(), mod.getGpi(), false, displayable,
                dataMgr, new IGridSlice[0]);
        this.mod = mod;

        // Need to check that the above call to mod.getGpi() did not fail
        if (!mod.isValid()) {
            statusHandler.handle(Priority.PROBLEM, "Can't get GPI: ",
                    this.mod.getErrorString());
        }

        // set the parm type
        // setParmType(Parm::VCPARM);

        // Determine dependent parms, and register for their ParmClient
        // notifications, determine initial inventory
        registerParmClients(dataMgr.getParmManager().getAllParms(), false);

        // set up messages to be received, Parm class already set up some
        dataMgr.getParmManager().addParmListChangedListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#finalize()
     */
    @Override
    protected void finalize() throws Throwable {
        // TODO: find a better way to get this called
        this.dispose();
    }

    public void dispose() {

        this.dataManager.getParmManager().removeParmListChangedListener(this);

        // Unregister for the parm client notifications
        for (Parm reg : registeredParms()) {
            unregisterPC(reg);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener#gridDataChanged
     * (com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID,
     * com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    public void gridDataChanged(final ParmID parmId, final TimeRange validTime) {
        // statusHandler.handle(Priority.DEBUG, "gridDataChanged for: " + parmId
        // + " " + validTime);

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                for (VCInventory inv : VCParm.this.vcInventory) {
                    for (DepParmInv dpi : inv.getDepParmInv()) {
                        if (dpi.getParmID().equals(parmId)) {
                            for (TimeRange tr : dpi.getTimes()) {
                                if (tr.getStart().equals(validTime.getStart())) {
                                    VCParm.this.grids.acquireReadLock();
                                    try {
                                        for (IGridData grid : VCParm.this.grids) {
                                            if (inv.getGridTimeRange().equals(
                                                    grid.getGridTime())) {
                                                grid.depopulate();
                                                gridDataHasChanged(
                                                        grid,
                                                        getDisplayAttributes()
                                                                .getDisplayMask(),
                                                        false);
                                                return;
                                            }
                                        }
                                    } finally {
                                        VCParm.this.grids.releaseReadLock();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        });

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.IParmListChangedListener#parmListChanged
     * (com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[])
     */
    @Override
    public void parmListChanged(final Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        // statusHandler.handle(Priority.DEBUG,
        // "ParmListChangedMsg received: ");

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                registerParmClients(parms, true);
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener#
     * parmInventoryChanged(com.raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    public void parmInventoryChanged(Parm parm, TimeRange timeRange) {
        // It would be better to only update those griddata objects
        // that needed it. But... that is much more difficult
        // than this simple brute force method. This seems to be
        // fast enough for now. So, unless there is some other
        // reason, we will to with the simplest thing that works
        // approach.
        // statusHandler.debug("ParmInventoryChanged notification for: "
        // + getParmID().toString());
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                recalcInventory(true);
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.VParm#populateGrid(com.raytheon.viz.gfe
     * .core.griddata.IGridData)
     */
    @Override
    public void populateGrid(IGridData grid) {
        for (VCInventory vcInvEntry : vcInventory) {
            if (vcInvEntry.getGridTimeRange().equals(grid.getGridTime())) {
                // statusHandler.debug("Populating Grid: "
                // + vcInvEntry.getGridTimeRange().toString());
                IGridSlice gs = mod.calcGrid(vcInvEntry);
                if (!mod.isValid()) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error calculating grid: ", mod.getErrorString());
                }
                IGridData gd = AbstractGridData.makeGridData(this, gs);
                grid.replace(gd);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.VParm#populateGrids(java.util.List)
     */
    @Override
    public void populateGrids(List<IGridData> grids) {
        for (IGridData grid : grids) {
            populateGrid(grid);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.VParm#deallocateUnusedGrids(int)
     */
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
        long now = System.currentTimeMillis();
        this.grids.acquireReadLock();
        try {
            for (IGridData grid : this.grids) {
                if (!grid.isPopulated()) {
                    continue;
                }

                if (grid.equals(se)) {
                    continue;
                } // grid overlaps spatial editor time -- skip it

                long lastAccess = 0;
                if (grid.getLastAccessTime() != null) {
                    lastAccess = grid.getLastAccessTime().getTime();
                }

                long delta = now - lastAccess;
                if (delta < milliseconds) {
                    continue; // recently accessed
                }

                // deallocate
                // statusHandler.debug("Deallocating " + getParmID() + " tr="
                // + grid.getGridTime());
                grid.depopulate();
            }
        } finally {
            this.grids.releaseReadLock();
        }
    }

    /**
     * Routine to create a set of "empty" (unpopulated) grids for the given
     * <code>VCInventory</code> and replacement time range.
     * 
     * @param replacementTimeRange
     *            the <code>TimeRange</code> for which to create unpopulated
     *            grids.
     * @param vcInventory
     *            The inventory of grids for this <code>VCParm</code>.
     * @param sendNotification
     *            Whether or not to notify listeners for this change.
     */
    private void createEmptyGrids(TimeRange replacementTimeRange,
            Collection<VCInventory> vcInventory, boolean sendNotification) {
        // create the grids
        List<IGridData> grids = new ArrayList<IGridData>();
        for (VCInventory inv : this.vcInventory) {
            IGridData gd = AbstractGridData.makeGridData(this,
                    getEmptyGridSlice(inv));
            if (gd != null) {
                grids.add(gd);
            }
        }

        // now perform the replacement into the parm's inventory
        replaceGrids(replacementTimeRange,
                grids.toArray(new IGridData[grids.size()]));

        // let the world know about grid inventory changes
        if (sendNotification) {
            sendInvChangedNotification(replacementTimeRange);
        }
    }

    /**
     * Creates and returns an <code>IGridSlice</code> without data
     * (unpopulated), given the VCInventory entry.
     * 
     * @param vcInv
     *            The <code>VCInvenoty</code> entry
     * @return An appropriate <code>IGridSlice</code> that corresponds to the
     *         specified inventory entry.
     */
    private IGridSlice getEmptyGridSlice(VCInventory vcInv) {
        TimeRange timeRange = vcInv.getGridTimeRange();
        List<GridDataHistory> hist = mod.calcHistory(vcInv);

        // If there is an error in calcHistory() it
        // will still give us one to use. So, we can just logProblem
        if (!mod.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error in history calculation for "
                            + getParmID().toString(), mod.getErrorString());
        }

        IGridSlice gs = null;
        switch (getGridInfo().getGridType()) {
        case SCALAR:
            gs = new ScalarGridSlice(timeRange, getGridInfo(), hist, null);
            break;
        case VECTOR:
            gs = new VectorGridSlice(timeRange, getGridInfo(), hist, null, null);
            break;
        case WEATHER:
            gs = new WeatherGridSlice(timeRange, getGridInfo(), hist, null,
                    null);
            break;
        case DISCRETE:
            gs = new DiscreteGridSlice(timeRange, getGridInfo(), hist, null,
                    null);
            break;
        }
        return gs;
    }

    /**
     * Recalculates the inventory, creates empty grids appropriately,
     * depopulates. Optionally sends out notification of inventory changes.
     * 
     * @param sendNotification
     *            Whether or not to notify listeners of the inventory change.
     */
    private void recalcInventory(boolean sendNotification) {
        // statusHandler.debug("recalcInventory.   sendNot=" +
        // sendNotification);

        // ensure we have parms* for all of the dependent parms
        List<ParmID> args = new ArrayList<ParmID>(mod.dependentParms());
        if (!mod.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting dependent WeatherElements: ",
                    mod.getErrorString());
        }
        Parm[] parms = this.dataManager.getParmManager().getParms(
                args.toArray(new ParmID[args.size()]));
        boolean okay = true;
        for (Parm parm : parms) {
            if (parm == null) {
                okay = false;
                break;
            }
        }

        // Determine the appropriate inventory, create empty grids
        if (okay) {
            // all required parms exist
            this.vcInventory = mod.getInventory();
            if (!mod.isValid()) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting inventory: ", mod.getErrorString());
            }
        } else {
            this.vcInventory = new ArrayList<VCInventory>();
        }

        createEmptyGrids(TimeRange.allTimes(), this.vcInventory,
                sendNotification);
    }

    /**
     * Sets up the parm client register/unregister based on the new list of
     * parms. Optionally will send out a notification of inventory changes (if
     * they occur).
     * 
     * @param parms
     *            A list of <code>Parms</code> that can be potentially
     *            registered to.
     * @param sendNotification
     *            Whether or not to notify listeners of any inventory changes.
     */
    private void registerParmClients(Parm[] parms, boolean sendNotification) {
        // get list of dependent parms
        List<ParmID> args = new ArrayList<ParmID>(mod.dependentParms());
        if (!mod.isValid()) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting dependent WeatherElements: ",
                    mod.getErrorString());
        }

        // get list of currently registered parms
        List<Parm> currRegistered = new ArrayList<Parm>(registeredParms());

        // get list of parms to unregister
        boolean changed = false;
        List<Parm> unreg = new ArrayList<Parm>(currRegistered);
        unreg.removeAll(Arrays.asList(parms));
        for (Parm parm : unreg) {
            unregisterPC(parm);
            changed = true;
        }

        // get list of parms to register
        List<Parm> maybeReg = new ArrayList<Parm>(Arrays.asList(parms));
        maybeReg.removeAll(currRegistered);
        for (Parm parm : maybeReg) {
            if (args.contains(parm.getParmID())) {
                registerPC(parm);
                changed = true;
            }
        }

        // if anything changed, then invalidate inventory and reset
        if (changed) {
            recalcInventory(sendNotification);
        }
    }

    private void registerPC(Parm parm) {
        parm.parmListeners.addGridChangedListener(this);
        parm.parmListeners.addParmInventoryChangedListener(this);
        this.registeredParms.add(parm);
    }

    private void unregisterPC(Parm parm) {
        parm.parmListeners.removeGridChangedListener(this);
        parm.parmListeners.removeParmInventoryChangedListener(this);
        this.registeredParms.remove(parm);
    }

    private Set<Parm> registeredParms() {
        return this.registeredParms;
    }

}
