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
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;

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
import com.raytheon.uf.common.util.RWLArrayList;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.AbstractGridData;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmListChangedListener;
import com.raytheon.viz.gfe.core.parm.vcparm.VCModule;
import com.raytheon.viz.gfe.core.parm.vcparm.VCModule.DepParmInv;
import com.raytheon.viz.gfe.core.parm.vcparm.VCModule.VCInventory;

/**
 * A VC (virtual calculated) parm is virtual (non-database ifpServer) and is
 * automatically defined by a calculation written in Python.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 17, 2011           dgilling  Initial creation
 * Feb 22, 2012  346      dgilling  Convert registeredParms to RWLArrayList to
 *                                  improve thread safety and fix disappearing
 *                                  ISC data. Also, remove overridden finalize
 *                                  function.
 * Feb 23, 2012  346      dgilling  Implement a dispose method.
 * Mar 02, 2012  346      dgilling  Use Parm's new disposed flag to prevent
 *                                  leaks through ListenerLists.
 * Jun 25, 2012  766      dgilling  Cleanup error logging so we don't spam
 *                                  alertViz in practice mode.
 * Jan 22, 2013  1515     dgilling  Handle Parm notifications on separate thread
 *                                  to prevent backup in ParmListener's
 *                                  notification job pool.
 * Feb 13, 2013  1515     dgilling  Handle RejectedExecutionExceptions if
 *                                  notification is received after Parm
 *                                  disposal.
 * Jan 04, 2018  7178     randerso  Changes to support IDataObject. Code cleanup
 * Dec 11, 2018  #7692    dgilling  Add additional debug logging.
 *
 * </pre>
 *
 * @author dgilling
 */

public class VCParm extends VParm implements IParmListChangedListener,
        IParmInventoryChangedListener, IGridDataChangedListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VCParm.class);

    private VCModule mod;

    private RWLArrayList<Parm> registeredParms = new RWLArrayList<>();

    private List<VCInventory> vcInventory;

    private final ExecutorService notificationWorkers;

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
            statusHandler.handle(Priority.EVENTB, "Can't get GPI: ",
                    this.mod.getErrorString());
        }

        notificationWorkers = Executors.newSingleThreadExecutor();

        // Determine dependent parms, and register for their ParmClient
        // notifications, determine initial inventory
        registerParmClients(dataMgr.getParmManager().getAllParms(), false);

        // set up messages to be received, Parm class already set up some
        dataMgr.getParmManager().addParmListChangedListener(this);
    }

    @Override
    public void dispose() {
        super.dispose();

        this.dataManager.getParmManager().removeParmListChangedListener(this);

        // Unregister for the parm client notifications
        for (Parm reg : registeredParms()) {
            unregisterPC(reg);
        }

        notificationWorkers.shutdownNow();
    }

    @Override
    public void gridDataChanged(final ParmID parmId,
            final TimeRange validTime) {
        synchronized (this) {
            if (disposed) {
                return;
            }
        }

        Runnable onNotificationTask = new Runnable() {

            @Override
            public void run() {
                synchronized (VCParm.this) {
                    if (disposed) {
                        return;
                    }
                }

                // statusHandler.handle(Priority.DEBUG, "gridDataChanged for: "
                // + parmId
                // + " " + validTime);

                for (VCInventory inv : vcInventory) {
                    for (DepParmInv dpi : inv.getDepParmInv()) {
                        if (dpi.getParmID().equals(parmId)) {
                            for (TimeRange tr : dpi.getTimes()) {
                                if (tr.getStart()
                                        .equals(validTime.getStart())) {
                                    grids.acquireReadLock();
                                    try {
                                        for (IGridData grid : grids) {
                                            if (inv.getGridTimeRange().equals(
                                                    grid.getGridTime())) {
                                                gridDataHasChanged(grid,
                                                        getDisplayAttributes()
                                                                .getDisplayMask(),
                                                        false);
                                                return;
                                            }
                                        }
                                    } finally {
                                        grids.releaseReadLock();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        };

        try {
            notificationWorkers.submit(onNotificationTask);
        } catch (RejectedExecutionException e) {
            // received a notification after thread pool shutdown
            // do nothing, but log
            String message = "Parm " + toString()
                    + " received GridDataChange notification after VCParm.dispose().";
            statusHandler.handle(Priority.EVENTB, message, e);
        }
    }

    @Override
    public void parmListChanged(final Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        synchronized (this) {
            if (disposed) {
                return;
            }
        }

        Runnable onNotificationTask = new Runnable() {

            @Override
            public void run() {
                /*
                 * forcing access to the disposed variable and subsequent
                 * registration/unregistation of listeners through this
                 * synchronized block seems to prevent VCParm objects being
                 * leaked through outdated listener list copies
                 */
                synchronized (VCParm.this) {
                    if (disposed) {
                        return;
                    }

                    // statusHandler.handle(Priority.DEBUG,
                    // "ParmListChangedMsg received: " +
                    // getParmID().toString());
                    registerParmClients(parms, true);
                }
            }
        };

        try {
            notificationWorkers.submit(onNotificationTask);
        } catch (RejectedExecutionException e) {
            // received a notification after thread pool shutdown
            // do nothing, but log
            String message = "Parm " + toString()
                    + " received ParmListChange notification after VCParm.dispose().";
            statusHandler.handle(Priority.EVENTB, message, e);
        }
    }

    @Override
    public void parmInventoryChanged(Parm parm, TimeRange timeRange) {
        synchronized (this) {
            if (disposed) {
                return;
            }
        }

        /*
         * It would be better to only update those griddata objects that needed
         * it. But... that is much more difficult than this simple brute force
         * method. This seems to be fast enough for now. So, unless there is
         * some other reason, we will go with the simplest thing that works
         * approach.
         */

        // statusHandler.debug("ParmInventoryChanged notification for: "
        // + getParmID().toString());
        Runnable onNotificationTask = new Runnable() {

            @Override
            public void run() {
                synchronized (VCParm.this) {
                    if (disposed) {
                        return;
                    }
                }

                recalcInventory(true);
            }
        };

        try {
            notificationWorkers.submit(onNotificationTask);
        } catch (RejectedExecutionException e) {
            // received a notification after thread pool shutdown
            // do nothing, but log
            String message = "Parm " + toString()
                    + " received ParmInventoryChange notification after VCParm.dispose().";
            statusHandler.handle(Priority.EVENTB, message, e);
        }
    }

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
                IGridData gd = AbstractGridData.makeGridData(this, gs, true);
                grid.replace(gd);
            }
        }
    }

    @Override
    public void populateGrids(List<IGridData> grids) {
        for (IGridData grid : grids) {
            populateGrid(grid);
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
        List<IGridData> grids = new ArrayList<>();
        for (VCInventory inv : this.vcInventory) {
            IGridData gd = AbstractGridData.makeGridData(this,
                    getEmptyGridSlice(inv), false);
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
            statusHandler
                    .handle(Priority.EVENTB,
                            "Error in history calculation for "
                                    + getParmID().toString(),
                            mod.getErrorString());
        }

        IGridSlice gs = null;
        switch (getGridInfo().getGridType()) {
        case SCALAR:
            gs = new ScalarGridSlice(timeRange, getGridInfo(), hist, null);
            break;
        case VECTOR:
            gs = new VectorGridSlice(timeRange, getGridInfo(), hist, null,
                    null);
            break;
        case WEATHER:
            gs = new WeatherGridSlice(timeRange, getGridInfo(), hist, null,
                    null);
            break;
        case DISCRETE:
            gs = new DiscreteGridSlice(timeRange, getGridInfo(), hist, null,
                    null);
            break;
        default:
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
        statusHandler.debug(
                "VCParm.recalcInventory: Entering for [" + getParmID() + "].");

        // ensure we have parms* for all of the dependent parms
        List<ParmID> args = new ArrayList<>(mod.dependentParms());
        if (!mod.isValid()) {
            statusHandler.handle(Priority.EVENTB,
                    "Error getting dependent WeatherElements: ",
                    mod.getErrorString());
        }

        statusHandler.debug("VCParm.recalcInventory: Dependent parms for ["
                + getParmID() + "]: " + args.toString());

        Parm[] parms = this.dataManager.getParmManager()
                .getParms(args.toArray(new ParmID[args.size()]));
        boolean okay = Arrays.stream(parms).allMatch(p -> p != null);

        // Determine the appropriate inventory, create empty grids
        if (okay) {
            // all required parms exist
            this.vcInventory = mod.getInventory();
            if (!mod.isValid()) {
                statusHandler.error(
                        "Error getting inventory for [" + getParmID() + "]: ",
                        mod.getErrorString());
            }
        } else {
            this.vcInventory = new ArrayList<>();
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
        List<ParmID> args = new ArrayList<>(mod.dependentParms());
        if (!mod.isValid()) {
            statusHandler.handle(Priority.EVENTB,
                    "Error getting dependent WeatherElements: ",
                    mod.getErrorString());
        }

        // get list of currently registered parms
        List<Parm> currRegistered = registeredParms();

        // get list of parms to unregister
        boolean changed = false;
        List<Parm> unreg = new ArrayList<>(currRegistered);
        unreg.removeAll(Arrays.asList(parms));
        for (Parm parm : unreg) {
            unregisterPC(parm);
            changed = true;
        }

        // get list of parms to register
        List<Parm> maybeReg = new ArrayList<>(Arrays.asList(parms));
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

        registeredParms.acquireWriteLock();
        try {
            this.registeredParms.add(parm);
        } finally {
            registeredParms.releaseWriteLock();
        }
    }

    private void unregisterPC(Parm parm) {
        parm.parmListeners.removeGridChangedListener(this);
        parm.parmListeners.removeParmInventoryChangedListener(this);

        registeredParms.acquireWriteLock();
        try {
            this.registeredParms.remove(parm);
        } finally {
            registeredParms.releaseWriteLock();
        }
    }

    private List<Parm> registeredParms() {
        List<Parm> retVal = new ArrayList<>();

        registeredParms.acquireReadLock();
        try {
            retVal.addAll(registeredParms);
        } finally {
            registeredParms.releaseReadLock();
        }

        return retVal;
    }
}
