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

package com.raytheon.viz.gfe.core.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.DbParm;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.core.parm.VCParm;
import com.raytheon.viz.gfe.core.parm.VParm;
import com.raytheon.viz.gfe.core.parm.vcparm.VCModule;
import com.raytheon.viz.gfe.types.MutableInteger;

/**
 * Class used to manage grid parms
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/10/08     #875       bphillip    Initial Creation
 * 04/18/08     #875       bphillip    More implementation
 * 05/07/08     #875       bphillip    Modified save forecast behavior
 * 05/19/08     #875       bphillip    Implemented save forecast for vectors
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 08/19/09     2547       rjpeter     Implement Test/Prac database display.
 * 02/23/12     #346       dgilling    Call Parm's dispose method when removing
 *                                     a Parm.
 * 06/25/12     #766       dgilling    Fix NullPointerException from VCModules
 *                                     when running in practice mode.
 * 05/02/13     #1969      randerso    Added code to explicitly create the mutable database
 *                                     if it doesn't exist. Used to just happen by accident
 *                                     when getParmList was called.
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class ParmManager extends AbstractParmManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParmManager.class);

    @SuppressWarnings("unused")
    private final GridLocation gloc;

    private List<DatabaseID> availableServerDatabases;

    private List<DatabaseID> availableVParmDatabases;

    private List<DatabaseID> availableVCParmDatabases;

    private List<DatabaseID> iscDbs;

    private final Map<DatabaseID, List<ParmID>> parmIDCacheServer;

    private final Map<DatabaseID, List<ParmID>> parmIDCacheVParm;

    private final Map<DatabaseID, List<ParmID>> parmIDCacheVCParm;

    public ParmManager(DataManager dmgr) throws GFEServerException {
        super(dmgr);

        // Get the composite grid location
        this.gloc = dataManager.getClient().getDBGridLocation();

        this.systemTimeRange = recalcSystemTimeRange();
        this.parmIDCacheServer = new HashMap<DatabaseID, List<ParmID>>();
        this.parmIDCacheVParm = new HashMap<DatabaseID, List<ParmID>>();
        this.parmIDCacheVCParm = new HashMap<DatabaseID, List<ParmID>>();

        updateDatabaseLists();

        this.productDB = determineProductDatabase();
    }

    /**
     * Determines the name of the database that is used by products. This is the
     * official database identifier or if none, the mutable database id.
     * 
     * @return
     * @throws GFEServerException
     */
    private DatabaseID determineProductDatabase() {
        DatabaseID mutableDb = this.getMutableDatabase();

        CAVEMode opMode = CAVEMode.getMode();
        if (opMode.equals(CAVEMode.PRACTICE) || opMode.equals(CAVEMode.TEST)) {
            return mutableDb;
        }

        ServerResponse<List<DatabaseID>> sr;
        try {
            sr = getDataManager().getClient().getOfficialDBName();
            List<DatabaseID> databases = sr.getPayload();

            if (!sr.isOkay()) {
                statusHandler.handle(Priority.PROBLEM,
                        "Network problem: Unable to get official DB names.\n"
                                + sr);
            }

            // if no official databases, then use the mutable database
            if (databases.isEmpty()) {
                return mutableDb;
            }

            // the official database names from the server don't include the
            // model time, so see if we can add a model time in if appropriate.
            for (int i = 0; i < databases.size(); i++) {
                // find a match for siteid, type and format with the mutable
                if (mutableDb.getSiteId().equals(databases.get(i).getSiteId())
                        && mutableDb.getDbType().equals(
                                databases.get(i).getDbType())
                        && mutableDb.getFormat().equals(
                                databases.get(i).getFormat())) {
                    // found a match -- now put the model time from the mutable
                    // database onto this database
                    DatabaseID off = databases.get(i);
                    return new DatabaseID(off.getSiteId(), off.getFormat(),
                            off.getDbType(), off.getModelName(),
                            mutableDb.getModelTime());

                }

            }
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Network problem: Unable to get official DB names.", e);
        }

        // can't find an official database that matches the mutable database
        return new DatabaseID();
    }

    @Override
    protected DataManager getDataManager() {
        return this.dataManager;
    }

    @Override
    public Parm addParm(ParmID pid, boolean mutableParm, boolean displayable) {
        if (!isParmInDatabase(pid)) {
            statusHandler.handle(Priority.EVENTA,
                    "Attempt to load a nonexistent parm: " + pid);
            return null;
        }

        List<ParmIDVis> addParms = new ArrayList<ParmIDVis>(1);
        addParms.add(new ParmIDVis(pid, displayable));
        setParms(addParms, new ArrayList<ParmID>(0));
        return getParm(pid);
    }

    @Override
    public Parm[] getAllParms() {
        parms.acquireReadLock();
        try {
            return parms.toArray(new Parm[parms.size()]);
        } finally {
            parms.releaseReadLock();
        }
    }

    @Override
    public Parm createVirtualParm(ParmID pid, GridParmInfo gpi,
            IGridSlice[] data, boolean mutableParm, boolean displayable) {
        // already exists?
        if (getParm(pid) != null) {
            return null; // already exists
        }

        // validate the parm, ensure it isn't already listed
        if (isParmInDatabase(pid)) {
            return null; // invalid parm - is already known
        }

        // Make the parm
        Parm parm = new VParm(pid, gpi, mutableParm, displayable,
                this.dataManager, data);
        statusHandler.handle(Priority.VERBOSE, "Created Parm: "
                + parm.getParmID().toString());

        List<Parm> additions = new ArrayList<Parm>(1);
        additions.add(parm);
        setParms(additions, new ArrayList<Parm>(0), new ArrayList<Parm>(0));

        return parm;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#deleteParm(com.raytheon.viz.gfe
     * .core.parm.Parm[])
     */
    @Override
    public void deleteParm(Parm... parms) {
        if (parms.length == 0) {
            return; // Nothing to do
        }

        this.parms.acquireReadLock();
        List<Parm> toBeDeleted = new ArrayList<Parm>();
        try {
            for (int i = 0; i < parms.length; i++) {
                if (!this.parms.contains(parms[i])) {
                    statusHandler.handle(
                            Priority.EVENTA,
                            "Attempt to delete unknown parm:"
                                    + parms[i].getParmID());
                    continue;
                }

                // skip modified parms
                if (!parms[i].isModified()) {
                    toBeDeleted.add(parms[i]);
                } else {
                    statusHandler.debug("Skipping parm: "
                            + parms[i].getParmID() + " due to modified state.");
                }
            }
        } finally {
            this.parms.releaseReadLock();
        }

        List<ParmID> ids = new ArrayList<ParmID>();
        for (int i = 0; i < toBeDeleted.size(); i++) {
            ids.add(toBeDeleted.get(i).getParmID());
        }
        // logDebug << "DeleteParm start: " << ids << std::endl;

        // continue the command to remove the specified ids
        setParms(new ArrayList<ParmIDVis>(0), ids);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getAvailableDbs()
     */
    @Override
    public List<DatabaseID> getAvailableDbs() {
        List<DatabaseID> dbs = new ArrayList<DatabaseID>(
                availableServerDatabases);
        for (DatabaseID dbId : availableVCParmDatabases) {
            if (!dbs.contains(dbId)) {
                dbs.add(dbId);
            }
        }
        for (DatabaseID dbId : availableVParmDatabases) {
            if (!dbs.contains(dbId)) {
                dbs.add(dbId);
            }
        }

        return Collections.unmodifiableList(dbs);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getDisplayedDbs()
     */
    @Override
    public List<DatabaseID> getDisplayedDbs() {
        Set<DatabaseID> dbs = new HashSet<DatabaseID>();
        parms.acquireReadLock();
        try {
            for (Parm parm : parms) {
                if (parm.getDisplayAttributes().isDisplayable()) {
                    DatabaseID dbid = parm.getParmID().getDbId();
                    dbs.add(dbid);
                }
            }
        } finally {
            parms.releaseReadLock();
        }
        return Collections.unmodifiableList(new ArrayList<DatabaseID>(dbs));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getUndisplayedDbs()
     */
    @Override
    public List<DatabaseID> getUndisplayedDbs() {
        Set<DatabaseID> dbs = new HashSet<DatabaseID>();
        parms.acquireReadLock();
        try {
            for (Parm parm : parms) {
                if (!parm.getDisplayAttributes().isDisplayable()) {
                    DatabaseID dbid = parm.getParmID().getDbId();
                    dbs.add(dbid);
                }
            }
        } finally {
            parms.releaseReadLock();
        }
        return Collections.unmodifiableList(new ArrayList<DatabaseID>(dbs));
    }

    @Override
    public ParmID[] getAllAvailableParms() {
        List<DatabaseID> uncachedDbs = new ArrayList<DatabaseID>();
        List<ParmID> parmIDs = new ArrayList<ParmID>(7500);

        // check the cache
        for (DatabaseID dbID : getAvailableDbs()) {
            List<ParmID> cacheParmIDs = null;
            synchronized (this.parmIDCacheServer) {
                cacheParmIDs = this.parmIDCacheServer.get(dbID);
            }
            if ((cacheParmIDs == null) && (dbID.getDbType().equals("V"))) {
                ParmID[] vcParms = getAvailableParms(dbID);
                parmIDs.addAll(Arrays.asList(vcParms));
            } else if ((cacheParmIDs == null)
                    && (!dbID.getDbType().equals("V"))) {
                uncachedDbs.add(dbID);
            } else {
                parmIDs.addAll(cacheParmIDs);

                // Add the vparms to the list, if they are in the right database
                List<ParmID> vParms = null;
                synchronized (this.parmIDCacheVParm) {
                    vParms = this.parmIDCacheVParm.get(dbID);
                }
                if (vParms != null) {
                    parmIDs.addAll(vParms);
                }
            }
        }

        if (!uncachedDbs.isEmpty()) {
            // retrieve cache
            try {
                List<ParmID> results = dataManager.getClient().getParmList(
                        uncachedDbs);
                parmIDs.addAll(results);

                // add results to parmIDCache
                // use two scans to initialize lists to correct size
                Map<DatabaseID, MutableInteger> listSizes = new HashMap<DatabaseID, MutableInteger>(
                        uncachedDbs.size());
                for (DatabaseID dbID : uncachedDbs) {
                    listSizes.put(dbID, new MutableInteger(0));
                }
                for (ParmID parm : results) {
                    listSizes.get(parm.getDbId()).add(1);
                }
                Map<DatabaseID, Set<ParmID>> dupRemovalMap = new HashMap<DatabaseID, Set<ParmID>>(
                        (int) (uncachedDbs.size() * 1.3) + 1);
                for (ParmID parm : results) {
                    DatabaseID dbID = parm.getDbId();
                    Set<ParmID> parmSet = dupRemovalMap.get(dbID);
                    if (!dbID.getDbType().endsWith("TMP")) {
                        if (parmSet == null) {
                            parmSet = new HashSet<ParmID>((int) (listSizes.get(
                                    dbID).getValue() * 1.3) + 1);
                            dupRemovalMap.put(dbID, parmSet);
                        }
                        parmSet.add(parm);
                    }
                }
                synchronized (this.parmIDCacheServer) {
                    for (Entry<DatabaseID, Set<ParmID>> entry : dupRemovalMap
                            .entrySet()) {
                        DatabaseID dbId = entry.getKey();
                        List<ParmID> parms = this.parmIDCacheServer.get(dbId);
                        if (parms == null) {
                            this.parmIDCacheServer.put(dbId,
                                    new ArrayList<ParmID>(entry.getValue()));
                        } else {
                            Set<ParmID> parmSet = entry.getValue();
                            parmSet.addAll(parms);
                            this.parmIDCacheServer.put(dbId,
                                    new ArrayList<ParmID>(parmSet));
                        }
                    }
                }
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error retrieving all parm IDs", e);
            }
        }

        return parmIDs.toArray(new ParmID[parmIDs.size()]);

    }

    @Override
    public ParmID[] getAvailableParms(DatabaseID dbID) {
        // a derivation from AWIPS1:
        // short-circuit the checks and just return an empty array back
        // if we have an invalid DatabaseID
        if ((dbID == null) || (!dbID.isValid())) {
            return new ParmID[0];
        }

        // Check the cache
        List<ParmID> cacheParmIDs = null;
        synchronized (this.parmIDCacheServer) {
            cacheParmIDs = this.parmIDCacheServer.get(dbID);
        }
        List<ParmID> parmIds = null;

        if (cacheParmIDs != null) {
            parmIds = new ArrayList<ParmID>(cacheParmIDs);
        } else {
            if ((!dbID.getDbType().endsWith("TMP"))
                    && (!dbID.getDbType().equals("V"))) {
                try {
                    parmIds = dataManager.getClient().getParmList(dbID);
                } catch (GFEServerException e) {
                    statusHandler
                            .handle(Priority.PROBLEM,
                                    "Unable to get parm list of db: "
                                            + dbID.toString(), e);
                }

                // cache the result for next time
                synchronized (this.parmIDCacheServer) {
                    this.parmIDCacheServer.put(dbID, parmIds);
                }
            }
        }

        if (parmIds == null) {
            parmIds = new ArrayList<ParmID>();
        }

        // look up the information in the vcparm database cache
        cacheParmIDs = null;
        synchronized (this.parmIDCacheVCParm) {
            cacheParmIDs = this.parmIDCacheVCParm.get(dbID);
        }
        if (cacheParmIDs != null) {
            parmIds.addAll(cacheParmIDs);
        } else if (availableVCParmDatabases.contains(dbID)) { // make cache
                                                              // entry
            List<ParmID> pids = new ArrayList<ParmID>();
            parms.acquireReadLock();
            try {
                for (VCModule mod : vcModules) {
                    ParmID pid = mod.getGpi().getParmID();
                    if (pid.getDbId().equals(dbID)) {
                        pids.add(pid);
                    }
                }
            } finally {
                parms.releaseReadLock();
            }
            parmIds.addAll(pids);
            synchronized (this.parmIDCacheVCParm) {
                this.parmIDCacheVCParm.put(dbID, pids);
            }
        }

        // look up the information in the vparm database cache
        cacheParmIDs = null;
        synchronized (this.parmIDCacheVParm) {
            cacheParmIDs = this.parmIDCacheVParm.get(dbID);
        }
        if (cacheParmIDs != null) {
            parmIds.addAll(cacheParmIDs);
        } else if (availableVParmDatabases.contains(dbID)) { // make cache entry
            List<ParmID> pids = new ArrayList<ParmID>();
            parms.acquireReadLock();
            try {
                for (Parm p : parms) {
                    if ((p instanceof VParm)
                            && (p.getParmID().getDbId().equals(dbID))) {
                        pids.add(p.getParmID());
                    }
                }
            } finally {
                parms.releaseReadLock();
            }
            parmIds.addAll(pids);
            synchronized (this.parmIDCacheVParm) {
                this.parmIDCacheVParm.put(dbID, pids);
            }
        }

        return parmIds.toArray(new ParmID[parmIds.size()]);
    }

    @Override
    public DatabaseID getMutableDatabase() {
        return mutableDb;
    }

    @Override
    public DatabaseID getOrigMutableDatabase() {
        return origMutableDb;
    }

    @Override
    public TimeRange getSystemTimeRange() {
        return this.systemTimeRange;
    }

    @Override
    public ParmID getUniqueParmID(ParmID pid, String nameHint,
            String categoryHint) {
        // modify the database id to include the type "TMP"
        final DatabaseID inDB = pid.getDbId();
        DatabaseID dbid = new DatabaseID(inDB.getSiteId(), inDB.getFormat(),
                inDB.getDbType() + categoryHint, inDB.getModelName(),
                inDB.getModelTime());

        // modify the parmname
        String pn = pid.getParmName() + nameHint;

        // attempt a lookup on this parmID
        int cycle = 0;
        ParmID parmID;
        while (true) {
            // make up the unique name
            cycle++;
            String pnCycle = new String(pn + cycle);

            parmID = new ParmID(pnCycle, dbid);
            // logDebug << "Unique parm id attempt for [" << parmID << "]"
            // << std::endl;

            // does this parm id exist?
            if (getParm(parmID) != null) {
                // logDebug << "Parm cycle " << cycle << " exists" << std::endl;
                continue; // yes, try the next sequence number
            }

            // no -- try a lookup through available databases and parms
            if (!isParmInDatabase(parmID)) {
                break;
            }

            // logDebug << "Parm exists in database" << std::endl;
        }

        return parmID;

    }

    @Override
    public boolean isParmInDatabase(ParmID parmId) {
        DatabaseID requestID = parmId.getDbId();
        List<ParmID> parmIDs = Arrays.asList(getAvailableParms(requestID));

        if (parmIDs.contains(parmId)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean saveParm(Parm parm) {
        return parm.saveParameter(true);
    }

    @Override
    public boolean saveParm(Parm parm, TimeRange[] timeRanges) {
        return parm.saveParameter(Arrays.asList(timeRanges));
    }

    // -- private
    // ----------------------------------------------------------------
    // ParmMgr::setParms()
    // Adds/removes/displaystatechanges the set of given parms. Handles the
    // bookkeeping and sends out notifications.
    // -- implementation
    // ---------------------------------------------------------
    // This is the complicated routine which handles all of the bookkeeping
    // and the notifications. Should never see a NULL parm* given to this
    // routine.
    // ---------------------------------------------------------------------------
    @Override
    protected void setParms(final Collection<Parm> addParms,
            final Collection<Parm> removeParms,
            final Collection<Parm> displayedStateModParms) {
        // logDebug << "--- setParms(parmsToAdd,parmsToRemov,displayedStateMod):
        // "
        // << printAMR(addParms, displayedStateModParms, removeParms) <<
        // std::endl;
        // System.out
        // .println("--- setParms(parmsToAdd,parmsToRemov,displayedStateMod): "
        // + "add: "
        // + addParms.toString()
        // + ", remove: "
        // + removeParms.toString()
        // + ", mod: "
        // + displayedStateModParms.toString());

        // update list of parms
        this.parms.acquireWriteLock();
        try {
            for (Parm addParm : addParms) {
                if (addParm != null && !this.parms.contains(addParm)) {
                    this.parms.add(addParm); // add the additions

                    if (addParm.getParmID().equals(
                            this.dataManager.getTopoManager()
                                    .getCompositeParmID())
                            && PlatformUI.isWorkbenchRunning()) {

                        ICommandService service = (ICommandService) PlatformUI
                                .getWorkbench().getService(
                                        ICommandService.class);
                        service.refreshElements(
                                "com.raytheon.viz.gfe.actions.topo", null);
                    }
                }
            }

            for (Parm removeParm : removeParms) {
                if (removeParm != null && this.parms.contains(removeParm)) {
                    this.parms.remove(removeParm);

                    if (removeParm.getParmID().equals(
                            this.dataManager.getTopoManager()
                                    .getCompositeParmID())
                            && PlatformUI.isWorkbenchRunning()) {
                        ICommandService service = (ICommandService) PlatformUI
                                .getWorkbench().getService(
                                        ICommandService.class);
                        service.refreshElements(
                                "com.raytheon.viz.gfe.actions.topo", null);
                    }
                }
            }
        } finally {
            this.parms.releaseWriteLock();
        }

        // recalculate the system time range changes, send notification
        TimeRange newSysTR = recalcSystemTimeRange();
        if (!newSysTR.equals(this.systemTimeRange)) {
            systemTimeRange = newSysTR;

            fireSystemTimeRangeChanged(systemTimeRange);
        }

        // send ParmListChanged notification
        if (addParms.size() > 0 || removeParms.size() > 0) {
            this.parms.acquireReadLock();
            try {
                fireParmListChanged(
                        this.parms.toArray(new Parm[this.parms.size()]),
                        addParms.toArray(new Parm[addParms.size()]),
                        removeParms.toArray(new Parm[removeParms.size()]));
            } finally {
                this.parms.releaseReadLock();
            }

        }

        List<Parm> addedDisplayed = new ArrayList<Parm>();
        List<Parm> removedDisplayed = new ArrayList<Parm>();
        for (Parm p : addParms) {
            // the displayable check here is a deviation from AWIPS1 but it
            // appears the viz components don't all properly handle this message
            // as AWIPS1 did and it causes numerous weather elements that should
            // not be displayed to be added to the grid manager
            if (p != null && p.getDisplayAttributes().isDisplayable()) {
                addedDisplayed.add(p);
            }
        }

        for (Parm p : removeParms) {
            if (p != null) {
                removedDisplayed.add(p);
            }
        }

        for (Parm p : displayedStateModParms) {
            if (p.getDisplayAttributes().isDisplayable()) {
                addedDisplayed.add(p);
            } else {
                removedDisplayed.add(p);
            }
        }

        // set isc parm flags as appropriate for the additions
        for (Parm p : addParms) {
            if (iscDbs.contains(p.getParmID().getDbId())) {
                p.getParmState().setIscParm(true);
            }
        }

        // send DisplayedParmListChanged notification
        if (removedDisplayed.size() > 0 || addedDisplayed.size() > 0) {
            this.parms.acquireReadLock();
            try {
                // System.out.println("Removed from display: "
                // + removedDisplayed.toString());

                fireDisplayedParmListChanged(this.parms
                        .toArray(new Parm[this.parms.size()]), addedDisplayed
                        .toArray(new Parm[addedDisplayed.size()]),
                        removedDisplayed.toArray(new Parm[removedDisplayed
                                .size()]));
            } finally {
                this.parms.releaseReadLock();
            }
        }

        // send AvailableSourcesChanged notification
        try {
            List<DatabaseID> prevAvailDbs = getAvailableDbs();

            updateDatabaseLists();

            List<DatabaseID> nowDbs = getAvailableDbs();

            List<DatabaseID> addedDbs = new ArrayList<DatabaseID>();
            addedDbs.addAll(nowDbs);
            addedDbs.removeAll(prevAvailDbs);

            List<DatabaseID> removedDbs = new ArrayList<DatabaseID>();
            removedDbs.addAll(prevAvailDbs);
            removedDbs.removeAll(nowDbs);

            updateParmIdCache(removedDbs, addParms, removeParms);
            if (addedDbs.size() > 0 || removedDbs.size() > 0) {
                fireAvailableSourcesChanged(nowDbs, removedDbs, addedDbs);
            }
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

        // dispose the old parms
        for (Parm p : removeParms) {
            p.dispose();
        }
    }

    private void updateParmIdCache(final Collection<DatabaseID> removedDbs,
            final Collection<Parm> addedParms,
            final Collection<Parm> removedParms) {
        // remove databases
        synchronized (this.parmIDCacheServer) {
            for (DatabaseID removedDb : removedDbs) {
                this.parmIDCacheServer.remove(removedDb);
            }

            // add parms
            Map<DatabaseID, Set<ParmID>> dupRemovalMap = new HashMap<DatabaseID, Set<ParmID>>();
            for (Parm aParm : addedParms) {
                if (aParm instanceof DbParm) {
                    ParmID pId = aParm.getParmID();
                    DatabaseID dbID = pId.getDbId();
                    Set<ParmID> parmSet = dupRemovalMap.get(dbID);
                    if (parmSet == null) {
                        List<ParmID> parms = this.parmIDCacheServer.get(dbID);
                        if (parms == null) {
                            parmSet = new HashSet<ParmID>();
                        } else {
                            parmSet = new HashSet<ParmID>(parms);
                        }
                        dupRemovalMap.put(dbID, parmSet);
                    }
                    parmSet.add(pId);
                }
            }
            for (Entry<DatabaseID, Set<ParmID>> entry : dupRemovalMap
                    .entrySet()) {
                DatabaseID dbID = entry.getKey();
                this.parmIDCacheServer.put(dbID,
                        new ArrayList<ParmID>(entry.getValue()));
            }
        }

        synchronized (this.parmIDCacheVParm) {
            for (DatabaseID removedDb : removedDbs) {
                this.parmIDCacheVParm.remove(removedDb);
            }

            // search for parms that have been removed, and remove them from
            // the cache, only the VParm cache needs to be updated
            removeFromCache(removedParms, parmIDCacheVParm);

            for (Parm p : addedParms) {
                if (p instanceof VParm) {
                    addToCache(p, parmIDCacheVParm);
                }
            }
        }

        synchronized (this.parmIDCacheVCParm) {
            for (DatabaseID removedDb : removedDbs) {
                this.parmIDCacheVCParm.remove(removedDb);
            }

            for (Parm p : addedParms) {
                if (p instanceof VCParm) {
                    addToCache(p, parmIDCacheVCParm);
                }
            }
        }
    }

    /**
     * Updates the parmID cache. Given a list of parms to remove and the cache.
     * 
     * @param parms
     *            List of Parms to remove from the cache.
     * @param cache
     *            Cache to be updated.
     */
    private void removeFromCache(Collection<Parm> parms,
            Map<DatabaseID, List<ParmID>> cache) {
        for (Parm p : parms) {
            if (p != null) {
                DatabaseID dbId = p.getParmID().getDbId();
                if (cache.containsKey(dbId)) {
                    List<ParmID> ids = cache.get(dbId);
                    ids.remove(p.getParmID());
                }
            }
        }
    }

    /**
     * Updates the parmID cache. Given a parm to add and the cache.
     * 
     * @param p
     * @param cache
     */
    private void addToCache(Parm p, Map<DatabaseID, List<ParmID>> cache) {
        DatabaseID db = p.getParmID().getDbId();
        if (cache.containsKey(db)) {
            List<ParmID> ids = cache.get(db);
            if (!ids.contains(p.getParmID())) {
                ids.add(p.getParmID());
            }
        }
    }

    private void updateDatabaseLists() throws GFEServerException {
        this.availableServerDatabases = new ArrayList<DatabaseID>(
                availableDatabases);
        this.availableVCParmDatabases = determineVCParmDatabases(vcModules);
        this.availableVParmDatabases = new ArrayList<DatabaseID>();
        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if ((p instanceof VParm)
                        && (!availableVParmDatabases.contains(p.getParmID()
                                .getDbId()))) {
                    availableVParmDatabases.add(p.getParmID().getDbId());
                }
            }
        } finally {
            parms.releaseReadLock();
        }

        // calculate the name of isc database(s)
        if (this.iscDbs == null) {
            iscDbs = new ArrayList<DatabaseID>();
        } else {
            iscDbs.clear();
        }

        Collections.sort(this.availableServerDatabases);
        DatabaseID mutableDbId = getMutableDatabase();
        if (mutableDbId.isValid()) {
            boolean containsMutable = availableServerDatabases
                    .contains(mutableDbId);

            if (!containsMutable) {
                ServerResponse<?> sr = this.dataManager.getClient()
                        .createNewDb(mutableDbId);
                containsMutable = sr.isOkay();
            }

            if (containsMutable) {
                // order of isc databases is important, since ISCDataAccess will
                // look for the first match. That's why we don't use
                // availableDbs()
                // and simplify the three loops into one.
                for (DatabaseID dbid : availableVCParmDatabases) {
                    if (dbid.getModelName().equals("ISC")
                            && !iscDbs.contains(dbid)) {
                        iscDbs.add(dbid);
                    }
                }
                for (DatabaseID dbid : availableVParmDatabases) {
                    if (dbid.getModelName().equals("ISC")
                            && !iscDbs.contains(dbid)) {
                        iscDbs.add(dbid);
                    }
                }
                for (DatabaseID dbid : availableServerDatabases) {
                    if (dbid.getModelName().equals("ISC")
                            && !iscDbs.contains(dbid)) {
                        iscDbs.add(dbid);
                    }
                }
            }
        }
    }

    /**
     * Determines the set of virtual calculated databases, from the definitions.
     * 
     * @param definitions
     *            The list of VCParm definitions as VCModules.
     * @return The unique list of DatabaseIDs defined.
     */
    private List<DatabaseID> determineVCParmDatabases(List<VCModule> definitions) {
        List<DatabaseID> dbs = new ArrayList<DatabaseID>();
        for (VCModule mod : definitions) {
            DatabaseID id = mod.getGpi().getParmID().getDbId();
            if (!dbs.contains(id)) {
                dbs.add(id);
            }
        }

        return dbs;
    }

    // -- private
    // ----------------------------------------------------------------
    // ParmMgr::createParmInternal()
    // This function creates a new parm - of type database or virtual
    // calculated.
    // -- implementation
    // ---------------------------------------------------------
    // Determines whether this is a database or virtual calculated parm,
    // and calls the appropriate create routine. Does not do bookkeeping; only
    // creates the parm and returns the pointer to the caller.
    // ---------------------------------------------------------------------------
    @Override
    protected Parm createParmInternal(final ParmID pid, boolean mutableParm,
            boolean displayable) throws GFEServerException {
        if (!isParmInDatabase(pid)) {
            return null; // unknown
        }

        // already created?
        if (getParm(pid) != null) {
            return null; // already in existance
        }

        // check first if parm is a virtual calculated?
        int index = vcIndex(pid);
        if (index != -1) {
            return createVirtualCalculatedParm(vcModules.get(index),
                    displayable);
        }

        // database parm?
        return createDbParmInternal(pid, mutableParm, displayable);
    }

    // -- private
    // ----------------------------------------------------------------
    // ParmMgr::createDbParmInternal()
    // Internal routine to create a db parm.
    // -- implementation
    // ---------------------------------------------------------
    // Ensures parm doesn't already exist. Gets the GridParmInfo and LockTables
    // from the ifpServer. Creates the parm and
    // returns the pointer.
    // ---------------------------------------------------------------------------
    private Parm createDbParmInternal(final ParmID pid, boolean mutableParm,
            boolean displayable) throws GFEServerException {

        if (getParm(pid) != null) {
            return null;
        }

        if (!isParmInDatabase(pid)) {
            return null;
        }

        GridParmInfo gpi = dataManager.getClient().getGridParmInfo(pid);

        return new DbParm(pid, gpi, mutableParm, displayable, this.dataManager);

    }

    private Parm createVirtualCalculatedParm(final VCModule module,
            boolean displayable) {
        // already exists?
        if (getParm(module.getGpi().getParmID()) != null) {
            return null; // already exists
        }

        Parm parm = new VCParm(dataManager, displayable, module);
        return parm;
    }

    public void setSystemTimeRange(TimeRange systemTimeRange) {
        this.systemTimeRange = systemTimeRange;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#compositeGridLocation()
     */
    @Override
    public GridLocation compositeGridLocation() {
        return gloc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#getParmInExpr(java.lang.String,
     * boolean)
     */
    @Override
    public Parm getParmInExpr(String exprName, boolean enableTopo) {
        return getParmInExpr(exprName, enableTopo, dataManager
                .getSpatialDisplayManager().getActivatedParm());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#enableDisableTopoParm(boolean,
     * boolean)
     */
    @Override
    public void enableDisableTopoParm(boolean wanted, boolean forceVisibility) {
        // find out if the topo parm already exists
        boolean exists = false;
        Parm topoParm = getParm(this.dataManager.getTopoManager()
                .getCompositeParmID());
        if (topoParm != null) {
            exists = true;
        }

        // nothing to do
        if (wanted && exists || !wanted && !exists) {
            return;
        }

        // if needed
        if (wanted) {
            // get the data from the topography manager
            IGridSlice gridSlice = this.dataManager.getTopoManager()
                    .getCompositeTopo();

            // ensure validity
            if (gridSlice != null && gridSlice.isValid() == null) {
                // create the parm
                topoParm = createVirtualParm(gridSlice.getGridInfo()
                        .getParmID(), gridSlice.getGridInfo(),
                        new IGridSlice[] { gridSlice }, false, true);

                // If forceVisibility, force the visibility to on,
                // and force the display to IMAGE
                if (forceVisibility) {
                    this.setParmDisplayable(topoParm, true);
                    this.dataManager.getSpatialDisplayManager().setDisplayMode(
                            topoParm, VisMode.IMAGE);
                    this.dataManager.getSpatialDisplayManager().makeVisible(
                            topoParm, true, false);
                }
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Topography Not Available");
            }
        }

        // not wanted
        else {
            deleteParm(new Parm[] { topoParm });
        }
    }

    @Override
    public List<DatabaseID> getIscDatabases() {
        return new ArrayList<DatabaseID>(iscDbs);
    }

    @Override
    public ParmID getISCParmID(ParmID pid) {
        List<DatabaseID> iscDbs = getIscDatabases();

        for (DatabaseID iscDb : iscDbs) {
            ParmID iscid = new ParmID(pid.getParmName(), iscDb,
                    pid.getParmLevel());
            if (this.isParmInDatabase(iscid)) {
                return iscid;
            }

        }

        return new ParmID(); // no match found
    }

    @Override
    public void purgeDbCacheForSite(String site) {
        synchronized (this.parmIDCacheServer) {
            Iterator<DatabaseID> iter = this.parmIDCacheServer.keySet()
                    .iterator();
            while (iter.hasNext()) {
                DatabaseID db = iter.next();
                if (db.getSiteId().equalsIgnoreCase(site)) {
                    iter.remove();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#createParm(com.raytheon.uf.common
     * .dataplugin.gfe.db.objects.ParmID, boolean, boolean)
     */
    @Override
    public Parm createParm(ParmID pid, boolean mutableParm, boolean displayable) {
        try {
            return createParmInternal(pid, mutableParm, displayable);
        } catch (GFEServerException e) {
            statusHandler
                    .handle(Priority.EVENTA,
                            "Failure to instantiate parm in createParmInternal: "
                                    + pid, e);
            return null;
        }
    }
}
