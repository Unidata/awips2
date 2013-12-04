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

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;

import org.eclipse.core.runtime.ListenerList;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID.DataType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.DBInvChangeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridHistoryUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GridUpdateNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.LockNotification;
import com.raytheon.uf.common.dataplugin.gfe.slice.IGridSlice;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.RWLArrayList;
import com.raytheon.uf.viz.core.jobs.JobPool;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.PythonPreferenceStore;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.internal.NotificationRouter.AbstractGFENotificationObserver;
import com.raytheon.viz.gfe.core.msgs.EnableDisableTopoMsg;
import com.raytheon.viz.gfe.core.msgs.EnableDisableTopoMsg.Action;
import com.raytheon.viz.gfe.core.msgs.IAvailableSourcesChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.INewModelAvailableListener;
import com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISystemTimeRangeChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.ShowISCGridsMsg;
import com.raytheon.viz.gfe.core.parm.ABVParmID;
import com.raytheon.viz.gfe.core.parm.DbParm;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.core.parm.VCParm;
import com.raytheon.viz.gfe.core.parm.VParm;
import com.raytheon.viz.gfe.core.parm.vcparm.VCModule;
import com.raytheon.viz.gfe.core.parm.vcparm.VCModuleJobPool;
import com.raytheon.viz.gfe.types.MutableInteger;

/**
 * Class used to manage grid parms
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/26/2008              chammack    Split non-mock code from MockParmManager
 * 04/10/2008     #875     bphillip    Initial Creation
 * 04/18/2008     #875     bphillip    More implementation
 * 05/07/2008     #875     bphillip    Modified save forecast behavior
 * 05/19/2008     #875     bphillip    Implemented save forecast for vectors
 * 06/17/2008     #940     bphillip    Implemented GFE Locking
 * 08/19/2009    #2547     rjpeter     Implement Test/Prac database display.
 * 02/23/2012     #346     dgilling    Dispose of VCParms from this class's
 *                                     dispose method.
 * 02/23/2012     #346     dgilling    Ensure all Parms are disposed when calling
 *                                     dispose method.
 * 02/23/2012     #346     dgilling    Call Parm's dispose method when removing
 *                                     a Parm.
 * 03/01/2012     #346     dgilling    Use identity-based ListenerLists.
 * 03/01/2012     #354     dgilling    Modify setParms to always load (but not
 *                                     necessarily display) the ISC parms that
 *                                     correspond to a visible mutable parm.
 * 06/25/2012     #766     dgilling    Move to a shared thread pool for VCModule
 *                                     execution.
 * 06/25/2012     #766     dgilling    Fix NullPointerException from VCModules
 *                                     when running in practice mode.
 * 08/20/2012    #1082     randerso    Moved calcStepTimes to AbstractParmManager for
 *                                     use in PngWriter
 * 01/22/2013    #1515     dgilling    Increase default size of VCModule thread pool
 *                                     to decrease UI hang-ups waiting for results.
 * 03/20/2013    #1774     randerso    Code cleanup
 * 04/11/2013    16028     ryu         Fixed setParmsRemoveISCDeps() to not remove
 *                                     modified parms.
 * 05/02/2013    #1969     randerso    Cleaned up and optimized processing of DBInvChangedNotification
 * 05/14/2013    #2004     randerso    Corrected logging levels
 * 05/14/2013    #2004     randerso    Improved error handling
 * 08/06/2013    #1561     njensen     Use pm.listFiles() instead of pm.listStaticFiles()
 * 05/02/2013    #1969     randerso    Added code to explicitly create the mutable database
 *                                     if it doesn't exist. Used to just happen by accident
 *                                     when getParmList was called.
 * 11/21/2013    #2331     randerso    Merge with AbstractParmManager and deleted MockParmManager
 *                                     to simplify maintenance of this class.
 *                                     Changed handling of enabling/disabling Topo parm
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public class ParmManager implements IParmManager, IMessageClient {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParmManager.class);

    private static final int NOTIFICATION_THREADS = 4;

    private static final ThreadLocal<SimpleDateFormat> dateFormat = new ThreadLocal<SimpleDateFormat>() {
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat df = new SimpleDateFormat(
                    DatabaseID.MODEL_TIME_FORMAT);
            df.setTimeZone(TimeZone.getTimeZone("GMT"));
            return df;
        }
    };

    private class ParmIDVis {
        private ParmID pid;

        private boolean vis;

        public ParmIDVis(ParmID pid, boolean visible) {
            this.pid = pid;
            this.vis = visible;
        }

        public ParmID getParmID() {
            return pid;
        }

        public boolean isVisible() {
            return vis;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("(");
            sb.append(pid.toString());
            sb.append(',');
            sb.append(vis);
            sb.append(')');
            return sb.toString();
        }
    }

    private class ParmIDVisDep {
        private ParmID pid;

        private boolean vis;

        private boolean dep;

        public ParmIDVisDep(ParmID pid, boolean visible, boolean dep) {
            this.pid = pid;
            this.vis = visible;
            this.dep = dep;
        }

        public ParmID getParmID() {
            return pid;
        }

        public boolean isVisible() {
            return vis;
        }

        public boolean isDependent() {
            return dep;
        }

        public void setDependent(boolean dep) {
            this.dep = dep;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("(");
            sb.append(pid.toString());
            sb.append(",V=");
            sb.append(vis);
            sb.append(",D=");
            sb.append(dep);
            sb.append(')');
            return sb.toString();
        }
    }

    private DataManager dataManager;

    private RWLArrayList<Parm> parms;

    // virtual parm definitions (modulename = key)
    private List<VCModule> vcModules;

    private ListenerList availableSourcesListeners;

    private ListenerList displayedParmListListeners;

    private ListenerList newModelListeners;

    private ListenerList parmIdChangedListeners;

    private ListenerList parmListChangedListeners;

    private ListenerList systemTimeRangeChangedListeners;

    private TimeRange systemTimeRange;

    private DatabaseID productDB;

    private Set<DatabaseID> availableDatabases;

    private DatabaseID mutableDb;

    private DatabaseID origMutableDb;

    private List<String> dbCategories;

    private AbstractGFENotificationObserver<DBInvChangeNotification> dbInvChangeListener;

    private AbstractGFENotificationObserver<GridUpdateNotification> gridUpdateListener;

    private AbstractGFENotificationObserver<GridHistoryUpdateNotification> gridHistoryUpdateListener;

    private AbstractGFENotificationObserver<LockNotification> lockNotificationListener;

    private AbstractGFENotificationObserver<SiteActivationNotification> siteActivationListener;

    private JobPool notificationPool;

    private VCModuleJobPool vcModulePool;

    private final GridLocation gloc;

    private List<DatabaseID> availableServerDatabases;

    private List<DatabaseID> availableVParmDatabases;

    private List<DatabaseID> availableVCParmDatabases;

    private List<DatabaseID> iscDbs;

    private final Map<DatabaseID, List<ParmID>> parmIDCacheServer;

    private final Map<DatabaseID, List<ParmID>> parmIDCacheVParm;

    private final Map<DatabaseID, List<ParmID>> parmIDCacheVCParm;

    /**
     * Constructor
     * 
     * @param dmgr
     *            the DataManager
     * @throws GFEServerException
     */
    @SuppressWarnings("unchecked")
    public ParmManager(DataManager dmgr) throws GFEServerException {
        this.dataManager = dmgr;
        this.parms = new RWLArrayList<Parm>();
        this.displayedParmListListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.parmListChangedListeners = new ListenerList(ListenerList.IDENTITY);
        this.systemTimeRangeChangedListeners = new ListenerList(
                ListenerList.IDENTITY);
        this.availableSourcesListeners = new ListenerList(ListenerList.IDENTITY);
        this.newModelListeners = new ListenerList(ListenerList.IDENTITY);
        this.parmIdChangedListeners = new ListenerList(ListenerList.IDENTITY);

        // Get virtual parm definitions
        vcModules = initVirtualCalcParmDefinitions();
        vcModulePool = new VCModuleJobPool("GFE Virtual ISC Python executor",
                this.dataManager, vcModules.size() + 2, Boolean.TRUE);

        PythonPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();
        String mutableModel = prefs.getString("mutableModel");
        String oldMutableModel = null;
        CAVEMode opMode = CAVEMode.getMode();
        switch (opMode) {
        case PRACTICE:
            oldMutableModel = mutableModel;
            mutableModel = "Prac_Fcst";
            break;
        case TEST:
            oldMutableModel = mutableModel;
            mutableModel = "Test_Fcst";
            break;

        default:
        }

        this.mutableDb = decodeDbString(mutableModel);
        if (this.mutableDb == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to decode mutable database string [" + mutableModel
                            + "]. No mutable model.");
        }

        if (oldMutableModel != null) {
            this.origMutableDb = decodeDbString(oldMutableModel);
            if (this.origMutableDb == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to decode mutable database string ["
                                + mutableModel + "]. No mutable model.");
            }
        } else {
            this.origMutableDb = mutableDb;
        }

        dbCategories = Arrays.asList(prefs.getStringArray("dbTypes"));

        this.availableDatabases = new HashSet<DatabaseID>(
                getDatabaseInventory());

        this.dbInvChangeListener = new AbstractGFENotificationObserver<DBInvChangeNotification>(
                DBInvChangeNotification.class) {

            @Override
            public void notify(DBInvChangeNotification notificationMessage) {
                updatedDatabaseList(notificationMessage.getDeletions(),
                        notificationMessage.getAdditions());
            }

        };

        this.gridUpdateListener = new AbstractGFENotificationObserver<GridUpdateNotification>(
                GridUpdateNotification.class) {

            @Override
            public void notify(GridUpdateNotification notificationMessage) {
                ParmID parmID = notificationMessage.getParmId();
                Parm parm = getParm(parmID);
                if (parm != null) {
                    parm.inventoryArrived(
                            notificationMessage.getReplacementTimeRange(),
                            notificationMessage.getHistories());
                }
            }

        };

        this.gridHistoryUpdateListener = new AbstractGFENotificationObserver<GridHistoryUpdateNotification>(
                GridHistoryUpdateNotification.class) {

            @Override
            public void notify(GridHistoryUpdateNotification notificationMessage) {
                ParmID parmID = notificationMessage.getParmId();
                Parm parm = getParm(parmID);
                if (parm != null) {
                    parm.historyUpdateArrived(notificationMessage
                            .getHistories());
                }
            }

        };

        this.lockNotificationListener = new AbstractGFENotificationObserver<LockNotification>(
                LockNotification.class) {

            @Override
            public void notify(LockNotification notificationMessage) {
                LockTable table = notificationMessage.getLockTable();
                Parm parm = getParm(table.getParmId());
                if (parm != null) {
                    table.resetWsId(dataManager.getWsId());
                    parm.lockTableArrived(table);
                }
            }
        };

        this.siteActivationListener = new AbstractGFENotificationObserver<SiteActivationNotification>(
                SiteActivationNotification.class) {

            @Override
            public void notify(SiteActivationNotification notificationMessage) {
                if (notificationMessage.isActivation()) {
                    statusHandler.info(notificationMessage.toString());
                } else {
                    statusHandler.info(notificationMessage.toString());
                    if (notificationMessage.isSuccess()) {
                        purgeDbCacheForSite(notificationMessage
                                .getModifiedSite());
                    }
                }
            }
        };

        dataManager.getNotificationRouter().addObserver(
                this.dbInvChangeListener);

        dataManager.getNotificationRouter().addObserver(
                this.lockNotificationListener);

        dataManager.getNotificationRouter()
                .addObserver(this.gridUpdateListener);

        dataManager.getNotificationRouter().addObserver(
                this.gridHistoryUpdateListener);

        dataManager.getNotificationRouter().addObserver(
                this.siteActivationListener);

        notificationPool = new JobPool("Parm Manager notification job",
                NOTIFICATION_THREADS, true);

        Message.registerInterest(this, EnableDisableTopoMsg.class);

        // Get the composite grid location
        this.gloc = dataManager.getClient().getDBGridLocation();

        this.systemTimeRange = recalcSystemTimeRange();
        this.parmIDCacheServer = new HashMap<DatabaseID, List<ParmID>>();
        this.parmIDCacheVParm = new HashMap<DatabaseID, List<ParmID>>();
        this.parmIDCacheVCParm = new HashMap<DatabaseID, List<ParmID>>();

        updateDatabaseLists();

        this.productDB = determineProductDatabase();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#dispose()
     */
    @SuppressWarnings("unchecked")
    @Override
    public void dispose() {
        dataManager.getNotificationRouter().removeObserver(
                this.dbInvChangeListener);

        dataManager.getNotificationRouter().removeObserver(
                this.lockNotificationListener);

        dataManager.getNotificationRouter().removeObserver(
                this.gridUpdateListener);

        dataManager.getNotificationRouter().removeObserver(
                this.gridHistoryUpdateListener);

        dataManager.getNotificationRouter().removeObserver(
                this.siteActivationListener);

        Message.unregisterInterest(this, EnableDisableTopoMsg.class);

        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                p.dispose();
            }
        } finally {
            parms.releaseReadLock();
        }

        notificationPool.cancel();

        vcModulePool.cancel();
        for (VCModule module : vcModules) {
            module.dispose();
        }
    }

    private DatabaseID decodeDbString(final String string) {
        String type = "";
        String model = "";
        String dtg = DatabaseID.NO_MODEL_TIME;

        int pos = string.indexOf('_');

        if ((pos < 0) || (string.length() < (pos + 2))) {
            return null;
        }

        type = string.substring(0, pos);
        int npos = pos + 1;
        pos = string.indexOf('_', npos);
        if (pos < 0) {
            model = string.substring(npos);
        } else {
            model = string.substring(npos, pos);

            if ((string.length() - pos) == 14) {
                try {
                    dtg = string.substring(pos + 1);
                    dateFormat.get().parse(dtg);
                } catch (ParseException e) {
                    return null;
                }
            }
        }

        String location = dataManager.getSiteID();
        return new DatabaseID(location, DataType.GRID, type, model, dtg);
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
            sr = this.dataManager.getClient().getOfficialDBName();
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
    public Parm addParm(ParmID pid, boolean mutableParm, boolean displayable) {
        if (!isParmInDatabase(pid)) {
            statusHandler.handle(Priority.PROBLEM,
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
                            Priority.DEBUG,
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
            if (this.availableServerDatabases.contains(dbID)) {
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

    /**
     * ParmMgr::setParms() Adds/removes/displaystatechanges the set of given
     * parms. Handles the bookkeeping and sends out notifications.
     * 
     * This is the complicated routine which handles all of the bookkeeping and
     * the notifications. Should never see a NULL parm* given to this routine.
     * 
     * @param addParms
     * @param removeParms
     * @param displayedStateModParms
     */
    private void setParms(final Collection<Parm> addParms,
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
                if ((addParm != null) && !this.parms.contains(addParm)) {
                    this.parms.add(addParm); // add the additions
                }
            }

            for (Parm removeParm : removeParms) {
                if ((removeParm != null) && this.parms.contains(removeParm)) {
                    this.parms.remove(removeParm);
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
        if ((addParms.size() > 0) || (removeParms.size() > 0)) {
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
            if ((p != null) && p.getDisplayAttributes().isDisplayable()) {
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
        if ((removedDisplayed.size() > 0) || (addedDisplayed.size() > 0)) {
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
            if ((addedDbs.size() > 0) || (removedDbs.size() > 0)) {
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

    /**
     * This function creates a new parm - of type database or virtual
     * calculated.
     * 
     * Determines whether this is a database or virtual calculated parm, and
     * calls the appropriate create routine. Does not do bookkeeping; only
     * creates the parm and returns the pointer to the caller.
     * 
     * @param pid
     * @param mutableParm
     * @param displayable
     * @return
     * @throws GFEServerException
     */
    private Parm createParmInternal(final ParmID pid, boolean mutableParm,
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

    /**
     * Set the system time range
     * 
     * @param systemTimeRange
     */
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
        if ((wanted && exists) || (!wanted && !exists)) {
            return;
        }

        // if needed
        if (wanted) {
            // get the data from the topography manager
            IGridSlice gridSlice = this.dataManager.getTopoManager()
                    .getCompositeTopo();

            // ensure validity
            if ((gridSlice != null) && (gridSlice.isValid() == null)) {
                // create the parm
                topoParm = createVirtualParm(gridSlice.getGridInfo()
                        .getParmID(), gridSlice.getGridInfo(),
                        new IGridSlice[] { gridSlice }, false, false);

                // If forceVisibility, force the visibility to on,
                // and force the display to IMAGE
                if (forceVisibility) {
                    // added the following line to fix race condition
                    // where GFESpatialDisplayManager.setDisplayMode could be
                    // called
                    // before the topo resource was added to the resource list
                    topoParm.getDisplayAttributes().setVisMode(VisMode.IMAGE);
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
                    .handle(Priority.PROBLEM,
                            "Failure to instantiate parm in createParmInternal: "
                                    + pid, e);
            return null;
        }
    }

    /**
     * Recalculate the system time range using the total time span of all
     * displayed parms and their locks
     * 
     * @return the system time range
     */
    private TimeRange recalcSystemTimeRange() {
        // Keep track of the total time span of all displayed parms,
        // including the locks
        TimeRange newSystemTR = new TimeRange();

        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if (p.getDisplayAttributes().isDisplayable()) {
                    newSystemTR = newSystemTR.combineWith(p.getParmTimeRange());
                }
            }
        } finally {
            parms.releaseReadLock();
        }

        // now get the current time and determine its thresholds limits

        Date baseTime = SimulatedTime.getSystemTime().getTime();
        int hoursPast = 1 * TimeUtil.HOURS_PER_DAY; // defaults
        int hoursFuture = 6 * TimeUtil.HOURS_PER_DAY; // defaults

        PythonPreferenceStore prefStore = Activator.getDefault()
                .getPreferenceStore();

        if (prefStore.contains("SystemTimeRange_beforeCurrentTime")) {
            hoursPast = prefStore.getInt("SystemTimeRange_beforeCurrentTime");
        }

        if (prefStore.contains("SystemTimeRange_afterCurrentTime")) {
            hoursFuture = prefStore.getInt("SystemTimeRange_afterCurrentTime");
        }

        if (hoursFuture == hoursPast) {
            hoursFuture++;
        }
        TimeRange cTR = new TimeRange(baseTime.getTime()
                - (hoursPast * TimeUtil.MILLIS_PER_HOUR), baseTime.getTime()
                + (hoursFuture * TimeUtil.MILLIS_PER_HOUR));

        newSystemTR = newSystemTR.combineWith(cTR);

        // now expand to the next hour boundary
        long newStartT = newSystemTR.getStart().getTime();
        newStartT -= newSystemTR.getStart().getTime()
                % TimeUtil.MILLIS_PER_HOUR;
        long newStopT = newSystemTR.getEnd().getTime();
        if ((newStopT % TimeUtil.MILLIS_PER_HOUR) != 0) {
            newStopT += TimeUtil.MILLIS_PER_HOUR
                    - (newStopT % TimeUtil.MILLIS_PER_HOUR);
        }
        // Add two hours on either side to work around a display/UI
        // related problem...
        newSystemTR = new TimeRange(newStartT - (2 * TimeUtil.MILLIS_PER_HOUR),
                newStopT + (2 * TimeUtil.MILLIS_PER_HOUR));

        return newSystemTR;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.IParmManager#getLockedParms()
     */
    @Override
    public Parm[] getLockedParms() {
        List<Parm> retVal = new ArrayList<Parm>();

        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if (p.isLocked()) {
                    retVal.add(p);
                }
            }
        } finally {
            parms.releaseReadLock();
        }
        return retVal.toArray(new Parm[retVal.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.parm.IParmManager#getParm(com.raytheon.viz.
     * gfe.core.parm.ParmID)
     */
    @Override
    public Parm getParm(ParmID parmID) {
        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if (p.getParmID().equals(parmID)) {
                    return p;
                }
            }
        } finally {
            parms.releaseReadLock();
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.IParmManager#getUndisplayedParms()
     */
    @Override
    public Parm[] getUndisplayedParms() {
        Parm[] all = getAllParms();
        Parm[] displayed = getDisplayedParms();
        Set<Parm> parms = new HashSet<Parm>(Arrays.asList(all));
        parms.removeAll(Arrays.asList(displayed));

        return parms.toArray(new Parm[parms.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getSelectedParms()
     */
    @Override
    public Parm[] getSelectedParms() {
        // Cycles through each available parm and asks if selected(). If so,
        // adds it to the return list.
        List<Parm> selParms = new ArrayList<Parm>();
        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if (p.getParmState().isSelected()) {
                    selParms.add(p);
                }
            }
        } finally {
            parms.releaseReadLock();
        }
        return selParms.toArray(new Parm[selParms.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getModifiedParms()
     */
    @Override
    public Parm[] getModifiedParms() {
        List<Parm> retVal = new ArrayList<Parm>();
        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if (p.isModified()) {
                    retVal.add(p);
                }
            }
        } finally {
            parms.releaseReadLock();
        }
        return retVal.toArray(new Parm[retVal.size()]);

    }

    /**
     * Returns a matching parm * (creates if necessary) for the given expression
     * and database id.
     * 
     * @param dbid
     *            the database
     * @param exprName
     *            the expression name
     * @return the parm
     */
    private Parm parmInExprDatabase(final DatabaseID dbid, final String exprName) {
        DatabaseID mutableDb = getMutableDatabase();

        ParmID topoID = this.dataManager.getTopoManager().getCompositeParmID();
        ParmID[] parmIDs = getAvailableParms(dbid);
        for (ParmID parmID : parmIDs) {
            if (parmID.expressionName(topoID, mutableDb, false)
                    .equals(exprName)
                    || parmID.expressionName(topoID, mutableDb, true).equals(
                            exprName)) {
                // match found -- parm already exists? or do we need to create
                // it?
                Parm p = getParm(parmID);
                if (p != null) {
                    return p;
                } else {
                    boolean mutableFlag = parmID.getDbId().equals(mutableDb);
                    return addParm(parmID, mutableFlag, false);
                }
            }
        }
        return null;
    }

    /**
     * ParmMgr::setParmsRemoveModParms() Helper function for setParms(). Removes
     * any modified parms from the "remove" list. Returns the modified list
     * through the calling argument.
     * 
     * @param removeParms
     */
    private void setParmsRemoveModParms(Collection<ParmID> removeParms) {
        Parm[] isModified = getModifiedParms();

        Iterator<ParmID> pidIterator = removeParms.iterator();

        while (pidIterator.hasNext()) {
            ParmID parm = pidIterator.next();
            for (int i = 0; i < isModified.length; i++) {
                if (isModified[i].getParmID().equals(parm)) {
                    pidIterator.remove();
                    break;
                }
            }
        }
    }

    /**
     * ParmMgr::setParmsDetermineModParms() Helper function for setParms().
     * Separate out the parms that are simply switching display states from the
     * addParms. Return these. Modifies the addParms list.
     * 
     * @param addParms
     * @return
     */
    private Collection<Parm> setParmsDetermineModParms(List<ParmIDVis> addParms) {
        List<Parm> modParms = new ArrayList<Parm>();

        for (int i = addParms.size() - 1; i >= 0; i--) {
            Parm p = getParm(addParms.get(i).getParmID()); // already exist?
            if (p != null) {
                p.getDisplayAttributes().setDisplayable(
                        addParms.get(i).isVisible());
                if (!modParms.contains(p)) {
                    modParms.add(p);
                }
                addParms.remove(addParms.get(i)); // remove the entry
                                                  // from addParms
            }
        }
        return modParms;
    }

    /**
     * ParmMgr::dependentParms() Given a ParmID, returns list of dependent
     * parms. Dependent parms are those due to VCparms, or ISC parms. The
     * considerISC says to consider ISC dependencies based on showISC and the
     * Fcst->ISC relationship
     * 
     * @param pid
     * @param considerISC
     * @return
     */
    private List<ParmID> dependentParms(final ParmID pid, boolean considerISC) {
        List<ParmID> ret = new ArrayList<ParmID>(1);

        // check for a virtual calculated parm
        int index = vcIndex(pid);
        if (index != -1) {
            Collection<ParmID> dp = vcModules.get(index).dependentParms();
            for (ParmID depPId : dp) {
                if (!ret.contains(depPId)) {
                    ret.add(depPId);
                }

                if (vcIndex(depPId) != -1) { // vcparm
                    List<ParmID> dpa = dependentParms(depPId, considerISC);
                    for (ParmID recPId : dpa) {
                        if (!ret.contains(recPId)) {
                            ret.add(recPId);
                        }
                    }
                }
            }
        }

        // isc dependent parm? (showISC and mutable database)
        else if (considerISC) {
            if (pid.getDbId().equals(getMutableDatabase())) {
                ParmID iscP = getISCParmID(pid);
                if (!ret.contains(iscP) && !iscP.equals(new ParmID())) {
                    ret.add(iscP);
                }
            }
        }

        return ret;
    }

    /**
     * Helper function for <code>setParms</code>. Takes the toBeLoaded and
     * removeParms lists, calculates non-visible ISC dependencies, and then
     * returns the updated lists through the calling arguments.
     * 
     * @param toBeLoaded
     * @param removeParms
     */
    private void setParmsRemoveISCDeps(List<ParmIDVisDep> toBeLoaded,
            Collection<ParmID> removeParms) {
        List<ParmID> removeList = new ArrayList<ParmID>(removeParms);

        for (int i = 0; i < removeList.size(); i++) {
            List<ParmID> depParms = dependentParms(removeList.get(i), true);
            for (ParmID pid : depParms) {
                int index = pivdIndex(toBeLoaded, pid);
                if ((index != -1)
                        && (!toBeLoaded.get(index).isVisible())
                        && (!getParm(toBeLoaded.get(index).getParmID())
                                .isModified())) {
                    removeList.add(toBeLoaded.get(index).getParmID());
                    toBeLoaded.remove(index);
                }
            }
        }

        for (ParmID pid : removeList) {
            if (!removeParms.contains(pid)) {
                removeParms.add(pid);
            }
        }
    }

    /**
     * Helper function for <code>setParms</code>. Takes the toBeLoaded,
     * addedParms, removeParms, and modParms lists, calculates dependencies, and
     * then returns the updated lists through the calling arguments.
     * 
     * @param toBeLoaded
     * @param addParms
     * @param removeParms
     * @param modParms
     */
    private void setParmsDependencies(List<ParmIDVisDep> toBeLoaded,
            Collection<ParmIDVis> addParms, Collection<ParmID> removeParms,
            Collection<Parm> modParms) {
        // determine the list of dependent parms that will be required. If
        // any are on the remove list, move them to modParms and make them
        // non-displayed. If any are missing from the "tobeloaded", then
        // add them as undisplayed.
        for (int i = 0; i < toBeLoaded.size(); i++) {
            // Here's a derivation from AWIPS1...
            // We've hard-coded the third parameter in the dependentParms() call
            // to true so that the ISC parms that correspond to the visible
            // mutable parms are always loaded in the ParmManager. This was
            // found to significantly improve performance when loading ISC data.
            List<ParmID> depParms = dependentParms(toBeLoaded.get(i)
                    .getParmID(), true);

            for (ParmID depParm : depParms) {
                // if not present, then add it to "tobeloaded" list
                int index = pivdIndex(toBeLoaded, depParm);
                if (index == -1) {
                    toBeLoaded.add(new ParmIDVisDep(depParm, false, true));
                    if (getParm(depParm) == null) { // doesn't exist
                        addParms.add(new ParmIDVis(depParm, false));
                    }
                } else { // simply set the dependent flag
                    toBeLoaded.get(index).setDependent(true);
                }
                // if on remove list, then remove it, add as modified
                // undisplayed
                if (removeParms.contains(depParm)) {
                    removeParms.remove(depParm);
                    Parm p = getParm(depParm);
                    if (p.getDisplayAttributes().isDisplayable()) {
                        p.getDisplayAttributes().setDisplayable(false);
                        modParms.add(p);
                    }
                }
            }
        }
    }

    /**
     * ParmMgr::setParmsMakeParmIDVisDep() Helper function for setParms(). Makes
     * the initial ParmIDVisDep "tobeloaded" list.
     * 
     * @param addParms
     * @param modParms
     * @param removeParms
     * @return
     */
    private List<ParmIDVisDep> setParmsMakeParmIDVisDep(
            Collection<ParmIDVis> addParms, Collection<Parm> modParms,
            Collection<ParmID> removeParms) {
        // make a list of the expected situation, use ParmIDVisDep to hold the
        // displayed/undisplayed flags.
        List<ParmIDVisDep> toBeLoaded = new ArrayList<ParmIDVisDep>();
        // new ones
        for (ParmIDVis addParm : addParms) {
            toBeLoaded.add(new ParmIDVisDep(addParm.getParmID(), addParm
                    .isVisible(), false));
        }

        // mod display state
        for (Parm parm : modParms) {
            if (pivdIndex(toBeLoaded, parm.getParmID()) == -1) {
                toBeLoaded.add(new ParmIDVisDep(parm.getParmID(), parm
                        .getDisplayAttributes().isDisplayable(), false));
            }
        }

        // others, already in existence, that aren't on the remove list
        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if ((pivdIndex(toBeLoaded, p.getParmID()) == -1)
                        && (!removeParms.contains(p.getParmID()))) {
                    toBeLoaded.add(new ParmIDVisDep(p.getParmID(), p
                            .getDisplayAttributes().isDisplayable(), false));
                }

            }
        } finally {
            parms.releaseReadLock();
        }

        return toBeLoaded;
    }

    /**
     * 
     * Command to create/remove parms based on ParmID. For additions, the Map
     * contains the ParmID and visibility.
     * 
     * implementation ---------------------------------------------------------
     * Note: addParms, removeParms is modified within this routine, thus they
     * are not passed in as const references.
     * 
     * Routine converts the ParmIDs into Parms*. Special cases for VCParms,
     * since they need to load other parms possibly. Thus the input add and
     * remove may not result in the same parms being created and destroyed.
     * ------
     * ---------------------------------------------------------------------
     * 
     * @param addParms
     * @param removeParms
     */
    private void setParms(List<ParmIDVis> addParms,
            Collection<ParmID> removeParms) {
        // setCursor(0); // turn ON wait cursor

        // statusHandler.debug("********* setParms() begin *****************");
        // statusHandler.debug("initial setParms()");
        // statusHandler.debug("add: " + addParms.toString() + ", remove: "
        // + removeParms.toString());
        // System.out.println("********* setParms() begin *****************");
        // System.out.println("initial setParms() " + "add: "
        // + addParms.toString() + ", remove: " + removeParms.toString());

        // Ensure that no modified parms are being removed.
        setParmsRemoveModParms(removeParms);

        // Some parms in the addparms may only be switching display states,
        // separate these out. After this section, addParms will only be
        // parms to be created.
        Collection<Parm> modParms = setParmsDetermineModParms(addParms);
        // statusHandler.debug("after dspChanges only:");
        // statusHandler.debug("add: " + addParms.toString() + ", remove: "
        // + removeParms.toString() + ", mod: " + modParms.toString());
        // System.out.println("after dspChanges only:" + "add: "
        // + addParms.toString() + ", remove: " + removeParms.toString()
        // + ", mod: " + modParms.toString());

        // we do this twice, since the first time we may not catch all of
        // the entries that should be unloaded
        List<ParmIDVisDep> toBeLoaded;
        for (int i = 0; i < 2; i++) {
            // make a list of the expected situation, use ParmIDVisDep to hold
            // the displayed/undisplayed flags.
            toBeLoaded = setParmsMakeParmIDVisDep(addParms, modParms,
                    removeParms);

            // Here's a derivation from AWIPS1...
            // We've modified setParmsDependencies to always ensure that the ISC
            // parms that correspond to parms from the mutable database are
            // always loaded (but not necessary visible) in the ParmManager.
            // However, this change made it impossible to unload parms if they
            // were a dependency to an invisible VCParm. Hence, this new
            // function was added which removes the mutable parm and the ISC
            // parm(s) if none of them should be visible.
            setParmsRemoveISCDeps(toBeLoaded, removeParms);

            // statusHandler.debug("PASS " + (i + 1)
            // + " toBeLoaded before dependencies="
            // + toBeLoaded.toString());
            // statusHandler.debug("add: " + addParms.toString() + ", remove: "
            // + removeParms.toString() + ", mod: " + modParms.toString());
            // System.out.println("PASS " + (i + 1)
            // + " toBeLoaded before dependencies="
            // + toBeLoaded.toString() + "\n add: " + addParms.toString()
            // + ", remove: " + removeParms.toString() + ", mod: "
            // + modParms.toString());

            // allow for dependencies, which could readd parms
            setParmsDependencies(toBeLoaded, addParms, removeParms, modParms);
            // statusHandler
            // .debug("PASS " + (i + 1)
            // + " toBeLoaded after dependencies="
            // + toBeLoaded.toString());
            // statusHandler.debug("add: " + addParms.toString() + ", remove: "
            // + removeParms.toString() + ", mod: " + modParms.toString());
            // System.out.println("PASS " + (i + 1)
            // + " toBeLoaded after dependencies=" + toBeLoaded.toString()
            // + "\n add: " + addParms.toString() + ", remove: "
            // + removeParms.toString() + ", mod: " + modParms.toString());

            // now unload the ones that are undisplayed, and not dependent
            // and already exist (part of _parmList, but not part of modList).
            setParmsUnloadOld(toBeLoaded, modParms, removeParms);
            // statusHandler.debug("PASS " + (i + 1)
            // + " toBeLoaded after unloadOld=" + toBeLoaded.toString());
            // statusHandler.debug("add: " + addParms.toString() + ", remove: "
            // + removeParms.toString() + ", mod: " + modParms.toString());
            // System.out.println("PASS " + (i + 1)
            // + " toBeLoaded after unloadOld=" + toBeLoaded.toString()
            // + "\n add: " + addParms.toString() + ", remove: "
            // + removeParms.toString() + ", mod: " + modParms.toString());
        }

        // create the desired parms
        List<Parm> parmsToAdd = new ArrayList<Parm>();
        for (ParmIDVis addParm : addParms) {
            boolean mutableFlag = addParm.getParmID().getDbId()
                    .equals(getMutableDatabase());

            try {
                Parm p = createParmInternal(addParm.getParmID(), mutableFlag,
                        addParm.isVisible());

                // In AWIPS1 this would've triggered an error alert, however, in
                // AWIPS2 since we're preloading ISC parms in another thread,
                // there may be times we hit a situation where the ISC
                // preloading has created the parm before we've called
                // createParmInternal() in the UI thread causing the created
                // parm to be returned as a null reference.
                if (p != null) {
                    parmsToAdd.add(p);
                }
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failure to instantiate parm in createParmInternal: "
                                + addParm.getParmID().toString(), e);
            }
        }

        // get the parms to remove
        Collection<Parm> parmsToRemove = getParms(removeParms);

        // now handle the bookkeeping and notifications
        setParms(parmsToAdd, parmsToRemove, modParms);

        // setCursor(1); // turn OFF wait cursor
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#getParmInExpr(java.lang.String,
     * boolean, com.raytheon.viz.gfe.core.parm.Parm)
     */
    @Override
    public Parm getParmInExpr(final String exprName, boolean enableTopo,
            Parm variableParm) {
        // ---------------------------------------------------------
        // Loop through all displayed and perhaps available parms,
        // get their expressionNames and compare to that specified.
        // Return the parm pointer when we find a match.
        // ---------------------------------------------------------------------------
        DatabaseID mutableDb = getMutableDatabase();
        // Check for variableElement
        if (exprName.equals("variableElement")) {
            return variableParm;
        }

        // Handle Topo: If Topo is not available, make it available
        if (enableTopo && exprName.equals("Topo")) {
            new EnableDisableTopoMsg(Action.ENABLE, false).send();
            return getParmInExpr(exprName, false, variableParm);
        }

        // Check the mutable database first
        // ParmID topoID = _dataMgr->topoMgr()->compositeParmID();
        if (mutableDb != null /* isValid() */) {
            Parm p = parmInExprDatabase(mutableDb, exprName);
            if (p != null) {
                return p;
            }
        }
        // Now search through all available databases (which are sorted in
        // time order), get all of the parmids from each, and attempt the
        // expression name match
        for (DatabaseID db : getAvailableDbs()) {
            if (db.equals(mutableDb)) {
                continue; // already processed this one
            }
            Parm p = parmInExprDatabase(db, exprName);
            if (p != null) {
                return p;
            }
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.parm.IParmManager#getDisplayedParms()
     */
    @Override
    public Parm[] getDisplayedParms() {
        List<Parm> retVal = new ArrayList<Parm>();
        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                if (p.getDisplayAttributes().isDisplayable()) {
                    retVal.add(p);
                }
            }
        } finally {
            parms.releaseReadLock();
        }
        return retVal.toArray(new Parm[retVal.size()]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#setDisplayedParms(com.raytheon
     * .edex.plugin.gfe.db.objects.ParmID[])
     */
    @Override
    public void setDisplayedParms(ParmID[] parmList) {
        // ensure that there are no duplicates
        Set<ParmID> desParms = new HashSet<ParmID>(Arrays.asList(parmList));

        // convert currently displayed list to ParmIDs
        ParmID[] displayed = getParmIDs(getDisplayedParms());
        // logDebug << "currently displayed: ids=" << displayed << std::endl;

        Set<ParmID> parmSet = new HashSet<ParmID>(Arrays.asList(displayed));

        // find differences
        Set<ParmID> deletions = new HashSet<ParmID>(parmSet);
        deletions.removeAll(desParms);

        Set<ParmID> additions = new HashSet<ParmID>(desParms);
        additions.removeAll(parmSet);
        // logDebug << "to be deleted: ids=" << deletions << std::endl;
        // logDebug << "to be added: ids=" << additions << std::endl;

        // process deletions, but only if they are not modified state
        // this validates the deletions list
        List<ParmID> deletionsList = new ArrayList<ParmID>(deletions);
        List<ParmID> additionsList = new ArrayList<ParmID>(additions);

        for (int i = deletionsList.size() - 1; i >= 0; i--) {
            Parm p = getParm(deletionsList.get(i));
            if (p == null) {
                deletions.remove(deletionsList.get(i)); // non-existant parm
            } else if (p.isModified()) {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Attempt to unload a parm that is modified through"
                                + " setDisplayedParms(). Parm="
                                + deletionsList.get(i));
                deletions.remove(deletionsList.get(i));
            }
        }

        // process additions, simply to validate them
        for (int i = additionsList.size() - 1; i >= 0; i--) {
            if (!isParmInDatabase(additionsList.get(i))) {
                statusHandler.handle(
                        Priority.DEBUG,
                        "Attempt to load a non-existent parm"
                                + additionsList.get(i));
                additions.remove(additionsList.get(i));
            }
        }

        // additions, deletions have been validated
        List<ParmIDVis> addParms = new ArrayList<ParmIDVis>(additions.size());
        for (ParmID pid : additions) {
            addParms.add(new ParmIDVis(pid, true));
        }
        setParms(addParms, deletions);

        return;
    }

    /**
     * Helper function for setParms(). Unloads old parms that are undisplayed
     * and not dependent. Calling arguments are modified.
     * 
     * Ensure parm is already in existance, and isn't on the modParms list, and
     * isn't in the cachedParmList.
     * 
     * @param toBeLoaded
     * @param modParms
     * @param removeParms
     */
    private void setParmsUnloadOld(List<ParmIDVisDep> toBeLoaded,
            Collection<Parm> modParms, Collection<ParmID> removeParms) {
        // only try to unload old undisplayed, if there are already parms
        // being removed.
        if (removeParms.isEmpty()) {
            return;
        }

        // process backwards, since we might delete records...
        for (int i = toBeLoaded.size() - 1; i >= 0; i--) {
            // interested in not displayed, and not dependent parms
            if ((!toBeLoaded.get(i).isDependent())
                    && (!toBeLoaded.get(i).isVisible())) {
                // the entries must be existing parms, and thus on the
                // _parmList. Can't be modified.
                Parm p = getParm(toBeLoaded.get(i).getParmID());
                if ((p != null) && (!p.isModified())) {
                    removeParms.add(toBeLoaded.get(i).getParmID());
                    toBeLoaded.remove(i);
                    if (modParms.contains(p)) {
                        modParms.remove(p);
                    }
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener#parmIDChanged(com
     * .raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID)
     */
    @Override
    public void parmIDChanged(Parm parm, ParmID newParmID) {
        if (Arrays.asList(this.getAllParms()).contains(parm)) {
            fireParmIDChanged(parm, newParmID);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener#
     * parmInventoryChanged(com.raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.uf.common.time.TimeRange)
     */
    @Override
    public void parmInventoryChanged(Parm parm, TimeRange timeRange) {
        if (!systemTimeRange.isValid()
                || (timeRange.getStart().getTime() < systemTimeRange.getStart()
                        .getTime())
                || (timeRange.getEnd().getTime() > systemTimeRange.getEnd()
                        .getTime())) {
            TimeRange newTR = recalcSystemTimeRange();

            if (!newTR.equals(systemTimeRange)) {
                systemTimeRange = newTR;

                fireSystemTimeRangeChanged(systemTimeRange);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.ILockTableChangedListener#lockTableChanged
     * (com.raytheon.viz.gfe.core.parm.Parm,
     * com.raytheon.edex.plugin.gfe.server.lock.LockTable)
     */
    @Override
    public void lockTableChanged(Parm parm, LockTable lockTable) {
        TimeRange newTR = recalcSystemTimeRange();
        if (!newTR.equals(systemTimeRange)) {
            systemTimeRange = newTR;
            fireSystemTimeRangeChanged(systemTimeRange);
        }
        return;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#addDisplayedParmListChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener)
     */
    @Override
    public void addDisplayedParmListChangedListener(
            IDisplayedParmListChangedListener listener) {
        this.displayedParmListListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#removeDisplayedParmListChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener)
     */
    @Override
    public void removeDisplayedParmListChangedListener(
            IDisplayedParmListChangedListener listener) {
        this.displayedParmListListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#addParmListChangedListener(com
     * .raytheon.viz.gfe.core.msgs.IParmListChangedListener)
     */
    @Override
    public void addParmListChangedListener(IParmListChangedListener listener) {
        this.parmListChangedListeners.add(listener);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#removeParmListChangedListener(
     * com.raytheon.viz.gfe.core.msgs.IParmListChangedListener)
     */
    @Override
    public void removeParmListChangedListener(IParmListChangedListener listener) {
        this.parmListChangedListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#addParmIDChangedListener(com.raytheon
     * .viz.gfe.core.msgs.IParmIDChangedListener)
     */
    @Override
    public void addParmIDChangedListener(IParmIDChangedListener listener) {
        this.parmIdChangedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#removeParmIDChangedListener(com
     * .raytheon.viz.gfe.core.msgs.IParmIDChangedListener)
     */
    @Override
    public void removeParmIDChangedListener(IParmIDChangedListener listener) {
        this.parmIdChangedListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#addSystemTimeRangeChangedListener
     * (com.raytheon.viz.gfe.core.msgs.ISystemTimeRangeChangedListener)
     */
    @Override
    public void addSystemTimeRangeChangedListener(
            ISystemTimeRangeChangedListener listener) {
        this.systemTimeRangeChangedListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#removeSystemTimeRangeChangedListener
     * (com.raytheon.viz.gfe.core.msgs.ISystemTimeRangeChangedListener)
     */
    @Override
    public void removeSystemTimeRangeChangedListener(
            ISystemTimeRangeChangedListener listener) {
        this.systemTimeRangeChangedListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#addAvailableSourcesChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IAvailableSourcesChangedListener)
     */
    @Override
    public void addAvailableSourcesChangedListener(
            IAvailableSourcesChangedListener listener) {
        this.availableSourcesListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#addNewModelAvailableListener(com
     * .raytheon.viz.gfe.core.msgs.INewModelAvailableListener)
     */
    @Override
    public void addNewModelAvailableListener(INewModelAvailableListener listener) {
        this.newModelListeners.add(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#removeAvailableSourcesChangedListener
     * (com.raytheon.viz.gfe.core.msgs.IAvailableSourcesChangedListener)
     */
    @Override
    public void removeAvailableSourcesChangedListener(
            IAvailableSourcesChangedListener listener) {
        this.availableSourcesListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#removeNewModelAvailableListener
     * (com.raytheon.viz.gfe.core.msgs.INewModelAvailableListener)
     */
    @Override
    public void removeNewModelAvailableListener(
            INewModelAvailableListener listener) {
        this.newModelListeners.remove(listener);
    }

    /**
     * Fire the displayed parm list changed listener
     * 
     * @param parms
     *            complete list of parms
     * @param adds
     *            parms that were added
     * @param deletes
     *            parms that were deleted
     */
    private void fireDisplayedParmListChanged(final Parm[] parms,
            final Parm[] adds, final Parm[] deletes) {
        for (Object listener : this.displayedParmListListeners.getListeners()) {
            final IDisplayedParmListChangedListener casted = (IDisplayedParmListChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.displayedParmListChanged(parms, deletes, adds);
                }
            };

            notificationPool.schedule(notTask);
        }
    }

    /**
     * Fire the ParmID changed event.
     * 
     * @param parm
     *            The parm which had its ParmID change
     * @param newParmId
     *            The new ParmID associated with parm.
     */
    private void fireParmIDChanged(final Parm parm, final ParmID newParmId) {
        for (Object listener : this.parmIdChangedListeners.getListeners()) {
            final IParmIDChangedListener casted = (IParmIDChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.parmIDChanged(parm, newParmId);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    /**
     * Fire the parm list changed listener
     * 
     * @param parms
     *            complete list of parms
     * @param adds
     *            parms that were added
     * @param deletes
     *            parms that were deleted
     */
    private void fireParmListChanged(final Parm[] parms, final Parm[] adds,
            final Parm[] deletes) {
        for (Object listener : this.parmListChangedListeners.getListeners()) {
            final IParmListChangedListener casted = (IParmListChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.parmListChanged(parms, deletes, adds);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    /**
     * Fire the system time range changed listener
     * 
     * @param systemTimeRange
     *            new system time range
     */
    private void fireSystemTimeRangeChanged(final TimeRange systemTimeRange) {
        for (Object listener : this.systemTimeRangeChangedListeners
                .getListeners()) {
            final ISystemTimeRangeChangedListener casted = (ISystemTimeRangeChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.systemTimeRangeChanged(systemTimeRange);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    /**
     * Fire the available sources changed event.
     * 
     * @param inventory
     *            The complete inventory
     * @param deletions
     *            The items removed from the inventory
     * @param additions
     *            The items added to the inventory
     */
    private void fireAvailableSourcesChanged(final List<DatabaseID> inventory,
            final List<DatabaseID> deletions, final List<DatabaseID> additions) {
        for (Object listener : this.availableSourcesListeners.getListeners()) {
            final IAvailableSourcesChangedListener casted = (IAvailableSourcesChangedListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.availableSourcesChanged(inventory, deletions,
                            additions);
                }
            };

            notificationPool.schedule(notTask);
        }
    }

    /**
     * Fire the new model available event.
     * 
     * @param newModel
     * @param additions
     *            The DatabaseID of the newly-available model
     */
    private void fireNewModelAvailable(final DatabaseID newModel) {
        for (Object listener : this.newModelListeners.getListeners()) {
            final INewModelAvailableListener casted = (INewModelAvailableListener) listener;

            Runnable notTask = new Runnable() {

                @Override
                public void run() {
                    casted.newModelAvailable(newModel);
                }
            };
            notificationPool.schedule(notTask);
        }
    }

    /**
     * Return a list of ParmIDs for a list of Parms
     * 
     * @param parms
     *            the parms
     * @return the parm IDs
     */
    @Override
    public ParmID[] getParmIDs(Parm[] parms) {
        ParmID[] parmIDs = new ParmID[parms.length];
        for (int i = 0; i < parms.length; i++) {
            parmIDs[i] = parms[i].getParmID();
        }

        return parmIDs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#getParms(com.raytheon.uf.common
     * .dataplugin.gfe.db.objects.ParmID[])
     */
    @Override
    public Parm[] getParms(ParmID[] parmIDs) {
        Parm[] parms = new Parm[parmIDs.length];
        for (int i = 0; i < parmIDs.length; i++) {
            parms[i] = getParm(parmIDs[i]);
        }

        return parms;
    }

    /**
     * Return a list of Parms for a list of ParmIDs with nulls in place of parms
     * that are not loaded.
     * 
     * @param parmIDs
     * @return
     */
    private List<Parm> getParms(final Collection<ParmID> parmIDs) {
        List<Parm> parms = new ArrayList<Parm>();
        for (ParmID pid : parmIDs) {
            Parm p = getParm(pid);

            // add nulls for placeholders
            parms.add(p);
        }

        return parms;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getAllAvailableParms()
     */
    // @Override
    // public ParmID[] getAllAvailableParms() {
    // List<ParmID> parmIDs = new ArrayList<ParmID>();
    // for (DatabaseID dbID : this.getAvailableDbs()) {
    // parmIDs.addAll(Arrays.asList(this.getAvailableParms(dbID)));
    // }
    //
    // return parmIDs.toArray(new ParmID[parmIDs.size()]);
    //
    // }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#setParmDisplayable(com.raytheon
     * .viz.gfe.core.parm.Parm, boolean)
     */
    @Override
    public void setParmDisplayable(Parm parm, boolean displayable) {

        if (displayable && !parm.getDisplayAttributes().isDisplayable()) {
            parm.getDisplayAttributes().setDisplayable(true);
        } else if (!displayable && parm.getDisplayAttributes().isDisplayable()
                && !parm.isModified()) {
            parm.getDisplayAttributes().setDisplayable(false);
        } else {
            return; // modified displayable, can't change to non-displayable
        }

        // handle bookkeeping and notifications
        List<ParmIDVis> stateChanges = new ArrayList<ParmIDVis>();
        stateChanges.add(new ParmIDVis(parm.getParmID(), true));
        setParms(stateChanges, new ArrayList<ParmID>());

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#deallocateUnusedGrids(int)
     */
    @Override
    public void deallocateUnusedGrids(int seconds) {
        parms.acquireReadLock();
        try {
            for (Parm p : parms) {
                p.deallocateUnusedGrids(seconds);
            }
        } finally {
            parms.releaseReadLock();
        }
    }

    @Override
    public DatabaseID findDatabase(String databaseName, int version) {
        List<DatabaseID> availableDatabases = getAvailableDbs();

        // make a stripped time database name from dbName
        String modelName;
        String optType = "";
        int index = databaseName.indexOf("_");
        if (index > -1) {
            optType = databaseName.substring(0, index);
            modelName = databaseName.substring(index + 1);
        } else {
            modelName = databaseName;
        }

        DatabaseID stripped = new DatabaseID(this.dataManager.getSiteID(),
                DataType.GRID, optType, modelName);

        // attempt a match in the available database list. Note that the
        // list is always sorted in model time order within each modelName.
        List<DatabaseID> matches = new ArrayList<DatabaseID>();
        for (DatabaseID dbId : availableDatabases) {
            if (stripped.equals(dbId.stripModelTime())) {
                matches.add(dbId);
            }
        }

        // sort in descending order
        Collections.sort(matches, Collections.reverseOrder());

        // singleton database?
        if ((matches.size() == 1) && matches.get(0).equals(stripped)) {
            return stripped;
        }

        // version database -- determine correct version
        version = -version; // since version is negative, we want a postive
        // index into the array
        if ((version < 0) || (version >= matches.size())) {
            return new DatabaseID(); // don't have a match for this version
        }

        return matches.get(matches.size() - 1 - version);
    }

    @Override
    public boolean iscMode() {
        return Message.inquireLastMessage(ShowISCGridsMsg.class).show();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#getProductDB()
     */
    @Override
    public DatabaseID getProductDB() {
        return this.productDB;
    }

    /**
     * Filters out a complete list of databaseIDs to those only allowed by the
     * dbCatagories in the gfeConfig. Sorts the final list.
     * 
     * @param dbIds
     *            The list of DatabaseIDs to filter
     * @return A sorted list of DatabseIDs that are GRID types and match the
     *         list of allowed model types in the gfeConfig.
     */
    private List<DatabaseID> filterDbIds(List<DatabaseID> dbIds) {
        List<DatabaseID> filteredIds = new ArrayList<DatabaseID>();

        for (DatabaseID dbId : dbIds) {
            if (dbId.getFormat() == DataType.GRID) {
                if (dbId.stripModelTime().equals(mutableDb.stripModelTime())
                        || dbCategories.contains(dbId.getDbType())) {
                    filteredIds.add(dbId);
                }
            }
        }

        Collections.sort(filteredIds);
        return filteredIds;
    }

    /**
     * Returns a filtered list of the available databases. The list includes the
     * mutable model, plus all other databases identified by the database
     * categories specified in the gfeConfig. The databases are filtered by
     * projection also, since the GFE can only handle one projection.
     * 
     * @return A filtered list of available databases.
     */
    private List<DatabaseID> getDatabaseInventory() {
        List<DatabaseID> dbIds = null;

        try {
            dbIds = dataManager.getClient().getAvailableDbs();
        } catch (GFEServerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return new ArrayList<DatabaseID>(0);
        }

        return filterDbIds(dbIds);
    }

    /**
     * This function is called when the list of available database has changed.
     * The list of available parms is updated based on the list of additions and
     * deletions.
     * 
     * @param deletions
     *            The items being removed from the inventory
     * @param additions
     *            The items being added from the inventory
     */
    public void updatedDatabaseList(List<DatabaseID> deletions,
            List<DatabaseID> additions) {

        // create list of additions we didn't already have
        List<DatabaseID> newAdditions = new ArrayList<DatabaseID>(additions);
        newAdditions.removeAll(availableDatabases);

        availableDatabases.addAll(additions);
        availableDatabases.removeAll(deletions);

        List<ParmID> toDelete = new ArrayList<ParmID>();

        for (Parm parm : getAllParms()) {
            ParmID pid = parm.getParmID();
            if (deletions.contains(pid.getDbId())) {
                toDelete.add(pid);
            }
        }

        // now unload the parms, which handles the deletions and updates
        setParms(new ArrayList<ParmIDVis>(0), toDelete);

        for (DatabaseID model : newAdditions) {
            updateModel(model);
            fireNewModelAvailable(model);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.IParmManager#updateModel(com.raytheon.uf.common
     * .dataplugin.gfe.db.objects.DatabaseID)
     */
    @Override
    public void updateModel(DatabaseID modelIdentifier) {
        boolean anyChanges = false;

        // find all of the parms that share the same model,format, type, and
        // siteID
        DatabaseID stripModelIdentifier = modelIdentifier.stripModelTime();
        List<Parm> parmsToReplace = new ArrayList<Parm>();
        for (Parm parm : getAllParms()) {
            DatabaseID dbId = parm.getParmID().getDbId().stripModelTime();
            if (dbId.equals(stripModelIdentifier)) {
                parmsToReplace.add(parm);
            }
        }

        // now eliminate any way old parms (parms with the same parmname that
        // exists in the parmsToReplace list). All of these are from the same
        // model source, although the valid model times may be different.
        // We only to want to swap with one copy of this model.
        List<ParmID> forceUnloadParms = new ArrayList<ParmID>();
        ParmID[] lprId = getParmIDs(parmsToReplace
                .toArray(new Parm[parmsToReplace.size()]));
        List<ParmID> lprIdList = new ArrayList<ParmID>();
        for (ParmID parmId : lprId) {
            lprIdList.add(parmId);
        }
        Collections.sort(lprIdList, Collections.reverseOrder());

        for (ListIterator<ParmID> iterator = lprIdList.listIterator(lprIdList
                .size()); iterator.previousIndex() > 0;) {
            ParmID parmId = iterator.previous();

            if (parmId.getParmName().equals(
                    lprIdList.get(iterator.previousIndex()).getParmName())
                    && parmId.getParmLevel().equals(
                            lprIdList.get(iterator.previousIndex())
                                    .getParmLevel())) {
                Parm parmToRemove = getParm(parmId);
                if (parmsToReplace.contains(parmToRemove)) {
                    parmsToReplace.remove(parmToRemove);
                }
                forceUnloadParms.add(parmId);
                iterator.remove();
            }
        }

        if (forceUnloadParms.size() > 0) {
            anyChanges = true;
            setParms(new ArrayList<ParmIDVis>(), forceUnloadParms);
        }

        // perform the swap
        for (Parm parm : parmsToReplace) {
            ParmID newParmId = new ParmID(parm.getParmID().getParmName(),
                    modelIdentifier, parm.getParmID().getParmLevel());

            if (!newParmId.equals(parm.getParmID())) {
                // create the parm
                // NOTE: that temporary parm is created/deleted within this
                // routine and thus no notifications are managed.
                Parm newParm;

                try {
                    newParm = createParmInternal(newParmId, false, false);
                } catch (GFEServerException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problem with creating model-swap parm: "
                                    + newParmId.toString()
                                    + " in updateModel()", e);
                    continue;
                }

                // swap them
                parm.swapParm(newParm);
                anyChanges = true;
            }
        }

        if (anyChanges) {
            statusHandler.handle(Priority.EVENTA, "Model Updated: "
                    + modelIdentifier.toString());
        }
    }

    @SuppressWarnings("unused")
    private int pivIndex(List<ParmIDVis> list, ParmID key) {
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).getParmID().equals(key)) {
                return i;
            }
        }
        return -1;
    }

    private int pivdIndex(List<ParmIDVisDep> list, ParmID key) {
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i).getParmID().equals(key)) {
                return i;
            }
        }
        return -1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.gfe.core.IParmManager#deleteTemporaryParms()
     */
    @Override
    public void deleteTemporaryParms() {
        // get list of temporary parms
        List<Parm> tempParms = new ArrayList<Parm>();
        parms.acquireReadLock();
        try {
            for (Parm parm : parms) {
                if (parm.getParmState().isTemporary()) {
                    tempParms.add(parm);
                }

                // interested in not displayed, and not dependent parms, and
                // not on the cache list. Attempt to delete these -- they will
                // come back if they are dependent or cached. Be sure they
                // aren't already on the tempParms list.
                if (!parm.getDisplayAttributes().isDisplayable()
                        && !tempParms.contains(parm)) {
                    tempParms.add(parm);
                }
            }
        } finally {
            parms.releaseReadLock();
        }

        deleteParm(tempParms.toArray(new Parm[tempParms.size()]));
    }

    @Override
    public ParmID fromExpression(String expression) {
        return new ABVParmID(this).parse(expression);
    }

    /**
     * @return
     */
    private List<VCModule> initVirtualCalcParmDefinitions() {
        // retrieve the inventory from the ifpServer
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationFile[] modules = pathMgr
                .listFiles(
                        pathMgr.getLocalSearchHierarchy(LocalizationType.COMMON_STATIC),
                        FileUtil.join("gfe", "vcmodule"),
                        new String[] { "py" }, false, true);

        List<VCModule> definitions = new ArrayList<VCModule>(modules.length);
        for (LocalizationFile mod : modules) {
            try {
                // gets the module from the ifpServer
                File textData = mod.getFile(true);

                // create the VCModule
                VCModule m = new VCModule(dataManager, this, textData);
                if (!m.isValid()) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error creating VCModule " + textData.getPath(),
                            m.getErrorString());
                    continue;
                }
                definitions.add(m);
            } catch (LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to retrieve VCMODULE " + mod.toString(), e);
                continue;
            }

        }

        return definitions;
    }

    /**
     * Returns the Virtual Parm index into vcModules for the given ParmID.
     * 
     * @param pid
     *            ParmID to search for.
     * @return The index of the ParmID if it is in vcModules. Else, -1.
     */
    private int vcIndex(ParmID pid) {
        for (int i = 0; i < vcModules.size(); i++) {
            if (vcModules.get(i).getGpi().getParmID().equals(pid)) {
                return i;
            }
        }

        return -1;
    }

    @Override
    public JobPool getNotificationPool() {
        return notificationPool;
    }

    @Override
    public VCModuleJobPool getVCModulePool() {
        return vcModulePool;
    }

    @Override
    public void receiveMessage(Message message) {
        if (message instanceof EnableDisableTopoMsg) {
            EnableDisableTopoMsg msg = (EnableDisableTopoMsg) message;
            boolean wanted = msg.getAction().equals(Action.ENABLE);
            enableDisableTopoParm(wanted, msg.isForceVisibility());
        } else {
            statusHandler
                    .error("ParmManager.receiveMessage() received unexpected message: "
                            + message.getClass().getName());
        }
    }
}
