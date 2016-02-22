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
package com.raytheon.viz.gfe.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetIscSendStatusRequest.IscSendStatus;
import com.raytheon.uf.common.dataplugin.gfe.request.IscRequestQueryRequest.IscQueryResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.gfe.ifpclient.IFPClient;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.internal.DataMgrInitStatus;
import com.raytheon.viz.gfe.core.internal.GFEParmCacheInitJob;
import com.raytheon.viz.gfe.core.internal.GFETopoManager;
import com.raytheon.viz.gfe.core.internal.NotificationRouter;
import com.raytheon.viz.gfe.core.internal.ParmManager;
import com.raytheon.viz.gfe.core.internal.ReferenceSetManager;
import com.raytheon.viz.gfe.core.internal.WEGroupManager;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.parm.ParmOp;
import com.raytheon.viz.gfe.gridmanager.IGridManager;
import com.raytheon.viz.gfe.jobs.AutoSaveJob;
import com.raytheon.viz.gfe.procedures.ProcedureJobPool;
import com.raytheon.viz.gfe.procedures.ProcedureMetadataManager;
import com.raytheon.viz.gfe.smarttool.EditActionProcessor;
import com.raytheon.viz.gfe.smarttool.GridCycler;
import com.raytheon.viz.gfe.smarttool.script.SmartToolJobPool;
import com.raytheon.viz.gfe.smarttool.script.SmartToolMetadataManager;
import com.raytheon.viz.gfe.textformatter.TextProductManager;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeProhibitedOpException;

/**
 * DataManager is the central singleton in GFE upon which other managers are
 * attached.
 * 
 * DataManager has very little of its own functionality, it provides a
 * correlation between the various other managers in the system.
 * 
 * In the current implementation, one DataManager is associated to each eclipse
 * workbench window.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03/12/2008              chammack    Initial Creation.
 * 02/15/2013    1507      dgilling    Force procedureInterface and
 *                                     smartToolInterface to be 
 *                                     initialized by constructor.
 * 04/24/2013    1936      dgilling    Move initialization of TextProductMgr
 *                                     to GFE startup.
 * 08/27/2013    2302      randerso    Code cleanup for AutoSaveJob
 * 09/05/2013    2307      dgilling    Use better PythonScript constructor.
 * 09/16/2013    2033      dgilling    Remove unused IToolController.
 * 12/09/2013    2367      dgilling    Instantiate ProcedureJobPool here.
 * 05/22/2014    3110      randerso    Attach router to edex.alerts.gfe earlier
 * 09/09/2014    3592      randerso    Added call to SampleSetManager.dispose()
 * 10/30/2014    3775      randerso    Added parmCacheInit to initStatus
 * 04/20/2015    4027      randerso    Let TextProductManager know we are not running in a GUI
 * 07/23/2015    4263      dgilling    Refactor to support initialization of script 
 *                                     controllers off main thread.
 * Aug 13, 2015  4749      njensen     Improved dispose(), parmEvictor can cancel                                    
 * 08/14/2015    4750      dgilling    Remove use of PythonScript in doIscRequestQuery.
 * 08/20/2015    4749      dgilling    Ensure TextProductManager is disposed on dispose.
 * 09/15/2015    4858      dgilling    Disable ISC when DRT mode is enabled.
 * 11/18/2015    5129      dgilling    Support new IFPClient.
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DataManager implements ISimulatedTimeChangeListener {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataManager.class);

    /**
     * Use {@link DataManagerUIFactory#getCurrentInstance()}
     */
    @Deprecated
    public static DataManager getCurrentInstance() {
        return DataManagerUIFactory.getCurrentInstance();
    }

    /**
     * Use {@link DataManagerUIFactory#findInstance(IWorkbenchWindow)}
     */
    @Deprecated
    public static DataManager findInstance(IWorkbenchWindow window) {
        return DataManagerUIFactory.findInstance(window);
    }

    /**
     * Use {@link DataManagerUIFactory#getInstance(IWorkbenchWindow)}
     */
    @Deprecated
    public static DataManager getInstance(IWorkbenchWindow window) {
        return DataManagerUIFactory.getInstance(window);
    }

    private final IParmManager parmManager;

    protected IISCDataAccess iscDataAccess;

    private final ISpatialDisplayManager spatialDisplayManager;

    private final IReferenceSetManager refManager;

    protected ISampleSetManager sampleSetManager;

    protected ISelectTimeRangeManager selectTimeRangeManager;

    private final IFPClient client;

    private final ParmOp parmOp;

    private final IWEGroupManager weGroupManager;

    private final NotificationRouter router;

    private final ITopoManager topoManager;

    private IGridManager gridManager;

    private AutoSaveJob autoSaveJob;

    private SmartToolMetadataManager smartToolInterface;

    private ProcedureMetadataManager procedureInterface;

    private TextProductManager textProductMgr;

    private EditActionProcessor editActionProcessor;

    /** interval that the parm evictor runs-- every 15 seconds */
    private static final int PARM_EVICTOR_SCHEDULE = 15;

    /** Threshold of how long a parm can be unused before evicted -- 30 seconds */
    private static final int PARM_EVICTOR_THRESHOLD = 30;

    private String siteId;

    private String officeType;

    private boolean iscSendState;

    private boolean requestISC = true;

    private boolean sendISConSave;

    private boolean sendISConPublish;

    private DataMgrInitStatus initStatus = new DataMgrInitStatus();

    private Map<String, String> officeTypeDict;

    private Set<String> knownOfficeTypes;

    private List<String> allSites;

    private final ProcedureJobPool procJobPool;

    private final SmartToolJobPool toolJobPool;

    private boolean previousIscSendState;

    private final AtomicBoolean smartToolsInitialized;

    private final AtomicBoolean proceduresInitialized;

    private final AtomicBoolean textProductsInitialized;

    private final ParmEvictor parmEvictorJob;

    public IISCDataAccess getIscDataAccess() {
        return iscDataAccess;
    }

    /**
     * Constructs a DataManager, passes in factory for creation of
     * {@link ISpatialDisplayManager} for this instance.
     * 
     * @param factory
     * @param discriminator
     *            used as key for this instance of DataManager in the factory's
     *            instance map. Normally this is the window GFE is running in.
     * @throws GFEServerException
     */
    DataManager(DataManagerFactory factory, Object discriminator)
            throws GFEServerException {
        this.spatialDisplayManager = factory.createSpatialDisplayManager(this,
                discriminator);
        this.client = new IFPClient(VizApp.getWsId(), LocalizationManager
                .getInstance().getSite());
        this.router = new NotificationRouter(this.getSiteID());
        NotificationManagerJob.addObserver("edex.alerts.gfe", this.router);

        this.parmManager = new ParmManager(this);
        GFEParmCacheInitJob cacheJob = new GFEParmCacheInitJob(this);
        cacheJob.setSystem(true);
        cacheJob.schedule();
        this.refManager = new ReferenceSetManager(this);
        SampleSetMgrInitJob ssInitJob = new SampleSetMgrInitJob(this);
        ssInitJob.setSystem(true);
        ssInitJob.schedule();
        // this.sampleSetManager = new SampleSetManager(this);
        SelectTRMgrInitJob strInitJob = new SelectTRMgrInitJob(this);
        strInitJob.setSystem(true);
        strInitJob.schedule();

        smartToolsInitialized = new AtomicBoolean(false);
        proceduresInitialized = new AtomicBoolean(false);
        textProductsInitialized = new AtomicBoolean(false);
        initializeScriptControllers(discriminator);
        waitForScriptControllers();
        procJobPool = new ProcedureJobPool(4, 4, this);
        toolJobPool = new SmartToolJobPool(3, 3, this);

        this.weGroupManager = new WEGroupManager(this);
        this.editActionProcessor = new EditActionProcessor(this);

        // get office type information, convert to Dictionary
        ServerResponse<Map<String, String>> sr = client
                .getKnownSitesWithOfficeType();
        this.officeTypeDict = Collections.unmodifiableMap(sr.getPayload());
        this.allSites = Collections.unmodifiableList(new ArrayList<>(
                this.officeTypeDict.keySet()));
        this.officeType = officeTypeDict.get(this.siteId);

        // determine all known office types
        this.knownOfficeTypes = Collections
                .unmodifiableSet(new HashSet<String>(this.officeTypeDict
                        .values()));

        ISCInitJob iscInitJob = new ISCInitJob(this);
        iscInitJob.setSystem(true);
        iscInitJob.schedule();

        // get the ISC states
        ServerResponse<IscSendStatus> sr2 = client.iscSendStatus();
        IscSendStatus iscSendStatus = sr2.getPayload();
        this.sendISConSave = iscSendStatus.isSendISConSave();
        this.sendISConPublish = iscSendStatus.isSendISConPublish();
        this.requestISC = iscSendStatus.isRequestISC();

        // set the ISC send state, which initially sends the message
        new ISCSendStatusChangedMsg(false).send(); // initial state
        if (CAVEMode.getMode().equals(CAVEMode.OPERATIONAL)
                && ((SimulatedTime.getSystemTime().isRealTime()))
                || (SimulatedTimeOperations.isTransmitAllowedinSimulatedTime())) {
            try {
                enableISCsend(true);
            } catch (SimulatedTimeProhibitedOpException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }
        // this.queryString = "siteID='" + this.getSiteID() + "'";
        this.router.start();

        this.parmOp = new ParmOp(this);
        this.topoManager = new GFETopoManager(this);

        this.autoSaveJob = new AutoSaveJob(this);

        this.parmEvictorJob = new ParmEvictor();

        if (CAVEMode.getMode() == CAVEMode.OPERATIONAL) {
            SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(this);
        }
        this.previousIscSendState = clientISCSendStatus();
    }

    /**
     * @return the autoSaveJob
     */
    public AutoSaveJob getAutoSaveJob() {
        return autoSaveJob;
    }

    /**
     * Disposes the DataManager object, should only be called through
     * {@link DataManagerFactory#dispose(Object)}
     */
    void dispose() {
        sampleSetManager.dispose();
        selectTimeRangeManager.dispose();
        refManager.dispose();
        parmManager.dispose();
        weGroupManager.dispose();
        autoSaveJob.dispose();
        autoSaveJob = null;

        if (smartToolInterface != null) {
            smartToolInterface.dispose();
        }

        if (procedureInterface != null) {
            procedureInterface.dispose();
        }

        if (textProductMgr != null) {
            textProductMgr.dispose();
        }

        // by moving the the pools' cancel calls to another thread, we prevent
        // GFE shutdown from freezing the UI thread until all jobs have
        // completed. The unfortunate side effect is that we get that annoying
        // "Job found still running after platform shutdown" warning from
        // Eclipse.
        Runnable killJobPools = new Runnable() {

            @Override
            public void run() {
                if (toolJobPool != null) {
                    toolJobPool.cancel();
                }

                if (procJobPool != null) {
                    procJobPool.cancel();
                }
            }
        };
        Thread killPoolsThread = new Thread(killJobPools, "shutdown-gfe-pools");
        killPoolsThread.setDaemon(false);
        killPoolsThread.start();

        parmEvictorJob.cancel();

        if (CAVEMode.getMode() == CAVEMode.OPERATIONAL) {
            SimulatedTime.getSystemTime().removeSimulatedTimeChangeListener(
                    this);
        }

        NotificationManagerJob.removeObserver("edex.alerts.gfe", router);
    }

    /**
     * Return the workstation ID
     * 
     * @return the workstation ID
     */
    public WsId getWsId() {
        return VizApp.getWsId();
    }

    /**
     * Return the parm manager
     * 
     * @return the parm manager
     */
    public IParmManager getParmManager() {
        return this.parmManager;
    }

    /**
     * Return the reference set manager
     * 
     * @return the reference set manager
     */
    public IReferenceSetManager getRefManager() {
        return this.refManager;
    }

    /**
     * Return the spatial display manager
     * 
     * @return the spatial display manager
     */
    public ISpatialDisplayManager getSpatialDisplayManager() {
        return this.spatialDisplayManager;
    }

    /**
     * @return the selectTimeRangeManager
     */
    public ISelectTimeRangeManager getSelectTimeRangeManager() {
        return selectTimeRangeManager;
    }

    /**
     * Return the grid cycler (used for smart tools support)
     * 
     * @return the grid cycler
     */
    public GridCycler getGridCycler() {
        return GridCycler.getInstance();
    }

    /**
     * Return the site id
     * 
     * @return the site id
     */
    public synchronized String getSiteID() {
        if (siteId == null) {
            ServerResponse<String> sr = getClient().getSiteID();
            siteId = sr.getPayload();
        }
        return siteId;
    }

    /**
     * Return the Client
     * 
     * @return the Client
     */
    public IFPClient getClient() {
        return client;
    }

    /**
     * Return the ParmOp
     * 
     * @return the ParmOp
     */
    public ParmOp getParmOp() {
        return parmOp;
    }

    /**
     * @return the sampleSetManager
     */
    public ISampleSetManager getSampleSetManager() {
        return this.sampleSetManager;
    }

    public ITopoManager getTopoManager() {
        return this.topoManager;
    }

    /**
     * Return the weather element group manager
     * 
     * @return the weather element group manager
     */
    public IWEGroupManager getWEGroupManager() {
        return this.weGroupManager;
    }

    /**
     * Return the notification router
     * 
     * @return the notification router
     */
    public NotificationRouter getNotificationRouter() {
        return this.router;
    }

    /**
     * Obtains and returns the current server inventory for the specified
     * weather element identifier.
     * 
     * @param parmID
     *            Weather element to retrieve inventory for.
     * @return inventory for the parm. In the case of failure, an empty list
     *         will be returned.
     */
    public List<TimeRange> serverParmInventory(final ParmID parmID) {
        ServerResponse<List<TimeRange>> sr = client.getGridInventory(parmID);
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Unable to obtain serverParmInventory for %s: %s", parmID,
                    sr.message()));
            return Collections.emptyList();
        }

        return sr.getPayload();
    }

    /**
     * Get the system's operating mode
     * 
     * @return the operating mode
     */
    public CAVEMode getOpMode() {
        return CAVEMode.getMode();
    }

    /**
     * ParmEvictor runs periodically to evict parms that have not been recently
     * used from memory
     * 
     * @author chammack
     * @version 1.0
     */
    private class ParmEvictor extends Job {

        public ParmEvictor() {
            super("Parm Evictor Job");
            this.setSystem(true);
            this.schedule(1000L * PARM_EVICTOR_SCHEDULE);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            try {
                getParmManager().deallocateUnusedGrids(PARM_EVICTOR_THRESHOLD);
            } catch (Throwable e) {
                // Make sure we catch ANYTHING so this job doesn't get
                // killed
                statusHandler.handle(Priority.PROBLEM,
                        "Error occured while evicting grids", e);
            }

            if (!monitor.isCanceled()) {
                this.schedule(1000L * PARM_EVICTOR_SCHEDULE);
            }

            return Status.OK_STATUS;
        }
    }

    public IGridManager getGridManager() {
        return gridManager;
    }

    public void setGridManager(IGridManager gridManager) {
        this.gridManager = gridManager;
    }

    private void initializeScriptControllers(final Object discriminator) {
        smartToolInterface = new SmartToolMetadataManager(this);
        IAsyncStartupObjectListener smartToolListener = new IAsyncStartupObjectListener() {

            @Override
            public void objectInitialized() {
                smartToolsInitialized.set(true);
            }
        };
        smartToolInterface.initialize(smartToolListener);

        procedureInterface = new ProcedureMetadataManager(this);
        IAsyncStartupObjectListener procedureListener = new IAsyncStartupObjectListener() {

            @Override
            public void objectInitialized() {
                proceduresInitialized.set(true);
            }
        };
        procedureInterface.initialize(procedureListener);

        textProductMgr = new TextProductManager();
        IAsyncStartupObjectListener textProductListener = new IAsyncStartupObjectListener() {

            @Override
            public void objectInitialized() {
                textProductsInitialized.set(true);
            }
        };
        boolean startLocalizationListener = (discriminator != null);
        textProductMgr.init(startLocalizationListener, textProductListener);
    }

    private void waitForScriptControllers() {
        long waitTime = 0;
        while (!(smartToolsInitialized.get() && proceduresInitialized.get() && textProductsInitialized
                .get())) {
            try {
                waitTime += 10;
                Thread.sleep(10);
            } catch (InterruptedException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }

        statusHandler.info("Waited " + waitTime
                + "ms for script controller initialization...");
    }

    public SmartToolMetadataManager getSmartToolInterface() {
        return smartToolInterface;
    }

    public ProcedureMetadataManager getProcedureInterface() {
        return procedureInterface;
    }

    /**
     * Returns the current vtecActiveTable, based upon the current
     * {@code CAVEMode}.
     * 
     * @return The current active table.
     */
    public List<ActiveTableRecord> getActiveTable() {
        ActiveTableMode tableName = (CAVEMode.getMode() == CAVEMode.PRACTICE) ? ActiveTableMode.PRACTICE
                : ActiveTableMode.OPERATIONAL;
        ServerResponse<List<ActiveTableRecord>> sr = getClient()
                .getVTECActiveTable(tableName);
        if (!sr.isOkay()) {
            statusHandler.error("Unable to obtain vtecActiveTable: "
                    + sr.message());
        }
        return sr.getPayload();
    }

    public IscQueryResponse doIscRequestQuery() {
        ServerResponse<IscQueryResponse> sr = client.iscRequestQuery();
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Unable to get server info from iscRequestQuery: %s",
                    sr.message()));
            return new IscQueryResponse();
        }

        Collection<String> parmsWanted = Collections.emptyList();
        Collection<String> parms = sr.getPayload().getRequestedParms();
        parmsWanted = new ArrayList<>(parms.size());
        for (String parm : parms) {
            parmsWanted.add(parm.replace("_SFC", StringUtils.EMPTY));
        }

        return new IscQueryResponse(sr.getPayload().getDomainDict(), sr
                .getPayload().getServerDictT2S(), parmsWanted);
    }

    public void makeISCRequest(String xmlRequest) {
        ServerResponse<?> sr = client.iscRequestMake(xmlRequest);
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Unable to process iscRequestMake: %s", sr.message()));
        }
    }

    public String officeType(String site) {
        return this.officeTypeDict.get(site);
    }

    public List<String> knownOfficeTypes() {
        return new ArrayList<String>(this.knownOfficeTypes);
    }

    public List<String> knownSites() {
        return this.allSites;
    }

    public String getOfficeType() {
        return officeType;
    }

    public void setOfficeType(String officeType) {
        this.officeType = officeType;
    }

    /**
     * Returns the current client isc send status from the ifpServer.
     * 
     * @return client isc send status
     */
    public boolean clientISCSendStatus() {
        return iscSendState;
    }

    /**
     * Returns the SendISConSave configuration item.
     * 
     * @return SendISConSave configuration item
     */
    public boolean sendIscOnSave() {
        return sendISConSave;
    }

    /**
     * Returns the SendISConPublish configuration item.
     * 
     * @return SendISConPublish configuration item
     */
    public boolean sendIscOnPublish() {
        return sendISConPublish;
    }

    /**
     * Returns the requestISC configuration item.
     * 
     * @return requestISC configuration item
     */
    public boolean requestISC() {
        return requestISC;
    }

    /**
     * Sets the overall isc send capability for this user.
     * 
     * @param state
     * @throws SimulatedTimeProhibitedOperationException
     */
    public void enableISCsend(boolean state)
            throws SimulatedTimeProhibitedOpException {
        if (state && !SimulatedTimeOperations.isTransmitAllowed()) {
            throw SimulatedTimeOperations
                    .constructProhibitedOpException("ISC Send");
        }

        if (state == iscSendState) {
            return; // do nothing
        }

        if (!CAVEMode.getMode().equals(CAVEMode.OPERATIONAL) || !requestISC()) {
            state = false; // always disable if not operational or requestISC
            // off

        }

        iscSendState = state; // as desired

        // send out a message to interested parties (both for success and fail)
        new ISCSendStatusChangedMsg(state).send();
    }

    public EditActionProcessor getEditActionProcessor() {
        return this.editActionProcessor;
    }

    public DataMgrInitStatus getInitStatus() {
        return initStatus;
    }

    public TextProductManager getTextProductMgr() {
        return textProductMgr;
    }

    public ProcedureJobPool getProcedureJobPool() {
        return procJobPool;
    }

    public SmartToolJobPool getSmartToolJobPool() {
        return toolJobPool;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.time.ISimulatedTimeChangeListener#timechanged()
     */
    @Override
    public void timechanged() {
        boolean tryToEnable = SimulatedTimeOperations.isTransmitAllowed();
        boolean newState;
        if (tryToEnable) {
            newState = previousIscSendState;
        } else {
            newState = false;
            previousIscSendState = clientISCSendStatus();
        }

        try {
            enableISCsend(newState);
        } catch (SimulatedTimeProhibitedOpException e) {
            /*
             * We should never hit this state...but better to log something just
             * in case.
             */
            statusHandler.handle(Priority.WARN,
                    "ISC send status got into an invalid state trying to change state to "
                            + newState, e);
        }
    }
}
