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
 *
 * Date          Ticket#     Engineer     Description
 * ------------- ----------- ------------ --------------------------
 * Mar 12, 2008              chammack     Initial Creation.
 * Feb 15, 2013  1507        dgilling     Force procedureInterface and
 *                                        smartToolInterface to be initialized
 *                                        by constructor.
 * Apr 24, 2013  1936        dgilling     Move initialization of TextProductMgr
 *                                        to GFE startup.
 * Aug 27, 2013  2302        randerso     Code cleanup for AutoSaveJob
 * Sep 05, 2013  2307        dgilling     Use better PythonScript constructor.
 * Sep 16, 2013  2033        dgilling     Remove unused IToolController.
 * Dec 09, 2013  2367        dgilling     Instantiate ProcedureJobPool here.
 * May 22, 2014  3110        randerso     Attach router to edex.alerts.gfe
 *                                        earlier
 * Sep 09, 2014  3592        randerso     Added call to
 *                                        SampleSetManager.dispose()
 * Oct 30, 2014  3775        randerso     Added parmCacheInit to initStatus
 * Apr 20, 2015  4027        randerso     Let TextProductManager know we are not
 *                                        running in a GUI
 * Jul 23, 2015  4263        dgilling     Refactor to support initialization of
 *                                        script controllers off main thread.
 * Aug 13, 2015  4749        njensen      Improved dispose(), parmEvictor can
 *                                        cancel
 * Aug 14, 2015  4750        dgilling     Remove use of PythonScript in
 *                                        doIscRequestQuery.
 * Aug 20, 2015  4749        dgilling     Ensure TextProductManager is disposed
 *                                        on dispose.
 * Sep 15, 2015  4858        dgilling     Disable ISC when DRT mode is enabled.
 * Nov 18, 2015  5129        dgilling     Support new IFPClient.
 * Mar 16, 2017  6092        randerso     Dispose spatialDisplayManager when
 *                                        disposed
 * Apr 21, 2017  6239        randerso     Code cleanup
 * Jan 04, 2018  7178        randerso     Removed ParmEvictor
 * Jan 08, 2018  19900       ryu          Fix CAVE crash when starting GFE for non-activated site.
 * Mar 07, 2018  6608        randerso     Remove killPoolsThread so CAVE window
 *                                        doesn't close with jobs still running
 *                                        in the background
 * Feb 18, 2020  74905       tjensen      Added constructor that takes a siteID
 *
 * </pre>
 *
 * @author chammack
 */

public class DataManager implements ISimulatedTimeChangeListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataManager.class);

    /**
     * Use {@link DataManagerUIFactory#getCurrentInstance()}
     */
    @Deprecated
    public static DataManager getCurrentInstance() {
        return DataManagerUIFactory.getCurrentInstance();
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

    private final EditActionProcessor editActionProcessor;

    private String siteId;

    private String officeType;

    private boolean iscSendState;

    private boolean requestISC = true;

    private boolean sendISConSave;

    private boolean sendISConPublish;

    private final DataMgrInitStatus initStatus = new DataMgrInitStatus();

    private Map<String, String> officeTypeDict;

    private final Set<String> knownOfficeTypes;

    private final List<String> allSites;

    private final ProcedureJobPool procJobPool;

    private final SmartToolJobPool toolJobPool;

    private boolean previousIscSendState;

    private final AtomicBoolean smartToolsInitialized;

    private final AtomicBoolean proceduresInitialized;

    private final AtomicBoolean textProductsInitialized;

    /**
     * ISCDataAccess
     *
     * @return ISCDataAccess
     */
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
        this(factory, discriminator,
                LocalizationManager.getInstance().getSite());
    }

    /**
     * Constructs a DataManager, passes in factory for creation of
     * {@link ISpatialDisplayManager} for this instance.
     *
     * @param factory
     * @param discriminator
     *            used as key for this instance of DataManager in the factory's
     *            instance map. Normally this is the window GFE is running in.
     * @param siteID
     * @throws GFEServerException
     */
    DataManager(DataManagerFactory factory, Object discriminator, String siteID)
            throws GFEServerException {
        this.spatialDisplayManager = factory.createSpatialDisplayManager(this,
                discriminator);
        this.client = new IFPClient(VizApp.getWsId(), siteID);
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
        if (sr.getPayload() != null) {
            this.officeTypeDict = Collections.unmodifiableMap(sr.getPayload());
        } else {
            this.officeTypeDict = Collections
                    .unmodifiableMap(Collections.emptyMap());
        }
        this.allSites = Collections.unmodifiableList(
                new ArrayList<>(this.officeTypeDict.keySet()));
        this.officeType = officeTypeDict.get(this.siteId);

        // determine all known office types
        this.knownOfficeTypes = Collections
                .unmodifiableSet(new HashSet<>(this.officeTypeDict.values()));

        ISCInitJob iscInitJob = new ISCInitJob(this);
        iscInitJob.setSystem(true);
        iscInitJob.schedule();

        // get the ISC states
        ServerResponse<IscSendStatus> sr2 = client.iscSendStatus();
        IscSendStatus iscSendStatus = sr2.getPayload();
        if (iscSendStatus != null) {
            this.sendISConSave = iscSendStatus.isSendISConSave();
            this.sendISConPublish = iscSendStatus.isSendISConPublish();
            this.requestISC = iscSendStatus.isRequestISC();
        }

        // set the ISC send state, which initially sends the message
        new ISCSendStatusChangedMsg(false).send();
        if ((CAVEMode.OPERATIONAL.equals(CAVEMode.getMode())
                && (SimulatedTime.getSystemTime().isRealTime()))
                || (SimulatedTimeOperations
                        .isTransmitAllowedinSimulatedTime())) {
            try {
                enableISCsend(true);
            } catch (SimulatedTimeProhibitedOpException e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }
        this.router.start();

        this.parmOp = new ParmOp(this);
        this.topoManager = new GFETopoManager(this);

        this.autoSaveJob = new AutoSaveJob(this);

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
        spatialDisplayManager.dispose();
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

        if (toolJobPool != null) {
            toolJobPool.cancel();
        }

        if (procJobPool != null) {
            procJobPool.cancel();
        }

        if (CAVEMode.getMode() == CAVEMode.OPERATIONAL) {
            SimulatedTime.getSystemTime()
                    .removeSimulatedTimeChangeListener(this);
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

    /**
     * @return the topo manager
     */
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
     * @return the GridManager
     */
    public IGridManager getGridManager() {
        return gridManager;
    }

    /**
     * Set the GridManager
     *
     * @param gridManager
     */
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
        while (!(smartToolsInitialized.get() && proceduresInitialized.get()
                && textProductsInitialized.get())) {
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

    /**
     * @return the SmartToolInterface
     */
    public SmartToolMetadataManager getSmartToolInterface() {
        return smartToolInterface;
    }

    /**
     * @return the ProcedureInterface
     */
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
        ActiveTableMode tableName = (CAVEMode.getMode() == CAVEMode.PRACTICE)
                ? ActiveTableMode.PRACTICE : ActiveTableMode.OPERATIONAL;
        ServerResponse<List<ActiveTableRecord>> sr = getClient()
                .getVTECActiveTable(tableName);
        if (!sr.isOkay()) {
            statusHandler
                    .error("Unable to obtain vtecActiveTable: " + sr.message());
        }
        return sr.getPayload();
    }

    /**
     * Queries the ISC routing table for the known ISC sites. Returns data about
     * the servers (mhsid, server, port, protocol, welist, and other
     * information).
     *
     * @return the IscQueryResponse
     */
    public IscQueryResponse doIscRequestQuery() {
        ServerResponse<IscQueryResponse> sr = client.iscRequestQuery();
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Unable to get server info from iscRequestQuery: %s",
                    sr.message()));
            return new IscQueryResponse();
        }

        Collection<String> parms = sr.getPayload().getRequestedParms();
        Collection<String> parmsWanted = new ArrayList<>(parms.size());
        for (String parm : parms) {
            parmsWanted.add(parm.replace("_SFC", StringUtils.EMPTY));
        }

        return new IscQueryResponse(sr.getPayload().getDomainDict(),
                sr.getPayload().getServerDictT2S(), parmsWanted);
    }

    /**
     * Make an ISC request
     *
     * @param xmlRequest
     */
    public void makeISCRequest(String xmlRequest) {
        ServerResponse<?> sr = client.iscRequestMake(xmlRequest);
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Unable to process iscRequestMake: %s", sr.message()));
        }
    }

    /**
     * Return the officeType (e.g. wfo, nc, rfc) for a particular site
     *
     * @param site
     * @return the office type
     */
    public String officeType(String site) {
        return this.officeTypeDict.get(site);
    }

    /**
     * @return the list of known office types
     */
    public List<String> knownOfficeTypes() {
        return new ArrayList<>(this.knownOfficeTypes);
    }

    /**
     * @return the list of known sites
     */
    public List<String> knownSites() {
        return this.allSites;
    }

    /**
     * @return the office type for this site
     */
    public String getOfficeType() {
        return officeType;
    }

    /**
     * Set the office type for this site
     *
     * @param officeType
     */
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
     * @throws SimulatedTimeProhibitedOpException
     */
    public void enableISCsend(boolean state)
            throws SimulatedTimeProhibitedOpException {
        if (state && !SimulatedTimeOperations.isTransmitAllowed()) {
            throw SimulatedTimeOperations
                    .constructProhibitedOpException("ISC Send");
        }

        if (state == iscSendState) {
            // do nothing
            return;
        }

        if (!CAVEMode.OPERATIONAL.equals(CAVEMode.getMode()) || !requestISC()) {
            /*
             * always disable if not operational or requestISC off
             */
            state = false;

        }

        // as desired
        iscSendState = state;

        // send out a message to interested parties (both for success and fail)
        new ISCSendStatusChangedMsg(state).send();
    }

    /**
     * @return the EditActionProcessor
     */
    public EditActionProcessor getEditActionProcessor() {
        return this.editActionProcessor;
    }

    /**
     * @return the DataMgrInitStatus
     */
    public DataMgrInitStatus getInitStatus() {
        return initStatus;
    }

    /**
     * @return the TextProductManager
     */
    public TextProductManager getTextProductMgr() {
        return textProductMgr;
    }

    /**
     * @return the ProcedureJobPool
     */
    public ProcedureJobPool getProcedureJobPool() {
        return procJobPool;
    }

    /**
     * @return the SmartToolJobPool
     */
    public SmartToolJobPool getSmartToolJobPool() {
        return toolJobPool;
    }

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
                            + newState,
                    e);
        }
    }
}
