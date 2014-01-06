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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import jep.JepException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.request.GetIscSendStatusRequest.IscSendStatus;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.internal.DataMgrInitStatus;
import com.raytheon.viz.gfe.core.internal.GFEParmCacheInitJob;
import com.raytheon.viz.gfe.core.internal.GFETopoManager;
import com.raytheon.viz.gfe.core.internal.IFPClient;
import com.raytheon.viz.gfe.core.internal.NotificationRouter;
import com.raytheon.viz.gfe.core.internal.ParmManager;
import com.raytheon.viz.gfe.core.internal.ReferenceSetManager;
import com.raytheon.viz.gfe.core.internal.WEGroupManager;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.parm.ParmOp;
import com.raytheon.viz.gfe.gridmanager.IGridManager;
import com.raytheon.viz.gfe.itool.IToolController;
import com.raytheon.viz.gfe.itool.IToolFactory;
import com.raytheon.viz.gfe.jobs.AutoSaveJob;
import com.raytheon.viz.gfe.procedures.ProcedureFactory;
import com.raytheon.viz.gfe.procedures.ProcedureJobPool;
import com.raytheon.viz.gfe.procedures.ProcedureUIController;
import com.raytheon.viz.gfe.smarttool.EditActionProcessor;
import com.raytheon.viz.gfe.smarttool.GridCycler;
import com.raytheon.viz.gfe.smarttool.script.SmartToolFactory;
import com.raytheon.viz.gfe.smarttool.script.SmartToolJobPool;
import com.raytheon.viz.gfe.smarttool.script.SmartToolUIController;
import com.raytheon.viz.gfe.textformatter.TextProductManager;

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
 * 12/09/2013    2367      dgilling    Instantiate ProcedureJobPool here.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DataManager {

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

    private SmartToolUIController smartToolInterface;

    private ProcedureUIController procedureInterface;

    private TextProductManager textProductMgr;

    private IToolController itoolInterface;

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

    private HashSet<String> knownOfficeTypes;

    private List<String> allSites;

    private final ProcedureJobPool procJobPool;

    private final SmartToolJobPool toolJobPool;

    public IISCDataAccess getIscDataAccess() {
        return iscDataAccess;
    }

    /**
     * Constructs a DataManager, passes in factory for creation of
     * {@link ISpatialDisplayManager} for this instance.
     * 
     * @param factory
     * @param discriminator
     * @throws GFEServerException
     */
    DataManager(DataManagerFactory factory, Object discriminator)
            throws GFEServerException {
        this.spatialDisplayManager = factory.createSpatialDisplayManager(this,
                discriminator);
        this.client = new IFPClient(VizApp.getWsId(), this);
        this.router = new NotificationRouter(this.getSiteID());

        this.parmManager = new ParmManager(this);
        GFEParmCacheInitJob cacheJob = new GFEParmCacheInitJob(this.parmManager);
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

        initializeScriptControllers();
        procJobPool = new ProcedureJobPool(4, 4, this);
        toolJobPool = new SmartToolJobPool(3, 3, this);

        this.weGroupManager = new WEGroupManager(this);
        this.editActionProcessor = new EditActionProcessor(this);

        // get office type information, convert to Dictionary
        this.allSites = client.getKnownSites();
        List<String> allOT = client.getKnownOfficeTypes();
        this.officeTypeDict = new HashMap<String, String>();
        for (int i = 0; i < allSites.size(); i++) {
            officeTypeDict.put(allSites.get(i), allOT.get(i));
        }
        this.officeType = officeTypeDict.get(this.siteId);

        // determine all known office types
        this.knownOfficeTypes = new HashSet<String>(allOT);

        ISCInitJob iscInitJob = new ISCInitJob(this);
        iscInitJob.setSystem(true);
        iscInitJob.schedule();

        // get the ISC states
        IscSendStatus iscSendStatus = client.getIscSendStatus();
        this.sendISConSave = iscSendStatus.isSendISConSave();
        this.sendISConPublish = iscSendStatus.isSendISConPublish();
        this.requestISC = iscSendStatus.isRequestISC();

        // set the ISC send state, which initially sends the message
        new ISCSendStatusChangedMsg(false).send(); // initial state
        if (CAVEMode.getMode().equals(CAVEMode.OPERATIONAL)) {
            enableISCsend(true);
        }
        // this.queryString = "siteID='" + this.getSiteID() + "'";
        NotificationManagerJob.addObserver("edex.alerts.gfe", this.router);
        this.router.start();

        this.parmOp = new ParmOp(this);
        this.topoManager = new GFETopoManager(this);

        this.autoSaveJob = new AutoSaveJob(this);

        new ParmEvictor();
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
        selectTimeRangeManager.dispose();
        refManager.dispose();
        parmManager.dispose();
        autoSaveJob.dispose();
        autoSaveJob = null;

        if (smartToolInterface != null) {
            smartToolInterface.dispose();
        }

        if (procedureInterface != null) {
            procedureInterface.dispose();
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
            try {
                siteId = getClient().getSiteID().get(0);
            } catch (GFEServerException e) {
                statusHandler.handle(Priority.PROBLEM, "Unable to get site ID",
                        e);
            }
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
     * Return the inventory for a parm
     * 
     * @param parmID
     * @return inventory for the parm
     * 
     * @throws GFEServerException
     */
    public List<TimeRange> serverParmInventory(final ParmID parmID)
            throws GFEServerException {
        return this.client.getGridInventory(parmID);
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

            this.schedule(1000L * PARM_EVICTOR_SCHEDULE);
            return Status.OK_STATUS;
        }
    }

    public IGridManager getGridManager() {
        return gridManager;
    }

    public void setGridManager(IGridManager gridManager) {
        this.gridManager = gridManager;
    }

    private void initializeScriptControllers() {
        // it would be really nice to be able to spawn the construction of these
        // two heavy objects into another thread. Unfortunately, Jep requires
        // creation and all subsequent access to happen on the same thread. So
        // we need to make use of runSync() here. It would be even be acceptable
        // if we could make this a UIJob; unfortunately the thread most often
        // used to create DataManager is the UI thread at perspective open, so
        // we can't block and wait on the UI thread for a job that
        // requires the UI thread to run.
        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                try {
                    DataManager.this.procedureInterface = ProcedureFactory
                            .buildUIController(DataManager.this);
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error initializing procedure interface", e);
                }

                try {
                    DataManager.this.smartToolInterface = SmartToolFactory
                            .buildUIController(DataManager.this);
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error initializing smart tool interface", e);
                }

                DataManager.this.textProductMgr = new TextProductManager();
            }
        });
    }

    public SmartToolUIController getSmartToolInterface() {
        return smartToolInterface;
    }

    public ProcedureUIController getProcedureInterface() {
        return procedureInterface;
    }

    public IToolController getIToolInterface() {
        if (itoolInterface == null) {
            try {
                this.itoolInterface = IToolFactory.buildController(this);
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error initializing itool interface", e);
            }
        }
        return itoolInterface;
    }

    /**
     * @return A List of Objects, the active table.
     * @throws VizException
     */
    public List<ActiveTableRecord> getActiveTable() throws VizException {
        List<ActiveTableRecord> result = getClient().getVTECActiveTable(siteId);
        return result;
    }

    @SuppressWarnings("unchecked")
    public Object[] doIscRequestQuery() {
        String xml = null;
        List<String> parmsWanted = new ArrayList<String>();
        Map<String, Map<String, List<Map<String, String>>>> domainDict = null;
        Map<String, String> serverDictS2T = null;
        Map<String, Map<String, String>> serverDictT2S = null;

        try {
            ServerResponse<List<Object>> sResponse = client.iscRequestQuery();
            if (!sResponse.getMessages().isEmpty()) {
                return null;
            }
            List<Object> response = sResponse.getPayload();
            xml = (String) response.get(0);
            List<String> parmList = (List<String>) response.get(2);
            for (String p : parmList) {
                parmsWanted.add(p.replace("_SFC", ""));
            }

            IPathManager pathMgr = PathManagerFactory.getPathManager();
            PythonScript script = new PythonScript(pathMgr
                    .getLocalizationFile(
                            pathMgr.getContext(LocalizationType.COMMON_STATIC,
                                    LocalizationLevel.BASE),
                            "isc" + File.separator + "requestScript.py")
                    .getFile().getPath(), PyUtil.buildJepIncludePath(pathMgr
                    .getFile(
                            pathMgr.getContext(LocalizationType.COMMON_STATIC,
                                    LocalizationLevel.BASE), "python")
                    .getPath()));
            Map<String, Object> args = new HashMap<String, Object>();
            args.put("str", response.get(1));
            Map<String, ?> obj = (Map<String, ?>) script.execute("unPickle",
                    args);
            domainDict = (Map<String, Map<String, List<Map<String, String>>>>) obj
                    .get("domains");
            serverDictS2T = (Map<String, String>) obj.get("serverDictS2T");
            serverDictT2S = (Map<String, Map<String, String>>) obj
                    .get("serverDictT2S");

        } catch (GFEServerException e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Server Problem: Unable to get server info from iscRequestQuery",
                            e);
        } catch (JepException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error getting domain dictionary!", e);
        }

        return new Object[] { xml, parmsWanted, domainDict, serverDictS2T,
                serverDictT2S };
    }

    public void makeISCRequest(String xmlreq) throws GFEServerException {
        client.iscMakeISCRequest(xmlreq);
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
     */
    public void enableISCsend(boolean state) {
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
}
