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
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import jep.JepException;

import org.apache.commons.lang.Validate;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

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
import com.raytheon.viz.gfe.core.internal.GFESpatialDisplayManager;
import com.raytheon.viz.gfe.core.internal.GFETopoManager;
import com.raytheon.viz.gfe.core.internal.IFPClient;
import com.raytheon.viz.gfe.core.internal.NotificationRouter;
import com.raytheon.viz.gfe.core.internal.OffscreenSpatialDisplayManager;
import com.raytheon.viz.gfe.core.internal.ParmManager;
import com.raytheon.viz.gfe.core.internal.ReferenceSetManager;
import com.raytheon.viz.gfe.core.internal.WEGroupManager;
import com.raytheon.viz.gfe.core.msgs.ICurrentDataManagerChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISCSendStatusChangedMsg;
import com.raytheon.viz.gfe.core.parm.ParmOp;
import com.raytheon.viz.gfe.gridmanager.IGridManager;
import com.raytheon.viz.gfe.itool.IToolController;
import com.raytheon.viz.gfe.itool.IToolFactory;
import com.raytheon.viz.gfe.jobs.AutoSaveJob;
import com.raytheon.viz.gfe.procedures.ProcedureFactory;
import com.raytheon.viz.gfe.procedures.ProcedureUIController;
import com.raytheon.viz.gfe.smarttool.EditActionProcessor;
import com.raytheon.viz.gfe.smarttool.GridCycler;
import com.raytheon.viz.gfe.smarttool.script.SmartToolFactory;
import com.raytheon.viz.gfe.smarttool.script.SmartToolUIController;
import com.raytheon.viz.ui.VizWorkbenchManager;

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
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataManager.class);

    private static class DataManagerKey {
        private final IWorkbenchWindow window;

        private DataManagerKey(IWorkbenchWindow window) {
            this.window = window;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return (window == null ? 0 : window.hashCode());
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (obj instanceof DataManagerKey) {
                IWorkbenchWindow otherWindow = ((DataManagerKey) obj).window;
                if (window == null) {
                    return otherWindow == null;
                }
                return window.equals(otherWindow);
            }
            return false;
        }
    }

    private static Map<DataManagerKey, DataManager> instanceMap = new ConcurrentHashMap<DataManagerKey, DataManager>();;

    private static Set<ICurrentDataManagerChangedListener> listeners;

    private static IWindowListener windowListener;

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

    /**
     * Not to be instantiated directly.
     * 
     * Use getCurrentInstance() or getInstance(IWorkbenchWindow) instead.
     * 
     * @param window
     * @throws GFEServerException
     */
    private DataManager(IWorkbenchWindow window) throws GFEServerException {
        if (window != null) {
            // When window is null, this is assumed to be a JUnit test case or
            // the GFEClient

            // This is a separate if statement from below due to ordering issues
            // which prevent putting them in the same if block.

            this.spatialDisplayManager = new GFESpatialDisplayManager(window,
                    this);
        } else {
            this.spatialDisplayManager = new OffscreenSpatialDisplayManager(
                    window, this);
        }

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
        // this.selectTimeRangeManager = new SelectTimeRangeManager(this);
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

    public IISCDataAccess getIscDataAccess() {
        return iscDataAccess;
    }

    /**
     * Default constructor to subclass for test. Intentionally left
     * package-private for safety.
     * 
     * @throws GFEServerException
     */
    DataManager() throws GFEServerException {
        this(null);
    }

    /**
     * Get the DataManager associated with the currently active workbench
     * window.
     * 
     * If no workbench window is active, or the GFE perspective has not been
     * activated in the active window, this method will return null.
     * 
     * @return the active DataManager
     */
    public static DataManager getCurrentInstance() {
        final IWorkbenchWindow[] window = new IWorkbenchWindow[1];

        window[0] = VizWorkbenchManager.getInstance().getCurrentWindow();

        if (window[0] == null && PlatformUI.isWorkbenchRunning()) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    window[0] = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow();
                }

            });
        }
        if (window[0] != null) {
            return findInstance(window[0]);
        } else {
            return findInstance(null);
        }
    }

    /**
     * Add a change listener to subscribe to a change of data managers
     * 
     * 
     * @param listener
     *            the listener to add
     * 
     */
    public static void addChangeListener(
            ICurrentDataManagerChangedListener listener) {
        Validate.notNull(listener, "Attempting to add null listener");
        synchronized (DataManager.class) {
            if (listeners == null) {
                listeners = new HashSet<ICurrentDataManagerChangedListener>();
            }
        }
        listeners.add(listener);
    }

    /**
     * Internal Method that is fired to indicate that the datamanager has
     * changed
     */
    public static void fireChangeListener() {
        DataManager dm = null;

        dm = getCurrentInstance();

        // kludge to get combine mode menu item updated
        ICommandService service = (ICommandService) PlatformUI.getWorkbench()
                .getService(ICommandService.class);

        service.refreshElements("com.raytheon.viz.gfe.actions.setCombineMode",
                null);

        if (dm != null && listeners != null) {
            for (ICurrentDataManagerChangedListener listener : listeners) {
                listener.currentDataManagerChanged(dm);
            }
        }
    }

    /**
     * Find the DataManager associated with a specific workbench window
     * 
     * If it does not exist, return null.
     * 
     * @param window
     *            the workbench window
     * @return the DataManager associated with the specified window
     */
    public static DataManager findInstance(IWorkbenchWindow window) {
        return instanceMap.get(new DataManagerKey(window));
    }

    /**
     * Get the DataManager associated with a specific workbench window
     * 
     * If it does not exist, create one.
     * 
     * @param window
     *            the window the DataManager is associated with
     * @return a DataManager associated to a specific window
     */
    public static DataManager getInstance(IWorkbenchWindow window) {
        if (window != null && windowListener == null) {
            windowListener = new IWindowListener() {

                @Override
                public void windowActivated(IWorkbenchWindow window) {
                    // do nothing
                }

                @Override
                public void windowClosed(IWorkbenchWindow window) {
                    DataManager.dispose(window);
                }

                @Override
                public void windowDeactivated(IWorkbenchWindow window) {
                    // do nothing
                }

                @Override
                public void windowOpened(IWorkbenchWindow window) {
                    // do nothing
                }

            };
            PlatformUI.getWorkbench().addWindowListener(windowListener);
        }
        DataManager dm = findInstance(window);
        if (dm == null) {
            synchronized (instanceMap) {
                // verify it hasn't been loaded by another
                dm = findInstance(window);
                if (dm == null) {
                    try {
                        dm = new DataManager(window);
                        instanceMap.put(new DataManagerKey(window), dm);
                        waitForJobs(dm);
                        if (window != null) {
                            postInitialize(dm);
                        }
                    } catch (GFEServerException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        }
        return dm;
    }

    /**
     * Check to see if any DataManagers are active
     * 
     * @return true if any DataManagers are active
     */
    public static boolean anyActive() {
        return !instanceMap.isEmpty();
    }

    /**
     * Start all auto save jobs
     */
    public static void enableAutoSave() {
        for (DataManager dm : instanceMap.values()) {
            dm.autoSaveJob.cancel();
            dm.autoSaveJob.reSchedule();
        }
    }

    /**
     * Stop all auto save jobs
     */
    public static void disableAutoSave() {
        for (DataManager dm : instanceMap.values()) {
            dm.autoSaveJob.cancel();
        }
    }

    /**
     * Dispose an instance of the DataManager associated with a particular
     * window
     * 
     * @param window
     */
    public static void dispose(IWorkbenchWindow window) {
        DataManager dm = instanceMap.remove(new DataManagerKey(window));

        if (dm != null) {
            dm.dispose();
        }
    }

    private void dispose() {
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

    private static void postInitialize(DataManager dataManager) {
        // Tell ParmManager which parms to load initially
        IParmManager parmMgr = dataManager.getParmManager();
        ParmID[] parmIDs = dataManager.getWEGroupManager().getParmIDs(
                dataManager.getWEGroupManager().getDefaultGroup(),
                parmMgr.getAllAvailableParms());

        parmMgr.setDisplayedParms(parmIDs);

    }

    /**
     * @param dataManager
     */
    protected static void waitForJobs(DataManager dataManager) {
        // wait to ensure init jobs are done
        long waitForJobs = 0;
        while (!dataManager.getInitStatus().isDone()) {
            try {
                waitForJobs += 10;
                Thread.sleep(10);
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        System.out.println("Waiting on Jobs: " + waitForJobs);

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

    public SmartToolUIController getSmartToolInterface() {
        if (smartToolInterface == null) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    try {
                        DataManager.this.smartToolInterface = SmartToolFactory
                                .buildUIController(DataManager.this);
                    } catch (JepException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error initializing smart tool interface", e);
                    }
                }
            });
        }
        return smartToolInterface;
    }

    public ProcedureUIController getProcedureInterface() {
        if (procedureInterface == null) {
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
                }
            });
        }
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

}
