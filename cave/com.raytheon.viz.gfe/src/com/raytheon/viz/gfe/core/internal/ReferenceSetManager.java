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

import java.awt.Point;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DBit;
import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;
import com.raytheon.uf.common.dataplugin.gfe.reference.GroupID;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.RefType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.FileUpdatedMessage.FileChangeType;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.protectedfiles.ProtectedFileLookup;
import com.raytheon.uf.common.python.concurrent.IPythonExecutor;
import com.raytheon.uf.common.python.concurrent.IPythonJobListener;
import com.raytheon.uf.common.python.concurrent.PythonInterpreterFactory;
import com.raytheon.uf.common.python.concurrent.PythonJobCoordinator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.msgs.GridDataChangedMsg;
import com.raytheon.viz.gfe.core.msgs.IEditAreaGroupInvChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetInvChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.query.QueryScript;
import com.raytheon.viz.gfe.query.QueryScriptExecutor;
import com.raytheon.viz.gfe.query.QueryScriptFactory;
import com.raytheon.viz.gfe.query.QueryScriptRecurseExecutor;
import org.locationtech.jts.geom.Envelope;

/**
 * The ReferenceSetMgr keeps track of the activeRefSet and interfaces to the
 * server to save, delete, and load Reference sets. It keeps track of the
 * availableRefSets and sends notification of changes to the inventory.
 *
 * Requests are made to the ReferenceSetMgr to change, clear, and toggle the
 * reference set.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 01, 2008  1053     randerso  Initial creation
 * Feb 14, 2013  1506     mnash     Move QueryScript to use new Python
 *                                  concurrency implementation
 * Feb 12, 2013  1597     randerso  Improved error message for exceptions
 *                                  evaluating queries
 * Feb 26, 2013  1708     randerso  Removed no longer needed near duplicate
 *                                  methods
 * Jun 21, 2013  14983    ryu       Added method for synchronous evaluation of
 *                                  query.
 * Aug 06, 2013  1561     njensen   Use pm.listFiles() instead of
 *                                  pm.listStaticFiles()
 * Sep 30, 2013  2361     njensen   Use JAXBManager for XML
 * Sep 08, 2014  3592     randerso  Changed to use new pm listStaticFiles()
 * Sep 29, 2014  2975     njensen   Only lookup COMMON_STATIC for editAreas
 * Aug 13, 2015  4749     njensen   Shut down coordinator on dispose()
 * Aug 26, 2015  4807     randerso  Change refDataCache so it will release
 *                                  unused edit areas Clean up deprecations and
 *                                  old style logging.
 * Aug 27, 2015  4947     njensen   Fixed removeReferenceSetIDChangedListener()
 * Nov 18, 2015  5129     dgilling  Use new IFPClient for get/save/delete of
 *                                  reference data.
 * Dec 14, 2015  4816     dgilling  Support refactored PythonJobCoordinator API.
 * Jun 30, 2016  5723     dgilling  Move safety check from saveRefSet to
 *                                  IFPClient.saveReferenceData.
 * Jun 14, 2017  6297     bsteffen  Make listeners thread safe.
 * Aug 07, 2017  6379     njensen   Use ProtectedFileLookup
 * Apr 27, 2018  7256     randerso  Make query evaluation errors debug log only.
 *
 * </pre>
 *
 * @author randerso
 */
public class ReferenceSetManager implements IReferenceSetManager,
        IMessageClient, ISpatialEditorTimeChangedListener {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReferenceSetManager.class);

    private static final String QUERY_THREAD_POOL_NAME = "gfequeryscript";

    private static final int NUM_QUERY_THREADS = 1;

    public static final String EDIT_AREAS_DIR = FileUtil.join("gfe",
            "editAreas");

    public static final String EDIT_AREA_GROUPS_DIR = FileUtil.join("gfe",
            "editAreaGroups");

    /**
     * Array of points containing the x and y increments for each of the 8
     * directions
     */
    private static final Point dirs[] = { new Point(0, 1), new Point(1, 1),
            new Point(1, 0), new Point(1, -1), new Point(0, -1),
            new Point(-1, -1), new Point(-1, 0), new Point(-1, 1) };

    private static final int STACK_LIMIT = 10;

    /** An empty reference set */
    private final ReferenceData EMPTY;

    /** A full reference set */
    private final ReferenceData FULL;

    /** Variable to keep track of Discrepancy Area id's */
    private static int areaIdNumber = 0;

    /** the active reference set */
    private ReferenceData activeRefSet;

    /** the previous reference set for Undo */
    private ReferenceData prevRefSet;

    /** the list of available reference sets */
    private List<ReferenceID> availableSets;

    private final Map<String, List<String>> groupMap;

    /** the DataManager instance */
    private final DataManager dataManager;

    /** a cache of initialized reference sets */
    private final Map<String, WeakReference<ReferenceData>> refDataCache;

    private final Set<IReferenceSetInvChangedListener> refSetInvChangedListeners;

    private final Set<IReferenceSetChangedListener> refSetChangedListeners;

    private final Set<IReferenceSetIDChangedListener> refSetChangedIDListeners;

    private final Set<IEditAreaGroupInvChangedListener> editAreaGroupInvChangedListeners;

    private RefSetMode mode = RefSetMode.REPLACE;

    private QuickSetMode quickSetMode = QuickSetMode.RESTORE;

    private ILocalizationFileObserver editAreaObserver;

    private ILocalizationFileObserver editAreaGroupObserver;

    private LocalizationFile editAreaDir;

    private LocalizationFile editAreaGroupDir;

    private List<GroupID> groupIdList;

    private final ArrayList<String> historyStack;

    private PythonJobCoordinator<QueryScript> coordinator;

    /**
     * Set the wait cursor on or off
     *
     * @param state
     *            0 = wait cursor on, 1 = wait cursor off
     */
    private void setCursor(final int state) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (PlatformUI.isWorkbenchRunning()) {
                    IWorkbenchWindow window = PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow();
                    if (window != null) {
                        Shell shell = window.getShell();

                        if (state == 0) {
                            shell.setCursor(shell.getDisplay()
                                    .getSystemCursor(SWT.CURSOR_WAIT));
                        } else {
                            shell.setCursor(shell.getDisplay()
                                    .getSystemCursor(SWT.CURSOR_ARROW));
                        }
                    }
                }
            }

        });
    }

    /**
     * Gets and updates the reference set inventory from the server.
     *
     * Uses net() to get a pointer to the reference network (referenceNet). Asks
     * the reference server for the inventory. Stores the inventory and sends a
     * RefSetInventoryChanged message if the inventory has changed.
     *
     */
    private void getInventory() {
        // load the complete list of edit areas
        List<ReferenceID> refIDs;
        ServerResponse<List<ReferenceID>> sr = dataManager.getClient()
                .getReferenceInventory();
        if (sr.isOkay()) {
            refIDs = sr.getPayload();
        } else {
            refIDs = Collections.emptyList();
            statusHandler.error(String.format(
                    "Unable to update inventory from IFPServer: %s",
                    sr.message()));
        }

        // load the edit area group lists
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile[] groupFiles = pm.listStaticFiles(
                LocalizationType.COMMON_STATIC, EDIT_AREA_GROUPS_DIR,
                new String[] { ".txt" }, false, true);
        if (groupFiles != null) {
            for (LocalizationFile lf : groupFiles) {
                loadGroup(lf);
            }
        }

        // update the available sets and send notifications
        if (!refIDs.equals(availableSets)) {
            List<ReferenceID> additions = new ArrayList<>(refIDs);
            additions.removeAll(availableSets);

            List<ReferenceID> deletions = new ArrayList<>(availableSets);
            deletions.removeAll(refIDs);

            availableSets = refIDs;

            sendReferenceSetInvChanged(getAvailableSets(), additions, deletions,
                    new ArrayList<ReferenceID>());
        }
        checkGroupConsistency();
    }

    private void loadGroup(LocalizationFile lf) {
        String groupName = LocalizationUtil.extractName(lf.getPath())
                .replace(".txt", "");
        GroupID group = new GroupID(groupName,
                ProtectedFileLookup.isProtected(lf),
                lf.getContext().getLocalizationLevel());
        if ("Misc".equals(group)) {
            statusHandler.handle(Priority.EVENTA,
                    "\"Misc\" is a reserved group name. Please rename or delete this group file: "
                            + lf);
            return;
        }

        boolean found = false;
        for (int i = 0; i < groupIdList.size(); i++) {
            if (groupIdList.get(i).getName().equals(group.getName())) {
                groupIdList.set(i, group);
                found = true;
                break;
            }
        }
        if (!found) {
            groupIdList.add(group);
        }

        List<String> groupList = groupMap.get(group.getName());
        if (groupList == null) {
            groupList = new ArrayList<>();
            groupMap.put(group.getName(), groupList);
        }
        groupList.clear();

        try (BufferedReader in = new BufferedReader(
                new InputStreamReader(lf.openInputStream()))) {
            String s = in.readLine();
            int count = Integer.parseInt(s);
            for (int i = 0; i < count; i++) {
                String area = in.readLine();
                groupList.add(area);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading group file: " + lf, e);
        }
    }

    private void sendReferenceSetInvChanged(List<ReferenceID> inventory,
            List<ReferenceID> additions, List<ReferenceID> deletions,
            List<ReferenceID> changes) {
        for (IReferenceSetInvChangedListener listener : refSetInvChangedListeners) {
            listener.referenceSetInvChanged(inventory, additions, deletions,
                    changes);
        }
    }

    @Override
    public List<String> getGroupInventory() {
        // Return the list of Edit Area groups

        List<String> groupInv = new ArrayList<>();
        for (String group : groupMap.keySet()) {
            groupInv.add(group);
        }

        Collections.sort(groupInv);
        return groupInv;
    }

    @Override
    public List<String> getGroupData(final String groupName) {
        if ("Misc".equals(groupName)) {
            return getMisc();
        }

        List<String> groupList = groupMap.get(groupName);
        List<String> areas = new ArrayList<>();
        if (groupList != null) {
            areas.addAll(groupList);
            Collections.sort(areas);
        }
        return areas;
    }

    @Override
    public List<GroupID> getGroupIds() {
        return Collections.unmodifiableList(this.groupIdList);
    }

    /**
     * Returns list of areas which are not in any group
     *
     * @return
     */
    private List<String> getMisc() {
        // Return a list of areas that are not in any group

        // Make list of group lists
        List<List<String>> groupLists = new ArrayList<>();
        List<String> groupNames = getGroupInventory();
        for (String groupName : groupNames) {
            if (!"Misc".equals(groupName)) {
                groupLists.add(getGroupData(groupName));
            }
        }

        // Check each area to be in group
        List<String> areas = new ArrayList<>();
        for (ReferenceID id : getAvailableSets()) {
            String areaName = id.getName();
            boolean areaFound = false;
            for (List<String> groupList : groupLists) {
                if (groupList.contains(areaName)) {
                    areaFound = true;
                    break;
                }
            }
            if (!areaFound) {
                areas.add(areaName);
            }
        }
        Collections.sort(areas);
        return areas;
    }

    @Override
    public void saveGroup(String groupName, List<String> areaNames) {
        statusHandler.handle(Priority.VERBOSE,
                "Save Edit Area Group: " + groupName + " areas:" + areaNames);
        if (groupName.isEmpty() || "Misc".equals(groupName)) {
            statusHandler.handle(Priority.PROBLEM,
                    "GroupName \"" + groupName + "\" is an invalid name");
        }
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);

        LocalizationFile lf = pm.getLocalizationFile(ctx,
                FileUtil.join(EDIT_AREA_GROUPS_DIR, groupName + ".txt"));

        // write out the local file
        try (SaveableOutputStream lfOut = lf.openOutputStream();
                BufferedWriter out = new BufferedWriter(
                        new OutputStreamWriter(lfOut))) {
            out.write(Integer.toString(areaNames.size()));
            out.write('\n');
            for (String name : areaNames) {
                out.write(name);
                out.write('\n');
            }
            out.close();
            lfOut.save();
        } catch (IOException | LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving edit area group", e);
        }
    }

    @Override
    public void deleteGroup(String groupName) {
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext ctx = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.USER);

        LocalizationFile lf = pm.getLocalizationFile(ctx,
                FileUtil.join(EDIT_AREA_GROUPS_DIR, groupName + ".txt"));
        try {
            lf.delete();
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to delete edit area group " + groupName
                            + " from server.",
                    e);
        }
    }

    private void sendEditAreaGroupInvChanged() {
        for (IEditAreaGroupInvChangedListener listener : editAreaGroupInvChangedListeners) {
            listener.editAreaGroupInvChanged();
        }
    }

    private void checkGroupConsistency() {
        // Check that all areas listed within a group exist
        // If not, remove them from the group
        List<ReferenceID> availSets = getAvailableSets();
        List<String> setNames = new ArrayList<>();
        for (ReferenceID id : availSets) {
            setNames.add(id.getName());
        }
        // Get the group inventory
        Set<String> groupInv = this.groupMap.keySet();

        // Check that all areas in each group are in the edit
        // area inventory
        for (String group : groupInv) {
            List<String> areas = this.groupMap.get(group);
            boolean changedFlag = false;
            List<String> newAreas = new ArrayList<>();
            for (String area : areas) {
                if (setNames.contains(area)) {
                    newAreas.add(area);
                } else {
                    changedFlag = true;
                }
            }
            // Save the new group if different
            if (changedFlag) {
                saveGroup(group, newAreas);
            }
        }
    }

    /**
     * Command to update the reference data cache based on additions, deletions,
     * and changes.
     *
     * @param additions
     * @param deletions
     * @param changes
     */
    private void updateCache(final List<ReferenceID> additions,
            final List<ReferenceID> deletions,
            final List<ReferenceID> changes) {

        // remove any deletions and changes from the cache.
        for (ReferenceID del : deletions) {
            refDataCache.remove(del.getName());
        }
        for (ReferenceID chg : changes) {
            refDataCache.remove(chg.getName());
        }
        for (ReferenceID add : additions) {
            refDataCache.remove(add.getName());
        }
    }

    /**
     * Constructor for ReferenceSet taking a pointer to the Data Manager
     *
     * Stores the pointer in private data. Gets the inventory. *
     *
     * @param dataManager
     */
    @SuppressWarnings("unchecked")
    public ReferenceSetManager(DataManager dataManager) {
        // ready the PythonJobCoordinator
        PythonInterpreterFactory<QueryScript> factory = new QueryScriptFactory(
                dataManager);
        coordinator = new PythonJobCoordinator<>(NUM_QUERY_THREADS,
                QUERY_THREAD_POOL_NAME, factory);

        // MessageClient("ReferenceSetMgr", msgHandler);
        this.dataManager = dataManager;

        EMPTY = new ReferenceData(
                dataManager.getParmManager().compositeGridLocation(),
                new ReferenceID("empty"), null,
                ReferenceData.CoordinateType.GRID);
        EMPTY.getGrid();

        FULL = new ReferenceData(EMPTY);
        FULL.setId(new ReferenceID("full"));
        FULL.invert();

        // Initialize _activeRefSet to default ReferenceData
        activeRefSet = EMPTY;
        prevRefSet = EMPTY;

        refDataCache = new HashMap<>();

        refSetInvChangedListeners = new CopyOnWriteArraySet<>();

        refSetChangedListeners = new CopyOnWriteArraySet<>();

        refSetChangedIDListeners = new CopyOnWriteArraySet<>();

        editAreaGroupInvChangedListeners = new CopyOnWriteArraySet<>();

        // Get the inventory of Reference sets
        availableSets = new ArrayList<>();
        groupMap = new HashMap<>();
        groupIdList = new ArrayList<>();
        getInventory();

        editAreaObserver = new ILocalizationFileObserver() {

            @Override
            public void fileUpdated(FileUpdatedMessage message) {
                editAreaUpdated(message);
            }

        };

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        editAreaDir = pathMgr.getLocalizationFile(
                pathMgr.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE),
                EDIT_AREAS_DIR);
        editAreaDir.addFileUpdatedObserver(editAreaObserver);

        editAreaGroupObserver = new ILocalizationFileObserver() {

            @Override
            public void fileUpdated(FileUpdatedMessage message) {
                groupUpdated(message);
            }

        };

        editAreaGroupDir = pathMgr.getLocalizationFile(
                pathMgr.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE),
                EDIT_AREA_GROUPS_DIR);
        editAreaGroupDir.addFileUpdatedObserver(editAreaGroupObserver);

        historyStack = new ArrayList<>(STACK_LIMIT);

        this.dataManager.getSpatialDisplayManager()
                .addSpatialEditorTimeChangedListener(this);

        Message.registerInterest(this, GridDataChangedMsg.class);

        // initialize quickset tool tips
        if (PlatformUI.isWorkbenchRunning()) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    final ICommandService service = PlatformUI.getWorkbench()
                            .getService(ICommandService.class);

                    service.refreshElements(
                            "com.raytheon.viz.gfe.actions.quickSet", null);
                }
            });
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public void dispose() {
        editAreaDir.removeFileUpdatedObserver(editAreaObserver);
        editAreaGroupDir.removeFileUpdatedObserver(editAreaGroupObserver);
        this.dataManager.getSpatialDisplayManager()
                .removeSpatialEditorTimeChangedListener(this);
        Message.unregisterInterest(this, GridDataChangedMsg.class);
        coordinator.shutdown();
    }

    @Override
    public ReferenceData getActiveRefSet() {
        return new ReferenceData(activeRefSet);
    }

    @Override
    public void setActiveRefSet(final ReferenceData refData) {
        // force it to a grid
        refData.getGrid();

        // statusHandler.handle(Priority.VERBOSE, "PrevActiveSet = "
        // + activeRefSet.getId());
        // turn on wait cursor
        setCursor(0);

        // save the updated ones
        prevRefSet = activeRefSet;
        activeRefSet = refData;

        // if RefData is a none ReferenceData object, do the correct version
        if (activeRefSet.equals(new ReferenceData())) {
            activeRefSet = emptyRefSet();
        }

        // Find the domains that are affected by the change
        List<Envelope> domains = new ArrayList<>(
                prevRefSet.getDomains(CoordinateType.LATLON));
        domains.addAll(activeRefSet.getDomains(CoordinateType.LATLON));

        // Send notification of change
        for (IReferenceSetChangedListener listener : refSetChangedListeners) {
            listener.referenceSetChanged(activeRefSet, domains);
        }

        // turn off wait cursor
        setCursor(1);
        // statusHandler.handle(Priority.VERBOSE,
        // "NewActiveSet = " + activeRefSet.getId());
    }

    @Override
    public List<ReferenceID> getAvailableSets() {
        return Collections.unmodifiableList(availableSets);
    }

    @Override
    public ReferenceData emptyRefSet() {
        return EMPTY;
    }

    @Override
    public ReferenceData fullRefSet() {
        return FULL;
    }

    @Override
    public ReferenceData loadRefSet(final ReferenceID refSetID) {
        // statusHandler.debug("LoadRefSet: " + refSetID);

        // in cache?
        ReferenceData refData = null;
        WeakReference<ReferenceData> ref = refDataCache.get(refSetID.getName());
        if (ref != null) {
            refData = ref.get();
            if (refData != null) {
                return refData;
            }
        }

        // get it from ifpServer
        ServerResponse<ReferenceData> sr = dataManager.getClient()
                .getReferenceData(refSetID);
        if (sr.isOkay()) {
            refData = sr.getPayload();
        } else {
            statusHandler.error(String.format(
                    "Failure to get reference data [%s] from IFPServer: %s",
                    refSetID.getName(), sr.message()));
        }

        if (refData != null) {
            // Convert to AWIPS, and then produce a grid
            if (!refData.isQuery()) {
                refData.getGrid();
            }

            // add to cache
            refDataCache.put(refData.getId().getName(),
                    new WeakReference<>(refData));
        }
        return refData;
    }

    /**
     * Retrieves a list of ReferenceData corresponding to the referenceIDs
     *
     * @param need
     *            the referenceIDs
     * @return a List of ReferenceData
     */
    @Override
    public List<ReferenceData> getReferenceData(List<ReferenceID> need) {
        List<ReferenceData> refData = new ArrayList<>();
        for (int i = 0; i < need.size(); i++) {
            refData.add(loadRefSet(need.get(i)));
        }
        return refData;
    }

    @Override
    public boolean saveActiveRefSet(final ReferenceID refID) {
        statusHandler.handle(Priority.VERBOSE, "saveActiveRefSet req=" + refID);

        // Reset the reference data id to the new value
        activeRefSet.setId(refID);

        // let everyone know the id has changed
        sendReferenceSetIDChanged(refID);

        return saveRefSet(activeRefSet);
    }

    private void sendReferenceSetIDChanged(final ReferenceID refID) {
        for (IReferenceSetIDChangedListener listener : refSetChangedIDListeners) {
            listener.referenceSetIDChanged(refID);
        }
    }

    @Override
    public boolean saveRefSet(final ReferenceData orefData) {
        statusHandler.handle(Priority.VERBOSE,
                "SaveRefSet id=" + orefData.getId());

        ReferenceData refData = new ReferenceData(orefData);
        ServerResponse<?> sr = dataManager.getClient()
                .saveReferenceData(Arrays.asList(refData));
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "UNable to save ReferenceData: %s with IFPServer: %s",
                    refData.getId().getName(), sr.message()));
            return false;
        }

        // cache it temporarily
        refDataCache.put(refData.getId().getName(),
                new WeakReference<>(refData));

        return true;
    }

    @Override
    public boolean deleteRefSet(final ReferenceID refID) {
        ServerResponse<?> sr = dataManager.getClient()
                .deleteReferenceData(Arrays.asList(refID));
        if (!sr.isOkay()) {
            statusHandler.error(String.format(
                    "Unable to delete ReferenceData: %s with IFPServer: %s",
                    refID.getName(), sr.message()));
            return false;
        }

        return true;
    }

    @Override
    public void undoRefSet() {
        ReferenceData newRefSet = prevRefSet;
        setActiveRefSet(newRefSet);
    }

    /**
     * Input network reference set notification from NetworkMgr indicating that
     * the reference set inventory has changed or the contents of a reference
     * set has changed. The bypassActiveCheck bypasses the checking for whether
     * a change has occurred in the active ref set. This is only set for saving
     * the active reference set to prevent extra work.
     *
     * Updates the reference data cache. Can update the active ref set. Sends
     * out inventory change notifications.
     *
     * @param inventory
     * @param additions
     * @param deletions
     * @param changes
     * @param bypassActiveCheck
     */
    private void networkNotification(final List<ReferenceID> additions,
            final List<ReferenceID> deletions, final List<ReferenceID> changes,
            boolean bypassActiveCheck) {
        statusHandler.handle(Priority.VERBOSE,
                "NetworkNotify: oldInv=" + availableSets);
        statusHandler.handle(Priority.VERBOSE,
                "NetworkNotify: additions=" + additions);
        statusHandler.handle(Priority.VERBOSE,
                "NetworkNotify: deletions=" + deletions);
        statusHandler.handle(Priority.VERBOSE,
                "NetworkNotify: changes=" + changes);
        statusHandler.handle(Priority.VERBOSE,
                "ActiveSet = " + activeRefSet.getId());
        statusHandler.handle(Priority.VERBOSE,
                "BypassActiveCheck = " + bypassActiveCheck);

        // update the inventory
        availableSets.removeAll(deletions);
        availableSets.addAll(additions);

        // update the cache
        updateCache(additions, deletions, changes);

        // loaded reference set changed?
        for (ReferenceID chg : changes) {
            if (chg.getName().equals(activeRefSet.getId().getName())) {
                statusHandler.handle(Priority.VERBOSE, "Loaded set changed");
                if (!bypassActiveCheck) {
                    setActiveRefSet(loadRefSet(chg));
                } else {
                    activeRefSet.setId(chg);
                    sendReferenceSetIDChanged(chg);
                }
            }
        }

        // loaded reference set deleted, if so, then clear the edit area
        for (ReferenceID del : deletions) {
            if (del.getName().equals(activeRefSet.getId().getName())) {
                statusHandler.handle(Priority.VERBOSE, "Loaded set deleted");
                setActiveRefSet(EMPTY);
            }
        }

        // loaded reference set added, if so, then set the area
        for (ReferenceID add : additions) {
            if (add.getName().equals(activeRefSet.getId().getName())) {
                statusHandler.handle(Priority.VERBOSE, "Loaded set added");
                if (!bypassActiveCheck) {
                    setActiveRefSet(loadRefSet(add));
                } else {
                    activeRefSet.setId(add);
                    sendReferenceSetIDChanged(add);
                }
            }
        }

        // inventory changed?
        if ((!additions.isEmpty()) || (!deletions.isEmpty())
                || (!changes.isEmpty())) {
            sendReferenceSetInvChanged(availableSets, additions, deletions,
                    changes);
            checkGroupConsistency();
        }

        // update quickset tool tips
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (PlatformUI.isWorkbenchRunning()) {
                    final ICommandService service = PlatformUI.getWorkbench()
                            .getService(ICommandService.class);

                    service.refreshElements(
                            "com.raytheon.viz.gfe.actions.quickSet", null);
                }
            }
        });
    }

    @Override
    public Grid2DFloat taperGrid(final ReferenceData refData, int taperFactor) {
        // Make a grid of zeros
        Point dim = refData.getGloc().gridSize();
        Grid2DFloat grid = new Grid2DFloat(dim.x, dim.y, 0.0f);

        // deal with the pointy taper option
        boolean pointyTaper = false;
        if (taperFactor == 0) {
            pointyTaper = true;
            // just a large number
            taperFactor = (dim.x + dim.y) * 10;
        }

        // Get the Grid2DBit
        Grid2DBit bits = refData.getGrid();

        // get the lower left and upper right extent of the set bits.
        Point ll = new Point();
        Point ur = new Point();
        if (!bits.extremaOfSetBits(ll, ur)) {
            return grid;
        }

        // Figure out the distance from the edge for each grid cell.
        int d, i, j, dist, minDist;
        int maxDist = 0;
        Point loc = new Point();
        for (i = ll.x; i <= ur.x; i++) {
            for (j = ll.y; j <= ur.y; j++) {
                if (!bits.getAsBoolean(i, j)) {
                    continue;
                }
                minDist = taperFactor;
                // look in all directions
                for (d = 0; d < dirs.length; d++) {
                    loc.x = i + dirs[d].x;
                    loc.y = j + dirs[d].y;
                    dist = 1;
                    while ((loc.x >= 0) && (loc.x < dim.x) && (loc.y >= 0)
                            && (loc.y < dim.y)) {
                        if (!bits.getAsBoolean(loc.x, loc.y)) {
                            // edge
                            if (dist < minDist) {
                                minDist = dist;
                            }
                        }

                        if (dist >= minDist) {
                            break;
                        }
                        // inc. location
                        loc.translate(dirs[d].x, dirs[d].y);
                        // inc dist
                        dist++;
                    }
                }
                // calc value
                grid.set(i, j, (float) minDist / (float) taperFactor);
                if (minDist > maxDist) {
                    maxDist = minDist;
                }
            }
        }
        // If it's a pointTaper, undo the division above and divide by the
        // maximum distance instead
        if (pointyTaper) {
            for (i = ll.x; i <= ur.x; i++) {
                for (j = ll.y; j <= ur.y; j++) {
                    grid.set(i, j, (grid.get(i, j) * taperFactor) / maxDist);
                }
            }
        }

        return grid;
    }

    @Override
    public Grid2DFloat directionTaperGrid(final ReferenceData refData,
            final String direction) {

        if ((direction.length() < 1) || (direction.length() > 3)) {
            throw new IllegalArgumentException("direction \"" + direction
                    + "\" must be 1-3 characters in length");
        }
        // Calculate the direction unit vector
        int i = 0;
        int j = 0;
        for (int c = 0; c < direction.length(); c++) {
            switch (direction.charAt(c)) {
            case 'N':
                j--;
                break;
            case 'E':
                i--;
                break;
            case 'S':
                j++;
                break;
            case 'W':
                i++;
                break;

            default:
                throw new IllegalArgumentException("direction \"" + direction
                        + "\" must contain only the characters N, S, E, or W");
            }
        }

        // primary direction
        Point v = new Point(i, j);
        // opposite direction
        Point av = new Point(-i, -j);
        Grid2DBit bitGrid = refData.getGrid();

        Point dim = refData.getGloc().gridSize();

        Grid2DFloat grid = new Grid2DFloat(dim.x, dim.y, 0.0f);
        for (i = 0; i < dim.x; i++) {
            for (j = 0; j < dim.y; j++) {
                if (bitGrid.getAsBoolean(i, j)) {
                    // Calculate the distance along each vector
                    double vdist = 0.0;
                    double avdist = 0.0;
                    int x = i;
                    int y = j;
                    while ((x >= 0) && (y >= 0) && (x < dim.x) && (y < dim.y)
                            && bitGrid.getAsBoolean(x, y)) {
                        vdist = vdist + Math.sqrt((v.x * v.x) + (v.y * v.y));
                        x = x + v.x;
                        y = y + v.y;
                    }
                    x = i;
                    y = j;
                    while ((x >= 0) && (y >= 0) && (x < dim.x) && (y < dim.y)
                            && bitGrid.getAsBoolean(x, y)) {
                        avdist = avdist
                                + Math.sqrt((av.x * av.x) + (av.y * av.y));
                        x = x + av.x;
                        y = y + av.y;
                    }
                    if ((vdist + avdist) == 0) {
                        grid.set(i, j, 0.0f);
                    } else {
                        grid.set(i, j, (float) (vdist / (vdist + avdist)));
                    }
                }
            }
        }

        return grid;
    }

    @Override
    public Grid2DBit siteGridpoints(final List<String> sites,
            boolean includeOwnSite) {
        String ownSite = null;
        if (!includeOwnSite) {
            ownSite = dataManager.getSiteID();
        }
        Point gridSize = dataManager.getParmManager().compositeGridLocation()
                .gridSize();
        Grid2DBit out = new Grid2DBit(gridSize.x, gridSize.y);
        for (String site : sites) {
            if (site.equals(ownSite) && !includeOwnSite) {
                continue;
            }

            // get the edit area name corresponding to the site
            String refName = "ISC_" + site;

            // ensure we have a valid ref set for this site
            for (int j = 0; j < availableSets.size(); j++) {
                if (availableSets.get(j).getName().equals(refName)) {
                    // found it, so get it, convert to Grid2D, or it
                    ReferenceData rd = loadRefSet(new ReferenceID(refName));
                    Grid2DBit bits = rd.getGrid();
                    if (bits.isValid()) {
                        out.orEquals(bits);
                    }
                    break;
                }
            }
        }
        return out;
    }

    @Override
    public Grid2DBit mySiteGridpoints() {
        return siteGridpoints(Arrays.asList(dataManager.getSiteID()), true);
    }

    /**
     * Returns the Discrepancy Area Id number
     *
     * @return the Discrepancy Area Id number
     */
    public int nextAreaIdNumber() {
        int currentNumber = areaIdNumber;
        areaIdNumber++;
        if (areaIdNumber > 25) {
            areaIdNumber = 0;
        }
        return currentNumber;
    }

    @Override
    public String toString() {
        return "--- ReferenceSetMgr ---";
    }

    @Override
    public RefSetMode getMode() {
        return mode;
    }

    @Override
    public void setMode(RefSetMode mode) {
        this.mode = mode;
    }

    @Override
    public void incomingRefSet(ReferenceData refData, final RefSetMode mode) {
        // Evaluate the Incoming refData if necessary
        // Then, change the active Reference Set using the given mode.
        ReferenceData ref = null;
        if (refData.isQuery()) {
            final String query = refData.getQuery();
            IPythonJobListener<ReferenceData> listener = new IPythonJobListener<ReferenceData>() {
                @Override
                public void jobFailed(Throwable e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Unable to run QueryScript job", e);
                }

                @Override
                public void jobFinished(ReferenceData result) {
                    result.setQuery(query);
                    setRefSet(result, mode);
                }
            };
            evaluateQuery(query, listener);
        } else {
            ref = refData;
            setRefSet(ref, mode);
        }
    }

    private void setRefSet(ReferenceData ref, RefSetMode mode) {
        mode = determineMode(mode);
        // Incorporate the Incoming refData
        ReferenceData active;
        if (mode == RefSetMode.REPLACE) {
            active = new ReferenceData(ref);
        } else {
            ReferenceData left = getActiveRefSet();
            ReferenceData right = ref;
            if (mode == RefSetMode.UNION) {
                active = left.or(right);
            } else if (mode == RefSetMode.INTERSECT) {
                active = left.and(right);
            } else if (mode == RefSetMode.SUBTRACT) {
                active = left.minus(right);
            } else {
                statusHandler.error("Unknown reference set mode: " + mode);
                return;
            }
        }

        setActiveRefSet(active);
    }

    private RefSetMode determineMode(RefSetMode mode) {
        // Determine the mode to use for an incoming Reference Set
        // The mode defines whether the incoming refData replaces,
        // intersects with, or is added to the existing set.
        // If mode is USE_CURRENT, the current this.__mode value is used.
        ReferenceData active = getActiveRefSet();
        if ((active.refType() == RefType.NONE)
                | !active.getGrid().isAnyBitsSet()) {
            return RefSetMode.REPLACE;
        }

        if (mode == RefSetMode.USE_CURRENT) {
            return this.mode;
        }

        // Use the incoming RefSet mode and override current mode
        return mode;
    }

    @Override
    public void clearRefSet() {
        incomingRefSet(emptyRefSet(), RefSetMode.REPLACE);
    }

    @Override
    public void toggleRefSet() {
        ReferenceData rs = new ReferenceData(getActiveRefSet());
        rs.invert();
        incomingRefSet(rs, RefSetMode.REPLACE);
    }

    @Override
    public void addReferenceSetInvChangedListener(
            IReferenceSetInvChangedListener listener) {
        refSetInvChangedListeners.add(listener);
    }

    @Override
    public void removeReferenceSetInvChangedListener(
            IReferenceSetInvChangedListener listener) {
        refSetInvChangedListeners.remove(listener);
    }

    @Override
    public void addReferenceSetChangedListener(
            IReferenceSetChangedListener listener) {
        refSetChangedListeners.add(listener);
    }

    @Override
    public void removeReferenceSetChangedListener(
            IReferenceSetChangedListener listener) {
        refSetChangedListeners.remove(listener);
    }

    @Override
    public void addReferenceSetIDChangedListener(
            IReferenceSetIDChangedListener listener) {
        refSetChangedIDListeners.add(listener);
    }

    @Override
    public void removeReferenceSetIDChangedListener(
            IReferenceSetIDChangedListener listener) {
        refSetChangedIDListeners.remove(listener);
    }

    @Override
    public void addEditAreaGroupInvChangedListener(
            IEditAreaGroupInvChangedListener listener) {
        editAreaGroupInvChangedListeners.add(listener);
    }

    @Override
    public void removeEditAreaGroupInvChangedListener(
            IEditAreaGroupInvChangedListener listener) {
        editAreaGroupInvChangedListeners.remove(listener);
    }

    @Override
    public void handleQuickSet(int button) {
        switch (quickSetMode) {
        case RESTORE:
            incomingRefSet(getQuickSet(button), RefSetMode.USE_CURRENT);
            break;
        case SAVE:
            setQuickSet(button);
            toggleQuickSetMode();
            break;
        default:
            statusHandler.error("Invalid QuickSet mode: " + quickSetMode);
        }
    }

    @Override
    public ReferenceData getQuickSet(int slot) {
        // Return the ReferenceData contents of the given quickSet button
        ReferenceData result = emptyRefSet();
        ReferenceID quickID = getQuickID(slot);
        for (ReferenceID id : getAvailableSets()) {
            if (quickID.getName().equals(id.getName())) {
                result = loadRefSet(quickID);
                break;
            }
        }
        return result;
    }

    private void setQuickSet(int button) {
        // Make a copy of the activeRefSet to store in quickset Button
        // Then save the quickset
        ReferenceData active = getActiveRefSet();
        ReferenceID id = getQuickID(button);
        ReferenceData refData = new ReferenceData(active.getGloc(), id,
                active.getQuery(), active.getGrid());
        saveRefSet(refData);
    }

    private ReferenceID getQuickID(int button) {
        ReferenceID id = new ReferenceID("QuickSet" + button, true,
                LocalizationLevel.UNKNOWN);
        return id;
    }

    @Override
    public void toggleQuickSetMode() {
        switch (quickSetMode) {
        case RESTORE:
            quickSetMode = QuickSetMode.SAVE;
            break;
        case SAVE:
            quickSetMode = QuickSetMode.RESTORE;
            break;
        default:
            statusHandler.error("Invalid QuickSet mode: " + quickSetMode);
        }

        // update button state
        final ICommandService service = PlatformUI.getWorkbench()
                .getService(ICommandService.class);

        service.refreshElements("com.raytheon.viz.gfe.actions.quickSetMode",
                null);
    }

    @Override
    public QuickSetMode getQuickSetMode() {
        return quickSetMode;
    }

    private void editAreaUpdated(FileUpdatedMessage message) {
        String name = message.getFileName();
        name = name.replace(".xml", "");
        int pos = name.lastIndexOf('/');
        if (pos >= 0) {
            name = name.substring(pos + 1);
        }
        ReferenceID refId = new ReferenceID(name, false,
                message.getContext().getLocalizationLevel());

        List<ReferenceID> additions = new ArrayList<>();
        List<ReferenceID> deletions = new ArrayList<>();
        List<ReferenceID> changes = new ArrayList<>();

        switch (message.getChangeType()) {
        case ADDED:
        case UPDATED:
            if (availableSets.contains(refId)) {
                changes.add(refId);
            } else {
                additions.add(refId);
            }
            break;
        case DELETED:
            // check to see if there is a file at another localization level
            String filePath = FileUtil.join(EDIT_AREAS_DIR,
                    refId.getName() + ".xml");
            LocalizationFile lf = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(LocalizationType.COMMON_STATIC,
                            filePath);

            if ((lf != null) && lf.exists()) {
                changes.add(refId);
            } else {
                deletions.add(refId);
            }
            break;

        default:
            statusHandler.error(
                    "Unknown FileChangeType: " + message.getChangeType());
            break;
        }

        networkNotification(additions, deletions, changes, false);
    }

    private void groupUpdated(FileUpdatedMessage message) {
        IPathManager pm = PathManagerFactory.getPathManager();

        // if a group was deleted
        if (message.getChangeType().equals(FileChangeType.DELETED)) {
            // delete our local copy if it exists
            File localFile = pm.getFile(message.getContext(),
                    message.getFileName());

            if (localFile.exists()) {
                localFile.delete();
            }

            // remove the group from the group map
            String s = message.getFileName();
            String group = s.substring(s.lastIndexOf(File.separatorChar) + 1)
                    .replace(".txt", "");
            groupMap.remove(group);

            for (int i = 0; i < groupIdList.size(); i++) {
                if (groupIdList.get(i).getName().equals(group)) {
                    groupIdList.remove(i);
                    break;
                }
            }
        }

        // find the file anywhere in the hierarchy
        LocalizationFile lf = pm.getStaticLocalizationFile(
                LocalizationType.COMMON_STATIC, message.getFileName());

        // if it exists, load it
        if ((lf != null) && lf.exists()) {
            loadGroup(lf);
        }
        checkGroupConsistency();
        sendEditAreaGroupInvChanged();
    }

    private void evaluateQuery(String query,
            IPythonJobListener<ReferenceData> listener) {
        Map<String, Object> argMap = new HashMap<>();

        argMap.put("expression", query);

        IPythonExecutor<QueryScript, ReferenceData> executor = new QueryScriptExecutor(
                "evaluate", argMap);
        try {
            coordinator.submitJobWithCallback(executor, listener);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to submit job to ExecutorService", e);
        }
    }

    /**
     * Evaluate the query and return the usable edit area.
     *
     * @param query
     *            The query to be evaluated.
     * @return The resulting edit area.
     */
    public ReferenceData evaluateQuery(String query) {
        ReferenceData ea = null;

        Map<String, Object> argMap = new HashMap<>();
        argMap.put("expression", query);

        IPythonExecutor<QueryScript, ReferenceData> executor = new QueryScriptExecutor(
                "evaluate", argMap);
        try {
            ea = coordinator.submitJob(executor).get();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to evaluate query: " + query, e);
        }

        return ea;
    }

    @Override
    public boolean willRecurse(String name, String query) {
        Map<String, Object> argMap = new HashMap<>();
        argMap.put("name", name);
        argMap.put("str", query);
        IPythonExecutor<QueryScript, Integer> executor = new QueryScriptRecurseExecutor(
                argMap);
        int result = 0;
        try {
            result = coordinator.submitJob(executor).get();
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to submit job to ExecutorService", e);
            return true;
        }
        return result != 0;
    }

    private void evaluateActiveRefSet(
            IPythonJobListener<ReferenceData> listener) {
        ReferenceData active = getActiveRefSet();
        if (active.isQuery()) {
            // Re-evaluate the activeRefSet
            evaluateQuery(active.getQuery(), listener);
        } else {
            // if non-query,need to fire, but do this in an else otherwise we
            // will fire the listener twice for queries
            listener.jobFinished(getActiveRefSet());
        }
    }

    @Override
    public void receiveMessage(final Message message) {
        IPythonJobListener<ReferenceData> listener = new IPythonJobListener<ReferenceData>() {
            @Override
            public void jobFailed(Throwable e) {
                statusHandler.debug(
                        "Unable to evaluate query: " + e.getLocalizedMessage(),
                        e);
            }

            @Override
            public void jobFinished(ReferenceData result) {
                getActiveRefSet();
                if (!result.getGrid().equals(getActiveRefSet().getGrid())) {
                    setActiveRefSet(result);
                }
            };
        };

        GridDataChangedMsg msg = (GridDataChangedMsg) message;

        Date spedTime = dataManager.getSpatialDisplayManager()
                .getSpatialEditorTime();
        if ((spedTime != null) && msg.getTimeRange().contains(spedTime)) {
            evaluateActiveRefSet(listener);
        }
    }

    @Override
    public void pushHistoryStack(String s) {
        // Push the query onto the historyStack

        // Do not add a duplicate query
        if (historyStack.contains(s)) {
            return;
        }

        // Keep stack to limit
        if (historyStack.size() == STACK_LIMIT) {
            historyStack.remove(STACK_LIMIT - 1);
        }

        historyStack.add(0, s);
    }

    @Override
    public List<String> getHistoryStack() {
        return Collections.unmodifiableList(historyStack);
    }

    @Override
    public void spatialEditorTimeChanged(Date date) {
        IPythonJobListener<ReferenceData> listener = new IPythonJobListener<ReferenceData>() {
            @Override
            public void jobFailed(Throwable e) {
                statusHandler.debug(
                        "Unable to evaluate query: " + e.getLocalizedMessage(),
                        e);
            }

            @Override
            public void jobFinished(ReferenceData result) {
                if (!result.getGrid().equals(activeRefSet.getGrid())) {
                    setActiveRefSet(result);
                }
            }
        };

        evaluateActiveRefSet(listener);
    }

    @Override
    public PythonJobCoordinator<QueryScript> getPythonThreadPool() {
        return coordinator;
    }
}
