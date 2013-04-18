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
package com.raytheon.viz.volumebrowser.datacatalog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.level.LevelUtilities;
import com.raytheon.uf.viz.derivparam.inv.AbstractInventory;
import com.raytheon.viz.volumebrowser.vbui.DataListsProdTableComp.DataSelection;
import com.raytheon.viz.volumebrowser.vbui.MenuItemManager;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 14, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractInventoryDataCatalog extends AbstractDataCatalog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractInventoryDataCatalog.class);

    protected static final String[] soundingParams = { "T", "GH", "uW", "vW",
            "DpT" };

    protected AbstractInventory getInventory() {
        return (AbstractInventory) DataCubeContainer
                .getInventory(getDefaultPlugin());
    }

    @Override
    public void getAvailableData(AvailableDataRequest request) {
        String[] selectedSources = request.getSelectedSources();
        String[] selectedFields = request.getSelectedFields();
        String[] selectedPlanes = request.getSelectedPlanes();
        boolean sourcesEmpty = selectedSources == null
                || selectedSources.length == 0;
        boolean fieldsEmpty = selectedFields == null
                || selectedFields.length == 0;
        // TODO find some way to filter sounding to soundingParams
        request.addAvailableField("Snd");
        final AbstractInventory inventory = getInventory();
        if (inventory == null) {
            return;
        }
        final Collection<String> sources3D = get3DSources(selectedPlanes);
        Collection<String> sourcesToProcess = null;
        Collection<String> sourcesToProcessFiltered3D = null;
        if (!sourcesEmpty) {
            sourcesToProcessFiltered3D = sourcesToProcess = new ArrayList<String>(
                    Arrays.asList(selectedSources));
            if (sources3D != null) {
                sourcesToProcessFiltered3D = new ArrayList<String>(
                        sourcesToProcess);
                sourcesToProcessFiltered3D.retainAll(sources3D);
            }
        }
        Collection<String> paramsToProcess = null;
        if (!fieldsEmpty) {
            paramsToProcess = Arrays.asList(selectedFields);
            if (paramsToProcess.contains("Snd")) {
                paramsToProcess = new ArrayList<String>(paramsToProcess);
                paramsToProcess.addAll(Arrays.asList(soundingParams));
            }
        }

        final Collection<Level> levelsToProcess = getLevels(selectedPlanes,
                null);
        final Collection<Level> levelsToProcessFiltered3D = getLevels(
                selectedPlanes, selectedSources);
        if (request.isCanceled()) {
            return;
        }
        Collection<? extends Level> levels3D = get3DLevels();

        // Make final versions of everything to pass into the thread.
        final Collection<String> fSourcesToProcess = sourcesToProcess;
        final Collection<String> fSourcesToProcessFiltered3D = sourcesToProcessFiltered3D;
        final Collection<String> fParamsToProcess = paramsToProcess;

        // The result queues.
        final BlockingQueue<String> sourceQueue = new LinkedBlockingQueue<String>();
        final BlockingQueue<String> fieldQueue = new LinkedBlockingQueue<String>();
        final BlockingQueue<String> levelQueue = new LinkedBlockingQueue<String>();
        // Query the inventory in a new Thread, this allows us to update the UI
        // as things are added to the queue, and also allows us to interupt this
        // thread if the availableDataRequest is canceled.
        Job inventoryJob = new Job("Loading available inventory") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    inventory.checkSources(sources3D, fParamsToProcess,
                            levelsToProcess, sourceQueue);
                    inventory.checkParameters(fSourcesToProcessFiltered3D,
                            null, levelsToProcessFiltered3D, false, fieldQueue);
                    inventory.checkLevels(fSourcesToProcess, fParamsToProcess,
                            null, levelQueue);
                } catch (InterruptedException e) {
                    ;
                }
                return Status.OK_STATUS;
            }

        };
        inventoryJob.setSystem(true);
        inventoryJob.schedule();

        while (inventoryJob.getResult() == null || !sourceQueue.isEmpty()
                || !fieldQueue.isEmpty() || !levelQueue.isEmpty()) {
            String source = sourceQueue.poll();
            while (source != null) {
                request.addAvailableSource(source);
                source = sourceQueue.poll();
            }
            String field = fieldQueue.poll();
            while (field != null) {
                request.addAvailableField(field);
                field = fieldQueue.poll();
            }
            String levelStr = levelQueue.poll();
            while (levelStr != null) {
                // Convert levels into planes.
                try {
                    Level level = LevelFactory.getInstance().getLevel(levelStr);

                    if (levels3D.contains(level)) {
                        for (String plane : get3DPlanes(sourcesToProcess)) {
                            request.addAvailablePlane(plane);
                        }
                    }
                    request.addAvailablePlane("spatial-"
                            + level.getMasterLevel().getName());
                    LevelMapping lm = LevelMappingFactory
                            .getInstance(
                                    LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                            .getLevelMappingForLevel(level);

                    if (lm != null) {
                        request.addAvailablePlane(lm.getKey());
                    }
                } catch (CommunicationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                levelStr = levelQueue.poll();

            }
            if (request.isCanceled()) {
                Thread thread = inventoryJob.getThread();
                if (thread != null) {
                    thread.interrupt();
                }
                break;
            }
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                ;
            }
        }
    }

    // GIven a list of planes, return the sources that are valid for any point
    // or line planes
    protected Collection<String> get3DSources(String[] planes) {
        return null;
    }

    // Get all the Point and Line planes that are valid for the given sources.
    protected Collection<String> get3DPlanes(Collection<String> sources) {
        Set<String> results = new HashSet<String>();
        if (sources == null || sources.isEmpty()) {
            results.addAll(getPointLineKeys());
            results.addAll(MenuItemManager.getInstance().getLatLonKeys());
        } else {
            List<String> allSources = getSupportedSourcesInternal();
            for (String source : sources) {
                if (allSources.contains(source)) {
                    results.addAll(getPointLineKeys());
                    results.addAll(MenuItemManager.getInstance()
                            .getLatLonKeys());
                }
            }
        }
        return results;
    }

    public List<String> getSupportedSourcesInternal() {
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        try {
            getInventory().checkSources(null, null, null, returnQueue);
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        List<String> result = new ArrayList<String>(returnQueue);
        result.retainAll(MenuItemManager.getInstance()
                .getMapOfKeys(DataSelection.SOURCES).keySet());
        return result;
    }

    @Override
    public List<String> getSupportedSources() {
        return getSupportedSourcesInternal();
    }

    /**
     * Determine what levels are included in a group of selected planes
     * 
     * @param selectedPlanes
     * @return
     * @throws InterruptedException
     */
    protected Set<Level> getLevels(String[] selectedPlanes,
            String[] selectedSources) {
        ViewMenu viewSelection = VolumeBrowserAction.getVolumeBrowserDlg()
                .getDialogSettings().getViewSelection();
        Set<Level> results = new HashSet<Level>();
        if (viewSelection == null) {
            return Collections.emptySet();
        }
        switch (viewSelection) {
        case PLANVIEW:
        case TIMESERIES:
            if (selectedPlanes == null || selectedPlanes.length == 0) {
                return null;
            }
            LevelMappingFactory lmf = LevelMappingFactory
                    .getInstance(LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE);
            for (String plane : selectedPlanes) {
                Collection<Level> levels = Collections.emptyList();
                if (plane.startsWith("spatial-")) {
                    try {
                        levels = LevelUtilities
                                .getOrderedSetOfStandardLevels(plane.replace(
                                        "spatial-", ""));
                    } catch (VizCommunicationException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                } else {
                    LevelMapping lm = lmf.getLevelMappingForKey(plane);
                    if (lm != null) {
                        try {
                            levels = lm.getLevels();
                        } catch (CommunicationException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                }
                for (Level l : levels) {
                    results.add(l);
                }

            }
            break;
        case VARVSHGT:
        case CROSSSECTION:
        case TIMEHEIGHT:
        case SOUNDING:
            if (selectedSources == null || selectedSources.length == 0) {
                results.addAll(get3DLevels());
            } else {
                Collection<String> planes3D = get3DPlanes(Arrays
                        .asList(selectedSources));
                if (selectedPlanes != null && selectedPlanes.length != 0) {
                    planes3D = new ArrayList<String>(planes3D);
                    planes3D.retainAll(Arrays.asList(selectedPlanes));
                }
                if (!planes3D.isEmpty()) {
                    results.addAll(get3DLevels());
                }
            }
            break;
        }
        return results;
    }

    protected boolean isValidSelection(SelectedData selData) {
        try {
            Set<Level> levels = getLevels(
                    new String[] { selData.getPlanesKey() },
                    new String[] { selData.getSourcesKey() });
            if (levels == null || levels.isEmpty()) {
                return false;
            }
            List<String> params = Arrays.asList(selData.getFieldsKey());
            if (selData.getFieldsKey().equals("Snd")) {
                return true;
            }
            BlockingQueue<String> returnQueue = new ArrayBlockingQueue<String>(
                    levels.size());
            getInventory().checkLevels(Arrays.asList(selData.getSourcesKey()),
                    params, levels, returnQueue);
            if (returnQueue.isEmpty()) {
                return false;
            }
        } catch (InterruptedException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error occured checking data availability", e);
            return false;
        }
        return true;
    }

    /**
     * @return
     */
    protected abstract Collection<? extends Level> get3DLevels();
}
