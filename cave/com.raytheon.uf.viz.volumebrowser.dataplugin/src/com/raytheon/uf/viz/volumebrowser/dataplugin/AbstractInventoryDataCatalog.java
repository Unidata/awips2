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
package com.raytheon.uf.viz.volumebrowser.dataplugin;

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

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMapping;
import com.raytheon.uf.common.dataplugin.level.mapping.LevelMappingFactory;
import com.raytheon.uf.common.dataplugin.level.util.LevelUtilities;
import com.raytheon.uf.common.derivparam.inv.AbstractInventory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.rsc.ResourceType;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.volumebrowser.datacatalog.AbstractDataCatalog;
import com.raytheon.viz.volumebrowser.datacatalog.AvailableDataRequest;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalog;
import com.raytheon.viz.volumebrowser.loader.ProductCreatorManager;
import com.raytheon.viz.volumebrowser.util.PointLineUtil;
import com.raytheon.viz.volumebrowser.vbui.DataListsProdTableComp.DataSelection;
import com.raytheon.viz.volumebrowser.vbui.MenuItemManager;
import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;

/**
 * Base {@link IDataCatalog} for plugin types that use an
 * {@link AbstractInventory} to track parameter availability.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 14, 2010  5021     bsteffen  Initial creation
 * Jul 25, 2013  2112     bsteffen  Fix volume browser sounding errors.
 * Jan 30, 2014  2725     ekladstr  updated exception handling during move of
 *                                  derived parameters to common
 * Sep 09, 2014  3356     njensen   Remove CommunicationException
 * May 18, 2015  4412     bsteffen  Use all level mappings for plane names
 * Aug 03, 2015  3861     bsteffen  Move resource creation to ProductCreators
 * Feb 01, 2016  5275     bsteffen  Extract InventoryUpdateJob
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public abstract class AbstractInventoryDataCatalog extends AbstractDataCatalog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractInventoryDataCatalog.class);

    public static final String SOUNDING_PARAMETER = "Snd";

    protected static final String[] soundingParams = { "T", "GH", "uW", "vW",
            "DpT" };

    protected AbstractInventory getInventory() {
        return (AbstractInventory) DataCubeContainer
                .getInventory(getDefaultPlugin());
    }

    /**
     * Check if there are any plugins supported by this catalog which are
     * supported by the {@link ProductCreatorManager} for the provided
     * {@link ResourceType}.
     */
    private boolean supportedResourceType(ResourceType resourceType) {
        ProductCreatorManager theMan = ProductCreatorManager.getInstance();
        for (String pluginName : getPlugins()) {
            if (theMan.getCreator(pluginName, resourceType) != null) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void getAvailableData(AvailableDataRequest request) {
        ResourceType resourceType = VolumeBrowserAction.getVolumeBrowserDlg()
                .getDialogSettings().getViewSelection().getResourceType();
        if (!supportedResourceType(resourceType)) {
            return;
        }
        final AbstractInventory inventory = getInventory();
        if (inventory == null) {
            return;
        }
        MenuItemManager menuManager = MenuItemManager.getInstance();

        boolean isSounding = false;

        InventoryUpdateJob job = new InventoryUpdateJob(inventory);

        /* Get all possible sources */
        job.setAllPossibleSources(menuManager
                .getAvailableKeys(DataSelection.SOURCES));

        /*
         * Get all possible parameters --- must transform Snd into other
         * parameters
         */
        Set<String> possibleParameters = menuManager
                .getAvailableKeys(DataSelection.FIELDS);
        if (possibleParameters.contains(SOUNDING_PARAMETER)) {
            isSounding = true;
            possibleParameters = new HashSet<>(possibleParameters);
            possibleParameters.remove(SOUNDING_PARAMETER);
            possibleParameters.addAll(Arrays.asList(soundingParams));
        }
        job.setAllPossibleParameters(possibleParameters);

        /*
         * Get all possible levels --- must perform level mappings and take into
         * account when planes contains points and lines, luckily getLevels does
         * all that for us.
         */
        Set<String> possiblePlanes = menuManager
                .getAvailableKeys(DataSelection.PLANES);

        Set<Level> possibleLevels = getLevels(
                possiblePlanes.toArray(new String[0]), null);
        job.setAllPossibleLevels(possibleLevels);

        /* Get the selected sources */
        String[] selectedSources = request.getSelectedSources();
        List<String> selectedSourcesAsList = null;
        if(selectedSources != null && selectedSources.length > 0){
            selectedSourcesAsList = Arrays.asList(selectedSources);
            job.setSelectedSources(selectedSourcesAsList);
        }
        
        /*
         * Get the selected parameters --- must transform Snd into other
         * parameters
         */
        String[] selectedFields = request.getSelectedFields();
        if(selectedFields != null && selectedFields.length > 0){
            Collection<String> selectedParameters = Arrays.asList(selectedFields);
            if(selectedParameters.contains(SOUNDING_PARAMETER)){
                selectedParameters = new HashSet<>(selectedParameters);
                selectedParameters.remove(SOUNDING_PARAMETER);
                selectedParameters.addAll(Arrays.asList(soundingParams));
            }
            job.setSelectedParameters(selectedParameters);
        }
        
        /*
         * Get the selected levels --- must perform level mappings and take into
         * account when planes contains points and lines, luckily getLevels does
         * all that for us.
         */
        String[] selectedPlanes = request.getSelectedPlanes();
        if (selectedPlanes != null && selectedPlanes.length > 0) {
            Set<Level> selectedLevels = getLevels(selectedPlanes, null);
            job.setSelectedLevels(selectedLevels);
        }

        if (selectedPlanes != null && selectedPlanes.length > 0) {
            job.setPlaneFilteredSources(get3DSources(selectedPlanes));
        }

        job.schedule();

        Collection<? extends Level> levels3D = get3DLevels();

        while (!job.isDone()) {
            String source = job.pollSource();
            while (source != null) {
                request.addAvailableSource(source);
                source = job.pollSource();
            }
            String field = job.pollParameter();
            while (field != null) {
                if (isSounding) {
                    request.addAvailableField(SOUNDING_PARAMETER);
                }
                request.addAvailableField(field);
                field = job.pollParameter();
            }
            Level level = job.pollLevel();
            while (level != null) {
                /* Convert levels into planes. */
                if (levels3D.contains(level)) {
                    for (String plane : get3DPlanes(selectedSourcesAsList)) {
                        request.addAvailablePlane(plane);
                    }
                }
                String spatialPlane = "spatial-"
                        + level.getMasterLevel().getName();
                request.addAvailablePlane(spatialPlane);
                Collection<LevelMapping> lms = LevelMappingFactory.getInstance(
                        LevelMappingFactory.VOLUMEBROWSER_LEVEL_MAPPING_FILE)
                        .getAllLevelMappingsForLevel(level);

                if (lms != null) {
                    for (LevelMapping lm : lms) {
                        request.addAvailablePlane(lm.getKey());
                    }
                }
                level = job.pollLevel();

            }
            if (request.isCanceled()) {
                job.interrupt();
                break;
            }
            try {
                Thread.sleep(10);
            } catch (InterruptedException e) {
                // no-op
            }
        }
    }

    // Given a list of planes, return the sources that are valid for any point
    // or line planes
    protected Collection<String> get3DSources(String[] planes) {
        return null;
    }

    // Get all the Point and Line planes that are valid for the given sources.
    protected Collection<String> get3DPlanes(Collection<String> sources) {
        Set<String> results = new HashSet<String>();
        if (sources == null || sources.isEmpty()) {
            results.addAll(PointLineUtil.getPointLineKeys());
            results.addAll(MenuItemManager.getInstance().getLatLonKeys());
        } else {
            List<String> allSources = getSupportedSourcesInternal();
            for (String source : sources) {
                if (allSources.contains(source)) {
                    results.addAll(PointLineUtil.getPointLineKeys());
                    results.addAll(MenuItemManager.getInstance()
                            .getLatLonKeys());
                }
            }
        }
        return results;
    }

    public List<String> getSupportedSourcesInternal() {
        BlockingQueue<String> returnQueue = new LinkedBlockingQueue<String>();
        AbstractInventory inventory = getInventory();
        if (inventory != null) {
            try {
                inventory.checkSources(null, null, null, returnQueue);
            } catch (InterruptedException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
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
                    levels = LevelUtilities.getOrderedSetOfStandardLevels(plane
                            .replace("spatial-", ""));
                } else {
                    LevelMapping lm = lmf.getLevelMappingForKey(plane);
                    if (lm != null) {
                        levels = lm.getLevels();
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
