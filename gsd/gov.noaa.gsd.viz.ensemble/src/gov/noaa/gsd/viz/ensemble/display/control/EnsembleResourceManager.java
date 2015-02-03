package gov.noaa.gsd.viz.ensemble.display.control;

import gov.noaa.gsd.viz.ensemble.display.common.GenericResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.NavigatorResourceList;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolManager;

import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.grid.rsc.general.AbstractGridResource;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Handle all ensemble products member resources, reference to any loaded grid
 * product may be treated as ensemble member, and ensemble calculation result
 * resources. This is the key architectural support class connecting the
 * ensemble display, GUI and D2D core. It tracks and the status of the handled
 * resources, controls them, keeps the ensemble tool works with D2D core display
 * synchronously. All registered resources are in the ensembleToolResourcesMap.
 * 
 * 
 * @author jing
 * @author polster
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb, 2014      5056       jing     Initial creation
 * 
 * </pre>
 */

public class EnsembleResourceManager implements IDisposeListener {
    private final static double DEFAULT_DENSITY = 0.15;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleResourceManager.class);

    /**
     * The ensemble marked resource list holds the loaded and generated
     * resources and flags, is used by ensemble display, GUI and calculation.
     */
    private ConcurrentHashMap<AbstractEditor, NavigatorResourceList> ensembleToolResourcesMap;

    /**
     * The instance of the manager, a singleton.
     */
    public static EnsembleResourceManager instance = null;

    /**
     * The listeners to any resource, GUI... change event Register to any bus or
     * event list for to catch interesting events
     */
    private EnsembleResourceManager() {
        ensembleToolResourcesMap = new ConcurrentHashMap<AbstractEditor, NavigatorResourceList>();

    }

    /**
     * Get the resource manager.
     * 
     * @return
     */
    public static EnsembleResourceManager getInstance() {
        if (instance == null) {
            instance = new EnsembleResourceManager();
        }
        return instance;
    }

    /**
     * call this, for example, at EnsembleToolManager initialization time when a
     * new tool layer is created but there are no resources yet added to it.
     * 
     * @param editor
     *            - The product is loaded into which editor/window.
     */
    public void registerToolLayer(AbstractEditor editor) {
        if (ensembleToolResourcesMap.get(editor) == null) {
            ensembleToolResourcesMap.put(editor, new NavigatorResourceList(
                    editor));
        }
    }

    /**
     * Get the resources in an editor.
     * 
     * @param editor
     *            - The product is loaded into which editor/window.
     * @return - list of resources.
     */
    public NavigatorResourceList getResourceList(AbstractEditor editor) {

        NavigatorResourceList list = null;

        if (editor != null) {
            list = ensembleToolResourcesMap.get(editor);
        }
        return list;
    }

    /**
     * Clean the resources in an editor.
     * 
     * @param editor
     *            - The product is loaded into which editor/window.
     */
    public void clearResourceList(AbstractEditor editor) {
        ensembleToolResourcesMap.get(editor).clear();
    }

    /**
     * Register a loaded model product in ensemble status
     * 
     * @param rsc
     *            - the loaded product resource
     * @param guiUpdate
     *            - if need update GUI
     */
    public synchronized void registerResource(AbstractVizResource<?, ?> rsc,
            AbstractEditor editor, boolean guiUpdate) {

        if ((rsc == null) || (!isToolLayerReady()))
            return;

        // if this is the first resource ever registered using this editor
        // then create the resource list and map it to the editor ...
        if (ensembleToolResourcesMap.get(editor) == null) {
            ensembleToolResourcesMap.put(editor, new NavigatorResourceList(
                    editor));

            if (!EnsembleToolManager.getInstance().hasToolLayer(editor)) {
                EnsembleToolManager.getInstance().addToolLayer(editor);
            }
        }

        // no duplicates
        if (rsc == null
                || (ensembleToolResourcesMap.get(editor).containsResource(rsc))) {
            return;
        }

        // TODO: Must refactor so we don't use setSystemResource(boolean)
        // method.
        rsc.getProperties().setSystemResource(true);
        rsc.registerListener((IDisposeListener) this);

        // Set to default density for all loaded resources if can
        // But it may be unmatched with current display. Fix it later
        if (rsc.getCapability(DensityCapability.class) != null) {
            DensityCapability densityCapability = (DensityCapability) rsc
                    .getCapability(DensityCapability.class);
            densityCapability.setDensity(DEFAULT_DENSITY);
        }

        GenericResourceHolder ensToolResource = GenericResourceHolder
                .createResourceHolder(rsc, true);
        ensToolResource.setGenerated(false);
        ensembleToolResourcesMap.get(editor).add(ensToolResource);

        syncRegisteredResource(editor);

        if (guiUpdate) {
            notifyClientListChanged();
            // now update the calculation ensemble resource, if not already
            // set ... there can only be one, and it will be the first
            // ensemble resource loaded into this editor ...
            ensembleToolResourcesMap.get(editor)
                    .updateEnsembleCalculationResource();
        }
    }

    /**
     * Remove a resource from the tracking list.
     * 
     * @param gr
     *            - the tracked resource.
     * @param editor
     *            - loaded editor.
     * @param notifyGUI
     *            - if update the GUI.
     */
    public synchronized void unregisterResource(GenericResourceHolder gr,
            AbstractEditor editor, boolean notifyGUI) {
        if (gr == null
                || ensembleToolResourcesMap.get(editor).getResourceHolders()
                        .isEmpty())
            return;
        ensembleToolResourcesMap.get(editor).remove(gr);
        gr.getRsc().unregisterListener((IDisposeListener) this);

        syncRegisteredResource(editor);

        // if requested, notify client the known loaded resources have changed
        if (notifyGUI) {
            notifyClientListChanged();
        }

    }

    /**
     * Register Ensemble generated resource. after creating or deleting a
     * ensemble generated resource.
     * 
     * @param rsc
     *            - the generated resource by calculation.
     */
    public synchronized void registerGenerated(AbstractVizResource<?, ?> rsc,
            AbstractEditor editor) {

        if ((rsc == null) || (!isToolLayerReady()))
            return;

        // This may be the first resource ever registered using this editor. If
        // so, then create the resource list and associate it with the editor
        // using the map ...

        if (ensembleToolResourcesMap.get(editor) == null) {
            ensembleToolResourcesMap.put(editor, new NavigatorResourceList(
                    editor));
        }

        // Only one instance of a generated resource is saved. If a duplicate is
        // created then have it overwrite the existing one.
        for (GenericResourceHolder gr : ensembleToolResourcesMap.get(editor)
                .getUserGeneratedRscs()) {

            // Remove any resource in this list, since it was unloaded/ isn't
            // existing.
            if (!gr.getRsc().getDescriptor().getResourceList()
                    .containsRsc(gr.getRsc())) {
                ensembleToolResourcesMap.get(editor).remove(gr);

            } else if (rsc.getClass().cast(rsc).getName()
                    .equals(gr.getRsc().getClass().cast(gr.getRsc()).getName())) {
                // Same generated resource name, unload old one
                ensembleToolResourcesMap.get(editor).remove(gr);
                gr.getRsc().getDescriptor().getResourceList()
                        .removeRsc(gr.getRsc());
            }

        }

        // Set to default density for all loaded resources if can
        // But it may be unmatched with current display. Fix it later

        // TODO: why are we testing for this for tool layer ready yet again?
        // Set resource as a system resource so we don't show the legend
        // in the main map, because we will display it in the ensemble
        // navigator view.
        if (isToolLayerReady()) {
            // TODO: Must refactor so we don't use setSystemResource(boolean)
            // method.
            rsc.getProperties().setSystemResource(true);
            rsc.registerListener((IDisposeListener) this);
        }

        GenericResourceHolder ensToolResource = GenericResourceHolder
                .createResourceHolder(rsc, true);
        ensToolResource.setGenerated(true);
        ensembleToolResourcesMap.get(editor).add(ensToolResource);

        syncRegisteredResource(editor);

        // notify any client?
        notifyClientListChanged();
    }

    /**
     * Remove a generated resource from the tracking list.
     * 
     * @param gr
     *            - the tracked resource.
     * @param editor
     *            - loaded editor.
     * @param notifyGUI
     *            - if update the GUI.
     */
    public synchronized void unregisterGenerated(GenericResourceHolder gr,
            AbstractEditor editor, boolean notifyGUI) {
        if (gr == null
                || ensembleToolResourcesMap.get(editor).getResourceHolders()
                        .isEmpty())
            return;
        ensembleToolResourcesMap.get(editor).remove(gr);
        gr.getRsc().unregisterListener((IDisposeListener) this);

        // notify client the generated resource change?

        syncRegisteredResource(editor);

        // if requested, notify client the known loaded resources have changed
        if (notifyGUI) {
            notifyClientListChanged();
        }
    }

    /**
     * Get the resource name string
     * 
     * @param editor
     *            - resource editor
     * @return- resource name
     */
    public String getTimeBasisResourceName(AbstractEditor editor) {

        return getResourceList(editor).getTimeBasisResourceName();

    }

    /**
     * Get the resource legend time
     * 
     * @param editorr
     *            - resource editor
     * @return- legend time
     */
    public String getTimeBasisLegendTime(AbstractEditor editor) {

        String s = "";
        if ((editor != null) && (getResourceList(editor) != null)) {
            s = getResourceList(editor).getTimeBasisLegendTime();
        }
        return s;

    }

    /**
     * Handle ENS GUI change
     * 
     * @param editor
     */
    public void updateFrameChanges(AbstractEditor editor) {
        syncRegisteredResource(editor);

        // update generated ensemble Resource if need
        updateGenerated(editor);
    }

    /**
     * Synchronous the registered resource list within editor(s) and GUI.
     * --verify if the resources in the list are existing. --Remove any resource
     * in this list, if it was unloaded. --notify GUI if there is any change.
     */
    public void syncRegisteredResource(AbstractEditor editor) {

        if (ensembleToolResourcesMap.get(editor) == null) {
            return;
        }

        if (ensembleToolResourcesMap.get(editor).getAllRscsAsList() == null
                || ensembleToolResourcesMap.get(editor).getAllRscsAsList()
                        .isEmpty())
            return;

        for (GenericResourceHolder gr : ensembleToolResourcesMap.get(editor)
                .getAllRscsAsList()) {
            // verify if the resources in the list are existing.
            if (!gr.getRsc().getDescriptor().getResourceList()
                    .containsRsc(gr.getRsc())) {

                // Remove any resource in this list, since it was unloaded/
                // isn't existing.
                ensembleToolResourcesMap.get(editor).remove(gr);

                // notify GUI if there is any change.
                notifyClientListChanged();
            } else {
                // don't show legend if Tool Layer is editable
                if (isToolLayerReady()) {
                    gr.getRsc().getProperties().setSystemResource(true);
                }

                // Marks to unselected if the resource is not visible
                // set resource selection to true.
                if (gr.getRsc().getProperties().isVisible()) {
                    gr.setSelected(true);
                } else {
                    gr.setSelected(false);
                }
            }
        }
    }

    /**
     * * check for all ensemble interested resources if they are out of control
     * by the ensemble resource manager. Current interested resources are
     * AbstractGridResource ETimeSeriesResouece, HistogramResource
     * 
     * @param editor
     * @return
     */
    public ResourceList searchUnryncRegisteredResource(AbstractEditor editor) {

        ResourceList unRegisteredResources = new ResourceList();

        // The registered resources for this editor in the manager
        List<GenericResourceHolder> resourceList = ensembleToolResourcesMap
                .get(editor).getAllRscsAsList();

        IDescriptor desc = (editor.getActiveDisplayPane().getDescriptor());
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {

            AbstractVizResource<?, ?> rsc = rp.getResource();
            if ((rsc instanceof AbstractGridResource)
                    || (rsc instanceof TimeSeriesResource)
                    || (rsc instanceof HistogramResource)) {

                boolean isRegistered = false;
                for (GenericResourceHolder rcsHolder : resourceList) {
                    if (rcsHolder.getRsc() == rsc) {
                        isRegistered = true;
                    }
                }
                if (!isRegistered) {
                    unRegisteredResources.add(rp);
                }
            }
        }
        return unRegisteredResources;
    }

    /**
     * check if the ensemble GUI is reday
     * 
     * @return
     */
    public boolean isToolLayerReady() {
        return EnsembleToolManager.getInstance().isReady();
    }

    /**
     * Check generated resources update data and display if need
     * 
     * @param editor
     */
    public void updateGenerated(AbstractEditor editor) {
        if (ensembleToolResourcesMap.get(editor) == null) {
            return;
        }
        if (ensembleToolResourcesMap.get(editor).getUserGeneratedRscs() == null
                || ensembleToolResourcesMap.get(editor).getUserGeneratedRscs()
                        .isEmpty())
            return;
        // Check each resource if need update
        for (GenericResourceHolder rsc : ensembleToolResourcesMap.get(editor)
                .getUserGeneratedRscs()) {
            // TODO
        }
    }

    /**
     * post process in this level
     * 
     * @param editor
     */
    public void disposal(AbstractEditor editor) {
        clearResourceList(editor);
        saveResourceList(editor);

        // notify client
        notifyClientListChanged();
    }

    /**
     * Marked ResourceList change event should be sent to client by the event
     * bus. The client can be GUI, calculator, etc, ...
     */
    private void notifyClientListChanged() {

        if (isToolLayerReady()) {
            EnsembleToolManager.getInstance().refreshView();
        }
    }

    /**
     * Save the registered resource list into a file
     * 
     * @param editor
     */
    private void saveResourceList(AbstractEditor editor) {
        clearResourceList(editor);

        // notify client
        notifyClientListChanged();
    }

    /**
     * post process for other interfaces
     * 
     */
    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {

        // TODO Find editor for resource ...

        // TODO ResourceManager.getInstance().unregisterResource(rsc);
        notifyClientListChanged();
    }

    /**
     * Set ensemble calculation resource name.
     * 
     * @param editor
     * @param rscGroupName
     */
    public void setEnsembleCalculationResourceName(AbstractEditor editor,
            String rscGroupName) {

        if (ensembleToolResourcesMap.get(editor) != null) {
            ensembleToolResourcesMap.get(editor)
                    .setEnsembleCalculationResource(rscGroupName);
        }
    }

    /**
     * Get ensemble calculation resource name.
     * 
     * @param editor
     * @return
     */
    public String getEnsembleCalculationResourceName(AbstractEditor editor) {

        String name = "";
        if (ensembleToolResourcesMap.get(editor) != null) {
            name = ensembleToolResourcesMap.get(editor)
                    .getEnsembleCalculationResource();
        }
        return name;
    }

}
