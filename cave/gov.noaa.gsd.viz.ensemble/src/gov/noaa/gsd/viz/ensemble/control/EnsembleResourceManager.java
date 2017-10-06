package gov.noaa.gsd.viz.ensemble.control;

import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.HistogramGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.NavigatorResourceList;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.services.IServiceLocator;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;

/**
 * (This class only interacts with CAVE if the Ensemble Tool is editable.)
 * 
 * 
 * This is the key architectural support class connecting the ensemble core
 * controller (<code>EnsembleTool</code>, ensemble tool viewer and D2D core,
 * though the design attempts to minimize any coupling; this class is mainly
 * coupled at the "refresh resources" event notification, notifying the core
 * controller that the gui needs to be refreshed (see method
 * <code>notifyClientListChanged</code>.
 * 
 * 
 * >>> Use Case <<<
 * 
 * When the ensemble tool is "editable" and the user loads product resources,
 * this class maintains, and allows access, to those resources.
 * 
 * Also, when the ensemble tool is "editable" there is guaranteed to be a tool
 * layer (<code>EnsembleToolLayer</code>) that offers a proxy/delegator
 * interface to those resources.
 * 
 * This class is fed these resources through the renderable display customizer
 * <code>EnsembleToolDisplayCustomizer</code> but only if they are deemed
 * "compatible" resources. It is the ensemble tool layer, of course, which
 * contains the <code>EnsembleToolLayer.isResourceCompatible </code> method.
 * 
 * It is also the tool layer which allows this class to pull relevant resources
 * for the active (and tool layer enabled) display. Storage and retrieval of
 * these resources are obtained via the navigator resource list (see
 * <code>Map<EnsembleToolLayer, NavigatorResourceList></code> ).
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
 * Nov, 2015      12977     polster  Make ingest poll asynchronously
 * 
 * </pre>
 */

public class EnsembleResourceManager implements IDisposeListener,
        IResourceRegisteredProvider {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnsembleResourceManager.class);

    /*
     * The single instance of this resource manager.
     */
    private static EnsembleResourceManager SINGLETON = null;

    /**
     * Get the resource manager singleton.
     * 
     * @return
     */
    public static EnsembleResourceManager getInstance() {
        if (SINGLETON == null) {
            SINGLETON = new EnsembleResourceManager();
        }
        return SINGLETON;
    }

    /**
     * Get the resource manager singleton.
     * 
     * @return
     */
    public static IResourceRegisteredProvider getResourceProvider() {
        return (IResourceRegisteredProvider) SINGLETON;
    }

    private final double DEFAULT_DENSITY = 0.15;

    /**
     * The ensemble marked resource list holds the loaded and generated
     * resources and flags, is used by ensemble display, GUI and calculation.
     */
    private ConcurrentHashMap<EnsembleToolLayer, NavigatorResourceList> ensembleToolResourcesMap;

    private ArrayBlockingQueue<AbstractVizResource<?, ?>> incomingSpooler = null;

    protected PollForIncomingResources registerIncomingResources = null;

    private List<VisibilityAndColorChangedListener> visibilityAndColorChangedListeners = null;

    private List<IResourceRegisteredListener> resourceRegisteredListeners = null;

    /**
     * The listeners to any resource, GUI... change event Register to any bus or
     * event list for to catch interesting events
     */
    private EnsembleResourceManager() {
        ensembleToolResourcesMap = new ConcurrentHashMap<>();
        visibilityAndColorChangedListeners = new CopyOnWriteArrayList<>();

        incomingSpooler = new ArrayBlockingQueue<>(500);
        resourceRegisteredListeners = new ArrayList<>();

        checkIsVBAvailable();
    }

    /**
     * Create the VB dialog if it has not yet been created. Then force it to be
     * hidden.
     * 
     * TODO: This is needed as of the 16.2.2 release as the Matrix navigation
     * mode will not work without it. For a future release (e.g. 16.4.1) we
     * would like to have the volume browser plugin allow us to call the correct
     * metadata initializers (i.e. for accessing metadata such as fields,
     * planes, and sources) so we don't have to open the entire VB user
     * interface.
     */
    private void checkIsVBAvailable() {

        if (!PlatformUI.getWorkbench().isClosing()) {
            if (VolumeBrowserAction.getVolumeBrowserDlg() == null) {

                /*
                 * Obtain IServiceLocator implementer, e.g. from
                 * PlatformUI.getWorkbench()
                 */
                IServiceLocator serviceLocator = PlatformUI.getWorkbench();

                ICommandService commandService = (ICommandService) serviceLocator
                        .getService(ICommandService.class);

                Command command = commandService
                        .getCommand("com.raytheon.viz.volumebrowser.volumeBrowserRef");

                /*
                 * Optionally pass a ExecutionEvent instance, default no-param
                 * arg creates blank event
                 */

                try {

                    command.executeWithChecks(new ExecutionEvent());
                } catch (ExecutionException | NotDefinedException
                        | NotEnabledException | NotHandledException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

            }

            VolumeBrowserAction.getVolumeBrowserDlg().hide();

        }
    }

    public void startSpooler() {
        registerIncomingResources = new PollForIncomingResources(
                "Ensemble Resource Spooler");
        registerIncomingResources.setSystem(true);
        registerIncomingResources.setPriority(Job.SHORT);
        registerIncomingResources.schedule();
    }

    /*
     * Viz resources register with the ensemble tool via this thread-safe FIFO
     * queue.
     */
    protected void addResourceForRegistration(AbstractVizResource<?, ?> rsc) {
        incomingSpooler.add(rsc);
        startSpooler();
    }

    /**
     * Get the resources in an editor.
     * 
     * @param toolLayer
     *            - The tool layer associated with the editor/window into which
     *            the resources are loaded .
     * @return - list of resources.
     */
    public NavigatorResourceList getResourceList(EnsembleToolLayer toolLayer) {

        NavigatorResourceList list = null;

        if (toolLayer != null) {
            list = ensembleToolResourcesMap.get(toolLayer);
        }
        return list;
    }

    public boolean isEmpty(EnsembleToolLayer toolLayer) {
        NavigatorResourceList list = ensembleToolResourcesMap.get(toolLayer);
        if (list == null || list.isEmpty()) {
            return true;
        }
        return false;
    }

    public static boolean isTimeEmpty(String timeBasisLegend) {
        return ((timeBasisLegend == null) || (timeBasisLegend.length() == 0) || (timeBasisLegend
                .compareTo(NavigatorResourceList.TIME_BASIS_EMPTY_STR) == 0));
    }

    /**
     * Find a visible and color listener by resource.
     */
    protected VisibilityAndColorChangedListener findListenerByResource(
            AbstractVizResource<?, ?> rsc) {
        VisibilityAndColorChangedListener foundListener = null;
        for (VisibilityAndColorChangedListener vccl : visibilityAndColorChangedListeners) {
            if (vccl.resource == rsc) {
                foundListener = vccl;
                break;
            }
        }
        return foundListener;
    }

    /**
     * Given a resource to load, find the EnsembleToolLayer in the same display
     * in order to be able to associate the incoming resource with the proper
     * tool layer.
     */
    public static EnsembleToolLayer getToolLayer(AbstractVizResource<?, ?> rsc) {
        EnsembleToolLayer toolLayer = null;

        /* find the ensemble tool layer */
        ResourceList r = rsc.getDescriptor().getResourceList();
        List<AbstractVizResource<?, ?>> resources = r
                .getResourcesByType(EnsembleToolLayer.class);
        if (resources == null || resources.isEmpty()) {
            return null;
        }
        AbstractVizResource<?, ?> innerResource = resources.get(0);
        if (innerResource == null) {
            return null;
        }
        toolLayer = (EnsembleToolLayer) innerResource;
        return toolLayer;
    }

    /**
     * Register a loaded model product in ensemble status
     * 
     * @param rsc
     *            - the loaded product resource
     * @param guiUpdate
     *            - if need update GUI
     */
    public synchronized void registerResource(AbstractVizResource<?, ?> rsc) {

        if (rsc == null) {
            return;
        }

        EnsembleToolLayer toolLayer = EnsembleResourceManager.getToolLayer(rsc);
        if (toolLayer == null) {
            return;
        }

        /*
         * is this a previously unknown tool layer? then create the resource
         * list and map it to the tool layer ...
         */
        if (ensembleToolResourcesMap.get(toolLayer) == null) {
            ensembleToolResourcesMap.put(toolLayer, new NavigatorResourceList(
                    toolLayer));
        }

        // no nulls or duplicates
        if (rsc == null
                || ensembleToolResourcesMap.get(toolLayer)
                        .containsResource(rsc)) {
            return;
        }

        /*
         * TODO: Must refactor so we don't use setSystemResource(boolean)
         * method.
         */
        rsc.getProperties().setSystemResource(true);

        rsc.registerListener((IDisposeListener) this);

        /*
         * For now, we suppress the 'Load as Image' menu item from the context
         * sensitive pop-up, for *both the Legends and Matrix modes.
         * 
         * TODO: This will probably want to eventually be put back in for Matrix
         * mode.
         */
        if (rsc.hasCapability(DisplayTypeCapability.class)) {
            rsc.getCapability(DisplayTypeCapability.class)
                    .setSuppressingMenuItems(true);
        }

        /*
         * TODO: We may need to eventually refactor the Legend browser and
         * Matrix navigator into two separate views/plug-ins. This type of
         * constant checking for which mode is a bit painful.
         */
        if (toolLayer.getToolMode() == EnsembleTool.EnsembleToolMode.LEGENDS_PLAN_VIEW) {

            // Set to default density for all loaded resources if can
            // But it may be unmatched with current display. Fix it later
            if (rsc.getCapability(DensityCapability.class) != null) {
                DensityCapability densityCapability = (DensityCapability) rsc
                        .getCapability(DensityCapability.class);
                densityCapability.setDensity(DEFAULT_DENSITY);
            }
        }

        AbstractResourceHolder ensToolResource = AbstractResourceHolder
                .createResourceHolder(rsc, true);
        ensToolResource.setGenerated(false);

        ensembleToolResourcesMap.get(toolLayer).add(ensToolResource);
        checkExistingRegisteredResources(toolLayer);

        /*
         * TODO: This resource manager should not have to worry or even know
         * about which mode the tool is in.
         * 
         * Currently, the only listeners are the field/plane controls in the
         * matrix navigator. This unusual addition is so that the UI controls
         * are updated with the resource color immediately at load time.
         */
        if (toolLayer.getToolMode() == EnsembleTool.EnsembleToolMode.MATRIX) {
            for (IResourceRegisteredListener listener : resourceRegisteredListeners) {
                listener.resourceRegistered(rsc);
            }
        }

        notifyClientListChanged();

        visibilityAndColorChangedListeners
                .add(new VisibilityAndColorChangedListener(rsc));

        /*
         * now update the calculation ensemble resource, if not already set ...
         * there can only be one, and it will be the first ensemble resource
         * loaded into this editor ...
         * ensembleToolResourcesMap.get(toolLayer).updateEnsembleCalculationResource
         * ();
         */

    }

    /**
     * Remove a resource from the tracking list.
     * 
     * @param gr
     *            - the tracked resource.
     * 
     * @param toolLayer
     *            - The tool layer associated with the editor/window into which
     *            the resource is loaded.
     * 
     * @param notifyGUI
     *            - if update the GUI.
     */
    public synchronized void unregisterResource(AbstractResourceHolder gr,
            EnsembleToolLayer toolLayer, boolean notifyGUI) {
        if (toolLayer == null
                || gr == null
                || ensembleToolResourcesMap.get(toolLayer).getResourceHolders()
                        .isEmpty())
            return;

        VisibilityAndColorChangedListener vccl = findListenerByResource(gr
                .getRsc());
        if (vccl != null) {
            visibilityAndColorChangedListeners.remove(vccl);
        }

        ensembleToolResourcesMap.get(toolLayer).remove(gr);
        gr.getRsc().unregisterListener((IDisposeListener) this);

        checkExistingRegisteredResources(toolLayer);

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
    public synchronized void registerGenerated(AbstractVizResource<?, ?> rsc) {

        if (rsc == null) {
            return;
        }

        EnsembleToolLayer toolLayer = EnsembleResourceManager.getToolLayer(rsc);
        if (toolLayer == null) {
            return;
        }

        /*
         * This may be the first resource ever registered using this editor. If
         * so, then create the resource list and associate it with the editor
         * using the map ...
         */

        if (ensembleToolResourcesMap.get(toolLayer) == null) {
            ensembleToolResourcesMap.put(toolLayer, new NavigatorResourceList(
                    toolLayer));
        }

        /*
         * Only one instance of a generated resource is saved. If a duplicate is
         * created then have it overwrite the existing one.
         */
        for (AbstractResourceHolder gr : ensembleToolResourcesMap
                .get(toolLayer).getUserGeneratedRscs()) {

            // Remove any resource in this list, since it was unloaded/ isn't
            // existing.
            if (!gr.getRsc().getDescriptor().getResourceList()
                    .containsRsc(gr.getRsc())) {
                ensembleToolResourcesMap.get(toolLayer).remove(gr);

            } else if (rsc.getClass().cast(rsc).getName()
                    .equals(gr.getRsc().getClass().cast(gr.getRsc()).getName())) {
                // Same generated resource name, unload old one
                ensembleToolResourcesMap.get(toolLayer).remove(gr);
                gr.getRsc().getDescriptor().getResourceList()
                        .removeRsc(gr.getRsc());
            }
        }

        /*
         * Set resource as a system resource so we don't show the legend in the
         * main map, because we will display it in the ensemble navigator view.
         * 
         * TODO: Must refactor so we don't use setSystemResource(boolean)
         * method.
         */
        rsc.getProperties().setSystemResource(true);
        rsc.registerListener((IDisposeListener) this);

        if (rsc.hasCapability(DisplayTypeCapability.class)) {
            rsc.getCapability(DisplayTypeCapability.class)
                    .setSuppressingMenuItems(true);
        }

        AbstractResourceHolder ensToolResource = AbstractResourceHolder
                .createResourceHolder(rsc, true);
        ensToolResource.setGenerated(true);
        ensembleToolResourcesMap.get(toolLayer).add(ensToolResource);

        checkExistingRegisteredResources(toolLayer);

        visibilityAndColorChangedListeners
                .add(new VisibilityAndColorChangedListener(rsc));

        notifyClientListChanged();

        /*
         * Turns off other same mode histogram tools.
         * 
         * Keeps only one tool working at same time.
         */
        if (ensToolResource instanceof HistogramGridResourceHolder) {
            EnsembleResourceManager.getInstance().turnOffOtherHistograms(
                    (HistogramGridResourceHolder) ensToolResource);

        }

    }

    /**
     * Remove a generated resource from the tracking list.
     * 
     * @param gr
     *            - the tracked resource.
     * @param toolLayer
     *            - The tool layer associated with the editor/window into which
     *            the resource is loaded .
     * @param notifyGUI
     *            - if update the GUI.
     */
    public synchronized void unregisterGenerated(AbstractResourceHolder gr,
            EnsembleToolLayer toolLayer, boolean notifyGUI) {
        if (gr == null
                || ensembleToolResourcesMap.get(toolLayer).getResourceHolders()
                        .isEmpty()) {
            return;
        }

        VisibilityAndColorChangedListener vccl = findListenerByResource(gr
                .getRsc());
        if (vccl != null) {
            visibilityAndColorChangedListeners.remove(vccl);
        }

        ensembleToolResourcesMap.get(toolLayer).remove(gr);
        gr.getRsc().unregisterListener((IDisposeListener) this);

        // notify client the generated resource change?

        checkExistingRegisteredResources(toolLayer);

        // if requested, notify client the known loaded resources have changed
        if (notifyGUI) {
            notifyClientListChanged();
        }
    }

    /**
     * Get the time series point location
     * 
     * @param activeToolLayer
     *            - the time series tool layer
     * 
     * @return - point location as a String
     */
    public String getTimeSeriesPoint(EnsembleToolLayer toolLayer) {
        String s = "";
        if (toolLayer != null && getResourceList(toolLayer) != null) {
            s = getResourceList(toolLayer).getTimeSeriesPoint();
        }
        return s;
    }

    /**
     * Get the resource name string
     * 
     * @param activeToolLayer
     *            - the time series tool layer
     * 
     * @return- resource name
     */
    public String getTimeBasisResourceName(EnsembleToolLayer toolLayer) {

        String s = "";
        if (toolLayer != null && getResourceList(toolLayer) != null) {
            s = getResourceList(toolLayer).getTimeBasisResourceName();
        }
        return s;

    }

    /**
     * Get the resource legend time
     * 
     * @param activeToolLayer
     *            - the time series tool layer
     * 
     * @return- legend time
     */
    public String getTimeBasisLegendTime(EnsembleToolLayer toolLayer) {

        String s = "";
        if (toolLayer != null && getResourceList(toolLayer) != null) {
            s = getResourceList(toolLayer).getTimeBasisLegendTime();
        }
        return s;

    }

    /**
     * Handle ENS GUI change
     * 
     * @param toolLayer
     *            - The tool layer associated with the editor/window into which
     *            the resources are loaded.
     */
    public void updateFrameChanges(EnsembleToolLayer toolLayer) {
        checkExistingRegisteredResources(toolLayer);
        // update generated ensemble Resource if need
        updateGenerated(toolLayer);
    }

    /**
     * Match up the registered resource list within editor(s) and GUI. Verify if
     * the resources in the list exist. Remove any resource in this list, if it
     * was unloaded. Notify GUI if there is any change.
     */
    public void checkExistingRegisteredResources(EnsembleToolLayer toolLayer) {

        if (!EnsembleTool.getInstance().isToolEditable()) {
            return;
        }
        if (ensembleToolResourcesMap.get(toolLayer) == null) {
            return;
        }

        if (ensembleToolResourcesMap.get(toolLayer).getAllRscsAsList() == null
                || ensembleToolResourcesMap.get(toolLayer).getAllRscsAsList()
                        .isEmpty())
            return;

        for (AbstractResourceHolder gr : ensembleToolResourcesMap
                .get(toolLayer).getAllRscsAsList()) {
            // verify if the resources in the list are existing.
            if (!gr.getRsc().getDescriptor().getResourceList()
                    .containsRsc(gr.getRsc())) {

                /*
                 * Remove any resource in this list, since it was unloaded it
                 * won't exist.
                 */
                ensembleToolResourcesMap.get(toolLayer).remove(gr);

                /* TODO: notify GUI if there is any change. */
                // notifyClientListChanged();
            } else {
                /*
                 * don't show legend when the ensemble tool is controlling the
                 * resource
                 */
                gr.getRsc().getProperties().setSystemResource(true);

                /*
                 * Marks to unselected if the resource is not visible set
                 * resource selection to true.
                 */
                if (gr.getRsc().getProperties().isVisible()) {
                    gr.setSelected(true);
                } else {
                    gr.setSelected(false);
                }
            }
        }
    }

    /**
     * Return all ensemble-related resources not currently registered with this
     * resource manager.
     * 
     * This method will allow the Ensemble Tool to read any given editor and
     * find the resources not yet registered, so, for example, we can then
     * register those resources.
     * 
     * @param editor
     * @return
     */
    public ResourceList getUnregisteredResources(IDisplayPaneContainer editor) {

        ResourceList unRegisteredResources = new ResourceList();

        // The registered resources for this editor in the manager
        List<AbstractResourceHolder> resourceList = ensembleToolResourcesMap
                .get(editor).getAllRscsAsList();

        IDescriptor desc = (editor.getActiveDisplayPane().getDescriptor());
        ResourceList rscList = desc.getResourceList();
        for (ResourcePair rp : rscList) {

            AbstractVizResource<?, ?> rsc = rp.getResource();
            if ((rsc instanceof AbstractGridResource)
                    || (rsc instanceof TimeSeriesResource)
                    || (rsc instanceof HistogramResource)) {

                boolean isRegistered = false;
                for (AbstractResourceHolder rcsHolder : resourceList) {
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
     * Check generated resources update data and display if need
     * 
     * @param toolLayer
     *            - The tool layer associated with the editor/window into which
     *            the resource is loaded .
     */
    public void updateGenerated(EnsembleToolLayer toolLayer) {
        if (toolLayer == null
                || ensembleToolResourcesMap.get(toolLayer) == null) {
            return;
        }
        if (ensembleToolResourcesMap.get(toolLayer).getUserGeneratedRscs() == null
                || ensembleToolResourcesMap.get(toolLayer)
                        .getUserGeneratedRscs().isEmpty())
            return;
        // Check each resource if need update
        for (AbstractResourceHolder rsc : ensembleToolResourcesMap.get(
                toolLayer).getUserGeneratedRscs()) {
            /**
             * TODO : Generated resources will not get auto-updated by the
             * server, so leave this for future use.
             */
        }
    }

    /**
     * Completely delete all owned resources
     */
    public void dispose() {

        /*
         * Clean out the managed resources ... this should be unnecessary as the
         * ensemble tool layer controls the resource cleanup. But it never hurts
         * to be a bit over precautious.
         */
        Set<Entry<EnsembleToolLayer, NavigatorResourceList>> set = ensembleToolResourcesMap
                .entrySet();
        Iterator<Entry<EnsembleToolLayer, NavigatorResourceList>> iterator = set
                .iterator();
        Entry<EnsembleToolLayer, NavigatorResourceList> entry = null;
        while (iterator.hasNext()) {
            entry = iterator.next();
            NavigatorResourceList rscList = entry.getValue();
            rscList.clear();
        }

        ensembleToolResourcesMap.clear();
        ensembleToolResourcesMap = null;

        incomingSpooler.clear();
        incomingSpooler = null;

        resourceRegisteredListeners.clear();
        resourceRegisteredListeners = null;

        visibilityAndColorChangedListeners.clear();
        visibilityAndColorChangedListeners = null;

        SINGLETON = null;
    }

    /**
     * Marked ResourceList change event should be sent to client by the event
     * bus. The client can be GUI, calculator, etc, ...
     */
    protected void notifyClientListChanged() {

        if (EnsembleTool.getInstance().isToolEditable()) {
            EnsembleTool.getInstance().refreshView();
        }
    }

    /**
     * post process for other interfaces
     * 
     */
    @Override
    public void disposed(AbstractVizResource<?, ?> rsc) {

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

    /**
     * This job sleeps for a poll-period and is only active when there are
     * resources being registered.
     * 
     * It runs for as long as the Ensemble Tool is open (i.e. not disposed). It
     * wakes when new resources need to be registered.
     * 
     */
    private class PollForIncomingResources extends Job {

        private IStatus status = Status.OK_STATUS;

        private AbstractVizResource<?, ?> nextRsc = null;

        public PollForIncomingResources(String name) {
            super(name);
            status = Status.OK_STATUS;
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {

            if (!monitor.isCanceled()) {
                nextRsc = incomingSpooler.poll();
                EnsembleResourceManager.getInstance().registerResource(nextRsc);
            }
            if (monitor.isCanceled()) {
                status = Status.CANCEL_STATUS;
            }

            return status;

        }
    }

    protected class VisibilityAndColorChangedListener implements
            IRefreshListener, IDisposeListener {

        public final AbstractVizResource<?, ?> resource;

        private boolean visible;

        private RGB color;

        protected VisibilityAndColorChangedListener(
                AbstractVizResource<?, ?> resource) {
            this.resource = resource;
            this.visible = resource.getProperties().isVisible();
            this.color = resource.getCapability(ColorableCapability.class)
                    .getColor();
            resource.registerListener((IRefreshListener) this);
            resource.registerListener((IDisposeListener) this);
        }

        @Override
        public void disposed(AbstractVizResource<?, ?> rsc) {
            if (rsc == resource && resource != null) {
                resource.unregisterListener((IRefreshListener) this);
                resource.unregisterListener((IDisposeListener) this);
            }
        }

        @Override
        public void refresh() {
            boolean visible = resource.getProperties().isVisible();
            if (this.visible != visible) {
                notifyClientListChanged();
                this.visible = visible;
            }
            RGB color = resource.getCapability(ColorableCapability.class)
                    .getColor();
            if (!this.color.equals(color)) {
                notifyClientListChanged();
                this.color = color;
            }
        }

    }

    public void removeMapping(EnsembleToolLayer toolLayer) {
        ensembleToolResourcesMap.remove(toolLayer);
    }

    /**
     * Turn off all other same mode histogram tools, when a histogram tool is
     * turn on or loading.
     * 
     * TODO: Need do more post processes histograms. Should do similar process
     * other loaded tools too.
     * 
     * @param hgr
     *            -the histogram tool is turn on or loading
     */
    public void turnOffOtherHistograms(HistogramGridResourceHolder hgr) {
        // Get current Layer
        EnsembleToolLayer toolLayer = EnsembleTool.getInstance().getToolLayer();

        // Search for same mode histogram tools
        NavigatorResourceList rList = EnsembleResourceManager.getInstance()
                .getResourceList(toolLayer);
        if (rList == null || rList.isEmpty()) {
            return;
        }

        // Turn off same mode histogram tools
        boolean isAnyTurnedOff = false;
        for (AbstractResourceHolder gr : rList.getUserGeneratedRscs()) {
            if (!(gr instanceof HistogramGridResourceHolder)) {
                continue;
            }

            if (hgr != gr
                    && ((HistogramResource<?>) (hgr.getRsc())).getMode() == ((HistogramResource<?>) (gr
                            .getRsc())).getMode() && gr.isSelected()) {
                gr.getRsc().getProperties().setVisible(false);
                gr.getRsc().issueRefresh();
                gr.setSelected(false);
                isAnyTurnedOff = true;
            }
        }

        if (isAnyTurnedOff) {
            // TODO Update histogram tools, Implement later
        }
    }

    /**
     * Update related generated resource(s) display when a resource changed.
     * Only process the Distribution Viewer in this release.
     * 
     * @param rh
     *            -The changed resource holder which can be a loaded or
     *            generated resource holder
     */
    public void updateGenerated(AbstractResourceHolder rh) {
        if (rh == null) {
            return;
        }

        // Get current Layer
        EnsembleToolLayer toolLayer = EnsembleTool.getInstance().getToolLayer();

        // Search for same mode histogram tools
        NavigatorResourceList rList = EnsembleResourceManager.getInstance()
                .getResourceList(toolLayer);
        if (rList == null || rList.isEmpty()) {
            return;
        }

        List<AbstractResourceHolder> generatedResources = rList
                .getUserGeneratedRscs();

        if (rh.isGenerated()) {
            /*
             * Update Generated products. Currently just clear the distribution
             * viewer display area.
             * 
             * TODO: This needs further evaluation in the next delivery.
             */
            if (rh instanceof HistogramGridResourceHolder
                    && ((HistogramResource<?>) (rh.getRsc())).getMode() == HistogramResource.DisplayMode.GRAPHIC_HISTGRAM
                    && EnsembleTool.getInstance().getEnsembleToolViewer() != null) {
                clearDistributionViewer();
            }

            return;
            // Loaded resource in Map may impact to the related loaded tool(s)
        } else if (rh instanceof GridResourceHolder && !rh.isGenerated()
                && rh.getRsc().getDescriptor() instanceof MapDescriptor
                && generatedResources != null && !generatedResources.isEmpty()) {
            for (AbstractResourceHolder gr : generatedResources) {
                /*
                 * A related generated resource is with same level and unit as
                 * the changed resource.
                 */
                if (!gr.getUnits().contentEquals(rh.getUnits())
                        || !gr.getLevel().contentEquals(rh.getLevel())) {
                    continue;
                }
                // Impact to a related Distribution Viewer, clear display.
                if (gr instanceof HistogramGridResourceHolder
                        && ((HistogramResource<?>) (gr.getRsc())).getMode() == HistogramResource.DisplayMode.GRAPHIC_HISTGRAM
                        && EnsembleTool.getInstance().getEnsembleToolViewer() != null
                        && gr.isSelected()) {
                    clearDistributionViewer();
                }

                /*
                 * TODO: Need to make sure a calculation is updated if it was
                 * created with this updated generated product.
                 */

                /*
                 * TODO: Need to make sure any related tool products are updated
                 * if they are impacted by this updated generated product.
                 */

            }

        }
        // TODO:Add loaded process for others like time series.
    }

    /**
     * Clear the distribution viewer.
     */
    public void clearDistributionViewer() {
        EnsembleTool.getInstance().getEnsembleToolViewer()
                .getDistributionViewer().getGhGUI().getDisp()
                .clearDistributionViewer();
    }

    @Override
    public void addResourceRegisteredListener(
            IResourceRegisteredListener listener) {
        resourceRegisteredListeners.add(listener);
    }

}
