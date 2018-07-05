package gov.noaa.gsd.viz.ensemble.navigator.ui.layer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;

import gov.noaa.gsd.viz.ensemble.control.EnsembleTool;
import gov.noaa.gsd.viz.ensemble.control.IResourceRegisteredListener;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.EnsembleMembersHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GeneratedTimeSeriesResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.GridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.HistogramGridResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.control.contour.ContourControlCapability;
import gov.noaa.gsd.viz.ensemble.display.rsc.GeneratedEnsembleGridResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.histogram.HistogramResource;
import gov.noaa.gsd.viz.ensemble.display.rsc.timeseries.GeneratedTimeSeriesResource;
import gov.noaa.gsd.viz.ensemble.util.Utilities;

/**
 * A list which holds Ensemble Tool resources. Any resources contained in this
 * list class will be associated with an ensemble tool layer of which there is
 * only one ensemble tool layer per editor. It is assumed this list is used to
 * allow the user to see what resources are loaded, into the ensemble tool, in
 * the ensemble viewer.
 * 
 * Offers accessor methods to get different flavors (generated, ensemble,
 * individual, tool, etc) of resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 17  2017   19443     polster     Initial creation
 * Jun 27  2017   19325     jing        Added contour capability.
 * Dec 01, 2017   41520     polster     Added find resource method
 * 
 * </pre>
 * 
 * @author jing
 * @author polster
 * @version 1.0
 */

public class NavigatorResourceList extends ResourceList {

    private static final long serialVersionUID = -2159782570794461252L;

    public static final double DEFAULT_DENSITY = 0.15;

    public final static String TIME_SERIES_POINT_EMPTY_STR = "<no associated point>";

    public final static String TIME_BASIS_EMPTY_STR = "<no associated time>";

    final private List<AbstractResourceHolder> ensembleToolResources = new CopyOnWriteArrayList<>();

    private EnsembleToolLayer theToolLayer;

    public NavigatorResourceList(EnsembleToolLayer layer) {
        theToolLayer = layer;
    }

    @Override
    public boolean add(AbstractVizResource<?, ?> rsc) {

        AbstractResourceHolder rscHolder = AbstractResourceHolder
                .createResourceHolder(rsc);

        // no duplicates
        if (containsRsc(rsc)) {
            return false;
        }

        removeSimilarGeneratedRsc(rscHolder);

        boolean added = super.add(rsc);
        if (added) {
            configureResourceForEnsembleTool(rscHolder);
            addResourceHolderForEnsembleTool(rscHolder);
            /* no need to repopulate on refresh */
            theToolLayer.forceRefresh(false);
        }

        return added;
    }

    /**
     * Add the resource holder to the holder list and then update the holder
     * list to add/update any ensemble member holder entries.
     */
    public void addToProductList(AbstractResourceHolder arh) {
        synchronized (ensembleToolResources) {
            ensembleToolResources.add(arh);
            updateEnsembleEntries();
        }
    }

    private void addResourceHolderForEnsembleTool(
            AbstractResourceHolder rscHolder) {

        rscHolder.getRsc().registerListener(rscHolder);

        addToProductList(rscHolder);

        /*
         * Make sure we turn off the image load capability for generated
         * resources
         */
        if (rscHolder.isGenerated() && rscHolder.getRsc()
                .hasCapability(DisplayTypeCapability.class)) {
            rscHolder.getRsc().getCapability(DisplayTypeCapability.class)
                    .setSuppressingMenuItems(true);
        }

        if (rscHolder.getRsc() instanceof AbstractGridResource<?>) {
            if (((AbstractGridResource<?>) rscHolder.getRsc())
                    .getDisplayType() == DisplayType.IMAGE) {
                /*
                 * Treat an image D2DResource as generated resource to be
                 * uncounted in the ensemble calculation.
                 */
                rscHolder.setGenerated(true);
            } else {
                rscHolder.setGenerated(false);
            }
        } else {
            /*
             * For contour resource
             */
            rscHolder.setGenerated(false);
        }

        /*
         * Turns off other same mode histogram tools.
         * 
         * Keeps only one tool working at same time.
         */
        if (rscHolder instanceof HistogramGridResourceHolder) {
            turnOffOtherHistograms((HistogramGridResourceHolder) rscHolder);
        }
    }

    @Override
    public boolean removeRsc(AbstractVizResource<?, ?> rsc) {

        boolean wasRemoved = false;
        if (super.containsRsc(rsc)) {
            for (AbstractResourceHolder arh : ensembleToolResources) {
                if (arh instanceof EnsembleMembersHolder) {
                    continue;
                }
                if (arh.getRsc().equals(rsc)) {
                    removeResourceHolderForEnsembleTool(arh);
                }
            }
            wasRemoved = super.removeRsc(rsc);
            /* no need to repopulate on refresh */
            theToolLayer.forceRefresh(false);
        }
        return wasRemoved;
    }

    private void removeResourceHolderForEnsembleTool(
            AbstractResourceHolder arh) {
        arh.getRsc().unregisterListener(arh);
        this.removeFromProductList(arh);
    }

    private void removeFromProductList(AbstractResourceHolder arh) {
        boolean wasRemoved = false;
        synchronized (ensembleToolResources) {
            wasRemoved = ensembleToolResources.remove(arh);
            if (wasRemoved) {
                updateEnsembleEntries();
            }
        }
    }

    /*
     * Don't allow the removeAll behavior.
     */
    @Override
    public boolean removeAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    /**
     * Walk the resource holder list and add any new or update any existing
     * ensemble information if necessary. This includes associating parents to
     * ensemble members and likewise defining which EnsembleMembersHolder
     * (parent) instances have what children.
     */
    private void updateEnsembleEntries() {

        List<AbstractResourceHolder> newList = new ArrayList<>(
                ensembleToolResources);

        List<EnsembleMembersHolder> ensembleMembersOnly = new ArrayList<>();

        /*
         * remove previous ensemble member holder entries
         */
        for (AbstractResourceHolder rsc : newList) {
            if (rsc instanceof EnsembleMembersHolder) {
                ensembleMembersOnly.add((EnsembleMembersHolder) rsc);
            }
        }
        Iterator<AbstractResourceHolder> iterHolder = newList.iterator();
        AbstractResourceHolder rscHolder = null;
        while (iterHolder.hasNext()) {
            rscHolder = iterHolder.next();
            for (EnsembleMembersHolder emh : ensembleMembersOnly) {
                if (emh == rscHolder) {
                    iterHolder.remove();
                    break;
                }
            }
        }

        List<AbstractResourceHolder> rscs = getUserLoadedRscs();

        String currGroupName = null;
        /*
         * Loop through and find the all possible ensemble names (aka group
         * names) and store them in a set of strings (no duplicates allowed) so
         * sub-members such as perturbations will only be counted once.
         */
        Set<String> rscGroupNames = new HashSet<>();
        for (AbstractResourceHolder arh : rscs) {

            /*
             * get any resource that has a perturbation/member id and save the
             * common prefix to be used as the ensemble group name.
             */
            if (arh.getEnsembleId() != null
                    && arh.getEnsembleId().length() > 0) {

                currGroupName = arh.getGroupName();
                if (currGroupName == null || currGroupName.length() == 0) {
                    continue;
                }
                if (!rscGroupNames.contains(currGroupName)) {
                    rscGroupNames.add(currGroupName);
                }

            }
        }

        EnsembleMembersHolder ensMembersHolder = null;
        ensembleMembersOnly.clear();

        /*
         * Create ensemble resource holders which will internally set
         * child/parent relationships
         */
        Iterator<String> iterString = rscGroupNames.iterator();
        while (iterString.hasNext()) {
            currGroupName = iterString.next();
            List<AbstractResourceHolder> members = findAssociatedMembers(
                    currGroupName);
            if (members.size() > 1) {
                ensMembersHolder = new EnsembleMembersHolder(currGroupName,
                        members);
                ensembleMembersOnly.add(ensMembersHolder);
            }
        }
        for (EnsembleMembersHolder emh : ensembleMembersOnly) {
            newList.add(emh);
        }

        ensembleToolResources.clear();
        for (AbstractResourceHolder arh : newList) {
            ensembleToolResources.add(arh);
        }

    }

    private void configureResourceForEnsembleTool(AbstractResourceHolder rh) {

        /*
         * The matrix navigator, if open, will be listening for incoming
         * resources.
         */
        if (theToolLayer
                .getToolMode() == EnsembleTool.EnsembleToolMode.MATRIX) {
            for (IResourceRegisteredListener listener : theToolLayer
                    .getResourceRegisteredListeners()) {
                listener.resourceRegistered(rh.getRsc());
            }
        }

        // Set color
        if (rh.getRsc().hasCapability(ColorableCapability.class)) {
            ColorableCapability colorable = (ColorableCapability) rh.getRsc()
                    .getCapability(ColorableCapability.class);
            RGB color = Utilities.getRandomNiceContrastColor();
            colorable.setColor(color);
        }

        /*
         * For now, we suppress the 'Load as Image' menu item from the context
         * sensitive pop-up, for both the Legends and Matrix modes.
         */
        if (rh.getRsc().hasCapability(DisplayTypeCapability.class)
                && !rh.isGenerated()) {
            rh.getRsc().getCapability(DisplayTypeCapability.class)
                    .setSuppressingMenuItems(false);
        } else {
            rh.getRsc().getCapability(DisplayTypeCapability.class)
                    .setSuppressingMenuItems(true);
        }

        /*
         * Add "Contour Control" to grid resource displayed as contour.
         */
        if (rh.getRsc() instanceof AbstractGridResource<?>
                && ((AbstractGridResource<?>) (rh.getRsc()))
                        .getDisplayType() != DisplayType.IMAGE) {
            rh.getRsc().getCapability(ContourControlCapability.class);
        }

        /*
         * TODO: We may need to eventually refactor the Legend browser and
         * Matrix navigator into two separate views/plug-ins. This type of
         * constant checking for which mode is a bit painful.
         */
        if (theToolLayer
                .getToolMode() == EnsembleTool.EnsembleToolMode.LEGENDS_PLAN_VIEW) {

            // Set to default density for all loaded resources if can
            // But it may be unmatched with current display. Fix it later
            if (rh.getRsc().getCapability(DensityCapability.class) != null
                    && !rh.isGenerated()) {
                DensityCapability densityCapability = (DensityCapability) rh
                        .getRsc().getCapability(DensityCapability.class);
                densityCapability.setDensity(DEFAULT_DENSITY);
            }
        }

    }

    public List<AbstractResourceHolder> getResourceHolders() {
        synchronized (ensembleToolResources) {
            return ensembleToolResources;
        }
    }

    private List<AbstractResourceHolder> findAssociatedMembers(
            String resourceGroupName) {

        List<AbstractResourceHolder> rscHolders = getUserLoadedRscs();

        List<AbstractResourceHolder> members = new ArrayList<>();

        for (AbstractResourceHolder rscHolder : rscHolders) {

            if (rscHolder instanceof EnsembleMembersHolder) {
                continue;
            }

            if (rscHolder.getSpecificName().startsWith(resourceGroupName)) {
                members.add(rscHolder);
            }
        }
        List<AbstractResourceHolder> cowMembers = new CopyOnWriteArrayList<>(
                members);

        return cowMembers;
    }

    public AbstractVizResource<?, ?> findResource(
            AbstractVizResource<?, ?> srcRsc) {

        AbstractVizResource<?, ?> foundRsc = null;
        for (ResourcePair rp : EnsembleTool.getInstance()
                .getActiveResourceList()) {
            if (rp.getResource().getName().equals(srcRsc.getName())) {
                foundRsc = rp.getResource();
                break;
            }
        }
        return foundRsc;
    }

    /**
     * All resources for an editor that were not generated.
     * 
     * @return
     */
    public List<AbstractResourceHolder> getUserLoadedRscs() {

        List<AbstractResourceHolder> rscs = new ArrayList<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (emr.isIndivdualProduct() && !emr.isGenerated()) {
                rscs.add(emr);
            }
        }
        List<AbstractResourceHolder> cowRscs = new CopyOnWriteArrayList<>(rscs);

        return cowRscs;
    }

    /**
     * All loaded resources for ensemble.
     * 
     * @param descriptor
     *            : the descriptor of the resource
     * @param selected
     *            :if it's selected.
     * @return
     */
    public Map<String, List<AbstractResourceHolder>> getUserLoadedRscs(
            Class<? extends IDescriptor> descriptor, boolean selected) {

        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            /* same type descriptor like Map */
            if (emr.isIndivdualProduct() && !emr.isGenerated()
                    && emr.isSelected() == selected
                    && emr.getRsc().getDescriptor().getClass() == descriptor) {
                if (emr.getRsc().getName() == null
                        || emr.getRsc().getName().equals("")) {
                    continue;
                }
                String model = emr.getModel();
                addResourceToMap(rscMap, model, emr);
            }
        }

        return rscMap;
    }

    /**
     * All loaded resources for ensemble.
     * 
     * @param descriptor
     *            :the descriptor of the resource
     * @param selected
     *            :if it's selected.
     * @param level
     *            :level of the resource.
     * @param unit
     *            : unit of the resource.
     * @return
     */
    public Map<String, List<AbstractResourceHolder>> getUserLoadedRscs(
            Class<? extends IDescriptor> descriptor, boolean selected,
            String level, String unit) {

        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();

        for (AbstractResourceHolder emr : ensembleToolResources) {
            // don't process an ensemble holder
            if (emr.isIndivdualProduct() == false) {
                continue;
            }
            if ((descriptor == MapDescriptor.class
                    && emr.getRsc().getDescriptor() instanceof IMapDescriptor)
                    || (descriptor == TimeSeriesDescriptor.class && emr.getRsc()
                            .getDescriptor() instanceof TimeSeriesDescriptor)) {

                if (!emr.isGenerated() && emr.isSelected() == selected
                /**
                 * TODO: Remember to handle surface level name issue (e.g.
                 * "0SURFACE")
                 */
                        && level.equals(emr.getLevel())
                        && unit.equals(emr.getUnits()) && emr.getRsc()
                                .getDescriptor().getClass() == descriptor) {

                    // Model name is the first string
                    if (emr.getRsc().getName() == null
                            || emr.getRsc().getName().equals(""))
                        continue;
                    String model = emr.getModel();
                    addResourceToMap(rscMap, model, emr);
                }
            }
        }

        return rscMap;
    }

    /**
     * All loaded resources for ensemble.
     * 
     * @param descriptor
     *            :the descriptor of the resource
     * @param selected
     *            :if it's selected.
     * @param unit
     *            : unit of the resource.
     * @return
     */
    public Map<String, List<AbstractResourceHolder>> getUserLoadedRscs(
            Class<? extends IDescriptor> descriptor, boolean selected,
            String unit) {
        Map<String, List<AbstractResourceHolder>> rscMap = new ConcurrentHashMap<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            // don't process an ensemble holder
            if (emr.isIndivdualProduct() == false) {
                continue;
            }
            if (!emr.isGenerated() && emr.isSelected() == selected
                    && unit.equals(emr.getUnits())
                    // same type descriptor like Map
                    && emr.getRsc().getDescriptor().getClass() == descriptor) {

                if (emr.getRsc().getName() == null
                        || emr.getRsc().getName().equals(""))
                    continue;
                String model = emr.getModel();
                addResourceToMap(rscMap, model, emr);
            }
        }

        return rscMap;
    }

    /**
     * All generated resources by ensemble display
     * 
     * @return
     */
    public List<AbstractResourceHolder> getUserGeneratedRscs() {

        List<AbstractResourceHolder> rscs = new ArrayList<>();
        for (AbstractResourceHolder emr : ensembleToolResources) {
            if (emr.isGenerated()) {
                rscs.add(emr);
            }
        }

        List<AbstractResourceHolder> cowRscs = new CopyOnWriteArrayList<>(rscs);
        return cowRscs;
    }

    /**
     * 
     * @param rscMap
     * @param model
     * @param gr
     */
    private void addResourceToMap(
            Map<String, List<AbstractResourceHolder>> rscMap, String model,
            AbstractResourceHolder gr) {
        if (rscMap.containsKey(model)) {
            rscMap.get(model).add(gr);
        } else {
            List<AbstractResourceHolder> rscList = new CopyOnWriteArrayList<>();
            rscList.add(gr);
            rscMap.put(model, rscList);
        }
    }

    public boolean isEmpty() {
        return ensembleToolResources.isEmpty();
    }

    @Override
    public String toString() {
        StringBuffer s = new StringBuffer("ResourceList[");

        super.toString();
        if (size() > 0) {
            s.append("\n\t");
            Iterator<AbstractResourceHolder> iter = ensembleToolResources
                    .iterator();
            s.append(iter.next());
            while (iter.hasNext()) {
                s.append(",\n\t");
                s.append(iter.next());
            }
            s.append("\n");
        }

        s.append("]");
        return s.toString();
    }

    public void instantiateResources(IDescriptor descriptor,
            boolean fireListeners) {
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

        if (ensembleToolResources == null || ensembleToolResources.isEmpty()) {
            return;
        }

        // Turn off same mode histogram tools
        boolean isAnyTurnedOff = false;
        for (AbstractResourceHolder gr : getUserGeneratedRscs()) {
            if (gr instanceof EnsembleMembersHolder
                    || !(gr instanceof HistogramGridResourceHolder)) {
                continue;
            }

            if (!hgr.equals(gr) && (hgr.getRsc()
                    .getMode() == ((HistogramResource<?>) gr.getRsc()).getMode()
                    && gr.isSelected())) {
                gr.getRsc().getProperties().setVisible(false);
                gr.getRsc().issueRefresh();
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

        if (rh == null || isEmpty()) {
            return;
        }

        // Search for same mode histogram tools
        List<AbstractResourceHolder> generatedResources = getUserGeneratedRscs();

        if (rh.isGenerated()) {
            /*
             * Update Generated products. Currently just clear the distribution
             * viewer display area.
             * 
             * TODO: This needs further evaluation in the next delivery.
             */
            if (rh instanceof HistogramGridResourceHolder
                    && ((HistogramResource<?>) (rh.getRsc()))
                            .getMode() == HistogramResource.DisplayMode.GRAPHIC_HISTOGRAM
                    && EnsembleTool.getInstance()
                            .getEnsembleToolViewer() != null) {
                clearDistributionViewer();
            }

            return;
            // Loaded resource in Map may impact to the related loaded tool(s)
        } else if (rh instanceof GridResourceHolder && !rh.isGenerated()
                && rh.getRsc().getDescriptor() instanceof MapDescriptor
                && generatedResources != null
                && !generatedResources.isEmpty()) {
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
                        && ((HistogramResource<?>) (gr.getRsc()))
                                .getMode() == HistogramResource.DisplayMode.GRAPHIC_HISTOGRAM
                        && EnsembleTool.getInstance()
                                .getEnsembleToolViewer() != null
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

    public void forceRefresh() {
        updateEnsembleEntries();
    }

    /**
     * Given any abstract resource holder that is a generated grid or time
     * series resource holder, and remove any already exisiting resource having
     * the same calculation, unit, and level.
     * 
     * @param rsc
     *            a generated resource used to see if another similar resource
     *            is already in the resource list
     */
    public void removeSimilarGeneratedRsc(AbstractResourceHolder arh) {

        GeneratedGridResourceHolder ggrh = null;
        GeneratedTimeSeriesResourceHolder tsrh = null;

        if (arh.getRsc() instanceof GeneratedEnsembleGridResource) {
            ggrh = (GeneratedGridResourceHolder) arh;
            removeSimilarGeneratedPlanViewRsc(ggrh);

        } else if (arh.getRsc() instanceof GeneratedTimeSeriesResource) {
            tsrh = (GeneratedTimeSeriesResourceHolder) arh;
            removeSimilarGeneratedTimeSeriesRsc(tsrh);
        }
    }

    /**
     * Given a generated resource, remove any already exisiting generated
     * resource having the same calculation, unit, and level.
     * 
     * @param rsc
     *            a generated resource used to see if another similar resource
     *            is already in the resource list
     */
    public void removeSimilarGeneratedPlanViewRsc(
            GeneratedGridResourceHolder arh) {
        GeneratedEnsembleGridResource foundDuplicateRsc = null;

        for (ResourcePair rp : this) {
            GeneratedGridResourceHolder currGenRscHolder = null;
            GeneratedEnsembleGridResource genRsc = null;
            if (rp.getResource() instanceof GeneratedEnsembleGridResource) {
                genRsc = (GeneratedEnsembleGridResource) rp.getResource();
                currGenRscHolder = new GeneratedGridResourceHolder(genRsc);
                if (arh.isSimilarTo(currGenRscHolder)) {
                    foundDuplicateRsc = genRsc;
                    break;
                }
            }
        }
        if (foundDuplicateRsc != null) {
            removeRsc(foundDuplicateRsc);
        }
    }

    /**
     * Given a generated resource, remove any already exisiting generated
     * resource having the same calculation, unit, and level.
     * 
     * @param rsc
     *            a generated resource used to see if another similar resource
     *            is already in the resource list
     */
    public void removeSimilarGeneratedTimeSeriesRsc(
            GeneratedTimeSeriesResourceHolder arh) {
        GeneratedTimeSeriesResource<?> foundDuplicateRsc = null;
        GeneratedTimeSeriesResourceHolder currGenRscHolder = null;

        for (ResourcePair rp : this) {
            GeneratedTimeSeriesResource<?> genRsc = null;
            if (rp.getResource() instanceof GeneratedEnsembleGridResource) {
                genRsc = (GeneratedTimeSeriesResource<?>) rp.getResource();
                currGenRscHolder = new GeneratedTimeSeriesResourceHolder(
                        genRsc);
                if (arh.isSimilarTo(currGenRscHolder)) {
                    foundDuplicateRsc = genRsc;
                    break;
                }
            }
        }
        if (foundDuplicateRsc != null) {
            removeRsc(foundDuplicateRsc);
        }
    }

}
