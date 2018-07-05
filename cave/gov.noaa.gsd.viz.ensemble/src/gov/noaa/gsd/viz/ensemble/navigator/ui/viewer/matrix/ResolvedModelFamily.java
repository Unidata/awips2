package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArraySet;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.display.VizMapEditor;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.ui.BundleLoader;
import com.raytheon.viz.ui.BundleLoader.BundleInfoType;
import com.raytheon.viz.ui.BundleProductLoader;

import gov.noaa.gsd.viz.ensemble.util.RequestableResourceMetadata;

/***
 * 
 * This class derives from <code>ModelFamily</code> and distinguishes itself
 * from its parent in that it associates a list of model sources with the
 * family. Only until a family has model sources associated is it considered to
 * be resolved.
 * 
 * This class also keeps a map of each field/plane pair and its associated
 * bundle.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2016   12566     polster     Initial creation
 * Nov 19, 2016  19443      polster     Refactoring for 17.1.1 release
 * Dec 01, 2017  41520      polster     Class name ModelSources changed to ModelSourceKind
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class ResolvedModelFamily extends ModelFamily {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ResolvedModelFamily.class);

    private static final String MODEL_VARIABLE = "modelName";

    private static final String TOTAL_PRECIP_VARIABLE = "TP";

    private static final String FRAME_COUNT_VARIABLE = "frameCount";

    private static final int INITIAL_LOAD_COUNT = 12;

    private List<ModelSourceKind> sources = null;

    /*
     * For each model source, store a list of load-info nodes that contain a
     * field/plane pair, a bundle, and a isLoaded Flag. The bundle will contain
     * only the load information necessary to load the associated (individual)
     * field plane pair.
     */
    private Map<ModelSourceKind, List<FPPLoadInfo>> resolvedFamilyMap = null;

    /*
     * For each Field/Plane pair, store all resources in the active editor that
     * have the matching field/plane pair.
     */
    private Map<FieldPlanePair, Set<AbstractVizResource<?, ?>>> associatedRscMap = null;

    public ResolvedModelFamily(ModelFamily givenFamily,
            List<ModelSourceKind> givenSources) throws VizException {

        // copy constructor
        super(givenFamily);

        if (givenSources == null || givenSources.size() == 0) {
            throw new VizException("Source list must contain valid sources.");
        }

        resolvedFamilyMap = new ConcurrentHashMap<>();

        associatedRscMap = new ConcurrentHashMap<>();

        sources = givenSources;
        sortSources();

        createIndividualBundles();
    }

    /**
     * Sorts the model sources by shortest model time-step first, which is the
     * also the order that the models get loaded.
     */
    private void sortSources() {
        /*
         * sort (ascending) by the shortest time resolution (time steps).
         */
        Collections.sort(sources, new Comparator<ModelSourceKind>() {

            @Override
            public int compare(ModelSourceKind o1, ModelSourceKind o2) {
                DatasetInfoLookup lookup = DatasetInfoLookup.getInstance();
                DatasetInfo info1 = lookup.getInfo(o1.getModelId());
                DatasetInfo info2 = lookup.getInfo(o2.getModelId());
                Integer dt1 = new Integer(info1.getDt());
                Integer dt2 = new Integer(info2.getDt());
                return dt1.compareTo(dt2);
            }
        });
    }

    /**
     * The associated resource set contains those resources sharing the same
     * field/plane pair. This method will create the set if one isn't already
     * associated with the field/plane pair.
     * 
     * This method creates a thread-safe, ordered set.
     */
    synchronized public Set<AbstractVizResource<?, ?>> getAssociatedRscSet(
            FieldPlanePair fpp) {

        Set<AbstractVizResource<?, ?>> s = associatedRscMap.get(fpp);
        if (s == null) {
            s = new CopyOnWriteArraySet<>();
            associatedRscMap.put(fpp, s);
        }
        return s;
    }

    public List<ModelSourceKind> getSources() {
        return sources;
    }

    /**
     * Get any resource that matches a model source and a field/plane pair.
     */
    public AbstractVizResource<?, ?> getResource(ModelSourceKind modelSource,
            FieldPlanePair fpp) {

        AbstractVizResource<?, ?> foundRsc = null;

        Set<AbstractVizResource<?, ?>> assocRscs = associatedRscMap.get(fpp);
        String modelId = modelSource.getModelId();
        for (AbstractVizResource<?, ?> rsc : assocRscs) {
            if (rsc.getName().contains(modelId)) {
                foundRsc = rsc;
                break;
            }
        }
        return foundRsc;
    }

    /**
     * Get the color of any resource that matches a field/plane pair.
     */
    public RGB getResourceColor(FieldPlanePair fpp) {
        RGB currColor = null;
        Set<AbstractVizResource<?, ?>> s = associatedRscMap.get(fpp);
        AbstractVizResource<?, ?> rsc = null;
        if (!s.isEmpty()) {
            rsc = (AbstractVizResource<?, ?>) s.iterator().next();
            currColor = rsc.getCapability(ColorableCapability.class).getColor();
        }
        return currColor;
    }

    /**
     * Get the total number of field/plane pairs in this model family.
     */
    public int getActiveFieldPlanePairCount() {
        int count = 0;
        Collection<List<FPPLoadInfo>> srcColl = resolvedFamilyMap.values();
        Iterator<List<FPPLoadInfo>> srcIter = srcColl.iterator();
        List<FPPLoadInfo> currLoadInfoList = srcIter.next();
        if (currLoadInfoList != null) {
            count = currLoadInfoList.size();
        }
        return count;
    }

    /**
     * Load the resources associated with the given resolved model family which
     * will therefore request the field/plane pairs for each model source
     * contained in therein.
     * 
     * @param loadFrameCountDefault
     * 
     * @param rmf
     *            the resolved model family containing the specific resources to
     *            associate with the family's field/plane pairs.
     */
    protected void loadModelsByFieldPlanePairs(VizMapEditor matrixEditor,
            FieldPlanePair fpp, boolean loadFrameCountDefault) {

        /*
         * Walk the sources and load the parameter-supplied field/plane pair for
         * each source.
         */
        Set<Entry<ModelSourceKind, List<FPPLoadInfo>>> srcSet = resolvedFamilyMap
                .entrySet();
        Iterator<Entry<ModelSourceKind, List<FPPLoadInfo>>> srcIter = srcSet
                .iterator();
        while (srcIter.hasNext()) {
            Entry<ModelSourceKind, List<FPPLoadInfo>> currSrcEntry = srcIter
                    .next();
            List<FPPLoadInfo> currFppLoadInfoList = currSrcEntry.getValue();
            for (FPPLoadInfo fppInfo : currFppLoadInfoList) {
                if (fppInfo.fieldPlanePair.equals(fpp)) {
                    /* load synchronously */
                    new BundleProductLoader(matrixEditor, fppInfo.bundle).run();
                }
            }
        }
    }

    /**
     * Create a map of a model source (key) with a map of field-plane
     * pair/bundles (value). The bundle will contain only the load information
     * necessary to load that field plane pair.
     */
    private void createIndividualBundles() {

        Map<String, String> variableTranslations = new HashMap<>();
        ModelFamilyDefinitions mfd = null;
        String modelDbId = null; // e.g. GFS20
        String totalPrecipValue = null;

        FPPLoadInfo fppLoadInfo = null;
        Bundle bundle = null;
        List<FPPLoadInfo> fppList = null;
        for (ModelSourceKind src : sources) {
            modelDbId = src.getModelId();
            mfd = getCurrFamilyDefinition();
            totalPrecipValue = mfd.getTotalPrecip();
            variableTranslations.put(TOTAL_PRECIP_VARIABLE, totalPrecipValue);
            variableTranslations.put(MODEL_VARIABLE, modelDbId);
            variableTranslations.put(FRAME_COUNT_VARIABLE,
                    Integer.toString(INITIAL_LOAD_COUNT));
            for (FieldPlanePair fpp : fieldPlanePairs.getNodes()) {
                try {
                    bundle = BundleLoader.getBundle(mfd.getFamilyFileName(),
                            variableTranslations, BundleInfoType.FILE_LOCATION);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to load bundle; " + e.getLocalizedMessage(),
                            e);
                    continue;
                }
                if (bundle != null) {
                    boolean isLoaded = false;
                    fppLoadInfo = new FPPLoadInfo(bundle, fpp, isLoaded);
                    if (fppList == null) {
                        fppList = new ArrayList<>();
                    }
                    fppList.add(fppLoadInfo);
                }
            }
            resolvedFamilyMap.put(src, fppList);
        }

        return;
    }

    /**
     * Contains the field/plane pair and it associated bundle. The bundle can be
     * used to load the distinct resources associated with one field/plane pair.
     */
    class FPPLoadInfo {

        Bundle bundle = null;

        FieldPlanePair fieldPlanePair = null;

        boolean isRequestedForLoad = false;

        FPPLoadInfo(Bundle b, FieldPlanePair fpp, boolean isSelectedForLoad) {
            fieldPlanePair = fpp;
            bundle = removeIrrelevantFieldPlanePairs(b, fpp);
            isRequestedForLoad = isSelectedForLoad;
        }

        /**
         * For a given field/plane pair, walk through the default model family
         * bundle and remove all ResourcePair nodes that don't match. That is to
         * say, create a bundle that has only one resource pair matching the
         * given field/plane pair.
         */
        private Bundle removeIrrelevantFieldPlanePairs(Bundle b,
                FieldPlanePair fpp) {

            String fieldAbbrev = fpp.getFieldAbbrev();
            String plane = fpp.getPlane();
            String targetField = null;
            String targetPlane = null;

            AbstractRenderableDisplay display = null;
            ResourcePair foundRscPair = null;
            ResourceList rscList = null;
            if (b != null) {
                /* Get the first display only */
                display = b.getDisplays()[0];
                rscList = display.getDescriptor().getResourceList();
                int i = 0;
                while (i < rscList.size()) {
                    ResourcePair rp = rscList.get(i);
                    if (rp != null && rp.getResourceData() != null) {
                        if (rp.getResourceData() instanceof AbstractRequestableResourceData) {
                            AbstractRequestableResourceData ard = (AbstractRequestableResourceData) rp
                                    .getResourceData();
                            RequestableResourceMetadata rrd = new RequestableResourceMetadata(
                                    ard);
                            targetField = rrd.getFieldAbbrev();
                            targetPlane = rrd.getPlane();
                            if (fieldAbbrev.equals(targetField)
                                    && (plane.equals(targetPlane))) {
                                foundRscPair = rp;
                                break;
                            }
                        }
                    }
                    i++;
                }
            }

            if (foundRscPair == null) {
                statusHandler.handle(Priority.PROBLEM,
                        "ResolvedModelFamily.removeIrrelevantFieldPlanePairs: Could not find fpp: "
                                + fieldAbbrev + "; " + plane);
            } else {
                rscList.clear();
                rscList.add(foundRscPair);
            }

            return b;

        }

    }

}
