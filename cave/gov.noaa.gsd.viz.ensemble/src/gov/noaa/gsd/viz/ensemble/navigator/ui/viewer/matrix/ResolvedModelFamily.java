package gov.noaa.gsd.viz.ensemble.navigator.ui.viewer.matrix;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.viz.core.exception.VizException;

/***
 * 
 * This class derives from <code>ModelFamily</code> and distinguishes itself
 * from its parent in that it associates a list of model sources with the
 * family. Only until a family has model sources associated is it considered to
 * be resolved.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2016   12566      polster     Initial creation
 * 
 * </pre>
 * 
 * @author polster
 * @version 1.0
 */
public class ResolvedModelFamily extends ModelFamily {

    private List<ModelSources> sources = null;

    public ResolvedModelFamily(ModelFamily givenFamily,
            List<ModelSources> givenSources) throws VizException {

        // copy ctor
        super(givenFamily);

        if (givenSources == null || givenSources.size() == 0) {
            throw new VizException("Source list must contain valid sources.");
        }

        sources = givenSources;
        sortSources();
    }

    /**
     * Sorts the model sources by shortest model time-step first, which is the
     * also the order that the models get loaded.
     */
    private void sortSources() {
        /*
         * sort (ascending) by the shortest time resolution (time steps).
         */
        Collections.sort(sources, new Comparator<ModelSources>() {

            @Override
            public int compare(ModelSources o1, ModelSources o2) {
                DatasetInfo info1 = DatasetInfoLookup.getInstance().getInfo(
                        o1.getModelId());
                DatasetInfo info2 = DatasetInfoLookup.getInstance().getInfo(
                        o2.getModelId());
                Integer dt1 = new Integer(info1.getDt());
                Integer dt2 = new Integer(info2.getDt());
                return dt1.compareTo(dt2);
            }
        });

    }

    public List<ModelSources> getSources() {
        return sources;
    }

    /**
     * Once this model family has both the element set and associated sources,
     * then make a map available of the two.
     */
    public Map<ModelSources, FieldPlanePairSet> getResolvedFamilyMap()
            throws InstantiationException {
        Map<ModelSources, FieldPlanePairSet> map = new HashMap<>();
        for (ModelSources s : sources) {
            map.put(s, fieldPlanePairs.clone());
        }
        return map;
    }

}
