package gov.noaa.gsd.viz.ensemble.display.rsc.histogram;

import gov.noaa.gsd.viz.ensemble.display.common.GenericResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.control.EnsembleResourceManager;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.grid.rsc.general.GeneralGridData;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Construct D2D ensamle Sampling resources and provide data.
 * 
 * @author jing
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July, 2014     5056       jing       Initial creation
 * 
 * </pre>
 */

public class HistogramResourceData extends AbstractResourceData {

    private IDescriptor mapDescriptor = null;

    /**
     * The grid resources holding the ensemble data for input to the ensemble
     * calculator The String key is the loaded model name of an ensemble
     * product, same model ran multiple times or multiple models ran same time.
     * Consider the last two case later. Need an ensemble manager to handle the
     * resources, so the loaded products can register and unregister when
     * loading and unloading. To do so, may be need extend D2DGridResource to
     * EnsembleGridResource(need EnsembleGridResourceData too) which can provide
     * more interfaces
     * 
     */
    private Map<String, List<GenericResourceHolder>> dataHolders = new ConcurrentHashMap<String, List<GenericResourceHolder>>();

    /**
     * Currently consider one resource case only. The multiple generated
     * resource may be created for difference level or unit. Do this later with
     * List<GeneratedEnsembleGridResource<GeneratedEnsembleGridResourceData>>
     * resources.
     */
    protected HistogramResource<HistogramResourceData> resource = null;

    // like "500MB", "" is not available or don't care.
    protected String level = "";

    // Like "Height", "" is not available or don't care.
    protected String unit = "";

    // Display mode
    protected HistogramResource.DisplayMode mode;

    public HistogramResourceData(HistogramResource.DisplayMode mode) {
        super();
        this.mode = mode;
    }

    public HistogramResourceData(IDescriptor md, String level, String unit,
            HistogramResource.DisplayMode mode) {

        this.mapDescriptor = md;// need not at here?
        this.level = level;
        this.unit = unit;
        this.mode = mode;
    }

    /**
     * Construct the resource whatever level and unit
     */
    @SuppressWarnings("unchecked")
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        AbstractVizResource<?, ?> rsc = null;

        rsc = new HistogramResource<HistogramResourceData>(this,
                loadProperties, descriptor, level, unit, mode);

        resource = (HistogramResource<HistogramResourceData>) rsc;

        update();
        ResourceProperties rp = new ResourceProperties();

        ResourcePair pair = new ResourcePair();
        pair.setResource(rsc);
        pair.setProperties(rp);
        descriptor.getResourceList().add(pair);

        // Register to the ensemble resource manager
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        EnsembleResourceManager.getInstance().registerGenerated(
                (AbstractVizResource<?, ?>) resource, editor);

        return rsc;
    }

    public Map<String, List<GenericResourceHolder>> getDataHolders() {
        return dataHolders;
    }

    /**
     * Search ensemble members resources grouped by model
     * 
     */
    public List<D2DGridResource> getAllMembersResources() {

        // whatever level or unit in this implementation
        Set<String> keys = dataHolders.keySet();
        List<D2DGridResource> members = new ArrayList<D2DGridResource>();
        for (String key : keys) {
            List<GenericResourceHolder> rcsList = dataHolders.get(key);
            if (rcsList == null || rcsList.size() == 0) {
                continue;
            }

            for (int i = 0; i < rcsList.size(); i++) {

                if (rcsList.get(i).getRsc() instanceof D2DGridResource) {
                    members.add((D2DGridResource) rcsList.get(i).getRsc());
                }
            }

        }

        return members;
    }

    /**
     * TODO: Available Data Times if all member may be used in future,
     */
    private List<DataTime> getAllMemberDataTimes() {
        // Use frame time instead real data time
        DataTime[] dataTimes = this.mapDescriptor.getFramesInfo()
                .getFrameTimes();
        List<DataTime> times = new ArrayList<DataTime>(Arrays.asList(dataTimes));

        return times;
    }

    /**
     * TODO: Get data set in all member; may be used in future, but the data
     * time should be checked if it's available time
     */
    private List<List<GeneralGridData>> getAllData(DataTime time) {
        List<List<GeneralGridData>> allData = new ArrayList<List<GeneralGridData>>();
        List<D2DGridResource> members = getAllMembersResources();

        for (D2DGridResource member : members) {
            List<GeneralGridData> data = member.requestData(time);

            if (data != null) {
                allData.add(data);
            }
        }

        return allData;
    }

    @Override
    public void update(Object updateData) {
        // TODO
    }

    /**
     * Update by checking on the loaded ensemble product resources Re-calculate
     * with the ensemble calculator to generate a new product(like mean), update
     * the dataMap in the AbstractGridResource Is it possible to update the
     * dataMap? The AbstractGridResource methods can be over written?
     * UsesetData(Map<DataTime, List<GeneralGridData>> data)
     * 
     * @throws VizException
     */
    public void update() throws VizException {
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        if (level != null && !level.equals("") && unit != null
                && !unit.equals("")) {
            // Same level and unit case
            dataHolders = EnsembleResourceManager
                    .getInstance()
                    .getResourceList(editor)
                    .getUserLoadedRscs((IDescriptor) new MapDescriptor(), true,
                            level, unit);

        } else if (unit != null && !unit.equals("")) {
            // TODO: same unit whatever level case
        } else {
            // TODO: whatever level or unit,for test only
        }

    }

    @Override
    public boolean equals(Object obj) {
        // TODO
        return false;
    }

}
