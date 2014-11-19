package gov.noaa.gsd.viz.ensemble.display.rsc.timeseries;

import gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator;
import gov.noaa.gsd.viz.ensemble.display.common.GenericResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.control.EnsembleResourceManager;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.xy.timeseries.display.TimeSeriesDescriptor;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResourceData;
import com.raytheon.viz.core.graphing.xy.XYDataList;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Uses cached data of loaded members in the time series display, generates an
 * new data and construct a resource for it.
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
 * Feb, 2014     5056       jing       Initial creation
 * 
 * 
 * </pre>
 */
public class GeneratedTimeSeriesResourceData extends TimeSeriesResourceData {

    private Map<String, List<GenericResourceHolder>> dataHolders = new ConcurrentHashMap<String, List<GenericResourceHolder>>();

    /**
     * Calculate loaded ensemble data to generate new data, such as mean.
     */
    protected EnsembleCalculator calculator = null;

    protected GeneratedTimeSeriesResource<GeneratedTimeSeriesResourceData> resource = null;

    // like "500MB", "" is not available or don't care.
    protected String level;

    // Like "Height", "" is not available or don't care.
    protected String unit;

    // create an empty metadata map
    protected HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>();

    public GeneratedTimeSeriesResourceData(EnsembleCalculator calculator,
            String level, String unit) {

        super();

        this.calculator = calculator;
        this.level = level;
        this.unit = unit;
        // interface to disable the request
        this.setRetrieveData(false);

        // No time request in time matching
        this.setRequeryNecessaryOnTimeMatch(false);
        this.setMetadataMap(metadataMap);

    }

    /**
     * Construct the resource whatever level and unit
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        update();

        // Not data, don't construct the resource for avoid problem
        if (dataHolders == null || dataHolders.isEmpty()
                || dataHolders.keySet().isEmpty()) {
            return null;
        }

        resource = new GeneratedTimeSeriesResource<GeneratedTimeSeriesResourceData>(
                this, loadProperties);
        resource.setCalculation(calculator.getCalculation());
        resource.setDescriptor((TimeSeriesDescriptor) descriptor);

        this.metadataMap = new HashMap<String, RequestConstraint>();

        // set the resource
        setup();
        resource.updateData();

        // update();
        ResourceProperties rp = new ResourceProperties();

        ResourcePair pair = new ResourcePair();
        pair.setResource(resource);
        pair.setProperties(rp);
        descriptor.getResourceList().add(pair);

        // TODO: Is this the correct way to associate the editor with the
        // resource?
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        EnsembleResourceManager.getInstance().registerGenerated(
                (AbstractVizResource<?, ?>) resource, editor);

        return resource;
    }

    // Uses the dataHolders to generate the new data for ensemble display
    @SuppressWarnings("unchecked")
    public XYDataList calculate() {
        return calculator.calculateTimeSeries(getAllData());
    }

    public void update() throws VizException {

        // TODO: Is this the correct way to associate the editor with the
        // resource?
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);

        if (level != null && !level.equals("") && unit != null
                && !unit.equals("")) {
            // Same level and unit case
            dataHolders = EnsembleResourceManager
                    .getInstance()
                    .getResourceList(editor)
                    .getUserLoadedRscs(
                            (IDescriptor) new TimeSeriesDescriptor(), true,
                            level, unit);

            // TODO: Reserved for future use.

            // dataHolders =
            // ResourceManager.getInstance().getResourceList(editor).getCalculationLoadedRscs((IDescriptor)new
            // TimeSeriesDescriptor(), true);
            // } else if (level != null && !level.equals("")
            // && unit != null && !unit.equals("")){
            // //same unit whatever level case
            // dataHolders =
            // ResourceManager.getInstance().getResourceList(editor)
            // .getUserLoadedRscs((IDescriptor)new TimeSeriesDescriptor(),
            // false, unit);
        } else {
            // TODO: Reserved for future use.

            // whatever level or unit,for test only
            // dataHolders =
            // ResourceManager.getInstance().getResourceList(editor)
            // .getUserLoadedRscs((IDescriptor)new TimeSeriesDescriptor(),
            // false);
            dataHolders = EnsembleResourceManager
                    .getInstance()
                    .getResourceList(editor)
                    .getUserLoadedRscs(
                            (IDescriptor) new TimeSeriesDescriptor(), true);
        }

    }

    /**
     * Set up the generated resourceData with a member resource.
     */
    private void setup() {
        List<TimeSeriesResource> rscs = getAllMembersResources();
        if (rscs.isEmpty()) {
            return;
        }
        resource.setParameters();
        // Set up generated resource data
        this.level = rscs.get(0).getResourceData().getLevelKey();

        this.unit = rscs.get(0).getUnits();

        this.setCoordinate(rscs.get(0).getResourceData().getCoordinate());
        this.setCombineOperation(rscs.get(0).getResourceData()
                .getCombineOperation());
        this.setFrozenTime(rscs.get(0).getResourceData().getFrozenTime());

        this.setLevelKey(rscs.get(0).getResourceData().getLevelKey());

        this.setPointCoordinate(rscs.get(0).getResourceData()
                .getPointCoordinate());
        this.setPointLetter(rscs.get(0).getResourceData().getPointLetter());

        this.setYParameter(rscs.get(0).getResourceData().getYParameter());
        this.setXParameter(rscs.get(0).getResourceData().getXParameter());

    }

    /**
     * May need not the Adapter if the generated resource and data can be set
     * correctly using a member resource.
     * 
     * @param object
     * @return
     */
    // private AbstractTimeSeriesAdapter<?> getAdapter(PluginDataObject object){
    // GeneratedTimeSeriesAdapter adapter = new GeneratedTimeSeriesAdapter();
    // return null;
    // }

    /**
     * get the data available times from all members
     */
    public List<DataTime> getAllDataTimes() {
        List<DataTime> dataTimes = new ArrayList<DataTime>();

        List<TimeSeriesResource> members = getAllMembersResources();
        for (TimeSeriesResource member : members) {
            DataTime[] meberDataTimes = member.getDataTimes();

            if (meberDataTimes != null && meberDataTimes.length != 0) {
                for (int i = 0; i < meberDataTimes.length - 1; i++) {
                    if (!dataTimes.contains(meberDataTimes[i])) {
                        dataTimes.add(meberDataTimes[i]);
                    }
                }
            }
        }
        return dataTimes;
    }

    /**
     * Get all data of members for calculation
     * 
     * @return
     */
    private List<XYDataList> getAllData() {
        List<XYDataList> allData = new ArrayList<XYDataList>();

        List<TimeSeriesResource> members = getAllMembersResources();
        for (TimeSeriesResource member : members) {
            XYDataList data = member.getData();
            if (data != null) {
                allData.add(data);
            }
        }
        return allData;
    }

    public TimeSeriesResource getAnyMembersResource() {
        List<TimeSeriesResource> members = getAllMembersResources();
        if (members == null || members.isEmpty()) {
            return null;
        }

        return members.get(0);
    }

    private List<TimeSeriesResource> getAllMembersResources() {

        // whatever level or unit in this implementation
        Set<String> keys = dataHolders.keySet();
        List<TimeSeriesResource> members = new ArrayList<TimeSeriesResource>();
        for (String key : keys) {
            List<GenericResourceHolder> rcsList = dataHolders.get(key);
            if (rcsList == null || rcsList.size() == 0) {
                continue;
            }

            for (int i = 0; i < rcsList.size(); i++) {
                if (rcsList.get(i).getRsc() instanceof TimeSeriesResource) {
                    members.add((TimeSeriesResource) rcsList.get(i).getRsc());
                }
            }

        }

        return members;
    }

    public String getLevel() {
        return level;
    }

    public void setLevel(String level) {
        this.level = level;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }
}
