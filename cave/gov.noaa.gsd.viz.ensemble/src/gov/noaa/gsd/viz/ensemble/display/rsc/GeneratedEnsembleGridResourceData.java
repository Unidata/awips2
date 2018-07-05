package gov.noaa.gsd.viz.ensemble.display.rsc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.IInitListener;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.grid.rsc.general.GridResource;

import gov.noaa.gsd.viz.ensemble.control.EnsembleResourceIngester;
import gov.noaa.gsd.viz.ensemble.display.calculate.ERFCalculator;
import gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator;
import gov.noaa.gsd.viz.ensemble.display.common.AbstractResourceHolder;
import gov.noaa.gsd.viz.ensemble.navigator.ui.layer.EnsembleToolLayer;

/**
 * Construct the GeneratedEnsembleGridResource and provide data for it by
 * calculating with member data
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
 * Jan 2014       5056       jing        Initial creation
 * Dec 2016       19325      jing        Dispaly calculated grid as image
 * 
 *          </pre>
 */

public class GeneratedEnsembleGridResourceData extends GridResourceData
        implements IInitListener {
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
    private Map<String, List<AbstractResourceHolder>> dataHolders = new ConcurrentHashMap<String, List<AbstractResourceHolder>>();

    /**
     * Calculate loaded ensemble data to generate new data, such as mean.
     */
    protected EnsembleCalculator calculator = null;

    /**
     * Flag if frozen loaded data of members
     */
    private boolean isFrozenData = false;

    /**
     * Currently consider one resource case only. The multiple generated
     * resource may be created for difference level or unit. Do this later with
     * List<GeneratedEnsembleGridResource<GeneratedEnsembleGridResourceData>>
     * resources.
     */
    protected GeneratedEnsembleGridResource resource = null;

    // like "500MB", "" is not available or don't care.
    protected String level = "";

    // Like "Height", "" is not available or don't care.
    protected String unit = "";

    // create an empty metadata map
    protected HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>();

    private EnsembleToolLayer toolLayer = null;

    public GeneratedEnsembleGridResourceData() {
        super();

        this.setMetadataMap(metadataMap);
        setRetrieveData(false);
    }

    public GeneratedEnsembleGridResourceData(EnsembleToolLayer tl,
            EnsembleCalculator calculator, IDescriptor md) {

        super();
        this.mapDescriptor = md;
        this.calculator = calculator;
        this.setRetrieveData(false);
        // No time request in time matching
        this.setRequeryNecessaryOnTimeMatch(false);
        this.setMetadataMap(metadataMap);
        toolLayer = tl;
    }

    public GeneratedEnsembleGridResourceData(EnsembleToolLayer tl,
            EnsembleCalculator calculator, IDescriptor md, String level,
            String unit) {

        super();
        this.mapDescriptor = md;
        this.calculator = calculator;
        this.level = level;
        this.unit = unit;

        // the interface to disable the request
        this.setRetrieveData(false);
        // No time request in time matching
        this.setRequeryNecessaryOnTimeMatch(false);
        this.setMetadataMap(metadataMap);
        toolLayer = tl;
    }

    protected GeneratedEnsembleGridResourceData(
            GeneratedEnsembleGridResourceData resourceData) {
        this.mapDescriptor = resourceData.mapDescriptor;
        this.calculator = resourceData.calculator;
        this.level = resourceData.level;
        this.unit = resourceData.unit;
        this.setRetrieveData(resourceData.retrieveData);
        this.setRequeryNecessaryOnTimeMatch(
                resourceData.isRequeryNecessaryOnTimeMatch);
        this.setMetadataMap(resourceData.metadataMap);
        this.toolLayer = resourceData.toolLayer;

    }

    /**
     * Construct the resource whatever level and unit
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        AbstractVizResource<?, ?> rsc = null;

        rsc = new GeneratedEnsembleGridResource(this, loadProperties,
                calculator);
        resource = (GeneratedEnsembleGridResource) rsc;

        resource.setCalculation(calculator.getCalculation());
        resource.registerListener(this);

        if (this.getCalculator().isImage()) {
            rsc.getLoadProperties().getCapabilities()
                    .getCapability(rsc.getResourceData(),
                            DisplayTypeCapability.class)
                    .setDisplayType(DisplayType.IMAGE);
        }

        update();
        ResourceProperties rp = new ResourceProperties();

        if (this.getCalculator().isImage()) {
            rp.setRenderingOrder(ResourceOrder.LOWEST.value);
        }
        ResourcePair pair = new ResourcePair();
        pair.setResource(rsc);
        pair.setProperties(rp);
        descriptor.getResourceList().add(pair);

        return rsc;
    }

    /**
     * Uses the dataHolders to generate the new data for ensemble display
     * 
     * @return
     */
    public Map<DataTime, List<GeneralGridData>> calculate() {

        // Do calculation with members of all models by looping
        Map<DataTime, List<GeneralGridData>> dataMap = new ConcurrentHashMap<DataTime, List<GeneralGridData>>();

        // Loop calculate. This solution is re-calculating all frames.
        // The second solution is just re-calculating changed or new frames
        List<DataTime> dataTimes = getAllMemberDataTimes();
        for (DataTime time : dataTimes) {

            List<List<GeneralGridData>> inputData = getAllData(time);
            if (inputData == null || inputData.size() < 1) {
                continue;
            }

            /**
             * Do units matching for ERF calculator.
             */
            if (calculator != null && unit != null && !unit.isEmpty()
                    && calculator instanceof ERFCalculator) {
                calculator.setDataUnit(inputData.get(0).get(0).getDataUnit());
                calculator.setDispUnit(Unit.valueOf(unit));
                ((ERFCalculator) (this.calculator)).matchUnit();

            }

            List<GeneralGridData> grids = calculator.calculate(inputData);
            if (grids == null || grids.size() < 1) {
                continue;
            }

            dataMap.put(time, grids);

        }

        return dataMap;
    }

    /**
     * Get all member resources in the manager
     * 
     * @return
     */
    public List<D2DGridResource> getMemberResources() {

        // whatever level or unit in this implementation
        Set<String> keys = dataHolders.keySet();
        List<D2DGridResource> members = new ArrayList<D2DGridResource>();
        for (String key : keys) {
            List<AbstractResourceHolder> rcsList = dataHolders.get(key);
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
     * Available Data Times of all members
     */
    private List<DataTime> getAllMemberDataTimes() {
        // Test just use frame time instead real data time
        DataTime[] dataTimes = this.mapDescriptor.getFramesInfo()
                .getFrameTimes();

        List<DataTime> times = new ArrayList<DataTime>(
                Arrays.asList(dataTimes));

        return times;
    }

    /**
     * Get data set in all member of one frame. The calculation should deal with
     * issues, -In same ensemble products, different grid data number of member.
     * -Different products for same level and unit are treated as ensemble
     * members. -Should keep members in same geometry(cover biggest area) by
     * re-projecting some members. The re-projection in once only if need.
     * -Whatever time matched data of the frames, just use them for calculation.
     */
    private List<List<GeneralGridData>> getAllData(DataTime time) {
        ArrayList<List<GeneralGridData>> allData = new ArrayList<List<GeneralGridData>>();
        List<D2DGridResource> members = getMemberResources();

        // Get the frame index by the frame time
        DataTime[] frameTimes = this.mapDescriptor.getFramesInfo()
                .getFrameTimes();

        if (frameTimes == null || frameTimes.length < 1) {
            return allData;
        }
        int i;
        for (i = 0; i < frameTimes.length; i++) {
            if (time.equals(frameTimes[i])) {
                break;
            }
        }
        if (i >= frameTimes.length) {
            return allData;
        }

        for (D2DGridResource member : members) {
            // Get a data time matched the frame, an option
            // DataTime[] dataTimes =member.getDataTimes();

            // use requestData()
            // Whatever matched time in the frame, just use it for calculation.
            // User can control the matching from D2D GUI
            DataTime memberTime = this.mapDescriptor.getFramesInfo()
                    .getTimeForResource(member, i);
            if (memberTime == null) {
                continue;
            }
            List<GeneralGridData> data = member.requestData(memberTime);
            if (data != null) {
                allData.add(data);
            }
        }
        return allData;
    }

    /**
     * Disable retrieve data in all members
     */
    private void disableMembersRetrieveData() {
        if (isFrozenData()) {
            List<D2DGridResource> members = getMemberResources();
            for (D2DGridResource member : members) {
                // It doesn't work? Check out later
                member.getResourceData().setRetrieveData(false);

            }
        }
    }

    /**
     * Enable retrieve data in all members
     */
    private void enableMembersRetrieveData() {

        List<D2DGridResource> members = getMemberResources();
        for (D2DGridResource member : members) {
            member.getResourceData().setRetrieveData(true);

        }

    }

    /**
     * Update by checking on the loaded ensemble product resources. Re-calculate
     * with the ensemble calculator to generate a new product(like mean), update
     * the dataMap in the AbstractGridResource Is it possible to update the
     * dataMap? The AbstractGridResource methods can be over written?
     * UsesetData(Map<DataTime, List<GeneralGridData>> data)
     * 
     * @throws VizException
     */
    public void update() throws VizException {

        if (level != null && !level.equals("") && unit != null
                && !unit.equals("")) {
            // Same level and unit case
            dataHolders = toolLayer.getResourceList()
                    .getUserLoadedRscs(MapDescriptor.class, true, level, unit);
        } else if (unit != null && !unit.equals("")) {
            // same unit whatever level case
            dataHolders = toolLayer.getResourceList()
                    .getUserLoadedRscs(MapDescriptor.class, true, unit);
        } else {
            // whatever level or unit,for test only
            dataHolders = toolLayer.getResourceList()
                    .getUserLoadedRscs(MapDescriptor.class, true);
        }

        if (!dataHolders.isEmpty() && !dataHolders.keySet().isEmpty()) {
            disableMembersRetrieveData();
            resource.setParameter((GridResource<?>) (dataHolders
                    .get((dataHolders.keySet().toArray())[0]).get(0).getRsc()));
            resource.updateData(calculate());
            enableMembersRetrieveData();
        }

    }

    public EnsembleCalculator getCalculator() {
        return calculator;
    }

    @Override
    public boolean equals(Object obj) {
        return false;
    }

    public boolean isFrozenData() {
        return isFrozenData;
    }

    /**
     * For GUI controls data updating.
     * 
     * @return
     */
    public void setFrozenData(boolean isFrozenData) {
        this.isFrozenData = isFrozenData;
    }

    public Map<String, List<AbstractResourceHolder>> getDataHolders() {
        return dataHolders;
    }

    public String getLevel() {
        return level;
    }

    public String getUnit() {
        return unit;
    }

    @Override
    public void inited(AbstractVizResource<?, ?> rsc) {

        EnsembleResourceIngester.getInstance()
                .register((AbstractVizResource<?, ?>) resource);

    }

}
