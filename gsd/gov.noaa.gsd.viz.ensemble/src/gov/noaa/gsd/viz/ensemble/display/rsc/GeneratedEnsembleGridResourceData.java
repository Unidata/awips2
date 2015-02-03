package gov.noaa.gsd.viz.ensemble.display.rsc;

import gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator;
import gov.noaa.gsd.viz.ensemble.display.common.GenericResourceHolder;
import gov.noaa.gsd.viz.ensemble.display.common.Utilities;
import gov.noaa.gsd.viz.ensemble.display.control.EnsembleResourceManager;

import java.util.ArrayList;
import java.util.Arrays;
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
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.rsc.general.D2DGridResource;
import com.raytheon.viz.grid.rsc.general.GeneralGridData;
import com.raytheon.viz.grid.rsc.general.GridResource;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

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
 * 
 * </pre>
 */

public class GeneratedEnsembleGridResourceData extends GridResourceData {
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
    protected GeneratedEnsembleGridResource<GeneratedEnsembleGridResourceData> resource = null;

    // like "500MB", "" is not available or don't care.
    protected String level = "";

    // Like "Height", "" is not available or don't care.
    protected String unit = "";

    // create an empty metadata map
    protected HashMap<String, RequestConstraint> metadataMap = new HashMap<String, RequestConstraint>();

    public GeneratedEnsembleGridResourceData() {
        super();

        this.setMetadataMap(metadataMap);
        setRetrieveData(false);
    }

    public GeneratedEnsembleGridResourceData(EnsembleCalculator calculator,
            IDescriptor md) {

        super();
        this.mapDescriptor = md;
        this.calculator = calculator;
        this.setRetrieveData(false);
        // No time request in time matching
        this.setRequeryNecessaryOnTimeMatch(false);
        this.setMetadataMap(metadataMap);
    }

    public GeneratedEnsembleGridResourceData(EnsembleCalculator calculator,
            IDescriptor md, String level, String unit) {

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
    }

    /**
     * Construct the resource whatever level and unit
     */
    @Override
    public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {

        AbstractVizResource<?, ?> rsc = null;

        rsc = new GeneratedEnsembleGridResource<Object>(this, loadProperties);
        resource = (GeneratedEnsembleGridResource<GeneratedEnsembleGridResourceData>) rsc;

        resource.setCalculation(calculator.getCalculation());

        update();
        ResourceProperties rp = new ResourceProperties();

        ResourcePair pair = new ResourcePair();
        pair.setResource(rsc);
        pair.setProperties(rp);
        descriptor.getResourceList().add(pair);

        // TODO: Is this the correct way to associate the editor with the
        // resource?
        AbstractEditor editor = EditorUtil
                .getActiveEditorAs(AbstractEditor.class);
        EnsembleResourceManager.getInstance().registerGenerated(
                (AbstractVizResource<?, ?>) resource, editor);

        return rsc;
    }

    /**
     * Uses the dataHolders to generate the new data for ensemble display
     * 
     * @return
     */
    @SuppressWarnings("unchecked")
    public Map<DataTime, List<GeneralGridData>> calculate() {

        // searchDataHolders();

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

            List<GeneralGridData> grids = calculator.calculate(inputData);
            if (grids == null || grids.size() < 1) {
                continue;
            }

            dataMap.put(time, grids);

        }

        return dataMap;
    }

    /**
     * Search ensemble members resources grouped by model used for test
     */
    private void searchDataHolders() {
        // search models of ensemble
        List<String> models = Utilities.getEnsembleModels();
        if (models == null || models.size() < 1 || resource == null) {
            return;
        }

        if (!dataHolders.isEmpty()) {
            dataHolders.clear();
        }

        // Add all ensemble member resources into the dataHolders
        for (String model : models) {
            List<D2DGridResource> rcsList = Utilities
                    .getResourcesEnsembleModel(model);

            if (rcsList != null) {

                if (rcsList.size() > 0) {
                    resource.setParameter(rcsList.get(0));
                }
            }
        }

        // Add loaded product(s) which are not ensemble product(s)
        // but same level and type of data(same unit) into the dataHolders

        // We can load same model with different run time to treat it as
        // an ensemble products. Add the members in the dataHolders too.

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
     * Available Data Times of all members
     */
    private List<DataTime> getAllMemberDataTimes() {
        // Test just use frame time instead real data time
        DataTime[] dataTimes = this.mapDescriptor.getFramesInfo()
                .getFrameTimes();

        List<DataTime> times = new ArrayList<DataTime>(Arrays.asList(dataTimes));

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
        for (i = 0; i < frameTimes.length; i++)
            if (time.equals(frameTimes[i])) {
                break;
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
     * Update by checking on the loaded ensemble product resources Re-calculate
     * with the ensemble calculator to generate a new product(like mean), update
     * the dataMap in the AbstractGridResource Is it possible to update the
     * dataMap? The AbstractGridResource methods can be over written?
     * UsesetData(Map<DataTime, List<GeneralGridData>> data)
     * 
     * @throws VizException
     */
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
                    .getUserLoadedRscs((IDescriptor) new MapDescriptor(), true,
                            level, unit);
            // dataHolders =
            // ResourceManager.getInstance().getResourceList(editor).getCalculationLoadedRscs((IDescriptor)new
            // MapDescriptor(), true);
        } else if (unit != null && !unit.equals("")) {
            // same unit whatever level case
            dataHolders = EnsembleResourceManager
                    .getInstance()
                    .getResourceList(editor)
                    .getUserLoadedRscs((IDescriptor) new MapDescriptor(), true,
                            unit);
        } else {
            // whatever level or unit,for test only
            dataHolders = EnsembleResourceManager.getInstance()
                    .getResourceList(editor)
                    .getUserLoadedRscs((IDescriptor) new MapDescriptor(), true);
        }

        if (!dataHolders.isEmpty() && !dataHolders.keySet().isEmpty()) {
            disableMembersRetrieveData();
            resource.setParameter((GridResource<?>) (dataHolders.get(
                    (dataHolders.keySet().toArray())[0]).get(0).getRsc()));
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

    public Map<String, List<GenericResourceHolder>> getDataHolders() {
        return dataHolders;
    }

}
