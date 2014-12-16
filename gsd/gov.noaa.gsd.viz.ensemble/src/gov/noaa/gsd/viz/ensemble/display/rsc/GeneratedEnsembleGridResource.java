package gov.noaa.gsd.viz.ensemble.display.rsc;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfo;
import com.raytheon.uf.common.dataplugin.grid.dataset.DatasetInfoLookup;
import com.raytheon.uf.common.dataplugin.grid.util.GridLevelTranslator;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.grid.rsc.GridResourceData;
import com.raytheon.viz.grid.rsc.general.GeneralGridData;
import com.raytheon.viz.grid.rsc.general.GridResource;
import com.raytheon.viz.grid.xml.FieldDisplayTypesFactory;

/**
 * Based on the loaded ensemble product(s) data generated new resource with a
 * calculate method. Implement steps: First-Basic contour display. Second-
 * Vector-wind bar display. Third- Image display and sampling. Forth-
 * Auto-updating and other. Issue:Since extend the GridResource, how to black
 * request data from EDEX? Current solution is to override and minor change
 * related
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
 * Jan 2014        5056     jing    Initial creation
 * 
 * </pre>
 */
@SuppressWarnings({ "hiding", "rawtypes", "unchecked" })
public class GeneratedEnsembleGridResource<GeneratedEnsembleGridResourceData>
        extends GridResource implements IGridNameResource {

    private Parameter parameter = null;

    private GridRecord randomRec = null;

    private ParamLevelMatchCriteria paramLevelMatchCriteria = null;

    private ArrayList<String> models = null;

    private Calculation calculation = Calculation.NONE;

    protected Random rand;

    /**
     * Calculate method like mean
     */
    // protected EnsembleCalculator calculator;

    /**
     * The descriptor for plan view;
     */
    protected IDescriptor mapDescriptor;

    /**
     * Constructor
     * 
     * @param resourceData
     *            - resource data to construct the generated grid resource.
     * @param loadProperties
     * @param calculator
     */
    public GeneratedEnsembleGridResource(
            GeneratedEnsembleGridResourceData resourceData,
            LoadProperties loadProperties, IMapDescriptor mapDescriptor) {

        super((GridResourceData) resourceData, loadProperties);
        this.setDescriptor(mapDescriptor);

        // pass name generator. not works, May implement it later
        if (((AbstractResourceData) resourceData).getNameGenerator() == null) {
            ((AbstractResourceData) resourceData)
                    .setNameGenerator(new GridNameGenerator());
        }

        this.getProperties().setSystemResource(true);

        // Set color
        ColorableCapability colorable = (ColorableCapability) this
                .getCapability(ColorableCapability.class);
        rand = new Random();
        RGB color = new RGB(rand.nextInt(206) + 50, rand.nextInt(206) + 50,
                rand.nextInt(206) + 50);
        colorable.setColor(color);

    };

    /**
     * Constructor
     * 
     * @param resourceData
     *            - resource data to construct the generated grid resource.
     * @param loadProperties
     */
    public GeneratedEnsembleGridResource(
            GeneratedEnsembleGridResourceData resourceData,
            LoadProperties loadProperties) {

        super((GridResourceData) resourceData, loadProperties);

        ColorableCapability colorable = (ColorableCapability) this
                .getCapability(ColorableCapability.class);
        RGB color = getRandomColor();
        colorable.setColor(color);

    }

    /**
     * generate a random color
     * 
     * @return
     */
    private RGB getRandomColor() {

        rand = new Random();
        final int lowerFilter = 80;
        final int upperFilter = 200;
        final int skewToBrightness = 256 - upperFilter;

        int r = rand.nextInt(upperFilter);
        if (r < lowerFilter) {
            r = lowerFilter;
        }

        int g = rand.nextInt(upperFilter);
        if (g < lowerFilter) {
            g = lowerFilter;
        }

        int b = rand.nextInt(upperFilter);
        if (b < lowerFilter) {
            b = lowerFilter;
        }

        r += skewToBrightness;
        g += skewToBrightness;
        b += skewToBrightness;

        return new RGB(r, g, b);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.general.AbstractGridResource#initInternal(com
     * .raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        // Set DisplayTypeCapability with CONTOUR IMAGE
        if (parameter != null) {
            String paramAbbrev = parameter.getAbbreviation();
            ((DisplayTypeCapability) this
                    .getCapability(DisplayTypeCapability.class))
                    .setAlternativeDisplayTypes(FieldDisplayTypesFactory
                            .getInstance().getDisplayTypes(paramAbbrev));
        }

        super.initInternal(target);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource#
     * getLegendParameters()
     */
    @Override
    public LegendParameters getLegendParameters() {
        GridRecord record = getCurrentGridRecord();
        if (record == null) {
            record = getAnyGridRecord();
            if (record == null) {
                return null;
            }
        }
        LegendParameters legendParams = new LegendParameters();
        DatasetInfo info = DatasetInfoLookup.getInstance().getInfo(
                record.getDatasetId());
        if (info == null) {
            legendParams.model = record.getDatasetId();
        } else {
            legendParams.model = info.getTitle();
        }
        legendParams.level = record.getLevel();
        legendParams.parameter = record.getParameter().getName();
        legendParams.ensembleId = record.getEnsembleId();
        legendParams.dataTime = descriptor.getFramesInfo().getTimeForResource(
                this);

        if (stylePreferences != null) {
            legendParams.unit = stylePreferences.getDisplayUnitLabel();
        }

        if (legendParams.unit == null || legendParams.unit.isEmpty()) {
            if (record.getParameter().getUnit().equals(Unit.ONE)) {
                legendParams.unit = "";
            } else {
                legendParams.unit = record.getParameter().getUnitString();
            }
        }
        List<DisplayType> displayTypes = FieldDisplayTypesFactory.getInstance()
                .getDisplayTypes(record.getParameter().getAbbreviation());
        DisplayType displayType = getDisplayType();
        if (displayTypes != null && !displayTypes.isEmpty()
                && displayTypes.get(0).equals(displayType)) {
            // The default type does not display in the legend
            legendParams.type = "";
        } else if (displayType == DisplayType.STREAMLINE) {
            legendParams.type = "Streamlines";
        } else if (displayType == DisplayType.BARB) {
            legendParams.type = "Wind Barbs";
        } else if (displayType == DisplayType.ARROW) {
            legendParams.type = "Arrows";
        } else if (displayType == DisplayType.IMAGE) {
            legendParams.type = "Img";
        }
        return legendParams;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.rsc.general.GridResource#getName()
     */
    @Override
    public String getName() {
        if (resourceData == null) {
            return super.getName();
        }
        AbstractNameGenerator generator = resourceData.getNameGenerator();
        if (generator == null) {

            return (int) (randomRec.getLevel().getLevelonevalue())
                    + randomRec.getLevel().getMasterLevel().getName() + " "
                    + calculation.getTitle() + " " + parameter.getName();

        }
        return calculation.getTitle() + " " + generator.getName(this);
    }

    /**
     * Update a calculation display if the data is changed The real update will
     * be implemented later.
     * 
     * @param dataMap
     *            -The loaded member data
     * 
     *            TODO
     */
    public void updateData(Map<DataTime, List<GeneralGridData>> dataMap) {

        // Replace the data. Clear all the data and related before setting
        clearRequestedData();
        this.setData(dataMap);

        // Set the data times
        this.dataTimes.addAll(dataMap.keySet());

        // If there is no any ensemble member, unload this resource

        // If any member data is changed re-calculate and update display

    }

    public ArrayList<String> getModels() {
        return models;
    }

    public void setModels(ArrayList<String> models) {
        this.models = models;
    }

    public Calculation getCalculation() {
        return calculation;
    }

    public void setCalculation(Calculation calculation) {
        this.calculation = calculation;
    }

    /**
     * Set parameter of any grid resource
     */
    public void setParameter(GridResource rcs) {
        if (parameter != null)
            return;

        GridRecord randomRec = rcs.getAnyGridRecord();

        setMatchCriteria(randomRec);

        if (randomRec != null) {
            parameter = randomRec.getParameter();
            this.randomRec = randomRec;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.rsc.general.GridResource#getMatchCriteria()
     */
    @Override
    public ParamLevelMatchCriteria getMatchCriteria() {
        return paramLevelMatchCriteria;
    }

    /**
     * Match criteria for generated grid displaying.
     * 
     * @param record
     *            - any grid record for get the parameters
     */
    private void setMatchCriteria(GridRecord record) {

        if (record == null) {
            return;
        }
        ParamLevelMatchCriteria matchCriteria = new ParamLevelMatchCriteria();
        matchCriteria.setParameterName(new ArrayList<String>());
        matchCriteria.setLevels(new ArrayList<Level>());
        matchCriteria.setCreatingEntityNames(new ArrayList<String>());
        String parameter = record.getParameter().getAbbreviation();
        SingleLevel level = GridLevelTranslator.constructMatching(record
                .getLevel());
        String creatingEntity = record.getDatasetId();
        if (!matchCriteria.getParameterNames().contains(parameter)) {
            matchCriteria.getParameterNames().add(parameter);
        }
        if (!matchCriteria.getLevels().contains(level)) {
            matchCriteria.getLevels().add(level);
        }
        if (!matchCriteria.getCreatingEntityNames().contains(creatingEntity)) {
            matchCriteria.getCreatingEntityNames().add(creatingEntity);
        }
        paramLevelMatchCriteria = matchCriteria;
    }

    /**
     * Implement later(after image loading), see D2DGridResource Data
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.rsc.general.AbstractGridResource#inspect(com.raytheon
     *      .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        // TODO
        return "Not implemented";
    }

    @Override
    public ResourceOrder getResourceOrder() {
        return ResourceOrder.HIGHEST;
    }

}
