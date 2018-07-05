package gov.noaa.gsd.viz.ensemble.display.rsc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.util.GridLevelTranslator;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.rsc.AbstractGridResource;
import com.raytheon.uf.viz.core.grid.rsc.data.GeneralGridData;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.DisplayType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;
import com.raytheon.uf.viz.core.rsc.capabilities.DisplayTypeCapability;
import com.raytheon.viz.grid.rsc.GridNameGenerator;
import com.raytheon.viz.grid.rsc.GridNameGenerator.IGridNameResource;
import com.raytheon.viz.grid.rsc.GridNameGenerator.LegendParameters;
import com.raytheon.viz.grid.rsc.general.GridResource;
import com.raytheon.viz.grid.xml.FieldDisplayTypesFactory;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;
import gov.noaa.gsd.viz.ensemble.display.calculate.ERFCalculator;
import gov.noaa.gsd.viz.ensemble.display.calculate.EnsembleCalculator;

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
 * Jan 10, 2014    5056     jing       Initial creation
 * Dec 26, 2016    19325    jing       Display and sample image
 * Feb 17, 2017    19325    jing       Added ERF image capability
 * 
 *          </pre>
 * 
 * @param <T>
 */
@SuppressWarnings("rawtypes")
public class GeneratedEnsembleGridResource
        extends AbstractGridResource<GeneratedEnsembleGridResourceData>
        implements IGridNameResource {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeneratedEnsembleGridResource.class);

    private Parameter parameter = null;

    private GridRecord randomRec = null;

    private ParamLevelMatchCriteria paramLevelMatchCriteria = null;

    private ArrayList<String> models = null;

    private Calculation calculation = Calculation.NONE;

    protected Random rand;

    /**
     * Calculate loaded ensemble data to generate new data, such as mean.
     */
    protected EnsembleCalculator calculator = null;

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
            LoadProperties loadProperties, IMapDescriptor mapDescriptor,
            EnsembleCalculator c) {

        super(resourceData, loadProperties);

        this.setDescriptor(mapDescriptor);

        calculator = c;
        // pass name generator. not works, May implement it later
        if (((AbstractResourceData) resourceData).getNameGenerator() == null) {
            ((AbstractResourceData) resourceData)
                    .setNameGenerator(new GridNameGenerator());
        }

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
            LoadProperties loadProperties, EnsembleCalculator c) {

        super(resourceData, loadProperties);

        calculator = c;

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
                            .setAlternativeDisplayTypes(
                                    FieldDisplayTypesFactory.getInstance()
                                            .getDisplayTypes(paramAbbrev));
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

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.rsc.general.GridResource#getName()
     */
    @Override
    public String getName() {
        String rscName = null;
        if (resourceData == null) {
            rscName = super.getName();
        } else {
            String image = "";
            if (calculator.isImage()) {
                image = " Img";
            }
            AbstractNameGenerator generator = resourceData.getNameGenerator();
            if (calculation == Calculation.ENSEMBLE_RELATIVE_FREQUENCY) {
                rscName = calculator.getName();
                rscName += image;
            } else if (generator == null) {

                rscName = (int) (randomRec.getLevel().getLevelonevalue())
                        + randomRec.getLevel().getMasterLevel().getName() + " "
                        + calculation.getTitle() + image + " "
                        + parameter.getName();
            } else {
                rscName = calculation.getTitle() + image + " "
                        + generator.getName(this);

            }
            rscName = rscName.concat(" (" + this.getDisplayUnit() + ")");
        }
        return rscName;
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

    @Override
    public DataTime[] getDataTimes() {
        /*
         * Update the calculated data if need
         */
        if (super.getDataTimes() == null || super.getDataTimes().length == 0) {
            try {
                this.getResourceData().update();
            } catch (VizException e) {
                statusHandler.handle(Priority.ERROR, "Error Recalculating", e);
            }
        }

        return super.getDataTimes();
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

        /**
         * Reset the parameter for ERF
         */
        if (calculator instanceof ERFCalculator) {
            parameter.setAbbreviation("ERF");
            parameter.setName("ERF");
            parameter.setUnitString("%");
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
        SingleLevel level = GridLevelTranslator
                .constructMatching(record.getLevel());
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

    @Override
    protected ColorMapParameters createColorMapParameters(GeneralGridData data)
            throws VizException {
        ColorMapParameters colorMapParam;
        try {
            colorMapParam = super.createColorMapParameters(data);
        } catch (VizException e) {
            throw new VizException("Unable to build colormap parameters", e);
        }

        // Set ERF image color bar labels
        if (colorMapParam != null && parameter.getName().contains("ERF")) {
            colorMapParam.setColorMapMin(-5);
            colorMapParam.setColorMapMax(105);
        }
        return colorMapParam;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {

        if (resourceData.isSampling()) {
            if (getDisplayType() == DisplayType.ARROW) {
                Map<String, Object> map = interrogate(coord);
                if (map == null) {
                    return "NO DATA";
                }
                double value = (Double) map.get(INTERROGATE_VALUE);
                return sampleFormat.format(value) + map.get(INTERROGATE_UNIT);
            } else if (getDisplayType() == DisplayType.CONTOUR) {
                if (parameter != null) {
                    return parameter.getAbbreviation() + "="
                            + super.inspect(coord);
                }
            }
        } else if (getDisplayType() != DisplayType.IMAGE) {
            return null;
        }
        return super.inspect(coord);
    }

    @Override
    public ResourceOrder getResourceOrder() {
        return ResourceOrder.HIGHEST;
    }

    @Override
    public List<GeneralGridData> getData(DataTime time,
            List<PluginDataObject> pdos) throws VizException {
        return null;
    }

}
