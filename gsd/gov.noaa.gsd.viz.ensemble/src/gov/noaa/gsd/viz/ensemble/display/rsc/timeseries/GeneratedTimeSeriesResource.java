package gov.noaa.gsd.viz.ensemble.display.rsc.timeseries;

import gov.noaa.gsd.viz.ensemble.display.calculate.Calculation;

import java.text.NumberFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.TreeSet;

import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.grid.util.GridLevelTranslator;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.d2d.xy.adapters.timeseries.GridTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.adapter.AbstractTimeSeriesAdapter;
import com.raytheon.uf.viz.xy.timeseries.rsc.TimeSeriesResource;
import com.raytheon.viz.core.graphing.util.GraphPrefsFactory;
import com.raytheon.viz.core.graphing.xy.XYData;
import com.raytheon.viz.core.rsc.ICombinedResourceData.CombineOperation;

/**
 * Uses cached data and parameters of loaded members in the time series display,
 * generates an new data and construct a resource for it. Implement steps:
 * 1,Basic calculation display in same time points on X axis case 2, Basic
 * calculation display including different time points on X axis case
 * 3,Interactive select a location from map 4, Continuously change the location.
 * Should check on if the performance is OK, because current D2D Time series
 * display architecture makes the display very slow on getting a point data and
 * repeatedly display the background graphics. Maybe the baseline code will be
 * improved someday.
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
public class GeneratedTimeSeriesResource<T extends AbstractResourceData>
        extends TimeSeriesResource {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GeneratedTimeSeriesResource.class);

    // TODO : Needed for future use.
    // private final SimpleDateFormat timeSampleFormat = new SimpleDateFormat(
    // "HH:mm'Z' EEE");

    private Calculation calculation;

    /** the level I would prefer to use **/
    protected Level preferredLevel = null;

    /** the unit to convert to, matches preferredLevel unit **/
    protected Unit<?> preferredUnit = Unit.ONE;

    /** a map of levels to units for quick lookup **/
    protected Map<Level, Unit<?>> levelUnitMap = new HashMap<Level, Unit<?>>();

    /** first received parameter name **/
    protected String parameterName = "";

    protected String parameterAbbreviation = "";

    protected Random rand;

    protected String[] titles = null;

    public GeneratedTimeSeriesResource(GeneratedTimeSeriesResourceData data,
            LoadProperties props) {
        super(data, props, null);

        // remember the resource data
        resourceData = data;

        // Set line thickness
        OutlineCapability outLine = (OutlineCapability) this
                .getCapability((OutlineCapability.class));
        outLine.setOutlineWidth(3);

        // Set color
        ColorableCapability colorable = (ColorableCapability) this
                .getCapability(ColorableCapability.class);
        rand = new Random();
        RGB color = new RGB(rand.nextInt(206) + 50, rand.nextInt(206) + 50,
                rand.nextInt(206) + 50);
        colorable.setColor(color);

        // Initial the data times for needing in the system, it will be updated
        this.dataTimes = new HashSet<DataTime>(data.getAllDataTimes());
    }

    @Override
    public String[] getTitles() {
        return titles;
    }

    public void setTitles(String[] titles) {
        this.titles = titles;
    }

    /**
     * Update the calculation generated time series resource when any member
     * resource or other changed. Will be fully implemented later.
     */
    public void updateData() {
        // Setup generated resource

        // TODO resource.secondaryResource? consider it later
        // TODO resource.combineOperation?consider it later

        this.setData(((GeneratedTimeSeriesResourceData) this.getResourceData())
                .calculate());

        // Set the data times for painting, that need changes dataTimes
        // to protected in TimeSeriesResource class.
        dataTimes = new TreeSet<DataTime>();
        for (XYData d : data.getData()) {
            dataTimes.add((DataTime) d.getX());
        }

    }

    public String getParameterName() {
        return parameterName;
    }

    public Calculation getCalculation() {
        return calculation;
    }

    public void setCalculation(Calculation c) {
        calculation = c;
    }

    /**
     * Prepare the parameters with any time series record.
     */
    public void setParameters() {

        AbstractTimeSeriesAdapter<?> adapter = ((GeneratedTimeSeriesResourceData) resourceData)
                .getAnyMembersResource().getAdapter();
        if (!(adapter instanceof GridTimeSeriesAdapter)) {
            return;
        }

        setTitles(((GeneratedTimeSeriesResourceData) resourceData)
                .getAnyMembersResource().getTitles());

        PluginDataObject pdo = ((GridTimeSeriesAdapter) adapter)
                .getArbitraryRecord();

        // store off records by level
        if (pdo instanceof GridRecord) {

            GridRecord record = (GridRecord) pdo;

            // set preferredLevel to first level
            if (preferredLevel == null) {
                preferredLevel = record.getLevel();
                ((GeneratedTimeSeriesResourceData) resourceData).level = preferredLevel
                        .toString();
                preferredUnit = record.getParameter().getUnit();
                ((GeneratedTimeSeriesResourceData) resourceData).unit = preferredUnit
                        .toString();
                parameterName = record.getParameter().getName();
                parameterAbbreviation = record.getParameter().getAbbreviation();
                if (parameterName == null || parameterName.isEmpty()) {
                    if (parameterAbbreviation == null) {
                        parameterAbbreviation = "";
                    }
                    parameterName = parameterAbbreviation;
                }
            }

            // add Unit to levelUnitMap if needed ( quick look-ups )
            Level lvl = record.getLevel();
            Unit<?> unit = levelUnitMap.get(lvl);
            if (unit == null) {
                unit = record.getParameter().getUnit();
                levelUnitMap.put(lvl, unit);
            }

        } else {
            // this shouldn't happen, code expects all pdo's for
            // GribTimeSeriesAdapter to be GridRecords
            String message = "Unexpected PluginDataObject type; got "
                    + pdo.getClass().getName() + " expected GridRecord";
            statusHandler.handle(Priority.PROBLEM, message, new Exception(
                    message));
        }

        // Added interface to TimeSeriesResource solution
        // parameterName = this.getAdapter().getParameterName();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        if (secondaryResource != null) {
            secondaryResource.setDescriptor(this.descriptor);
            secondaryResource.init(target);
        }

        // Load the Graph Preferences
        if (prefs == null) {
            try {
                String name = preferredLevel.getMasterLevel().getName();
                SingleLevel level = GridLevelTranslator.construct(name);
                level.setValue(preferredLevel.getLevelonevalue());
                prefs = GraphPrefsFactory.buildPreferences(
                        ((GeneratedTimeSeriesResourceData) resourceData)
                                .getYParameter().code, level);

            } catch (StyleException e) {
                throw new VizException(e);
            }
        }

        if (prefs != null && prefs.getDisplayUnits() != null) {
            units = prefs.getDisplayUnitLabel();
        }

        if (secondaryResource != null
                && combineOperation != CombineOperation.NONE) {
            // TODO
            // combineData();
        }

    }

    /*
     * implement later for secondaryResource
     * 
     * @Override private void combineData() { if (secondaryResource.prefs !=
     * null && secondaryResource.prefs.getDisplayUnits() != null && prefs !=
     * null && prefs.getDisplayUnits() != null) { }
     */

    /**
     * Get the legend name of this resource
     */
    @Override
    public String getName() {

        String resultingName = "<No Generated Times Series: "
                + calculation.getTitle() + ">";
        String levelKey = resourceData.getLevelKey();
        String levelUnit = levelKey.replaceAll("[^a-zA-Z]", "");
        boolean isHeight = levelUnit.equalsIgnoreCase("mb")
                || levelUnit.equalsIgnoreCase("agl")
                || levelUnit.contains("Agl");

        NumberFormat nf = NumberFormat.getInstance();
        nf.setMaximumFractionDigits(1);
        String lon = "";
        String lat = "";
        if (resourceData.getCoordinate() != null) {
            double x = resourceData.getCoordinate().x;
            double y = resourceData.getCoordinate().y;
            lon = nf.format(Math.abs(x));
            lat = nf.format(Math.abs(y));

            String stnID = "";

            String source = calculation.getTitle();

            // Make legend for point data
            StringBuilder sb = new StringBuilder(String.format(
                    "%s pt%s %s%s %s%s", source, resourceData.getPointLetter(),
                    lat, y >= 0 ? "N" : "S", lon, x >= 0 ? "E" : "W"));

            if (stnID != null) {
                sb.append(" ").append(stnID);
            }
            if (!isHeight) {
                sb.append(" ").append(resourceData.getLevelKey());
            }
            sb.append(String.format(" %s %s %s", parameterName, "TSer",
                    units != null && units.equals("") == false ? "(" + units
                            + ")" : ""));
            resultingName = sb.toString();
        }

        return resultingName;

    }

}
