/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.cloudheight.rsc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.Measure;
import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Temperature;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.eclipse.swt.graphics.RGB;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.grid.GridConstants;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.TransformFactory;
import com.raytheon.uf.common.sounding.VerticalSounding;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.common.sounding.adapter.IVerticalSoundingProvider;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.cloudheight.data.CloudHeightData;
import com.raytheon.uf.viz.cloudheight.data.CloudHeightData.DisplayOption;
import com.raytheon.uf.viz.cloudheight.impl.CloudHeightCalculatorPorted;
import com.raytheon.uf.viz.cloudheight.impl.CloudHeightCalculatorPorted.CloudHeightResult;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceGroup;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.map.GeoUtil;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Cloud height resource, uses an {@link IVerticalSoundingProvider} to obtain a
 * sounding for calculating the height of a satellite temperature reading
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 18, 2011  2190     mschenke    Initial creation
 * Oct  2, 2013  2333     mschenke    Converted to use IGridGeometryProvider
 * Nov 20, 2013  2492     bsteffen    Refactor deprecated references to
 *                                    ColorMapParameters.getDataUnit
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CloudHeightResource extends
        AbstractVizResource<CloudHeightResourceData, IMapDescriptor> {

    /** String id to look for satellite-provided data values */
    public static final String SATELLITE_DATA_INTERROGATE_ID = "satelliteDataValue";

    /** String id for specifying the cloud top height based on the sat reading */
    public static final String HEIGHT_INTERROGATE_ID = "height";

    /** String id for specifying the temperature associated with the height */
    public static final String DATA_VALUE_INTERROGATE_ID = "dataValue";

    private static final CloudHeightData algorithmData = CloudHeightData
            .getCloudHeightData();

    private static final Unit<Temperature> TEMP_UNIT = SI.CELSIUS;

    private UnitConverter tempToK = TEMP_UNIT.getConverterTo(SI.KELVIN);

    /** Provider used to provide the sounding for the calculation */
    private IVerticalSoundingProvider soundingProvider;

    /** Sounding used to calculate the cloud-height */
    private VerticalSounding sounding;

    /** Temperature cloud-height was calculated for */
    private Measure<?, ?> temperature;

    /** Height of the temperature value */
    private Measure<?, ?> cloudHeight;

    /**
     * The cloud-height calculated based on {@link DisplayOption}, may be null
     * if {@link DisplayOption#NONE} is set
     */
    private Measure<?, ?> modeCloudHeight;

    /** Status returned from the cloud height calculator */
    private String cloudHeightStatus;

    /** The location the cloud-height was calculated for */
    private Coordinate cloudHeightLocation;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected CloudHeightResource(CloudHeightResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapability(ColorableCapability.class).setColor(
                new RGB(255, 255, 255));
    }

    @Override
    public String getName() {
        return "Cloud Height";
    }

    @Override
    public synchronized String inspect(ReferencedCoordinate coord)
            throws VizException {
        // Ensure cloud height is computed for this coordinate
        computeCloudHeightParameters(coord);

        if (sounding == null) {
            return null;
        } else if (sounding.size() == 0 || cloudHeight == null) {
            return "NO "
                    + String.valueOf(soundingProvider.getSoundingSource())
                            .toUpperCase() + " DATA";
        }

        // Convert cloudHeight to feet and rount to nearest hundreds place
        int heightInFeet = ((int) (getDataValue(cloudHeight, NonSI.FOOT) / 100 + 0.5) * 100);
        String heightStr = String.valueOf(heightInFeet);

        if (modeCloudHeight != null) {
            int otherHeightInFeet = ((int) (getDataValue(modeCloudHeight,
                    NonSI.FOOT) / 100 + 0.5) * 100);
            if (otherHeightInFeet >= 0) {
                heightStr += "/" + otherHeightInFeet;
            }
        }

        return String.format("%s feet (%s %s)", heightStr, sounding
                .getStationId(), cloudHeightStatus != null ? cloudHeightStatus
                : "");
    }

    /**
     * Attempts to extract a numeric data value from a {@link Measure} object
     * converting it into desiredUnit before returning. {@link Double#NaN} will
     * be returned if non-numeric measure or units are not compatible
     * 
     * @param measure
     * @param desiredUnit
     * @return
     */
    private double getDataValue(Measure<?, ?> measure, Unit<?> desiredUnit) {
        if (desiredUnit.isCompatible(measure.getUnit())) {
            Object measuredObject = measure.getValue();
            if (measuredObject instanceof Number) {
                return measure.getUnit().getConverterTo(desiredUnit)
                        .convert(((Number) measuredObject).doubleValue());
            }
        }
        return Double.NaN;
    }

    @Override
    public synchronized Map<String, Object> interrogate(
            ReferencedCoordinate coord) throws VizException {
        // Ensure cloud height is computed for this resource
        computeCloudHeightParameters(coord);

        Map<String, Object> dataMap = new HashMap<String, Object>();
        dataMap.put(HEIGHT_INTERROGATE_ID, cloudHeight);
        dataMap.put(DATA_VALUE_INTERROGATE_ID, temperature);
        dataMap.put(DataTime.class.toString(),
                sounding != null ? sounding.getDataTime() : null);
        return dataMap;
    }

    private void computeCloudHeightParameters(ReferencedCoordinate coord) {
        Coordinate location = null;
        Measure<?, ?> cloudHeight = null;
        Measure<?, ?> modeCloudHeight = null;
        Measure<?, ?> temperature = null;
        String cloudHeightStatus = null;
        VerticalSounding sounding = null;
        IVerticalSoundingProvider provider = null;

        // Get coordinate as lat/lon
        try {
            location = coord.asLatLon();
        } catch (Exception e) {
            UFStatus.getHandler().handle(Priority.PROBLEM,
                    "Error converting sample coordinate", e);
        }

        // Get the provider to use
        for (IVerticalSoundingProvider vsp : descriptor.getResourceList()
                .getResourcesByTypeAsType(IVerticalSoundingProvider.class)) {
            if (vsp.getSoundingSource() != null) {
                provider = vsp;
            }
        }

        // Ensure valid provider/location
        if (provider != null && location != null) {
            if (cloudHeightLocation != null
                    && cloudHeightLocation.equals(location)
                    && provider == soundingProvider
                    && provider.getSoundingSource().equals(
                            soundingProvider.getSoundingSource())) {
                // Don't compute for same point/source
                return;
            }

            FramesInfo currInfo = descriptor.getFramesInfo();

            // Get contributing resource
            AbstractVizResource<?, ?> contributingResource = null;
            float cloudTemp = Float.NaN;
            Float otherTempToUse = null;
            List<AbstractVizResource<?, ?>> contributors = getValidContributors(
                    new ArrayList<AbstractVizResource<?, ?>>(),
                    descriptor.getResourceList());
            Collections.reverse(contributors);
            for (AbstractVizResource<?, ?> resource : contributors) {
                float[] temps = getTemperaturesForResource(resource, currInfo,
                        location);
                if (temps != null) {
                    contributingResource = resource;
                    cloudTemp = temps[0];

                    switch (algorithmData.getDisplayOption()) {
                    case PEAK:
                        otherTempToUse = temps[1];
                        break;
                    case PREDOMINANT:
                        otherTempToUse = temps[2];
                        break;
                    case LOW:
                        otherTempToUse = temps[3];
                        break;
                    default:
                        otherTempToUse = null;
                        break;
                    }
                    break;
                }
            }

            if (contributingResource != null) {
                // We have valid temperature contributing resource
                // Get a sounding for time of contributing resource
                sounding = provider.getSounding(
                        currInfo.getTimeForResource(contributingResource),
                        location);

                if (sounding != null) {
                    // Sounding found, calculate height of temps
                    float height = -1, otherHeight = -1;
                    if (sounding.size() == 0
                            && GridConstants.PLUGIN_NAME.equals(provider
                                    .getSoundingSource())) {
                        // Compute CLIMO VerticalSounding
                        sounding = new VerticalSounding();
                        sounding.setStationId("CLIMO");
                        sounding.setName(GeoUtil.formatCoordinate(location));
                        int day = Calendar.getInstance().get(
                                Calendar.DAY_OF_YEAR);

                        // Populates the sounding
                        height = CloudHeightCalculatorPorted
                                .getCloudHeightClimo(
                                        (float) tempToK.convert(cloudTemp),
                                        (float) location.y, day, sounding);

                        if (otherTempToUse != null) {
                            // Just calculates the height
                            otherHeight = CloudHeightCalculatorPorted
                                    .getCloudHeightClimo((float) tempToK
                                            .convert(otherTempToUse),
                                            (float) location.y, day, null);
                        }
                    } else if (sounding.size() > 0) {
                        // use sounding to compute heights
                        float[] muParcelTrajectory = WxMath
                                .derivemuParcelTrajectory(sounding);

                        CloudHeightResult result = CloudHeightCalculatorPorted
                                .getCloudHeightGrid(
                                        (float) tempToK.convert(cloudTemp),
                                        sounding, muParcelTrajectory);
                        cloudHeightStatus = result.status;
                        height = result.value;

                        if (otherTempToUse != null) {
                            result = CloudHeightCalculatorPorted
                                    .getCloudHeightGrid((float) tempToK
                                            .convert(otherTempToUse), sounding,
                                            muParcelTrajectory);
                            otherHeight = result.value;
                        }
                    }

                    if (height >= 0) {
                        cloudHeight = Measure.valueOf(height, SI.METER);
                        if (otherHeight >= 0) {
                            modeCloudHeight = Measure.valueOf(otherHeight,
                                    SI.METER);
                        }
                        temperature = Measure.valueOf(cloudTemp, TEMP_UNIT);
                    }
                }
            }
        }

        // Assign newly calculated fields
        this.soundingProvider = provider;
        this.sounding = sounding;
        this.cloudHeight = cloudHeight;
        this.modeCloudHeight = modeCloudHeight;
        this.temperature = temperature;
        this.cloudHeightStatus = cloudHeightStatus;
        this.cloudHeightLocation = location;
    }

    /**
     * Returns all resources that have a data unit compatible with
     * {@link #TEMP_UNIT} in their {@link ColorMapParameters}.
     * 
     * @param descriptor
     * @return
     */
    private static List<AbstractVizResource<?, ?>> getValidContributors(
            List<AbstractVizResource<?, ?>> resources, ResourceList list) {
        for (ResourcePair rp : list) {
            if (rp.getProperties().isVisible()
                    && isValidContributor(rp.getResource())) {
                resources.add(rp.getResource());
            }
        }

        List<AbstractVizResource<?, ?>> groups = list
                .getResourcesByType(IResourceGroup.class);
        for (AbstractVizResource<?, ?> group : groups) {
            if (group.getProperties().isVisible()) {
                getValidContributors(resources,
                        ((IResourceGroup) group).getResourceList());
            }
        }
        return resources;
    }

    public static boolean isValidContributor(AbstractVizResource<?, ?> rsc) {
        if (rsc != null && rsc.hasCapability(ColorMapCapability.class)) {
            ColorMapParameters params = rsc.getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            if (params != null && params.getColorMapUnit() != null
                    && TEMP_UNIT.isCompatible(params.getColorMapUnit())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Gets the cloud temp, predominate, peak, and low temps for the resource or
     * null if not valid temperatures
     * 
     * @param resource
     * @param currInfo
     * @param location
     * @return
     */
    private float[] getTemperaturesForResource(
            AbstractVizResource<?, ?> resource, FramesInfo currInfo,
            Coordinate location) {
        float[] temps = null;
        // Ensure resource has time at this frame
        DataTime timeForRsc = currInfo.getTimeForResource(resource);
        if (timeForRsc != null) {
            try {
                // Verify resource provides expected objects from
                // interrogate
                Map<String, Object> interMap = resource
                        .interrogate(new ReferencedCoordinate(location));
                if (interMap != null
                        && interMap.get(SATELLITE_DATA_INTERROGATE_ID) instanceof Measure
                        && interMap.containsKey(IGridGeometryProvider.class
                                .toString())) {
                    // Extract temperature values from the resource
                    float[] rscTemps = extractTemps(
                            location,
                            resource,
                            ((IGridGeometryProvider) interMap
                                    .get(IGridGeometryProvider.class.toString()))
                                    .getGridGeometry());
                    boolean good = true;
                    for (int i = 0; i < rscTemps.length; ++i) {
                        if (Float.isNaN(rscTemps[i])) {
                            good = false;
                        }
                    }
                    if (good) {
                        temps = rscTemps;
                    }
                }
            } catch (VizException e) {
                UFStatus.getHandler().handle(Priority.PROBLEM,
                        "Error interrogating resource", e);
            }
        }
        return temps;
    }

    private Coordinate convert(MathTransform mt, double[] point) {
        try {
            double[] out = new double[point.length];
            mt.transform(point, 0, out, 0, 1);
            return new Coordinate(out[0], out[1]);
        } catch (Exception e) {
            // Ingore and return null
        }
        return null;
    }

    private float[] extractTemps(Coordinate latLon,
            AbstractVizResource<?, ?> rsc, GridGeometry2D gridGeometry) {
        // Method loosely ported from SatPVImageDepict.C::interogate
        float[] temps = new float[] { Float.NaN, Float.NaN, Float.NaN,
                Float.NaN };

        MathTransform gridToLatLon, latLonToGrid;
        try {
            gridToLatLon = TransformFactory.gridToLatLon(gridGeometry,
                    PixelInCell.CELL_CENTER);
            latLonToGrid = gridToLatLon.inverse();
        } catch (Exception e) {
            // Ignore and return early
            return temps;
        }

        Coordinate c = convert(latLonToGrid,
                new double[] { latLon.x, latLon.y });

        if (c == null) {
            return temps;
        }

        int x = (int) c.x;
        int y = (int) c.y;

        double temp = getTemperature(rsc, latLon);
        if (Double.isNaN(temp)) {
            return temps;
        }

        double[] elements = new double[6400];
        int numElements = 0;
        int i, j, ii, jj;
        int yStart = -(int) (algorithmData.getNy() / 2);
        int yEnd = (int) algorithmData.getNy() / 2;

        int xStart = -(int) (algorithmData.getNx() / 2);
        int xEnd = (int) algorithmData.getNx() / 2;

        for (j = yStart; j < yEnd; j++) {
            jj = y + j;
            for (i = xStart; i < xEnd; i++) {
                ii = x + i;
                elements[numElements++] = getTemperature(rsc, gridToLatLon, ii,
                        jj);
            }
        }

        UnitConverter converter = CloudHeightCalculatorPorted.ALGORITHM_UNIT
                .getConverterTo(TEMP_UNIT);
        double[] calculated = new double[3];
        CloudHeightCalculatorPorted.findHighPredLowBrightness(elements,
                numElements, calculated);
        temps[0] = (float) converter.convert(temp);
        temps[1] = (float) converter.convert(calculated[0]);
        temps[2] = (float) converter.convert(calculated[1]);
        temps[3] = (float) converter.convert(calculated[2]);

        return temps;
    }

    /**
     * Interrogates the resource at the gridToLatLon transform, gridX,gridY
     * location and reads out the temperature value converting to
     * {@link CloudHeightCalculatorPorted#ALGORITHM_UNIT}
     * 
     * @param resource
     * @param gridToLatLon
     * @param gridX
     * @param gridY
     * @return
     */
    private double getTemperature(AbstractVizResource<?, ?> resource,
            MathTransform gridToLatLon, int gridX, int gridY) {
        return getTemperature(resource,
                convert(gridToLatLon, new double[] { gridX, gridY }));
    }

    /**
     * Interrogates the resource at the specified lat/lon location and reads out
     * the temperature value converting to
     * {@link CloudHeightCalculatorPorted#ALGORITHM_UNIT}
     * 
     * @param resource
     * @param spatialObject
     * @param gridX
     * @param gridY
     * @return
     */
    private double getTemperature(AbstractVizResource<?, ?> resource,
            Coordinate latLon) {
        double temperature = Double.NaN;
        if (latLon != null) {
            try {
                Map<String, Object> dataMap = resource
                        .interrogate(new ReferencedCoordinate(latLon));
                Object obj = dataMap.get(SATELLITE_DATA_INTERROGATE_ID);
                if (obj instanceof Measure) {
                    temperature = getDataValue((Measure<?, ?>) obj,
                            CloudHeightCalculatorPorted.ALGORITHM_UNIT);
                }
            } catch (VizException e) {
                // Ignore
            }
        }
        return temperature;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

    }
}
