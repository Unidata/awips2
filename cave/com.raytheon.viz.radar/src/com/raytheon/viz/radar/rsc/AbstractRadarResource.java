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
package com.raytheon.viz.radar.rsc;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.measure.Quantity;
import javax.measure.Unit;
import javax.measure.UnitConverter;
import javax.measure.quantity.Length;

import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.cache.CacheObject.ICacheObjectCallback;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.extratext.ExtraTextResourceData;
import com.raytheon.uf.viz.core.rsc.extratext.IExtraTextGeneratingResource;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogatable;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.raytheon.uf.viz.core.rsc.interrogation.StringInterrogationKey;
import com.raytheon.uf.viz.d2d.core.sampling.ID2DSamplingResource;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.core.time.ID2DTimeMatchingExtension;
import com.raytheon.uf.viz.d2d.core.time.TimeMatcher;
import com.raytheon.viz.radar.DefaultVizRadarRecord;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.frame.RadarDataTime;
import com.raytheon.viz.radar.frame.SailsFrameCoordinator;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.interrogators.RadarDefaultInterrogator;
import com.raytheon.viz.radar.textcontributors.IRadarTextContributor;
import com.raytheon.viz.radar.textcontributors.UpperTextSet;

import si.uom.NonSI;
import systems.uom.common.USCustomary;
import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

/**
 * Top level radar resource that contains the code that is shared by all below
 * resources
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ------------------------------------------
 * Aug 03, 2010  6584     mnash      Initial creation
 * Mar 05, 2013  15313    kshresth   Added sampling for DMD
 * Apr 11, 2013  16030    dfriedman  Fix NPE.
 * May 05, 2014  17201    dfriedman  Enable same-radar time matching.
 * Jun 11, 2014  2061     bsteffen   Move rangeable methods to radial resource
 * May 13, 2015  4461     bsteffen   Add sails frame coordinator.
 * Sep 03, 2015  4779     njensen    Removed IDataScale
 * Nov 03, 2015  4857     bsteffen   Set volume scan interval in time matcher
 * May 19, 2016  3253     bsteffen   Refactor of extra text.
 * Sep 13, 2016  3239     nabowle    Implement Interrogatable.
 * Nov 28, 2017  5863     bsteffen   Change dataTimes to a NavigableSet
 * Apr 15, 2019  7596     lsingh     Updated units framework to JSR-363.
 *                                   Handled unit conversion
 * Mar 02, 2021  22247    smoorthy   Added volume scan time calculation logic using
 *                                   cached data times.
 *
 * </pre>
 *
 * @author mnash
 */
public class AbstractRadarResource<D extends IDescriptor>
        extends AbstractVizResource<RadarResourceData, D>
        implements IResourceDataChanged, IExtraTextGeneratingResource,
        ICacheObjectCallback<RadarRecord>, ID2DTimeMatchingExtension,
        Interrogatable {

    /*
     * TODO This is dumb that a class with a name starting with Abstract is not
     * actually abstract. Either rename this class or actually apply the
     * abstract modifier.
     */

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractRadarResource.class);

    public enum InspectLabels {
        Mnemonic, Value, Angle, Shear, MSL, AGL, Azimuth, Range, ICAO
    }

    private static final Set<InterrogationKey<?>> defaultInspectLabels = new HashSet<>(
            Arrays.asList(new InterrogationKey<?>[] { Interrogator.VALUE,
                    RadarDefaultInterrogator.VALUE_STRING,
                    RadarDefaultInterrogator.SHEAR,
                    RadarDefaultInterrogator.MSL, RadarDefaultInterrogator.AGL,
                    IRadarInterrogator.RANGE, IRadarInterrogator.AZIMUTH,
                    IRadarInterrogator.ICAO }));

    private static final Set<InterrogationKey<?>> primaryInspectLabels = new HashSet<>(
            Arrays.asList(new InterrogationKey<?>[] {
                    RadarDefaultInterrogator.MNEMONIC, Interrogator.VALUE,
                    RadarDefaultInterrogator.VALUE_STRING,
                    RadarDefaultInterrogator.SHEAR,
                    RadarDefaultInterrogator.MSL, RadarDefaultInterrogator.AGL,
                    IRadarInterrogator.RANGE, IRadarInterrogator.AZIMUTH,
                    IRadarInterrogator.ICAO }));

    private static final Set<InterrogationKey<?>> secondaryInspectLabels = new HashSet<>(
            Arrays.asList(new InterrogationKey<?>[] { Interrogator.VALUE,
                    RadarDefaultInterrogator.VALUE_STRING,
                    RadarDefaultInterrogator.SHEAR, IRadarInterrogator.ICAO }));

    private static final Set<InterrogationKey<?>> offscreenInspectLabels = new HashSet<>(
            Arrays.asList(new InterrogationKey<?>[] {
                    RadarDefaultInterrogator.MNEMONIC, Interrogator.VALUE,
                    RadarDefaultInterrogator.VALUE_STRING,
                    RadarDefaultInterrogator.PRIMAY_ELEVATION_ANGLE,
                    RadarDefaultInterrogator.SHEAR }));

    private IRadarInterrogator interrogator;

    public String icao;

    protected DataTime displayedDate;

    protected float displayedLevel = -1;

    protected String actualLevel = "";

    protected Map<DataTime, VizRadarRecord> radarRecords;

    protected Map<DataTime, String[]> upperTextMap = new HashMap<>();

    protected Coordinate radarLocation = null;

    protected static final RadarInfoDict infoDict;

    static {
        File radarInfo = PathManagerFactory.getPathManager()
                .getStaticFile("radarInfo.txt");
        if (radarInfo != null) {
            infoDict = RadarInfoDict.getInstance(radarInfo.getParent());
        } else {
            infoDict = null;
        }
    }

    protected AbstractRadarResource(RadarResourceData resourceData,
            LoadProperties loadProperties, IRadarInterrogator interrogator) {
        this(resourceData, loadProperties);
        this.interrogator = interrogator;
    }

    protected AbstractRadarResource(RadarResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        getCapability(ColorableCapability.class);
        resourceData.addChangeListener(this);

        radarRecords = Collections
                .synchronizedMap(new HashMap<DataTime, VizRadarRecord>());
        icao = "";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        SailsFrameCoordinator.addToDescriptor(descriptor);
        ExtraTextResourceData.addExtraTextResource(descriptor);
    }

    @Override
    protected void disposeInternal() {
        radarRecords.clear();
        upperTextMap.clear();
    }

    public void addRecord(PluginDataObject record) {
        if (!(record instanceof RadarRecord)) {
            statusHandler.handle(Priority.PROBLEM,
                    "" + this.getClass().getName() + " expected : "
                            + RadarRecord.class.getName() + " Got: " + record);
            return;
        }
        RadarRecord radarRecord = (RadarRecord) record;
        radarRecord.setAddSpatial(!resourceData.latest);
        icao = radarRecord.getIcao();
        if (radarRecord.getLatitude() != null
                && radarRecord.getLongitude() != null
                && radarLocation == null) {
            radarLocation = new Coordinate(radarRecord.getLongitude(),
                    radarRecord.getLatitude());
        }
        DataTime d = radarRecord.getDataTime();

        VizRadarRecord existing = getRadarRecord(d);
        if (existing != null) {
            if (existing.getNumLevels() != null && !existing.getNumLevels()
                    .equals(radarRecord.getNumLevels())) {
                // Use the one with the most levels
                if (existing.getNumLevels().intValue() < radarRecord
                        .getNumLevels().intValue()) {
                    remove(d);
                    existing = null;
                }
            } else if (existing.getGateResolution() != null
                    && !existing.getGateResolution()
                            .equals(radarRecord.getGateResolution())) {
                // use the one with the smallest resolution
                if (existing.getGateResolution().intValue() > radarRecord
                        .getGateResolution().intValue()) {
                    remove(d);
                    existing = null;
                }
            } else if (existing.getNumBins()
                    * existing.getNumRadials() != radarRecord.getNumBins()
                            * radarRecord.getNumRadials()) {
                // use the one with the most pixels
                if (existing.getNumBins()
                        * existing.getNumRadials() < radarRecord.getNumBins()
                                * radarRecord.getNumRadials()) {
                    remove(d);
                    existing = null;
                }
            } else if (existing.getInsertTime().getTimeInMillis() < radarRecord
                    .getInsertTime().getTimeInMillis()) {
                // Use the newest one
                remove(d);
                existing = null;
            }
        }
        if (existing == null) {
            existing = createVizRadarRecord(radarRecord);
            radarRecords.put(d, existing);
            synchronized (dataTimes) {
                dataTimes.add(d);
            }
        }
    }

    protected VizRadarRecord createVizRadarRecord(RadarRecord radarRecord) {
        return new DefaultVizRadarRecord(radarRecord);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        /*
         * TODO if this class actually goes abstract, this method should be
         * deleted and force subclasses to implement it
         */
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdoArr = (PluginDataObject[]) object;
            for (PluginDataObject record : pdoArr) {
                addRecord(record);
            }
        }
        issueRefresh();
    }

    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        InterrogateMap dataMap;
        if ("CZ-Pg".equals(resourceData.mode)) {
            return null;
        }
        // Grab current time
        DataTime displayedDate = descriptor.getTimeForResource(this);

        if (displayedDate == null) {
            FramesInfo fi = descriptor.getFramesInfo();
            DataTime[] times = fi.getTimeMap().get(this);
            int index = fi.getFrameIndex();
            if (times != null && index > 0 && index < times.length) {
                displayedDate = times[index];
            }
        }
        if (displayedDate == null) {
            displayedDate = this.displayedDate;
        }

        try {
            dataMap = interrogate(latLon, displayedDate,
                    getInterrogationKeys().toArray(new InterrogationKey<?>[0]));
        } catch (Exception e) {
            throw new VizException("Error converting coordinate for hover", e);
        }
        // determine if we are blended, if so are we the primary.
        boolean primary = true;
        if (this.hasCapability(BlendedCapability.class)) {
            int myIndex = this.getCapability(BlendedCapability.class)
                    .getResourceIndex();
            int hiddenIndex = this.getCapability(BlendedCapability.class)
                    .getBlendableResource().getResource()
                    .getCapability(BlendableCapability.class)
                    .getResourceIndex();
            primary = myIndex != hiddenIndex;

        }
        // determine if all pane sampling is enabled
        List<ID2DSamplingResource> samplingResources = descriptor
                .getResourceList()
                .getResourcesByTypeAsType(ID2DSamplingResource.class);
        boolean allPaneSample = false;
        if (!samplingResources.isEmpty()) {
            allPaneSample = samplingResources.get(0).isAllPanelSampling();
        }
        if (allPaneSample) {
            // When all pane sampling is on paint lots of info for the primary
            // on the visible pane, other panes return minimal info
            boolean visible = descriptor.getRenderableDisplay().getContainer()
                    .getActiveDisplayPane().getDescriptor() == descriptor;
            if (visible && primary) {
                return "="
                        + inspect(displayedDate, primaryInspectLabels, dataMap);
            } else {
                return " " + inspect(displayedDate, offscreenInspectLabels,
                        dataMap);
            }
        } else if (primary) {
            return inspect(displayedDate, dataMap);
        } else {
            // The secondary returns slightly less data
            return inspect(displayedDate, secondaryInspectLabels, dataMap);
        }
    }

    public String inspect(DataTime dataTime, InterrogateMap dataMap) {
        return inspect(dataTime, defaultInspectLabels, dataMap);
    }

    /**
     * Given the map of data values, return the inspection string
     *
     * @param dataMap
     * @return
     */
    public String inspect(DataTime dataTime, Set<InterrogationKey<?>> keys,
            InterrogateMap dataMap) {
        if (dataMap == null) {
            return "NO DATA";
        }

        StringBuilder displayedData = new StringBuilder();

        boolean containsValueString = containsNonNullKey(
                IRadarInterrogator.VALUE_STRING, keys, dataMap, null);

        Set<InterrogationKey<?>> keysToSkip;
        if (containsValueString) {
            keysToSkip = this.interrogator.getValueStringKeys();
        } else {
            keysToSkip = Collections.emptySet();
        }

        if (containsNonNullKey(IRadarInterrogator.MNEMONIC, keys, dataMap,
                keysToSkip)) {
            displayedData.append(dataMap.get(IRadarInterrogator.MNEMONIC))
                    .append(" ");
        }

        /*
         * Append either the value string or a formatted Value, but not both, as
         * the value string is either already formatted or more meaningful if
         * non-null.
         */
        if (containsValueString) {
            displayedData.append(dataMap.get(IRadarInterrogator.VALUE_STRING));
        } else if (containsNonNullKey(Interrogator.VALUE, keys, dataMap,
                keysToSkip)) {
            Quantity<?> value = dataMap
                    .get(Interrogator.VALUE);
            String format;
            if (value.getValue() instanceof Double
                    || value.getValue() instanceof Float) {
                format = "%.2f%s";
            } else {
                format = "%.0f%s";
            }
            displayedData.append(formatQuantity(value, null, format));
        }

        if (containsNonNullKey(IRadarInterrogator.PRIMAY_ELEVATION_ANGLE, keys,
                dataMap, keysToSkip)) {
            while (displayedData.length() < 15) {
                displayedData.append(" ");
            }
            displayedData.append(
                    dataMap.get(IRadarInterrogator.PRIMAY_ELEVATION_ANGLE));
        }

        if (containsNonNullKey(IRadarInterrogator.SHEAR, keys, dataMap,
                keysToSkip)) {
            displayedData.append(String.format(" %.4f/s",
                    dataMap.get(IRadarInterrogator.SHEAR)));
        }

        if (containsNonNullKey(IRadarInterrogator.MSL, keys, dataMap,
                keysToSkip)) {
            Quantity<Length> msl = dataMap
                    .get(IRadarInterrogator.MSL);
            displayedData.append(formatQuantity(msl, USCustomary.FOOT, " %.0f%sMSL "));

            Quantity<Length> agl = dataMap
                    .get(IRadarInterrogator.AGL);
            if (agl == null || Double.isNaN(agl.getValue().doubleValue())) {
                displayedData.append("???ft");
            } else {
                displayedData.append(formatQuantity(agl, USCustomary.FOOT, "%.0f%s"));
            }
            displayedData.append("AGL");
        }

        if (containsNonNullKey(IRadarInterrogator.AZIMUTH, keys, dataMap,
                keysToSkip)) {
            Quantity<Length> range = dataMap
                    .get(IRadarInterrogator.RANGE);
            displayedData
                    .append(formatQuantity(range, USCustomary.NAUTICAL_MILE,
                            " %.0fnm"))
                    .append("@").append(dataMap.get(IRadarInterrogator.AZIMUTH)
                            .to(NonSI.DEGREE_ANGLE).getValue().intValue());
        }

        if (containsNonNullKey(IRadarInterrogator.ICAO, keys, dataMap,
                keysToSkip)) {
            displayedData.append(' ')
                    .append(dataMap.get(IRadarInterrogator.ICAO));
        }

        if (displayedData.toString().contains("null")
                || displayedData.toString().isEmpty()) {
            displayedData.replace(0, displayedData.length(), "NO DATA");
        }

        return displayedData.toString();
    }

    /**
     * Converts the measure to the desired output unit (if provided) and then
     * returns a formatted string using the converted measure value and unit
     * string, provided in that respective order.
     *
     * If outputUnit is null or AbstractUnit.ONE, the output unit will be the measure's
     * current unit, if non-null.
     *
     * @param measure
     *            The measure. Must be non-null and have a non-null value, or
     *            null will be returned. If the unit is null, no conversion will
     *            happen and "" will be used as the unit String.
     * @param outputUnit
     *            The desired output unit. If null or AbstractUnit.ONE, measure will not
     *            be converted, and the current unit will be used for the unit
     *            string.
     * @param format
     *            The format String. Arguments to this will be the double value
     *            of the measure and the unit String, in that order. This must
     *            be non null.
     * @return The formatted output.
     */
    protected String formatQuantity(Quantity<?> quantity,
            Unit<?> outputUnit, String format) {
        if (quantity == null || quantity.getValue() == null || format == null) {
            return null;
        }

        Unit<?> currentUnit = quantity.getUnit();
        String unitString;
        if (currentUnit == null) {
            unitString = "";
        } else {
            unitString = SimpleUnitFormat.getInstance(SimpleUnitFormat.Flavor.ASCII).format(quantity.getUnit());
        }

        double value = quantity.getValue().doubleValue();
        if (quantity.getUnit() != outputUnit && quantity.getUnit() != null
                && outputUnit != null && !outputUnit.equals(AbstractUnit.ONE)) {
            UnitConverter toOutputUnit = UnitConv
                    .getConverterToUnchecked(quantity.getUnit(), outputUnit);
            value = toOutputUnit.convert(value);
            unitString = SimpleUnitFormat
                    .getInstance(SimpleUnitFormat.Flavor.ASCII)
                    .format(outputUnit);

        }
        return String.format(format, value, unitString);
    }

    protected boolean containsNonNullKey(InterrogationKey<?> key,
            Set<InterrogationKey<?>> keys, InterrogateMap dataMap,
            Set<InterrogationKey<?>> keysToSkip) {
        return (keysToSkip == null || !keysToSkip.contains(key))
                && keys.contains(key) && dataMap.containsKey(key)
                && dataMap.get(key) != null;
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        try {
            DataTime displayedDate = descriptor.getTimeForResource(this);

            if (displayedDate == null) {
                displayedDate = this.displayedDate;
            }
            InterrogateMap dataMap = interrogate(displayedDate,
                    coord.asLatLon());
            Map<String, Object> map = new HashMap<>();
            for (InterrogationKey<?> key : dataMap.keySet()) {

                if (key == Interrogator.VALUE) {
                    map.put("numericValue", dataMap.get(Interrogator.VALUE)
                            .getValue().toString());
                } else if (key.equals(RadarDefaultInterrogator.CRS_LOCATION)) {
                    double[] point = dataMap
                            .get(RadarDefaultInterrogator.CRS_LOCATION);
                    map.put(RadarDefaultInterrogator.CRS_LOCATION.getId(),
                            point[0] + "," + point[1]);
                } else if (key.equals(IRadarInterrogator.RANGE)) {
                    map.put(IRadarInterrogator.RANGE.getId(),
                            formatQuantity(dataMap.get(IRadarInterrogator.RANGE),
                                    USCustomary.NAUTICAL_MILE, "%.0fnm"));
                } else if (key instanceof StringInterrogationKey<?>) {
                    StringInterrogationKey<?> sKey = (StringInterrogationKey<?>) key;

                    String stringVal = null;
                    Object obj = dataMap.get(sKey);
                    if (obj instanceof Quantity<?>) {
                        stringVal = ((Quantity<?>) obj).getValue().toString();
                    } else {
                        stringVal = obj.toString();
                    }
                    map.put(sKey.getId(), stringVal);
                }
            }
            return map;
        } catch (TransformException e) {
            throw new VizException(
                    "Transformation error creating lat/lon from referenced coordinate",
                    e);
        } catch (FactoryException e) {
            throw new VizException(
                    "Error creating lat/lon from referenced coordinate", e);
        }

    }

    public InterrogateMap interrogate(DataTime dataTime, Coordinate latLon) {
        if (interrogator == null) {
            return new InterrogateMap();
        }
        ColorMapParameters params = null;
        if (hasCapability(ColorMapCapability.class)) {
            params = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
        }

        VizRadarRecord radarRecord = getRadarRecord(dataTime);
        if (radarRecord != null && radarRecord.getStoredDataAsync() != null) {
            return interrogator.sample(radarRecord, latLon, params,
                    interrogator.getInterrogationKeys());
        }
        return new InterrogateMap();
    }

    /**
     * Given the dataTime, returns the upper text info for that time
     *
     * @param time
     * @return
     */
    @Override
    public String[] getExtraText(DataTime time) {
        VizRadarRecord record = getRadarRecord(time);
        if (record == null) {
            return null;
        }
        String[] result = upperTextMap.get(time);
        if (result == null && !upperTextMap.containsKey(time)) {
            if (record.getStoredDataAsync() == null) {
                return null;
            }
            List<IRadarTextContributor> lines = UpperTextSet
                    .getContributors(record.getProductCode());
            if (lines != null) {
                result = new String[lines.size()];
                for (int i = 0; i < result.length; i++) {
                    result[i] = lines.get(i).contributeText(record);
                }
                // Remove blank lines from the end.
                while (result[result.length - 1].isEmpty()) {
                    result = Arrays.copyOfRange(result, 0, result.length - 1);
                }
            }
            upperTextMap.put(time, result);
        }
        return result;
    }

    public VizRadarRecord getRadarRecord(DataTime time) {
        return radarRecords.get(time);
    }

    public Map<DataTime, VizRadarRecord> getRadarRecords() {
        return radarRecords;
    }

    public Coordinate getRadarLocation() {
        return radarLocation;
    }

    @Override
    public void remove(DataTime dataTime) {
        synchronized (dataTimes) {
            super.remove(dataTime);
        }
        radarRecords.remove(dataTime);
        upperTextMap.remove(dataTime);
    }

    @Override
    public void objectArrived(RadarRecord object) {
        issueRefresh();
    }

    /**
     * The purpose of this method is to allow TDWR, SAILS, and MESO SAILS data
     * to time match products at different elevation angles correctly.
     *
     * For example MESO SAILS has a 0.5 elevation product about every 1.5
     * minutes and a 1.5 elevation product every 6 minutes. Normally when the
     * 4th 0.5 product comes in it stops matching the 1.5 product because the
     * 1.5 product is too old. This method determines what the actual interval
     * is between volume scans and sets it in the time matcher so that the time
     * matcher will match all the times within that interval.
     */
    @Override
    public void modifyTimeMatching(D2DTimeMatcher d2dTimeMatcher,
            AbstractVizResource<?, ?> rsc, TimeMatcher timeMatcher) {
        /*
         * In order to use the radar customizations, the time match basis must
         * be an AbstractRadarResource for the same icao. If it is not, return
         * early.
         */
        AbstractVizResource<?, ?> basis = d2dTimeMatcher.getTimeMatchBasis();
        if (!(basis instanceof AbstractRadarResource)) {
            return;
        }
        AbstractRadarResource<?> radarBasis = (AbstractRadarResource<?>) basis;
        RequestConstraint icaoRC = getResourceData().getMetadataMap()
                .get("icao");
        RequestConstraint basisIcaoRC = radarBasis.getResourceData()
                .getMetadataMap().get("icao");
        if (icaoRC == null || !icaoRC.equals(basisIcaoRC)) {
            return;
        }
        /*
         * Gather all the frame times that we can, sorted by elevation number.
         * The time between two frames with the same elevation number is the
         * volume scan interval.
         */
        Set<RadarRecord> records = new HashSet<>();
        records.addAll(this.getRadarRecords().values());
        records.addAll(radarBasis.getRadarRecords().values());
        Map<Integer, SortedSet<Date>> elevationTimeMap = new HashMap<>();
        for (RadarRecord record : records) {
            Integer elevation = record.getElevationNumber();
            SortedSet<Date> times = elevationTimeMap.get(elevation);
            if (times == null) {
                times = new TreeSet<>();
                elevationTimeMap.put(elevation, times);
            }
            times.add(record.getDataTime().getRefTime());
        }
        long minInterval1 = getMinVolumeScanInterval(
                radarBasis.getRadarRecords().values());
        long minInterval2 = getMinVolumeScanInterval(
                this.getRadarRecords().values());
        long minInteval = Math.min(minInterval1, minInterval2);
        if (minInteval < TimeUtil.MILLIS_PER_HOUR) {
            /*
             * 1 second padding to ensure that consecutive volume scans do not
             * overlap
             */
            minInteval -= TimeUtil.MILLIS_PER_SECOND;
            timeMatcher.setRadarOnRadar(minInteval);
        } else {
            long interval2 = intervalCalc(radarBasis);
            interval2 -= TimeUtil.MILLIS_PER_SECOND;
            timeMatcher.setRadarOnRadar(interval2);
        }
    }

    /**
     * Determine the minimum interval between volume scans for all the volume
     * scans that are covered by the provided records. If there is not enough
     * records at the same elvation number than this will return
     * {@link Long#MAX_VALUE}.
     */
    private static long getMinVolumeScanInterval(
            Collection<? extends RadarRecord> records) {
        /*
         * Sorting by elevation number ensures that VCP changes and SAILS frames
         * are properly separated and do not invalidate the result.
         */
        Map<Integer, SortedSet<Date>> elevationTimeMap = new HashMap<>();
        for (RadarRecord record : records) {
            Integer elevation = record.getElevationNumber();
            SortedSet<Date> times = elevationTimeMap.get(elevation);
            if (times == null) {
                times = new TreeSet<>();
                elevationTimeMap.put(elevation, times);
            }
            times.add(record.getDataTime().getRefTime());
        }
        long minVolumeScanInterval = Long.MAX_VALUE;
        for (SortedSet<Date> times : elevationTimeMap.values()) {
            if (times.size() > 1) {
                Date prev = null;
                for (Date date : times) {
                    if (prev != null) {
                        long interval = date.getTime() - prev.getTime();
                        minVolumeScanInterval = Math.min(interval,
                                minVolumeScanInterval);
                    }
                    prev = date;
                }

            }
        }
        return minVolumeScanInterval;
    }

    /**
     * Helper function for intervalCalc. Takes a list of DataTimes, expected to be RadarDataTimes, and
     * calculates the difference between the 2 latest times with elevation number 1. Return
     * empty Optional if this cannot be calculated.
     */
    public static Optional<Long> intervalCalcHelper(DataTime[] times) {

        long newest = -1;
        long secondNewest = -1;

        for (DataTime dt: times) {
            if (!(dt instanceof RadarDataTime)) { // should be RadarDataTimes only, stop calculating
                newest = -1;
                secondNewest = -1;
                break;
            }

            RadarDataTime rdt = ((RadarDataTime)dt);

            if (rdt.getElevationNumber() == 1 ) { //only considering elevation numbers of 1 for this
                long timeNew = rdt.getRefTime().getTime();
                if (timeNew > newest) {
                    secondNewest=newest;
                    newest = timeNew;
                }
                else if (timeNew > secondNewest) {
                    secondNewest = timeNew;
                }
            }
        }
        if (newest < 0 || secondNewest < 0) { //we don't have 2 elevation numbers of "1".
            return Optional.empty();
        }
        else {
            return Optional.of(newest - secondNewest);
        }
    }




    /**
     * Calculate the volume scan time estimate for a radarRsc utilizing the cached RadarDataTimes.
     * Use the difference between the 2 latest times with elevation number 1.
     */
    public static long intervalCalc(AbstractRadarResource<?> radarRsc) {
         try {
             //cached DataTimes
             DataTime[] radarRscTimes = radarRsc.getResourceData().getAvailableTimes();

             Optional<Long> volumeScanTime = intervalCalcHelper(radarRscTimes);

             //return the calculated volumeScanTime, or the default if the time is null
             return volumeScanTime.orElse(5 * TimeUtil.MILLIS_PER_MINUTE);
         }
        catch(Exception e) {
            statusHandler.handle(Priority.PROBLEM,
               "Exception occurred during calculation of volume scan time interval.", e);
        }
        return 5 * TimeUtil.MILLIS_PER_MINUTE; //return original default on error
    }






    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        if (this.interrogator != null) {
            return this.interrogator.getInterrogationKeys();
        }

        return Collections.emptySet();
    }

    @Override
    public InterrogateMap interrogate(ReferencedCoordinate coordinate,
            DataTime time, InterrogationKey<?>... keys) {
        if (keys == null || keys.length == 0 || this.interrogator == null) {
            return new InterrogateMap();
        }

        Set<InterrogationKey<?>> keySet = new HashSet<>(Arrays.asList(keys));

        RadarRecord record = radarRecords.get(time);
        if (record == null) {
            return new InterrogateMap();
        }

        ColorMapParameters params;
        if (hasCapability(ColorMapCapability.class)) {
            params = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
        } else {
            params = null;
        }

        try {
            return this.interrogator.sample(record, coordinate.asLatLon(),
                    params, keySet);
        } catch (TransformException | FactoryException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to convert coordinate to lat/lon.", e);
            return new InterrogateMap();
        }
    }

}
