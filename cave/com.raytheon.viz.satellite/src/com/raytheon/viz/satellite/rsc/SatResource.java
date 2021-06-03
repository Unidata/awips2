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
package com.raytheon.viz.satellite.rsc;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.measure.Measure;
import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Temperature;
import javax.measure.unit.Unit;
import org.eclipse.swt.graphics.Rectangle;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;
import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.PersistedParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.image.DataScale;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.style.image.SamplePreferences;
import com.raytheon.uf.common.style.level.Level;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.util.VariableSubstitutor;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension.ImageProvider;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.interrogation.ClassInterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogatable;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogateMap;
import com.raytheon.uf.viz.core.rsc.interrogation.InterrogationKey;
import com.raytheon.uf.viz.core.rsc.interrogation.Interrogator;
import com.raytheon.uf.viz.core.rsc.interrogation.StringInterrogationKey;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.awipstools.IToolChangedListener;
import com.raytheon.viz.satellite.SatelliteConstants;
import com.raytheon.viz.satellite.inventory.DerivedSatelliteRecord;
import com.raytheon.viz.satellite.tileset.SatDataRetriever;
import com.raytheon.viz.satellite.tileset.SatRenderable;
import com.raytheon.viz.satellite.tileset.SatRenderable.InterrogationResult;
/**
 * Provides satellite raster rendering support
 * 
 * This resource implements the {@link Interrogatable} interface and provides
 * the keys for accessing the value at a point({@link Interrogator#VALUE}), the
 * full {@link SatelliteRecord}({@link #SATELLITE_RECORD_INTERROGATE_KEY}), a
 * {@link GeographicDataSource} for the currently displayed tileset/level(
 * {@link #DATA_SOURCE_INTERROGATE_KEY}), and a special key that returns the
 * same value but is only applicable to satellite data types(
 * {@link #SATELLITE_DATA_INTERROGATE_ID})
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 01, 2007           chammack  Initial Creation.
 * Feb 17, 2009  1960     njensen   Refactored to new rsc architecture.
 * Mar 02, 2009  2032     jsanchez  Added check for displayedDate if no data.
 * Mar 25, 2009  2086     jsanchez  Mapped correct converter to parameter type.
 *                                  Updated the call to
 *                                  ColormapParametersFactory.build
 * Mar 30, 2009  2169     jsanchez  Updated numLevels handling.
 * Jul 17, 2012  798      jkorman   Use decimationLevels from SatelliteRecord.
 *                                  Removed hard-coded data set names.
 * Jun 20, 2013  2122     mschenke  Modified to use SatTileSetRenderable
 * Jul 31, 2013  2190     mschenke  Switched to use Measure objects for
 *                                  interrogation
 * Nov 20, 2013  2492     bsteffen  Always get min/max values from style rules.
 * Apr 09, 2014  2947     bsteffen  Improve flexibility of sat derived
 *                                  parameters, implement ImageProvider
 * May 06, 2014           njensen   Improve error message
 * Jun 12, 2014  3238     bsteffen  Implement Interrogatable
 * Aug 21, 2014  17313    jgerth    Set no data value if no data mapping
 * Oct 15, 2014  3681     bsteffen  create renderable in interrogate if
 *                                  necessary.
 * Feb 17, 2015  4135     bsteffen  Set no data value for derived products.
 * Mar 03, 2015  14960    jgerth    Retrieve legend from style rules if
 *                                  available
 * Apr 15, 2014  4388     bsteffen  Use fill value from record.
 * Jul 28, 2015  4633     bsteffen  Create tileset in resource so it can be
 *                                  overridden for daylight transition.
 * Oct 08, 2015  4937     bsteffen  Move SatRenderable to new class.
 * Jul 13, 2016  18781    jburks    Added ability to mask incomplete frames.
 * Aug 03, 2016  5786     bsteffen  Schedule the loading of all frames on
 *                                  initialization
 * Mar 22, 2018  6846     bsteffen  Perform substitution of style units in
 *                                  legend strings.
 * Apr 04, 2018  6889     njensen   Use brightness from ImagePreferences if
 *                                  present but missing in ImagingCapability
 * 
 * </pre>
 * 
 * @author chammack
 */
public class SatResource extends
        AbstractPluginDataObjectResource<SatResourceData, IMapDescriptor>
        implements ImageProvider, Interrogatable, IToolChangedListener {
    /**
     * String id to look for satellite-provided data values
     * 
     * @deprecated use #SATELLITE_DATA_INTERROGATE_KEY and
     *             {@link #interrogate(ReferencedCoordinate, DataTime, InterrogationKey...)}
     */
    @Deprecated
    public static final String SATELLITE_DATA_INTERROGATE_ID = "satelliteDataValue";
    public static final InterrogationKey<Measure<? extends Number, ?>> SATELLITE_DATA_INTERROGATE_KEY = new StringInterrogationKey<>(
            SATELLITE_DATA_INTERROGATE_ID, Interrogator.getTypedMeasureClass());
    private static final InterrogationKey<SatelliteRecord> SATELLITE_RECORD_INTERROGATE_KEY = new ClassInterrogationKey<>(
            SatelliteRecord.class);
    private static final InterrogationKey<GeographicDataSource> DATA_SOURCE_INTERROGATE_KEY = new ClassInterrogationKey<>(
            GeographicDataSource.class);
    /**
     * Property that controls whether all frames of satellite data will be
     * loaded in the background when the resource is initialized. When the data
     * is loaded in the background it causes the first loop through the data to
     * be much smoother. The background loading increases memory and bandwidth
     * usage so it needs to be disabled when these resources are limited.
     * 
     * {@link Boolean#getBoolean(String)} is not used because it defaults to
     * false and the default value for this property is true.
     */
    private static final boolean BACKGROUND_LOAD = !"false".equalsIgnoreCase(
            System.getProperty("d2d.sat.background.load", "true"));
    protected String legend;
    protected SamplePreferences sampleRange;
    /** Flag to avoid reinitializing ColorMapParameters from style rules */
    private boolean initialized = false;
    /**
     * Constructor
     */
    public SatResource(SatResourceData data, LoadProperties props) {
        super(data, props);
        addDataObject(data.getRecords());
    }
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        if (!BACKGROUND_LOAD) {
            return;
        }
        IRenderableDisplay display = getDescriptor().getRenderableDisplay();
        if (display == null) {
            return;
        }
        IExtent extent = new PixelExtent(
                descriptor.getGridGeometry().getGridRange());
        Rectangle canvasBounds = display.getBounds();
        List<DataTime> times = new ArrayList<>(Arrays.asList(getDataTimes()));
        /* Make current time first */
        DataTime currentTime = getTimeForResource();
        times.add(0, currentTime);
        for (DataTime time : times) {
            IRenderable renderable = getOrCreateRenderable(time);
            if (renderable instanceof SatRenderable) {
                ((SatRenderable) renderable).scheduleImagesWithinExtent(target,
                        extent, canvasBounds);
            }
        }
    }
    @Override
    protected DataTime getDataObjectTime(PluginDataObject pdo) {
        if (!initialized) {
            try {
                initializeFirstFrame((SatelliteRecord) pdo);
            } catch (VizException e) {
                throw new IllegalStateException(
                        "Unable to initialize the satellite resource", e);
            }
            initialized = true;
        }
        DataTime pdoTime = pdo.getDataTime();
        if (resourceData.getBinOffset() != null) {
            pdoTime = resourceData.getBinOffset().getNormalizedTime(pdoTime);
        }
        return pdoTime;
    }
    private void initializeFirstFrame(SatelliteRecord record)
            throws VizException {
        getCapability(ImagingCapability.class).setProvider(this);
        ColorMapParameters colorMapParameters = null;
        IColorMap colorMap = null;
        String cmName = null;
        PersistedParameters persisted = null;
        if (hasCapability(ColorMapCapability.class)) {
            colorMapParameters = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            if (colorMapParameters != null) {
                persisted = colorMapParameters.getPersisted();
                colorMap = colorMapParameters.getColorMap();
                cmName = colorMapParameters.getColorMapName();
            }
        }
        Unit<?> unit = SatDataRetriever.getRecordUnit(record);
        /*
         * TODO default to NaN instead of 0. 0 is the default to support older
         * data(before the gini decoder set a fill value), when all decoders and
         * all decoded data has a proper fill value this should be changed to
         * Double.NaN.
         */
        double fillValue = 0;
        double defaultMin = 0;
        double defaultMax = 0xff;
        try {
            String dataSetName = DataStoreFactory.createDataSetName(null,
                    SatelliteRecord.SAT_DATASET_NAME, 0);
            IDataRecord dataRecord = DataCubeContainer.getDataRecord(record,
                    Request.buildPointRequest(new java.awt.Point(0, 0)),
                    dataSetName)[0];
            Number fillObj = dataRecord.getFillValue();
            if (fillObj != null) {
                fillValue = fillObj.doubleValue();
            }
            if (dataRecord instanceof ShortDataRecord) {
                if (SatDataRetriever.isSigned(dataRecord)) {
                    defaultMin = Short.MIN_VALUE;
                    defaultMax = Short.MAX_VALUE;
                } else {
                    defaultMin = 0;
                    defaultMax = 0xFFFF;
                    fillValue = ((int) fillValue) & 0xFFFF;
                }
            } else if (dataRecord instanceof ByteDataRecord) {
                if (SatDataRetriever.isSigned(dataRecord)) {
                    defaultMin = Byte.MIN_VALUE;
                    defaultMax = Byte.MAX_VALUE;
                } else {
                    defaultMin = 0;
                    defaultMax = 0xFF;
                    fillValue = ((int) fillValue) & 0xFF;
                }
            }
            Unit<?> dataUnit = SatDataRetriever.getDataUnit(unit, dataRecord);
            if (dataUnit != null && unit != null
                    && dataUnit.isCompatible(unit)) {
                UnitConverter converter = dataUnit.getConverterTo(unit);
                defaultMin = converter.convert(defaultMin);
                defaultMax = converter.convert(defaultMax);
            }
        } catch (DataCubeException e) {
            statusHandler.handle(Priority.WARN,
                    "Unable to request sample record", e);
        }
        SingleLevel level = new SingleLevel(Level.LevelType.SURFACE);
        String physicalElement = record.getPhysicalElement();
        // Grab the sampleRange from the preferences
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setParameterName(Arrays.asList(physicalElement));
        match.setLevels(Arrays.asList((Level) level));
        match.setCreatingEntityNames(Arrays.asList(record.getCreatingEntity()));
        String lg = null;
        StyleRule sr = null;
        try {
            sr = StyleManager.getInstance()
                    .getStyleRule(StyleManager.StyleType.IMAGERY, match);
            ImagePreferences preferences = null;
            if (sr == null
                    || !(sr.getPreferences() instanceof ImagePreferences)) {
                // No style rule, this is a best guess at what might look good.
                preferences = new ImagePreferences();
                if (unit != null && unit.isCompatible(Temperature.UNIT)) {
                    preferences.setDefaultColormap("Sat/IR/CIRA (IR Default)");
                } else {
                    preferences.setDefaultColormap("Sat/VIS/ZA (Vis Default)");
                }
                DataScale range = new DataScale();
                range.setScaleType(DataScale.Type.LINEAR);
                range.setMinValue(defaultMin);
                range.setMaxValue(defaultMax);
                preferences.setDataScale(range);
            } else {
                preferences = (ImagePreferences) sr.getPreferences();
            }
            colorMapParameters = ColorMapParameterFactory.build(preferences,
                    unit);
            sampleRange = preferences.getSamplePrefs();
            lg = preferences.getLegend();
            // test, so legend is not over written with empty string
            if (lg != null && lg.trim().isEmpty()) {
                lg = null;
            }
           
        } catch (StyleException e) {
            throw new VizException(e.getLocalizedMessage(), e);
        }
        // If null, set from style rules
        if (cmName == null) {
            cmName = colorMapParameters.getColorMapName();
        }
        if (colorMap == null) {
            colorMap = colorMapParameters.getColorMap();
        }
        // Load colormap into parameters
        if (colorMap == null) {
            if (cmName == null) {
                cmName = "Sat/VIS/ZA (Vis Default)";
            }
            try {
                colorMap = ColorMapLoader.loadColorMap(cmName);
            } catch (ColorMapException e) {
                throw new VizException("Unable to load clormap: " + cmName, e);
            }
        }
        if (colorMap != null) {
            colorMapParameters.setColorMap(colorMap);
        }
        if (persisted != null) {
            colorMapParameters.applyPersistedParameters(persisted);
        }
        if (colorMapParameters.getDataMapping() == null) {
            colorMapParameters.setNoDataValue(fillValue);
        }
        getCapability(ColorMapCapability.class)
                .setColorMapParameters(colorMapParameters);
        if (lg == null) {
            lg = getLegend(record);
        }
        this.legend = performLegendSubstitution(record, sr, lg);
    }
    @Override
    public String getName() {
        if (this.legend != null) {
            return this.legend + " ";
        }
        return "";
    }
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        DataTime displayedDate = descriptor.getFramesInfo()
                .getTimeForResource(this);
        Map<String, Object> dataMap = new HashMap<>();
        SatRenderable renderable = (SatRenderable) getRenderable(displayedDate);
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        double dataValue = Double.NaN;
        Unit<?> dataUnit = parameters.getColorMapUnit();
        if (renderable != null) {
            try {
                InterrogationResult result = renderable
                        .interrogate(coord.asLatLon(), dataUnit);
                if (result != null) {
                    dataValue = result.getValue();
                    dataMap.put(IGridGeometryProvider.class.toString(),
                            result.getRecord().getCoverage());
                }
            } catch (Exception e) {
                throw new VizException("Error interrogating raw data", e);
            }
        }
        dataMap.put(SATELLITE_DATA_INTERROGATE_ID,
                Measure.valueOf(dataValue, dataUnit));
        return dataMap;
    }
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> dataMap = interrogate(coord);
        Measure<?, ?> value = (Measure<?, ?>) dataMap
                .get(SATELLITE_DATA_INTERROGATE_ID);
        double measuredValue = Double.NaN;
        if (value != null && value.getValue() instanceof Double) {
            measuredValue = (Double) value.getValue();
        }
        if (Double.isNaN(measuredValue)) {
            return "NO DATA";
        }
        ColorMapParameters cmp = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        // check if data mapping preferences exist
        DataMappingPreferences dataMapping = cmp.getDataMapping();
        if (dataMapping != null) {
            // if the pixel value matches the data mapping entry use that
            // label instead
            String label = dataMapping
                    .getSampleOrLabelValueForDataValue(measuredValue);
            if (label != null) {
                return label;
            }
        }
        Unit<?> unit = cmp.getDisplayUnit();
        Unit<?> measuredUnit = value.getUnit();
        if (unit != null && measuredUnit != null) {
            UnitConverter dataToDisplay = measuredUnit.getConverterTo(unit);
            if (dataToDisplay != null) {
                measuredValue = dataToDisplay.convert(measuredValue);
            }
        }
        // Had to use 'bit' as the display unit because
        // counts was not an acceptable unit.
        String unitString = unit == null ? ""
                : "bit".equals(unit.toString()) ? "counts" : unit.toString();
        double f1 = Double.NEGATIVE_INFINITY;
        double f2 = Double.POSITIVE_INFINITY;
        if (sampleRange != null) {
            f1 = sampleRange.getMaxValue();
            f2 = sampleRange.getMinValue();
        }
        if (measuredValue > f1 && measuredValue > f2) {
            return String.format(">%.1f%s", Math.max(f1, f2), unitString);
        }
        if (measuredValue < f1 && measuredValue < f2) {
            return String.format("<%.1f%s", Math.min(f1, f2), unitString);
        }
        return String.format("%.1f%s", measuredValue, unitString);
    }
    private String getLegend(SatelliteRecord record) {
        String productName = record.getPhysicalElement();
        if (record instanceof DerivedSatelliteRecord) {
            productName = ((DerivedSatelliteRecord) record).getName();
        }
        return SatelliteConstants.getLegend(productName,
                record.getCreatingEntity());
    }
    /**
     * Use the {@link VariableSubstitutor} to replace any variables in the
     * legend with real values from the record or style rule
     * 
     * @param record
     *            an example satellite record whose fields are used to populate
     *            the substitution
     * @param styleRule
     *            an optional style rule for units information, this may be
     *            null.
     * @param legend
     *            the legend string retrieved form style rules, constants, or
     *            elsewhere.
     * @return A new legend with any variables replaced.
     * @throws VizException
     *             if an error occurs parsing variables.
     */
    private String performLegendSubstitution(SatelliteRecord record,
            StyleRule styleRule, String legend) throws VizException {
        if (legend.indexOf('$') >= 0) {
            Map<String, String> subs = new HashMap<>();
            subs.put("creatingEntity", record.getCreatingEntity());
            subs.put("physicalElement", record.getPhysicalElement());
            subs.put("sectorID", record.getSectorID());
            subs.put("source", record.getSource());
            subs.put("units", record.getUnits());
            if (styleRule != null) {
                String unitLabel = styleRule.getPreferences()
                        .getDisplayUnitLabel();
                if (unitLabel != null) {
                    subs.put("units", unitLabel);
                }
            }
            try {
                legend = VariableSubstitutor.processVariables(legend, subs);
            } catch (ParseException e) {
                throw new VizException("Error building product legend", e);
            }
        }
        return legend;
    }
    @Override
    public List<DrawableImage> getImages(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        SatRenderable renderable = (SatRenderable) getOrCreateRenderable(
                paintProps.getDataTime());
        if (renderable != null) {
            return new ArrayList<>(
                    renderable.getImagesToRender(target, paintProps));
        }
        return Collections.emptyList();
    }
    @Override
    protected void capabilityChanged(IRenderable renderable,
            AbstractCapability capability) {
        issueRefresh();
    }
    @Override
    protected void disposeRenderable(IRenderable renderable) {
        ((SatRenderable) renderable).dispose();
    }
    @Override
    protected boolean projectRenderable(IRenderable renderable,
            CoordinateReferenceSystem crs) throws VizException {
        ((SatRenderable) renderable).project();
        return true;
    }
    @Override
    protected IRenderable constructRenderable(DataTime time,
            List<PluginDataObject> records) throws VizException {
        SatRenderable renderable = new SatRenderable(this, time);
        updateRenderable(renderable, records.toArray(new PluginDataObject[0]));
        renderable.project();
        return renderable;
    }
    @Override
    protected boolean updateRenderable(IRenderable renderable,
            PluginDataObject... pdos) {
        SatRenderable sr = (SatRenderable) renderable;
        for (PluginDataObject object : pdos) {
            if (object instanceof SatelliteRecord) {
                sr.addRecord((SatelliteRecord) object);
            }
        }
        return true;
    }
    @Override
    public Set<InterrogationKey<?>> getInterrogationKeys() {
        Set<InterrogationKey<?>> result = new HashSet<>();
        result.add(Interrogator.VALUE);
        result.add(SATELLITE_DATA_INTERROGATE_KEY);
        result.add(SATELLITE_RECORD_INTERROGATE_KEY);
        result.add(DATA_SOURCE_INTERROGATE_KEY);
        return result;
    }
    @Override
    public InterrogateMap interrogate(ReferencedCoordinate coordinate,
            DataTime time, InterrogationKey<?>... keys) {
        InterrogateMap result = new InterrogateMap();
        SatRenderable renderable = null;
        try {
            renderable = (SatRenderable) getOrCreateRenderable(time);
        } catch (VizException e) {
            statusHandler.error("Unable to interrogate " + getSafeName(), e);
        }
        if (renderable == null) {
            return result;
        }
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        Unit<?> dataUnit = parameters.getColorMapUnit();
        InterrogationResult renderableResult = null;
        try {
            renderableResult = renderable.interrogate(coordinate.asLatLon(),
                    dataUnit);
        } catch (VizException | TransformException | FactoryException e) {
            statusHandler.error("Cannot interrogate satellite data", e);
        }
        if (renderableResult == null) {
            return result;
        }
        double dataValue = renderableResult.getValue();
        Measure<Double, ?> value = Measure.valueOf(dataValue, dataUnit);
        for (InterrogationKey<?> key : keys) {
            if (Interrogator.VALUE.equals(key)) {
                result.put(Interrogator.VALUE, value);
            } else if (SATELLITE_DATA_INTERROGATE_KEY.equals(key)) {
                result.put(SATELLITE_DATA_INTERROGATE_KEY, value);
            } else if (SATELLITE_RECORD_INTERROGATE_KEY.equals(key)) {
                result.put(SATELLITE_RECORD_INTERROGATE_KEY,
                        renderableResult.getRecord());
            } else if (DATA_SOURCE_INTERROGATE_KEY.equals(key)) {
                result.put(DATA_SOURCE_INTERROGATE_KEY,
                        renderableResult.getDataSource());
            }
        }
        return result;
    }
    @Override
    public void toolChanged() {
        issueRefresh();
    }
    public void redoMainTimeMatch() {
        try {
            descriptor.getTimeMatcher().redoTimeMatching(this);
            descriptor.getTimeMatcher().redoTimeMatching(descriptor);
        } catch (VizException e) {
            statusHandler.error(
                    "Unable to redo the time matching " + getSafeName(), e);
        }
    }
}