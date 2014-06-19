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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.PersistedParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatMapCoverage;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
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
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.IRenderable;
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
import com.raytheon.viz.satellite.SatelliteConstants;
import com.raytheon.viz.satellite.inventory.DerivedSatelliteRecord;
import com.raytheon.viz.satellite.tileset.SatDataRetriever;
import com.raytheon.viz.satellite.tileset.SatTileSetRenderable;
import com.vividsolutions.jts.geom.Coordinate;

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
 *  Date          Ticket#   Engineer    Description
 *  ------------- --------  ----------- --------------------------
 *  Mar 01, 2007            chammack    Initial Creation.
 *  Feb 17, 2009            njensen     Refactored to new rsc architecture.
 *  Mar 02, 2009  2032	    jsanchez    Added check for displayedDate if no
 *                                      data.
 *  Mar 25, 2009  2086      jsanchez    Mapped correct converter to parameter
 *                                      type. Updated the call to
 *                                      ColormapParametersFactory.build
 *  Mar 30, 2009  2169      jsanchez    Updated numLevels handling.
 *  Jul 17, 2012  798       jkorman     Use decimationLevels from
 *                                      SatelliteRecord. Removed hard-coded
 *                                      data set names.
 *  Jun 20, 2013  2122      mschenke    Modified to use SatTileSetRenderable
 *  Jul 31, 2013  2190      mschenke    Switched to use Measure objects for
 *                                      interrogation
 *  Nov 20, 2013  2492      bsteffen    Always get min/max values from style
 *                                      rules.
 *  Apr 09, 2014  2947      bsteffen    Improve flexibility of sat derived
 *                                      parameters, implement ImageProvider
 *  May 06, 2014            njensen     Improve error message
 *  Jun 12, 2014  3238      bsteffen    Implement Interrogatable
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SatResource extends
        AbstractPluginDataObjectResource<SatResourceData, IMapDescriptor>
        implements ImageProvider, Interrogatable {

    /**
     * String id to look for satellite-provided data values
     * 
     * @deprecated use #SATELLITE_DATA_INTERROGATE_KEY and
     *             {@link #interrogate(ReferencedCoordinate, DataTime, InterrogationKey...)}
     */
    @Deprecated
    public static final String SATELLITE_DATA_INTERROGATE_ID = "satelliteDataValue";

    @SuppressWarnings("unchecked")
    public static final InterrogationKey<Measure<? extends Number, ?>> SATELLITE_DATA_INTERROGATE_KEY = new StringInterrogationKey<>(
            SATELLITE_DATA_INTERROGATE_ID,
            (Class<Measure<? extends Number, ?>>) ((Class<?>) Measure.class));

    private static final InterrogationKey<SatelliteRecord> SATELLITE_RECORD_INTERROGATE_KEY = new ClassInterrogationKey<>(
            SatelliteRecord.class);

    private static final InterrogationKey<GeographicDataSource> DATA_SOURCE_INTERROGATE_KEY = new ClassInterrogationKey<>(
            GeographicDataSource.class);

    private static class InterrogationResult {

        private final SatTileSetRenderable renderable;

        private final double value;

        public InterrogationResult(SatTileSetRenderable renderable, double value) {
            this.renderable = renderable;
            this.value = value;
        }

        public SatelliteRecord getRecord() {
            return renderable.getSatelliteRecord();
        }

        public double getValue() {
            return value;
        }

        public GeographicDataSource getDataSource() {
            return renderable.getCurrentLevelDataSource();
        }

    }

    private class SatRenderable implements IRenderable {

        private Map<SatMapCoverage, SatTileSetRenderable> tileMap = new HashMap<SatMapCoverage, SatTileSetRenderable>();

        private DataTime renderableTime;

        public SatRenderable(DataTime renderableTime) {
            this.renderableTime = renderableTime;
        }

        @Override
        public void paint(IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            Collection<DrawableImage> images = getImagesToRender(target,
                    paintProps);
            if (images.isEmpty() == false) {
                target.drawRasters(paintProps,
                        images.toArray(new DrawableImage[0]));
            }
        }

        public Collection<DrawableImage> getImagesToRender(
                IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            List<DrawableImage> images = new ArrayList<DrawableImage>();
            synchronized (tileMap) {
                for (SatTileSetRenderable renderable : tileMap.values()) {
                    images.addAll(renderable.getImagesToRender(target,
                            paintProps));
                }
            }
            return images;
        }

        public void addRecord(SatelliteRecord record) {
            synchronized (tileMap) {
                SatTileSetRenderable tileSet = tileMap
                        .get(record.getCoverage());
                if (tileSet != null) {
                    SatelliteRecord existingRecord = tileSet
                            .getSatelliteRecord();
                    if (existingRecord.equals(record) == false) {
                        // Different record, same spatial area for same frame
                        // Determine if new one is better than existing
                        long existingTimeMillis = existingRecord.getDataTime()
                                .getMatchRef();
                        long newRecordTimeMillis = record.getDataTime()
                                .getMatchRef();
                        long normalTimeMillis = renderableTime.getMatchRef();
                        if (Math.abs(normalTimeMillis - newRecordTimeMillis) < Math
                                .abs(normalTimeMillis - existingTimeMillis)) {
                            // New is better since it's data time is closer to
                            // the normal time than the existing record's time!
                            tileSet.dispose();
                            tileSet = null;
                        }
                    }
                }
                if (tileSet == null) {
                    tileSet = new SatTileSetRenderable(SatResource.this, record);
                    tileSet.project(descriptor.getGridGeometry());
                    tileMap.put(record.getCoverage(), tileSet);
                }
            }
        }

        public void project() {
            synchronized (tileMap) {
                for (SatTileSetRenderable renderable : tileMap.values()) {
                    renderable.project(descriptor.getGridGeometry());
                }
            }
        }

        public void dispose() {
            synchronized (tileMap) {
                for (SatTileSetRenderable renderable : tileMap.values()) {
                    renderable.dispose();
                }
                tileMap.clear();
            }
        }

        public InterrogationResult interrogate(Coordinate latLon,
                Unit<?> requestUnit) throws VizException {
            InterrogationResult result = null;
            synchronized (tileMap) {
                for (SatTileSetRenderable renderable : tileMap.values()) {
                    double rValue = renderable.interrogate(latLon, requestUnit);
                    if (Double.isNaN(rValue) == false) {
                        result = new InterrogationResult(renderable, rValue);
                    }
                }
            }
            return result;
        }
    }

    protected String legend;

    protected SamplePreferences sampleRange;

    /** Flag to avoid reinitializing ColorMapParameters from style rules */
    private boolean initialized = false;

    /**
     * Constructor
     * 
     * @throws VizException
     */
    public SatResource(SatResourceData data, LoadProperties props) {
        super(data, props);
        addDataObject(data.getRecords());
    }

    @Override
    protected DataTime getDataObjectTime(PluginDataObject pdo) {
        if (initialized == false) {
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

        SingleLevel level = new SingleLevel(Level.LevelType.SURFACE);
        String physicalElement = record.getPhysicalElement();

        // Grab the sampleRange from the preferences
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setParameterName(Arrays.asList(physicalElement));
        match.setLevels(Arrays.asList((Level) level));
        match.setCreatingEntityNames(Arrays.asList(record.getCreatingEntity()));
        Unit<?> unit = SatDataRetriever.getRecordUnit(record);
        try {
            StyleRule sr = StyleManager.getInstance().getStyleRule(
                    StyleManager.StyleType.IMAGERY, match);

            ImagePreferences preferences = null;
            if (sr == null
                    || sr.getPreferences() instanceof ImagePreferences == false) {
                // No style rule, this is a best guess at what might look good.
                preferences = new ImagePreferences();
                if (unit != null && unit.isCompatible(Temperature.UNIT)) {
                    preferences.setDefaultColormap("Sat/IR/CIRA (IR Default)");
                } else {
                    preferences.setDefaultColormap("Sat/VIS/ZA (Vis Default)");
                }
                DataScale range = new DataScale();
                range.setScaleType(DataScale.Type.LINEAR);
                range.setMinValue(0.0);
                range.setMaxValue(255.0);
                preferences.setDataScale(range);
            } else {
                preferences = (ImagePreferences) sr.getPreferences();
            }
            colorMapParameters = ColorMapParameterFactory.build(preferences,
                    unit);

            sampleRange = preferences.getSamplePrefs();
            String lg = preferences.getLegend();
            // test, so legend is not over written with empty string
            if (lg != null && !lg.trim().isEmpty()) {
                legend = lg;
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
            colorMap = ColorMapLoader.loadColorMap(cmName);
        }

        if (colorMap != null) {
            colorMapParameters.setColorMap(colorMap);
        }

        if (persisted != null) {
            colorMapParameters.applyPersistedParameters(persisted);
        }
        colorMapParameters.setNoDataValue(0);

        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);

        this.legend = getLegend(record);
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
        DataTime displayedDate = descriptor.getFramesInfo().getTimeForResource(
                this);
        Map<String, Object> dataMap = new HashMap<String, Object>();

        SatRenderable renderable = (SatRenderable) getRenderable(displayedDate);
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        double dataValue = Double.NaN;
        Unit<?> dataUnit = parameters.getColorMapUnit();
        if (renderable != null) {
            try {
                InterrogationResult result = renderable.interrogate(
                        coord.asLatLon(), dataUnit);
                if (result != null) {
                    dataValue = result.getValue();
                    dataMap.put(IGridGeometryProvider.class.toString(), result
                            .getRecord().getCoverage());
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
            String label = dataMapping.getLabelValueForDataValue(measuredValue);
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
                : unit.toString().equals("bit") ? "counts" : unit.toString();
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

    @Override
    public List<DrawableImage> getImages(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        SatRenderable renderable = (SatRenderable) getOrCreateRenderable(paintProps
                .getDataTime());
        if (renderable != null) {
            return new ArrayList<DrawableImage>(renderable.getImagesToRender(
                    target, paintProps));
        }
        return Collections.emptyList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * capabilityChanged(com.raytheon.uf.viz.core.drawables.IRenderable,
     * com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability)
     */
    @Override
    protected void capabilityChanged(IRenderable renderable,
            AbstractCapability capability) {
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * disposeRenderable(com.raytheon.uf.viz.core.drawables.IRenderable)
     */
    @Override
    protected void disposeRenderable(IRenderable renderable) {
        ((SatRenderable) renderable).dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * projectRenderable(com.raytheon.uf.viz.core.drawables.IRenderable,
     * org.opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    protected boolean projectRenderable(IRenderable renderable,
            CoordinateReferenceSystem crs) throws VizException {
        ((SatRenderable) renderable).project();
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * constructRenderable(com.raytheon.uf.common.time.DataTime, java.util.List)
     */
    @Override
    protected IRenderable constructRenderable(DataTime time,
            List<PluginDataObject> records) throws VizException {
        SatRenderable renderable = new SatRenderable(time);
        updateRenderable(renderable, records.toArray(new PluginDataObject[0]));
        renderable.project();
        return renderable;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource#
     * updateRenderable(com.raytheon.uf.viz.core.drawables.IRenderable,
     * com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
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
        SatRenderable renderable = (SatRenderable) getRenderable(time);
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

}
