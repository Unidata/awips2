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
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.SatelliteUnits;
import com.raytheon.uf.common.dataplugin.satellite.units.counts.DerivedWVPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.generic.GenericPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.PolarPrecipWaterPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.water.BlendedTPWPixel;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractPluginDataObjectResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.AbstractCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.ParamLevelMatchCriteria;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.style.level.Level;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.uf.viz.derivparam.library.DerivedParameterRequest;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.raytheon.viz.core.style.image.SamplePreferences;
import com.raytheon.viz.satellite.SatelliteConstants;
import com.raytheon.viz.satellite.tileset.SatTileSetRenderable;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Provides satellite raster rendering support
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Mar 1, 2007              chammack    Initial Creation.
 *  02/17/2009               njensen     Refactored to new rsc architecture.
 *  03/02/2009		2032	 jsanchez	 Added check for displayedDate if no data.
 *  03/25/2009      2086     jsanchez    Mapped correct converter to parameter type.
 *                                       Updated the call to ColormapParametersFactory.build
 *  03/30/2009      2169     jsanchez    Updated numLevels handling.
 * - AWIPS2 Baseline Repository --------
 *  07/17/2012      798      jkorman     Use decimationLevels from SatelliteRecord. Removed hard-coded
 *                                       data set names.
 *  06/20/2013      2122     mschenke    Modified to use SatTileSetRenderable
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class SatResource extends
        AbstractPluginDataObjectResource<SatResourceData, IMapDescriptor> {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatResource.class);

    public static String RAW_VALUE = "rawValue";

    private static class InterrogationResult {

        private final SatelliteRecord record;

        private final double value;

        public InterrogationResult(SatelliteRecord record, double value) {
            this.record = record;
            this.value = value;
        }

        public SatelliteRecord getRecord() {
            return record;
        }

        public double getValue() {
            return value;
        }

    }

    private class SatRenderable implements IRenderable {

        private Map<ISpatialObject, SatTileSetRenderable> tileMap = new HashMap<ISpatialObject, SatTileSetRenderable>();

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
                SatTileSetRenderable tileSet = tileMap.get(record
                        .getSpatialObject());
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
                    tileMap.put(record.getSpatialObject(), tileSet);
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

        public InterrogationResult interrogate(Coordinate latLon)
                throws VizException {
            InterrogationResult result = null;
            synchronized (tileMap) {
                for (SatTileSetRenderable renderable : tileMap.values()) {
                    double rValue = renderable.interrogate(latLon);
                    if (Double.isNaN(rValue) == false && rValue != fillValue) {
                        result = new InterrogationResult(
                                renderable.getSatelliteRecord(), rValue);
                    }
                }
            }
            return result;
        }
    }

    protected String legend;

    protected SamplePreferences sampleRange;

    protected double fillValue = 0;

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
        SatelliteRecord record = (SatelliteRecord) pdo;
        if (dataTimes.isEmpty()) {
            try {
                initializeFirstFrame(record);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        DataTime pdoTime = pdo.getDataTime();
        if (resourceData.getBinOffset() != null) {
            pdoTime = resourceData.getBinOffset().getNormalizedTime(pdoTime);
        }

        return pdoTime;
    }

    private void initializeFirstFrame(SatelliteRecord record)
            throws VizException {
        SingleLevel level = new SingleLevel(Level.LevelType.SURFACE);

        SatelliteUnits.register();
        Unit<?> unit = null;
        String physicalElement = null;
        DerivedParameterRequest request = (DerivedParameterRequest) record
                .getMessageData();
        if (request == null) {
            physicalElement = record.getPhysicalElement();
        } else {
            physicalElement = request.getParameterAbbreviation();
        }
        if (record.getUnits() != null && request == null) {
            try {
                unit = UnitFormat.getUCUMInstance().parseSingleUnit(
                        record.getUnits(), new ParsePosition(0));
            } catch (ParseException e) {
                throw new VizException("Unable parse units ", e);
            }
        } else if (request != null) {
            if (physicalElement.equals("satDivWVIR")) {
                unit = new DerivedWVPixel();
            } else {
                unit = new GenericPixel();
            }
        }

        String creatingEntity = null;
        if (physicalElement.equals(SatelliteConstants.PRECIP)) {
            creatingEntity = record.getCreatingEntity();
            if (creatingEntity.equals(SatelliteConstants.DMSP)
                    || creatingEntity.equals(SatelliteConstants.POES)) {
                unit = new PolarPrecipWaterPixel();
            } else if (creatingEntity.equals(SatelliteConstants.MISC)) {
                unit = new BlendedTPWPixel();
            }
        }

        ColorMapParameters colorMapParameters = null;

        IColorMap colorMap = null;
        String cmName = null;
        if (hasCapability(ColorMapCapability.class)) {
            colorMapParameters = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
            if (colorMapParameters != null) {
                colorMap = colorMapParameters.getColorMap();
                cmName = colorMapParameters.getColorMapName();
            }
        }

        // Grab the sampleRange from the preferences
        ParamLevelMatchCriteria match = new ParamLevelMatchCriteria();
        match.setParameterName(Arrays.asList(physicalElement));
        match.setLevels(Arrays.asList((Level) level));
        match.setCreatingEntityNames(Arrays.asList(creatingEntity));
        StyleRule sr = StyleManager.getInstance().getStyleRule(
                StyleManager.StyleType.IMAGERY, match);
        if (sr != null && sr.getPreferences() instanceof ImagePreferences) {
            sampleRange = ((ImagePreferences) sr.getPreferences())
                    .getSamplePrefs();
            String lg = ((ImagePreferences) sr.getPreferences()).getLegend();
            // test, so legend is not over written with empty string
            if (lg != null && !lg.trim().isEmpty()) {
                legend = lg;
            }
        }

        colorMapParameters = ColorMapParameterFactory.build((Object) null,
                physicalElement, unit, level, creatingEntity);
        // TODO: Figure out data/color map min/max values better
        if (unit == null) {
            colorMapParameters.setColorMapMin(0.0f);
            colorMapParameters.setColorMapMax(255.0f);
        }
        if (unit instanceof GenericPixel) {
            // Derived parameter data will be signed
            colorMapParameters.setDataMin(-128.0f);
            colorMapParameters.setDataMax(127.0f);
        } else if (unit instanceof BlendedTPWPixel) {
            colorMapParameters.setDataMin(0.0f);
            colorMapParameters.setDataMax(252.0f);

            colorMapParameters.setColorMapMin(0.0f);
            colorMapParameters.setColorMapMax(252.0f);
        } else {
            colorMapParameters.setDataMin(0.0f);
            colorMapParameters.setDataMax(255.0f);
        }

        if (colorMap == null) {
            if (cmName == null) {
                cmName = "Sat/VIS/ZA (Vis Default)";
            }
            colorMap = ColorMapLoader.loadColorMap(cmName);
        }

        if (colorMap != null) {
            colorMapParameters.setColorMap(colorMap);
        }

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
        if (renderable != null) {
            try {
                InterrogationResult result = renderable.interrogate(coord
                        .asLatLon());
                if (result != null) {
                    double dataValue = result.getValue();
                    UnitConverter dataToDisplay = getCapability(
                            ColorMapCapability.class).getColorMapParameters()
                            .getDataToDisplayConverter();
                    if (dataToDisplay != null) {
                        dataValue = dataToDisplay.convert(dataValue);
                    }
                    dataMap.put(RAW_VALUE, dataValue);
                    dataMap.put(ISpatialObject.class.toString(), result
                            .getRecord().getSpatialObject());
                }
            } catch (Exception e) {
                throw new VizException("Error interrogating raw data", e);
            }
        }
        return dataMap;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> dataMap = interrogate(coord);
        Double value = (Double) dataMap.get(RAW_VALUE);
        if (value == null) {
            return "NO DATA";
        }
        ColorMapParameters cmp = getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        // check if data mapping preferences exist
        DataMappingPreferences dataMapping = cmp.getDataMapping();
        if (dataMapping != null) {
            // convert to pixel value for checking labels
            double pixelValue = cmp.getDisplayToDataConverter().convert(
                    value.doubleValue());
            // if the pixel value matches the data mapping entry use that
            // label instead
            String label = dataMapping.getLabelValueForDataValue(pixelValue);
            if (label != null) {
                return label;
            }
        }

        Unit<?> unit = cmp.getDisplayUnit();
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
        if (value > f1 && value > f2) {
            return String.format(">%.1f%s", Math.max(f1, f2), unitString);
        }
        if (value < f1 && value < f2) {
            return String.format("<%.1f%s", Math.min(f1, f2), unitString);
        }
        return String.format("%.1f%s", value, unitString);
    }

    private String getLegend(PluginDataObject record) {
        String productName = null;
        DerivedParameterRequest request = (DerivedParameterRequest) record
                .getMessageData();
        if (request == null) {
            productName = ((SatelliteRecord) record).getPhysicalElement();
        } else {
            productName = request.getParameterAbbreviation();
        }
        return SatelliteConstants.getLegend(productName,
                ((SatelliteRecord) record).getCreatingEntity());
    }

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

}
