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

import java.awt.Rectangle;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.measure.IncommensurableException;
import javax.measure.UnconvertibleException;
import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.geotools.referencing.CRS;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters.PersistedParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarUtil;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ParamLevelMatchCriteria;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleManager.StyleType;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.units.UnitConv;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.util.DataUtilities;

import tec.uom.se.AbstractConverter;
import tec.uom.se.function.MultiplyConverter;

/**
 * Base resource for any radar record that can be displayed on a map as a
 * colormapped image.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 04, 2010           mnash     Initial creation
 * Jun 11, 2014  2061     bsteffen  Move rangeable capability to radial resource
 * Nov 08, 2016  5976     bsteffen  Remove IMeshCallback interface
 * Mar 21, 2018  7164     njensen   Use persisted colormap min and max when present
 * Apr 04, 2018  6889     njensen   Use brightness from ImagePreferences if
 *                                  present but missing in ImagingCapability
 * Apr 15, 2019  7596     lsingh     Updated units framework to JSR-363.
 *                                   Handled unit conversion
 * 
 * </pre>
 * 
 * @author mnash
 */
public class RadarImageResource<D extends IDescriptor>
        extends AbstractRadarResource<D> {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarImageResource.class);

    private static final int RANGE_CIRCLE_PTS = 360;

    protected Map<Float, IWireframeShape> rangeCircle;

    protected Map<DataTime, DrawableImage> images = new ConcurrentHashMap<>();

    protected RadarImageResource(RadarResourceData resourceData,
            LoadProperties loadProperties, IRadarInterrogator interrogator) {
        super(resourceData, loadProperties, interrogator);
        rangeCircle = new HashMap<>();
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();

        for (IWireframeShape shape : rangeCircle.values()) {
            if (shape != null) {
                shape.dispose();
            }
        }
        rangeCircle.clear();
        for (DrawableImage image : images.values()) {
            disposeImage(image);
        }
        images.clear();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        // If there is no color map yet, attempt to create one
        // This is mostly done so it can be passed up to a BestRes resource so
        // it can unify the color maps of all resources
        VizRadarRecord mostSevere = null;
        for (VizRadarRecord rec : radarRecords.values()) {
            if (mostSevere == null) {
                mostSevere = rec;
                continue;
            }
            // If you have records with more levels prefer that color map
            if (mostSevere.getNumLevels() < rec.getNumLevels()) {
                mostSevere = rec;
            }
            // Pick the one that is in storm mode if there is one.
            if (mostSevere.getOperationalMode() < rec.getOperationalMode()) {
                mostSevere = rec;
            }
        }
        if (mostSevere != null) {
            // Get the object sync as we are in init and getColorMapParameters
            // expects a populated record
            getColorMapParameters(target, mostSevere);
        }

        ImagingCapability imgCap = getCapability(ImagingCapability.class);
        if (mostSevere != null && !imgCap.isBrightnessSet()) {
            // if there's no brightness value, look for one in the style rules
            try {
                ParamLevelMatchCriteria matchCriteria = new ParamLevelMatchCriteria();
                List<String> params = new ArrayList<>();
                params.add(Integer.toString(mostSevere.getProductCode()));
                matchCriteria.setParameterName(params);
                if (resourceData.mode != null && !resourceData.mode.isEmpty()) {
                    List<String> creatingEntities = new ArrayList<>();
                    creatingEntities.add(resourceData.mode);
                    matchCriteria.setCreatingEntityNames(creatingEntities);
                }
                StyleRule sr = StyleManager.getInstance()
                        .getStyleRule(StyleType.IMAGERY, matchCriteria);
                if (sr.getPreferences() instanceof ImagePreferences) {
                    ImagePreferences imgPrefs = (ImagePreferences) sr
                            .getPreferences();
                    if (imgPrefs.getBrightness() != null) {
                        imgCap.setBrightness(imgPrefs.getBrightness());
                    }
                }
            } catch (StyleException e) {
                statusHandler.error("Error getting style rule for product code "
                        + mostSevere.getProductCode(), e);
            }
        }
    }

    /**
     * Create the radar tile given the tileRecord to populate and a RadarRecord
     * with all data populated
     * 
     * @param target
     * @param tiltRecord
     * @param populatedRecord
     * @throws StorageException
     * @throws IOException
     * @throws ClassNotFoundException
     * @throws VizException
     */
    protected void createTile(IGraphicsTarget target,
            VizRadarRecord populatedRecord) throws StorageException,
            IOException, ClassNotFoundException, VizException {
        ColorMapParameters params = getColorMapParameters(target,
                populatedRecord);

        PixelCoverage coverage = buildCoverage(target, populatedRecord);
        if (coverage.getMesh() == null) {
            coverage.setMesh(buildMesh(target, populatedRecord));
        }

        IImage image = createImage(target, params, populatedRecord,
                new Rectangle(0, 0, populatedRecord.getNumBins(),
                        populatedRecord.getNumRadials()));
        DrawableImage dImage = images.put(populatedRecord.getDataTime(),
                new DrawableImage(image, coverage));
        if (dImage != null) {
            disposeImage(dImage);
        }
    }

    /**
     * Get the colormap parameters, expects a radar record populated with data
     * 
     * @param target
     * @param record
     * @return
     * @throws VizException
     */
    protected ColorMapParameters getColorMapParameters(IGraphicsTarget target,
            RadarRecord record) throws VizException {
        ColorMapParameters paramsCapability = getCapability(
                ColorMapCapability.class).getColorMapParameters();
        String colorMapName = "";
        IColorMap colorMap = null;
        if (paramsCapability != null
                && paramsCapability.getDataUnit() != null) {
            return paramsCapability;
        } else if (paramsCapability != null) {
            colorMapName = paramsCapability.getColorMapName();
            colorMap = paramsCapability.getColorMap();
        }

        // Setup the ColorMap settings
        int prodCode = record.getProductCode();
        Unit<?> dataUnit = record.getDataUnit();

        ColorMapParameters params;
        try {
            params = ColorMapParameterFactory.build((Object) null,
                    Integer.toString(prodCode), dataUnit, null,
                    resourceData.mode);
        } catch (StyleException e) {
            throw new VizException(e.getLocalizedMessage(), e);
        }

        if (params.getDisplayUnit() == null) {
            params.setDisplayUnit(record.getUnitObject());
        }
        if (params.getImageUnit() == dataUnit && record.getNumLevels() <= 16) {
            DataMappingPreferences dataMapping = new DataMappingPreferences();
            Object[] thresholds = record.getDecodedThresholds();
            for (int i = 1; i < record.getNumLevels(); i++) {
                DataMappingEntry entry = new DataMappingEntry();

                // Sets the position left or right, should be normalized to
                // the numLevels
                entry.setPixelValue((double) (i * 16));

                // Set the data value
                if (thresholds[i] instanceof Float) {
                    entry.setDisplayValue(
                            params.getDataToDisplayConverter().convert(i));
                } else if (thresholds[i] instanceof String) {
                    entry.setLabel((String) thresholds[i]);
                } else if (thresholds[i] == null) {
                    entry.setLabel("");
                } else {
                    entry.setLabel(thresholds[i].toString());
                }
                dataMapping.addEntry(entry);
            }
            params.setDataMapping(dataMapping);
            params.setColorBarIntervals(null);
        }
        getCapability(ColorMapCapability.class).setColorMapParameters(params);

        if (colorMap != null) {
            params.setColorMap(colorMap);
            params.setColorMapName(colorMapName);
        }

        if (params.getColorMap() == null) {
            if (("").equals(colorMapName)) {
                colorMapName = params.getColorMapName();
            }
            if (colorMapName == null) {
                colorMapName = "Radar/OSF/16 Level Reflectivity";
            }

            try {
                params.setColorMap(ColorMapLoader.loadColorMap(colorMapName));
            } catch (ColorMapException e) {
                throw new VizException(e);
            }

        }

        PersistedParameters persisted = null;
        if (paramsCapability != null) {
            persisted = paramsCapability.getPersisted();
        }
        if (persisted != null && persisted.getColorMapMax() != null
                && persisted.getColorMapMin() != null) {
            params.applyPersistedParameters(paramsCapability.getPersisted());
        } else {
            params.setColorMapMax(255);
            params.setColorMapMin(0);
        }
        params.setDataMax(255);
        params.setDataMin(0);
        params.setNoDataValue(0.0);
        return params;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        displayedDate = null;
        if ((paintProps == null) || (paintProps.getDataTime() == null)) {
            return;
        }

        displayedDate = paintProps.getDataTime();
        VizRadarRecord radarRecord = getRadarRecord(displayedDate);
        displayedLevel = displayedDate.getLevelValue().floatValue();

        if (radarRecord == null) {
            issueRefresh();
            return;
        }

        paintRadar(target, paintProps);

        if (resourceData.rangeRings) {
            IWireframeShape rangeCircle = this.rangeCircle.get(actualLevel);

            Float elev = 0.0f;
            if (radarRecord.getPrimaryElevationAngle() != null) {
                elev = radarRecord.getPrimaryElevationAngle().floatValue();
            }
            // create range circle
            rangeCircle = this.rangeCircle.get(elev);
            if (rangeCircle == null) {
                // Attempt to create envelope, adapted from AbstractTileSet
                double maxExtent = RadarUtil.calculateExtent(radarRecord);
                rangeCircle = computeRangeCircle(target, radarRecord.getCRS(),
                        maxExtent);
                if (rangeCircle != null) {
                    this.rangeCircle.put(elev, rangeCircle);
                }
            }

            if ((rangeCircle != null)
                    && (getCapability(OutlineCapability.class).isOutlineOn())) {
                target.drawWireframeShape(rangeCircle,
                        getCapability(ColorableCapability.class).getColor(),
                        getCapability(OutlineCapability.class)
                                .getOutlineWidth(),
                        getCapability(OutlineCapability.class).getLineStyle(),
                        paintProps.getAlpha());
            }
        }
    }

    public void paintRadar(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        displayedDate = paintProps.getDataTime();
        synchronized (this.images) {
            VizRadarRecord record = getRadarRecord(displayedDate);
            if (record == null) {
                return;
            }

            displayedLevel = displayedDate.getLevelValue().floatValue();

            this.actualLevel = String.format("%1.1f",
                    record.getTrueElevationAngle());
            DrawableImage image = getImage(target, displayedDate);
            if (image != null) {
                ImagingCapability cap = getCapability(ImagingCapability.class);
                image.getImage().setBrightness(cap.getBrightness());
                image.getImage().setContrast(cap.getContrast());
                image.getImage().setInterpolated(cap.isInterpolationState());
                target.drawRasters(paintProps, image);
            }

            if (image == null || image.getCoverage() == null
                    || image.getCoverage().getMesh() == null) {
                issueRefresh();
            }
        }
    }

    /**
     * Get the radar image for the given time
     * 
     * @param target
     * @param dataTime
     * @return
     * @throws VizException
     */
    public DrawableImage getImage(IGraphicsTarget target, DataTime dataTime)
            throws VizException {
        DrawableImage image = images.get(dataTime);
        if (image == null || image.getCoverage() == null) {
            VizRadarRecord record = getRadarRecord(dataTime);
            if (record != null) {
                if (record.getNumRadials() == 0 || record.getNumBins() == 0) {
                    // This is expected for dual pol precip products(DSD,DOD)
                    // when it isn't raining.
                    return null;
                } else if (record.getStoredDataAsync() == null) {
                    issueRefresh();
                } else {
                    try {
                        createTile(target, record);
                        image = images.get(dataTime);
                    } catch (Exception e) {
                        String msg = e.getMessage();
                        if (msg == null) {
                            msg = "Error rendering radar";
                        }
                        throw new VizException(msg, e);
                    }
                }
            }
        }
        return image;
    }

    /**
     * Shared by image and non-image
     * 
     * @param target
     * @param crs
     * @param range
     * @return
     */
    public IWireframeShape computeRangeCircle(IGraphicsTarget target,
            CoordinateReferenceSystem crs, double range) {
        IWireframeShape rangeCircle = target.createWireframeShape(true,
                descriptor);

        try {
            MathTransform mt = CRS.findMathTransform(crs,
                    MapUtil.getLatLonProjection());

            double[][] pts = new double[RANGE_CIRCLE_PTS + 1][2];
            double azDelta = 2 * Math.PI / RANGE_CIRCLE_PTS;
            double az = 0.0;
            double[] input = new double[2];
            double[] output = new double[2];
            for (int i = 0; i < pts.length; i++) {
                input[0] = range * Math.cos(az);
                input[1] = range * Math.sin(az);
                mt.transform(input, 0, output, 0, 1);
                pts[i] = descriptor.worldToPixel(output);
                az += azDelta;
            }
            pts[RANGE_CIRCLE_PTS] = pts[0];

            rangeCircle.allocate(pts.length);
            rangeCircle.addLineSegment(pts);
        } catch (TransformException | FactoryException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to compute the range circle", e);
            return null;
        }

        return rangeCircle;
    }

    protected byte[] createConversionTable(ColorMapParameters params,
            RadarRecord record) {
        // Sometimes the data unit may not match what is in the params so always
        // use what we really have
        UnitConverter dataToImage = null;
        Unit<?> dataUnit = record.getDataUnit();
        if (dataUnit != null && !dataUnit.equals(params.getDataUnit())) {
            Unit<?> imageUnit = params.getImageUnit();
            if (imageUnit != null && dataUnit.isCompatible(imageUnit)) {
                dataToImage = UnitConv.getConverterToUnchecked(dataUnit,
                        imageUnit);
            } else if (imageUnit != null) {
                dataUnit = DataUtilities.getDataUnit(record, resourceData.mode);
                if (dataUnit.isCompatible(imageUnit)) {
                    dataToImage = UnitConv.getConverterToUnchecked(dataUnit,
                            imageUnit);
                }
            }
        } else {
            dataToImage = params.getDataToImageConverter();
        }
        if (dataToImage == null && record.getNumLevels() <= 16) {
            dataToImage = new MultiplyConverter(16);
        } else if (dataToImage == null) {
            dataToImage = AbstractConverter.IDENTITY;
        }
        // precompute the converted value for every possible value in the
        // record.
        byte[] table = new byte[record.getNumLevels()];
        for (int i = 0; i < record.getNumLevels(); i++) {
            double image = dataToImage.convert(i);
            if (Double.isNaN(image)) {
                double d;
                try {
                    d = dataUnit.getConverterToAny(params.getDisplayUnit())
                            .convert(i);
                } catch (IncommensurableException | UnconvertibleException e) {
                    d = Double.NaN;
                }
                if (Double.isNaN(d)) {
                    // This means that the data is a non-numeric value, most
                    // likely a flag of some sort
                    if (record.getNumLevels() <= 16) {
                        // For 3 and 4 bit products products try to match the
                        // flag value to a string value in the params
                        String value = record.getDecodedThreshold(i).toString();
                        for (DataMappingEntry entry : params.getDataMapping()
                                .getEntries()) {
                            if (value.equals(entry.getLabel())
                                    || value.equals(entry.getSample())) {
                                table[i] = entry.getPixelValue().byteValue();
                                break;
                            }
                        }
                    } else {
                        // For 8 bit products the flag value should be
                        // specifically handled in the style rules. For example
                        // if 1 is a flag for RF than pixel value 1 in the style
                        // rules will need to be RF. This is not
                        // a graceful seperation of data and representation but
                        // it works
                        table[i] = (byte) i;
                    }
                } else {
                    // the data value is outside the range of the colormap
                    UnitConverter image2disp = params
                            .getImageToDisplayConverter();
                    if (image2disp == null) {
                        continue;
                    }
                    for (int j = 0; j < 256; j++) {
                        double disp = image2disp.convert(j);
                        if (Double.isNaN(disp)) {
                            continue;
                        }
                        if (d < disp) {
                            // Map data values smaller than the colormap min to
                            // 0, which should be no data.
                            // table[i] = (byte) 0;
                            // If we want small values to appear as the lowest
                            // data value than do this next line instead
                            // This was changed for the DUA product so
                            // differences less than -5 get mapped to a data
                            // value.
                            table[i] = (byte) j;
                            break;
                        }
                        if (d > disp) {
                            // map data values larger than the colormap max to
                            // the highest value
                            table[i] = (byte) j;
                        }

                    }
                }
            } else {
                table[i] = (byte) Math.round(image);
            }
        }
        return table;
    }

    protected IImage createImage(IGraphicsTarget target,
            ColorMapParameters params, RadarRecord record, Rectangle rect)
            throws VizException {
        byte[] table = createConversionTable(params, record);
        return target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(
                        new RadarImageDataRetrievalAdapter(record, table, rect),
                        params);
    }

    public IMesh buildMesh(IGraphicsTarget target, VizRadarRecord radarRecord)
            throws VizException {
        return null;
    }

    public PixelCoverage buildCoverage(IGraphicsTarget target,
            VizRadarRecord timeRecord) throws VizException {
        return new PixelCoverage(new Coordinate(0, 0), 0, 0);
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        for (IWireframeShape ring : rangeCircle.values()) {
            ring.dispose();
        }
        rangeCircle.clear();
    }

    public void redoImage(DataTime time) {
        disposeImage(images.remove(time));
    }

    protected void disposeImage(DrawableImage image) {
        if (image != null) {
            image.dispose();
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        final DrawableImage image = images.remove(dataTime);
        if (image == null) {
            return;
        }
        // Run this in the UI thread to avoid accidentally disposing of things
        // that are painting. This is better than synchronizing because it makes
        // it much more difficult to deadlock.
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                disposeImage(image);
            }
        });
    }

    protected static class RadarImageDataRetrievalAdapter
            implements IColorMapDataRetrievalCallback {

        protected final RadarRecord record;

        protected final byte[] table;

        protected Rectangle rect;

        private final int hashCode;

        public RadarImageDataRetrievalAdapter(RadarRecord record, byte[] table,
                Rectangle rect) {
            this.record = record;
            this.table = table;
            this.rect = rect;

            final int prime = 31;
            int hashCode = 1;
            hashCode = prime * hashCode
                    + ((record == null) ? 0 : record.hashCode());
            hashCode = prime * hashCode + Arrays.hashCode(table);
            hashCode = prime * hashCode + rect.hashCode();
            this.hashCode = hashCode;
        }

        @Override
        public ColorMapData getColorMapData() {
            return new ColorMapData(ByteBuffer.wrap(convertData()),
                    new int[] { rect.width, rect.height });
        }

        public byte[] convertData() {
            return record.getRawData();
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            RadarImageDataRetrievalAdapter other = (RadarImageDataRetrievalAdapter) obj;
            if (record == null) {
                if (other.record != null) {
                    return false;
                }
            } else if (!record.equals(other.record)) {
                return false;
            }
            if (!Arrays.equals(table, other.table)) {
                return false;
            }
            return true;
        }

    }

}
