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
package com.raytheon.uf.viz.npp.viirs.rsc;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.measure.Quantity;
import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.prep.PreparedGeometry;
import org.locationtech.jts.geom.prep.PreparedGeometryFactory;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.colormap.ColorMapException;
import com.raytheon.uf.common.colormap.ColorMapLoader;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.IGridGeometryProvider;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.geospatial.util.EnvelopeIntersection;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ImageryLabelingPreferences;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.StyleManager;
import com.raytheon.uf.common.style.StyleManager.StyleType;
import com.raytheon.uf.common.style.StyleRule;
import com.raytheon.uf.common.style.image.DataScale;
import com.raytheon.uf.common.style.image.ImagePreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension.ImageProvider;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.tile.Tile;
import com.raytheon.uf.viz.core.tile.TileSetRenderable;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.uf.viz.npp.viirs.Activator;
import com.raytheon.uf.viz.npp.viirs.style.VIIRSDataRecordCriteria;

import tec.uom.se.AbstractUnit;
import tec.uom.se.quantity.Quantities;

/**
 * NPP VIIRS resource. Responsible for drawing a single color band.
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 30, 2011           mschenke  Initial creation
 * Feb 21, 2012  30       mschenke  Fixed sampling issue
 * Aug 02, 2013  2190     mschenke  Switched interrogate to use Measure objects
 * Aug 27, 2013  2190     mschenke  Made interrogate more efficient
 * Nov 08, 2016  5976     bsteffen  Update deprecated method calls.
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Apr 04, 2018  6889     njensen   Use brightness from ImagePreferences if
 *                                  present but missing in ImagingCapability
 * Nov 29, 2018  7605     bsteffen  Set colormap unit, remove deprecated calls.
 * Apr 15, 2019  7596     lsingh    Updated units framework to JSR-363.
 * May 29, 2019 60162     ksunil    changes to absorb new Contour Label structure
 *
 * </pre>
 *
 * @author mschenke
 */
public class VIIRSResource
        extends AbstractVizResource<VIIRSResourceData, IMapDescriptor>
        implements IResourceDataChanged, ImageProvider {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VIIRSResource.class);

    /** String id to look for satellite-provided data values */
    public static final String SATELLITE_DATA_INTERROGATE_ID = "satelliteDataValue";

    private static final DecimalFormat NUMBER_FORMAT = new DecimalFormat(
            "0.00");

    private class VIIRSTileImageCreator implements TileImageCreator {

        private VIIRSDataRecord record;

        private VIIRSTileImageCreator(VIIRSDataRecord record) {
            this.record = record;
        }

        @Override
        public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
                GeneralGridGeometry targetGeometry) throws VizException {
            IImage image = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(
                            new VIIRSDataCallback(record, tile.tileLevel,
                                    tile.getRectangle()),
                            getCapability(ColorMapCapability.class)
                                    .getColorMapParameters());
            IMesh mesh = target.getExtension(IMapMeshExtension.class)
                    .constructMesh(tile.tileGeometry, targetGeometry);
            return new DrawableImage(image, new PixelCoverage(mesh),
                    RasterMode.ASYNCHRONOUS);
        }
    }

    /**
     * Every {@link VIIRSDataRecord} will have a corresponding RecordData object
     */
    private class RecordData {

        private double INTERSECTION_FACTOR = 10.0;

        /** Intersection geometry for the target */
        private List<PreparedGeometry> targetIntersection;

        /** Flag designated if a project call is required next paint */
        private boolean project;

        /** Renderable for the data record */
        private TileSetRenderable tileSet;

        private final double resolution;

        public RecordData(VIIRSDataRecord dataRecord) {
            this.tileSet = new TileSetRenderable(
                    getCapability(ImagingCapability.class),
                    dataRecord.getCoverage().getGridGeometry(),
                    new VIIRSTileImageCreator(dataRecord),
                    dataRecord.getLevels(), 256);
            this.resolution = Math.min(dataRecord.getCoverage().getDx(),
                    dataRecord.getCoverage().getDy()) * INTERSECTION_FACTOR;
            this.project = true;
        }

        public Collection<DrawableImage> getImagesToRender(
                IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            if (project) {
                projectInternal();
                project = false;
            }
            if (targetIntersection != null) {
                return tileSet.getImagesToRender(target, paintProps);
            } else {
                return Collections.emptyList();
            }
        }

        public void project() {
            this.project = true;
        }

        private void projectInternal() {
            GeneralGridGeometry targetGeometry = descriptor.getGridGeometry();
            if (tileSet.getTargetGeometry() != targetGeometry) {
                tileSet.project(targetGeometry);

                try {
                    Envelope tileSetEnvelope = tileSet.getTileSetGeometry()
                            .getEnvelope();

                    targetIntersection = null;
                    Geometry intersection = EnvelopeIntersection
                            .createEnvelopeIntersection(tileSetEnvelope,
                                    targetGeometry.getEnvelope(), resolution,
                                    (int) (tileSetEnvelope.getSpan(0)
                                            / (resolution
                                                    * INTERSECTION_FACTOR)),
                                    (int) (tileSetEnvelope.getSpan(1)
                                            / (resolution
                                                    * INTERSECTION_FACTOR)));
                    if (intersection != null) {
                        int numGeoms = intersection.getNumGeometries();
                        targetIntersection = new ArrayList<>(numGeoms);
                        for (int n = 0; n < numGeoms; ++n) {
                            targetIntersection.add(
                                    PreparedGeometryFactory.prepare(intersection
                                            .getGeometryN(n).buffer(resolution
                                                    * INTERSECTION_FACTOR)));
                        }
                    }
                } catch (Exception e) {
                    Activator.statusHandler.handle(Priority.PROBLEM,
                            "Error constructing envelope intersection for tileset",
                            e);
                }
            }
        }

        public double interrogate(Coordinate latLon, Unit<?> resultUnit,
                double nanValue) throws VizException {
            return tileSet.interrogate(latLon, resultUnit, nanValue);
        }

        public boolean contains(Geometry geom) {
            if (targetIntersection != null) {
                for (PreparedGeometry pg : targetIntersection) {
                    if (pg.contains(geom)) {
                        return true;
                    }
                }
            }
            return false;
        }

        public void dispose() {
            tileSet.dispose();
            tileSet = null;
            targetIntersection = null;
        }

    }

    /**
     * Map for data records to renderable data, synchronized on for painting,
     * disposing, adding, and removing
     */
    private Map<VIIRSDataRecord, RecordData> dataRecordMap;

    private Map<DataTime, Collection<VIIRSDataRecord>> groupedRecords;

    private String name;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public VIIRSResource(VIIRSResourceData resourceData,
            LoadProperties loadProperties,
            List<VIIRSDataRecord> initialRecords) {
        super(resourceData, loadProperties, false);
        dataRecordMap = new LinkedHashMap<>();
        groupedRecords = new HashMap<>();
        resourceData.addChangeListener(this);
        for (VIIRSDataRecord record : initialRecords) {
            dataRecordMap.put(record, null);
        }
    }

    @Override
    public String getName() {
        if (name == null) {
            return "NPP VIIRS";
        }
        return name;
    }

    /**
     * Add a data record to be displayed. Should be synchronized on
     * {@link #dataMap} when called
     * 
     * @param dataRecord
     * @throws VizException
     * @throws Exception
     */
    protected void addRecord(VIIRSDataRecord dataRecord) throws VizException {
        if (name == null) {
            initializeFirstRecord(dataRecord);
        }

        RecordData data = dataRecordMap.get(dataRecord);
        if (data == null) {
            // New record
            dataRecordMap.put(dataRecord, new RecordData(dataRecord));
        }
    }

    /**
     * First record was added, initialize any fields that require information
     * from a record
     * 
     * @param dataRecord
     * @throws VizException
     */
    protected void initializeFirstRecord(VIIRSDataRecord dataRecord)
            throws VizException {
        name = "NPP VIIRS";
        // First record, process name and parameters
        if (dataRecord.getChannelType() != null) {
            name += " " + dataRecord.getChannelType();
        }
        String parameter = dataRecord.getParameter();
        if (dataRecord.getWavelength() != null) {
            parameter = dataRecord.getWavelength() + parameter;
        }
        name += " " + parameter;

        // Get style rule preferences
        StyleRule styleRule;
        try {
            styleRule = StyleManager.getInstance().getStyleRule(
                    StyleType.IMAGERY, new VIIRSDataRecordCriteria(dataRecord));
        } catch (StyleException e1) {
            throw new VizException(e1.getLocalizedMessage(), e1);
        }
        ImagePreferences preferences = null;
        if (styleRule != null) {
            preferences = (ImagePreferences) styleRule.getPreferences();
        }

        // Create colormap parameters
        ColorMapParameters colorMapParameters = getCapability(
                ColorMapCapability.class).getColorMapParameters();
        if (colorMapParameters == null) {
            colorMapParameters = new ColorMapParameters();
        }

        // Setup colormap
        if (colorMapParameters.getColorMap() == null) {
            // None set, set name and load
            String name = colorMapParameters.getColorMapName();
            if (name == null) {
                if (preferences != null) {
                    // check preference
                    name = preferences.getDefaultColormap();
                }
                if (name == null) {
                    // no preference, absolute default colormap
                    name = "NPP/VIIRS/IR Default";
                }
            }
            try {
                colorMapParameters
                        .setColorMap(ColorMapLoader.loadColorMap(name));
            } catch (ColorMapException e) {
                throw new VizException(e);
            }
        }

        // Setup units for display and data
        Unit<?> displayUnit = AbstractUnit.ONE;
        if (preferences != null) {
            if (preferences.getDisplayUnits() != null) {
                displayUnit = preferences.getDisplayUnits();
            }
        }

        try {
            IDataRecord record = DataCubeContainer.getDataRecord(dataRecord,
                    Request.buildPointRequest(new java.awt.Point(0, 0)),
                    VIIRSDataRecord.getDataSet(0))[0];
            Map<String, Object> attrs = record.getDataAttributes();
            if (attrs != null) {
                if (attrs.containsKey(VIIRSDataRecord.MISSING_VALUE_ID)) {
                    colorMapParameters.setNoDataValue(((Number) attrs
                            .get(VIIRSDataRecord.MISSING_VALUE_ID))
                                    .doubleValue());
                }
            }

            colorMapParameters
                    .setColorMapUnit(VIIRSDataCallback.getDataUnit(record));

            if (record instanceof ShortDataRecord) {
                // Setup Min/Max values
                // Static, should be moved from ColorMapParameters into
                // GLDataFormat
                colorMapParameters.setColorMapMin(0);
                colorMapParameters.setColorMapMax(0xFFFF);
            }
        } catch (Exception e) {
            throw new VizException(e);
        }
        colorMapParameters.setDisplayUnit(displayUnit);

        if (preferences != null) {
            try {
                Unit<?> colorMapUnit = preferences.getColorMapUnitsObject();
                if (colorMapUnit != null) {
                    colorMapParameters.setColorMapUnit(colorMapUnit);
                }
            } catch (StyleException e) {
                throw new VizException(e);
            }
            DataScale scale = preferences.getDataScale();
            if (scale != null) {
                UnitConverter displayToColorMap = colorMapParameters
                        .getDisplayToColorMapConverter();
                if (scale.getMinValue() != null) {
                    colorMapParameters.setColorMapMin(displayToColorMap
                            .convert(scale.getMinValue()).floatValue());
                }
                if (scale.getMaxValue() != null) {
                    colorMapParameters.setColorMapMax(displayToColorMap
                            .convert(scale.getMaxValue()).floatValue());
                }
            }
            if (preferences.getColorbarLabeling() != null) {
                ImageryLabelingPreferences lPrefs = preferences
                    .getColorbarLabeling();
                colorMapParameters.setColorBarIntervals(lPrefs.getValues());
            }
        }

        if (colorMapParameters.getPersisted() != null) {
            colorMapParameters.applyPersistedParameters(
                    colorMapParameters.getPersisted());
        }

        getCapability(ColorMapCapability.class)
                .setColorMapParameters(colorMapParameters);

        /*
         * If the capability already has a brightness it was most likely loaded
         * from a procedure/bundle and that should take precedent. If brightness
         * is not set, then try style rules to get a brightness.
         */
        ImagingCapability imgCap = getCapability(ImagingCapability.class);
        if (!imgCap.isBrightnessSet() && preferences != null
                && preferences.getBrightness() != null) {
            imgCap.setBrightness(preferences.getBrightness());
        }
    }

    public void addRecords(PluginDataObject... records) {
        synchronized (dataRecordMap) {
            for (PluginDataObject record : records) {
                if (record instanceof VIIRSDataRecord) {
                    try {
                        addRecord((VIIRSDataRecord) record);
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error adding record from update: "
                                        + e.getLocalizedMessage(),
                                e);

                    }
                }
            }

            Map<DataTime, Collection<VIIRSDataRecord>> groupedRecords = resourceData
                    .groupRecordTimes(dataRecordMap.keySet());
            this.dataTimes.retainAll(groupedRecords.keySet());
            this.dataTimes.addAll(groupedRecords.keySet());
            this.groupedRecords = groupedRecords;
        }
    }

    @Override
    public void remove(DataTime dataTime) {
        synchronized (dataRecordMap) {
            Collection<VIIRSDataRecord> records = groupedRecords
                    .remove(dataTime);
            if (records != null) {
                for (VIIRSDataRecord record : records) {
                    RecordData data = dataRecordMap.remove(record);
                    if (data != null) {
                        data.dispose();
                    }
                }
            }
        }
        super.remove(dataTime);
    }

    @Override
    protected void disposeInternal() {
        synchronized (dataRecordMap) {
            for (RecordData data : dataRecordMap.values()) {
                data.dispose();
            }
            dataRecordMap.clear();
            groupedRecords.clear();
            dataTimes.clear();
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        Collection<DrawableImage> images = getImages(target, paintProps);
        if (!images.isEmpty()) {
            if (!target.drawRasters(paintProps,
                    images.toArray(new DrawableImage[images.size()]))) {
                issueRefresh();
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        getCapability(ImagingCapability.class).setProvider(this);
        synchronized (dataRecordMap) {
            PluginDataObject[] pdos = dataRecordMap.keySet()
                    .toArray(new PluginDataObject[0]);
            resourceChanged(ChangeType.DATA_UPDATE, pdos);
        }
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            VIIRSDataRecord[] records = new VIIRSDataRecord[pdos.length];
            for (int i = 0; i < pdos.length; ++i) {
                records[i] = (VIIRSDataRecord) pdos[i];
            }

            addRecords(records);
        }
        issueRefresh();
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> interMap = interrogate(coord);
        double value = Double.NaN;
        Unit<?> unit = AbstractUnit.ONE;
        Quantity<?> dataVal = (Quantity<?>) interMap
                .get(SATELLITE_DATA_INTERROGATE_ID);
        if (dataVal != null) {
            value = (Double) dataVal.getValue();
            unit = dataVal.getUnit();
        }
        if (Double.isNaN(value)) {
            return "NO DATA";
        } else {
            String inspectStr = NUMBER_FORMAT.format(value);
            if (unit != null && unit != AbstractUnit.ONE) {
                inspectStr += " " + unit.toString();
            }
            return inspectStr;
        }
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        Map<String, Object> interMap = new HashMap<>();
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        double dataValue = Double.NaN;
        Unit<?> dataUnit = params.getDisplayUnit();
        double noDataValue = params.getNoDataValue();
        Coordinate latLon = null;
        Coordinate crs = null;
        try {
            latLon = coord.asLatLon();
            Coordinate grid = coord.asGridCell(descriptor.getGridGeometry(),
                    PixelInCell.CELL_CENTER);
            MathTransform mt = descriptor.getGridGeometry()
                    .getGridToCRS(PixelInCell.CELL_CENTER);
            double[] out = new double[2];
            mt.transform(new double[] { grid.x, grid.y }, 0, out, 0, 1);
            crs = new Coordinate(out[0], out[1]);
        } catch (Exception e) {
            throw new VizException(
                    "Could not get lat/lon from ReferencedCoordinate", e);
        }

        Point crsPoint = new GeometryFactory().createPoint(crs);

        VIIRSDataRecord bestRecord = null;

        synchronized (dataRecordMap) {
            Collection<VIIRSDataRecord> records = groupedRecords
                    .get(descriptor.getTimeForResource(this));
            if (records != null) {
                // Since there is overlap between granules, the best value is
                // the one that is closest to 0
                for (VIIRSDataRecord record : records) {
                    RecordData data = dataRecordMap.get(record);
                    if (data.contains(crsPoint)) {
                        double value = data.interrogate(latLon, dataUnit, noDataValue);
                        if (!Double.isNaN(value)) {
                            dataValue = value;
                            bestRecord = record;
                            break;
                        }
                    }
                }
            }
            if (bestRecord != null) {
                interMap.put(IGridGeometryProvider.class.toString(),
                        bestRecord);
            }
        }

        interMap.put(SATELLITE_DATA_INTERROGATE_ID,
                Quantities.getQuantity(dataValue, dataUnit));
        return interMap;
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        synchronized (dataRecordMap) {
            try {
                for (RecordData data : dataRecordMap.values()) {
                    data.project();
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reprojecting VIIRS data", e);
            }
        }
    }

    @Override
    public Collection<DrawableImage> getImages(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        List<DrawableImage> images = new ArrayList<>();
        synchronized (dataRecordMap) {
            Collection<VIIRSDataRecord> records = groupedRecords
                    .get(paintProps.getDataTime());
            if (records != null) {
                for (VIIRSDataRecord record : records) {
                    RecordData data = dataRecordMap.get(record);
                    if (data != null) {
                        images.addAll(
                                data.getImagesToRender(target, paintProps));
                    }
                }
            }
        }
        return images;
    }
}
