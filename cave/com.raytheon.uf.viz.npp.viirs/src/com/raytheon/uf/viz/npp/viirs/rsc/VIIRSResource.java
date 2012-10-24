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
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.ShortDataRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
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
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.uf.viz.core.style.StyleManager;
import com.raytheon.uf.viz.core.style.StyleManager.StyleType;
import com.raytheon.uf.viz.core.style.StyleRule;
import com.raytheon.uf.viz.core.tile.Tile;
import com.raytheon.uf.viz.core.tile.TileSetRenderable;
import com.raytheon.uf.viz.core.tile.TileSetRenderable.TileImageCreator;
import com.raytheon.uf.viz.npp.viirs.style.VIIRSDataRecordCriteria;
import com.raytheon.viz.core.style.image.DataScale;
import com.raytheon.viz.core.style.image.ImagePreferences;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * NPP VIIRS resource. Responsible for drawing a single color band.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            mschenke    Initial creation
 * Feb 21, 2012 #30        mschenke    Fixed sampling issue
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSResource extends
        AbstractVizResource<VIIRSResourceData, IMapDescriptor> implements
        IResourceDataChanged, ImageProvider {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VIIRSResource.class);

    public static final String DATA_KEY = "dataValue";

    public static final String UNIT_KEY = "unit";

    private static final DecimalFormat NUMBER_FORMAT = new DecimalFormat("0.00");

    private class VIIRSTileImageCreator implements TileImageCreator {

        private VIIRSDataRecord record;

        private VIIRSTileImageCreator(VIIRSDataRecord record) {
            this.record = record;
        }

        @Override
        public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
                GeneralGridGeometry targetGeometry) throws VizException {
            IImage image = target
                    .getExtension(IColormappedImageExtension.class)
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
    private static class RecordData {

        /** Indicates if data intersects current descriptor */
        private boolean intersects = false;

        /** Indicates if the {@link #tileSet} needs to be projected */
        private boolean project = true;

        /** Renderable for the data record */
        private TileSetRenderable tileSet;

    }

    /**
     * Map for data records to renderable data, synchronized on for painting,
     * disposing, adding, and removing
     */
    private Map<VIIRSDataRecord, RecordData> dataRecordMap;

    private String name;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public VIIRSResource(VIIRSResourceData resourceData,
            LoadProperties loadProperties, List<VIIRSDataRecord> initialRecords) {
        super(resourceData, loadProperties);
        dataRecordMap = new LinkedHashMap<VIIRSDataRecord, RecordData>();
        resourceData.addChangeListener(this);
        dataTimes = new ArrayList<DataTime>();
        for (VIIRSDataRecord record : initialRecords) {
            dataRecordMap.put(record, null);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
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
     * @return false if record should be discarded
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
            data = new RecordData();
            data.tileSet = new TileSetRenderable(
                    getCapability(ImagingCapability.class), dataRecord
                            .getCoverage().getGridGeometry(),
                    new VIIRSTileImageCreator(dataRecord),
                    dataRecord.getLevels(), 256);
            dataRecordMap.put(dataRecord, data);
        }

        // Make sure record intersects map
        data.intersects = intersects(dataRecord);
    }

    private boolean intersects(VIIRSDataRecord dataRecord) {
        // Make sure spatial record intersects map
        PixelCoverage dataCoverage = descriptor.worldToPixel(dataRecord
                .getCoverage().getEnvelope().getEnvelopeInternal());
        return dataCoverage.intersects(new PixelExtent(descriptor
                .getGridGeometry().getGridRange()));
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
        StyleRule styleRule = StyleManager.getInstance().getStyleRule(
                StyleType.IMAGERY, new VIIRSDataRecordCriteria(dataRecord));
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
            colorMapParameters.setColorMap(ColorMapLoader.loadColorMap(name));
        }

        // Setup units for display and data
        Unit<?> displayUnit = Unit.ONE;
        if (preferences != null) {
            if (preferences.getDisplayUnits() != null) {
                displayUnit = preferences.getDisplayUnits();
            }
        }
        Unit<?> dataUnit = Unit.ONE;

        try {
            IDataRecord record = DataCubeContainer.getDataRecord(dataRecord,
                    Request.buildPointRequest(new java.awt.Point(0, 0)),
                    VIIRSDataRecord.getDataSet(0))[0];
            Map<String, Object> attrs = record.getDataAttributes();
            if (attrs != null) {
                Float offset = (Float) attrs.get(VIIRSDataRecord.OFFSET_ID);
                Float scale = (Float) attrs.get(VIIRSDataRecord.SCALE_ID);
                String unitStr = (String) attrs.get(VIIRSDataRecord.UNIT_ID);
                if (attrs.containsKey(VIIRSDataRecord.MISSING_VALUE_ID)) {
                    colorMapParameters.setNoDataValue(((Number) attrs
                            .get(VIIRSDataRecord.MISSING_VALUE_ID))
                            .doubleValue());
                }
                if (unitStr != null) {
                    try {
                        dataUnit = UnitFormat.getUCUMInstance().parseObject(
                                unitStr, new ParsePosition(0));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
                if (offset != null && offset != 0.0) {
                    dataUnit = dataUnit.plus(offset);
                }
                if (scale != null && scale != 0.0) {
                    dataUnit = dataUnit.times(scale);
                }
            }

            if (record instanceof ShortDataRecord) {
                // Setup Min/Max values
                // Static, should be moved from ColorMapParameters into
                // GLDataFormat
                colorMapParameters.setDataMin(0);
                colorMapParameters.setDataMax(0xFFFF);
            }
        } catch (Exception e) {
            throw new VizException(e);
        }

        colorMapParameters.setDataUnit(dataUnit);
        colorMapParameters.setDisplayUnit(displayUnit);

        colorMapParameters.setColorMapMin(colorMapParameters.getDataMin());
        colorMapParameters.setColorMapMax(colorMapParameters.getDataMax());
        if (preferences != null) {
            DataScale scale = preferences.getDataScale();
            if (scale != null) {
                UnitConverter displayToData = colorMapParameters
                        .getDisplayToDataConverter();
                if (scale.getMinValue() != null) {
                    colorMapParameters.setColorMapMin((float) displayToData
                            .convert(scale.getMinValue()));
                }
                if (scale.getMaxValue() != null) {
                    colorMapParameters.setColorMapMax((float) displayToData
                            .convert(scale.getMaxValue()));
                }
                if (scale.getMinValue2() != null) {
                    colorMapParameters.setDataMin((float) displayToData
                            .convert(scale.getMinValue2()));
                }
                if (scale.getMaxValue2() != null) {
                    colorMapParameters.setDataMax((float) displayToData
                            .convert(scale.getMaxValue2()));
                }
            }
        }

        if (preferences != null && preferences.getColorbarLabeling() != null) {
            LabelingPreferences lPrefs = preferences.getColorbarLabeling();
            colorMapParameters.setColorBarIntervals(lPrefs.getValues());
        }

        if (colorMapParameters.getPersisted() != null) {
            colorMapParameters.applyPersistedParameters(colorMapParameters
                    .getPersisted());
        }

        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#remove(com.raytheon.
     * uf.common.time.DataTime)
     */
    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        synchronized (dataRecordMap) {
            Iterator<Entry<VIIRSDataRecord, RecordData>> iter = dataRecordMap
                    .entrySet().iterator();
            while (iter.hasNext()) {
                Entry<VIIRSDataRecord, RecordData> entry = iter.next();
                VIIRSDataRecord record = entry.getKey();
                if (VIIRSResourceData.withinRange(dataTime.getValidPeriod(),
                        record.getDataTime())) {
                    iter.remove();
                    RecordData data = entry.getValue();
                    data.tileSet.dispose();
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        synchronized (dataRecordMap) {
            for (RecordData data : dataRecordMap.values()) {
                data.tileSet.dispose();
            }
            dataRecordMap.clear();
            dataTimes.clear();
        }
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
        Collection<DrawableImage> images = getImages(target, paintProps);
        if (images.size() > 0) {
            if (!target.drawRasters(paintProps,
                    images.toArray(new DrawableImage[images.size()]))) {
                issueRefresh();
            }
        }
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
        getCapability(ImagingCapability.class).setProvider(this);
        synchronized (dataRecordMap) {
            resourceChanged(ChangeType.DATA_UPDATE, dataRecordMap.keySet()
                    .toArray(new PluginDataObject[dataRecordMap.size()]));
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdos = (PluginDataObject[]) object;
            VIIRSDataRecord[] records = new VIIRSDataRecord[pdos.length];
            for (int i = 0; i < pdos.length; ++i) {
                records[i] = (VIIRSDataRecord) pdos[i];
            }
            synchronized (dataRecordMap) {
                long t0 = System.currentTimeMillis();
                for (VIIRSDataRecord record : records) {
                    try {
                        addRecord(record);
                    } catch (VizException e) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Error adding record from update: "
                                        + e.getLocalizedMessage(), e);

                    }
                }
                System.out.println("Time to add " + records.length
                        + " records: " + (System.currentTimeMillis() - t0)
                        + "ms");
                dataTimes = new ArrayList<DataTime>(resourceData
                        .groupRecordTimes(dataRecordMap.keySet()).keySet());
            }
        }
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> interMap = interrogate(coord);
        double value = Double.NaN;
        Number dataVal = (Number) interMap.get(DATA_KEY);
        if (dataVal != null) {
            value = dataVal.doubleValue();
        }
        if (Double.isNaN(value)) {
            return "NO DATA";
        } else {
            String inspectStr = NUMBER_FORMAT.format(value);
            Unit<?> unit = (Unit<?>) interMap.get(UNIT_KEY);
            if (unit != null && unit != Unit.ONE) {
                inspectStr += " " + UnitFormat.getUCUMInstance().format(unit);
            }
            return inspectStr;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
     * .uf.common.geospatial.ReferencedCoordinate)
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        Map<String, Object> interMap = new HashMap<String, Object>();
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        interMap.put(UNIT_KEY, params.getDisplayUnit());
        double noDataValue = params.getNoDataValue();
        Coordinate latLon = null;
        try {
            latLon = coord.asLatLon();
        } catch (Exception e) {
            throw new VizException(
                    "Could not get lat/lon from ReferencedCoordinate", e);
        }
        DataTime currTime = descriptor.getTimeForResource(this);
        if (currTime != null) {
            double bestValue = Double.NaN;
            // Since there is overlap between granules, the best value is the
            // one that is closest to 0
            synchronized (dataRecordMap) {
                for (VIIRSDataRecord record : dataRecordMap.keySet()) {
                    if (VIIRSResourceData.withinRange(
                            currTime.getValidPeriod(), record.getDataTime())) {
                        RecordData data = dataRecordMap.get(record);
                        if (data.intersects && data.project == false) {
                            double value = data.tileSet.interrogate(latLon);
                            if (Double.isNaN(value) == false
                                    && value != noDataValue) {
                                bestValue = value;
                                break;
                            }
                        }
                    }
                }
            }
            interMap.put(DATA_KEY,
                    params.getDataToDisplayConverter().convert(bestValue));
        }
        return interMap;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        synchronized (dataRecordMap) {
            try {
                for (VIIRSDataRecord record : dataRecordMap.keySet()) {
                    RecordData data = dataRecordMap.get(record);
                    data.intersects = intersects(record);
                    data.project = true;
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error reprojecting VIIRS data", e);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.ext.IImagingExtension.ImageProvider
     * #getImages(com.raytheon.uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    public Collection<DrawableImage> getImages(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        List<DrawableImage> images = new ArrayList<DrawableImage>();
        DataTime currTime = paintProps.getDataTime();
        if (currTime != null) {
            TimeRange tr = currTime.getValidPeriod();
            synchronized (dataRecordMap) {
                for (VIIRSDataRecord record : dataRecordMap.keySet()) {
                    if (VIIRSResourceData.withinRange(tr, record.getDataTime())) {
                        RecordData data = dataRecordMap.get(record);
                        if (data.intersects) {
                            if (data.project) {
                                data.tileSet.project(descriptor
                                        .getGridGeometry());
                                data.project = false;
                            }
                            images.addAll(data.tileSet.getImagesToRender(
                                    target, paintProps));
                        }
                    }
                }
            }
        }
        return images;
    }
}
