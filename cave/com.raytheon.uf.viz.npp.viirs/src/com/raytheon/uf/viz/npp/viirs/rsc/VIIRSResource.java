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

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSDataRecord;
import com.raytheon.uf.common.dataplugin.npp.viirs.VIIRSSpatialRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.RasterMode;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.uf.viz.core.rsc.hdf5.ImageTile;

/**
 * NPP VIIRS resource. Responsible for drawing a single color band
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class VIIRSResource extends
        AbstractVizResource<VIIRSResourceData, IMapDescriptor> implements
        IResourceDataChanged {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VIIRSResource.class);

    protected static class VIIRSData {
        public VIIRSDataCallback callback;

        public IImage image;

        public ImageTile tile;

        public float[][] projectionData;
    }

    protected static class VIIRSFrame {
        public Map<Object, VIIRSData> dataMap = new HashMap<Object, VIIRSData>();
    }

    private static final String NAME_FORMAT = "NPP VIIRS %s %s (%s microns)";

    protected Map<DataTime, VIIRSFrame> frameData;

    private String name;

    private List<VIIRSDataRecord> records;

    private IGraphicsTarget target;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public VIIRSResource(VIIRSResourceData resourceData,
            LoadProperties loadProperties, List<VIIRSDataRecord> initialRecords) {
        super(resourceData, loadProperties);
        this.records = new ArrayList<VIIRSDataRecord>(initialRecords);
        resourceData.addChangeListener(this);
        frameData = new HashMap<DataTime, VIIRSFrame>();
        dataTimes = new ArrayList<DataTime>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    protected boolean addRecord(VIIRSDataRecord dataRecord) throws Exception {
        // TODO: Ignore isHasSpatial == false
        if (name == null) {
            initializeFirstRecord(dataRecord);
        }

        // Valid dataRecord with spatial data
        DataTime time = dataRecord.getDataTime();
        DataTime normalTime = normalizeTime(time);

        VIIRSFrame frame = frameData.get(normalTime);
        if (frame == null) {
            // First record for normalized time
            frame = new VIIRSFrame();
            frameData.put(normalTime, frame);
            dataTimes.add(normalTime);
        }

        Object recordKey = getRecordKey(dataRecord);
        VIIRSData data = frame.dataMap.get(recordKey);
        if (data == null) {
            data = new VIIRSData();
            IDataStore dataStore = DataStoreFactory.getDataStore(HDF5Util
                    .findHDF5Location(dataRecord));
            String dataURI = dataRecord.getSpatialURI();
            IDataRecord[] latLons = dataStore
                    .retrieveDatasets(
                            new String[] {
                                    dataURI
                                            + DataURI.SEPARATOR
                                            + VIIRSSpatialRecord
                                                    .getLatitudeDataSet(0),
                                    dataURI
                                            + DataURI.SEPARATOR
                                            + VIIRSSpatialRecord
                                                    .getLongitudeDataSet(0) },
                            Request.ALL);
            if (latLons == null || latLons.length != 2) {
                // No spatial data available
                return false;
            }

            IDataRecord lats = latLons[0];
            IDataRecord lons = latLons[1];

            float[] latFloats = ((FloatDataRecord) lats).getFloatData();
            float[] lonFloats = ((FloatDataRecord) lons).getFloatData();

            // Get the width and valid height of the data
            int width = dataRecord.getWidth();
            int height = (Integer) lats.getDataAttributes().get(
                    VIIRSSpatialRecord.VALID_HEIGHT_ID);

            latFloats = Arrays.copyOf(latFloats, height * width);
            lonFloats = Arrays.copyOf(lonFloats, height * width);
            data.projectionData = new float[][] { lonFloats, latFloats };

            data.tile = new ImageTile();
            // data.tile.rect = new Rectangle(0, 0, width, height);
            // data.tile.envelope = new Envelope(0, width, 0, height);

            calculateMesh(data, target);

            data.callback = new VIIRSDataCallback(dataRecord, new Rectangle(0,
                    0, width, height));

            data.image = target.getExtension(IColormappedImageExtension.class)
                    .initializeRaster(
                            data.callback,
                            getCapability(ColorMapCapability.class)
                                    .getColorMapParameters());

            frame.dataMap.put(recordKey, data);
            return true;
        } else {
            System.err.println("Recieved duplicate record for VIIRS data");
            return false;
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
        // First record, process name and parameters
        name = String.format(
                NAME_FORMAT,
                dataRecord.getChannelType(),
                (dataRecord.getChannel() != null ? "Channel "
                        + dataRecord.getChannel() : "Band"),
                dataRecord.getWavelength());

        ColorMapParameters colorMapParameters = getCapability(
                ColorMapCapability.class).getColorMapParameters();
        if (colorMapParameters == null) {
            colorMapParameters = new ColorMapParameters();
        }
        if (colorMapParameters.getColorMap() == null) {
            String name = colorMapParameters.getColorMapName();
            if (name == null) {
                name = "NPP/VIIRS/IR Default";
            }
            colorMapParameters.setColorMap(ColorMapLoader.loadColorMap(name));
        }

        // TODO: Get data unit from record and display unit from style rules
        Unit<?> displayUnit = Unit.ONE;
        Unit<?> dataUnit = Unit.ONE;

        // Static, should be moved from ColorMapParameters into GLDataFormat
        colorMapParameters.setDataMin(0);
        colorMapParameters.setDataMax(0xFFFF);

        try {
            IDataStore ds = DataStoreFactory.getDataStore(HDF5Util
                    .findHDF5Location(dataRecord));
            IDataRecord record = ds.retrieve(dataRecord.getDataURI(),
                    VIIRSDataRecord.getDataSet(0),
                    Request.buildPointRequest(new Point(0, 0)));
            Map<String, Object> attrs = record.getDataAttributes();
            Float offset = (Float) attrs.get(VIIRSDataRecord.OFFSET_ID);
            Float scale = (Float) attrs.get(VIIRSDataRecord.SCALE_ID);
            if (offset != null && scale != null) {
                dataUnit = dataUnit.plus(offset).times(scale);
            }
        } catch (Exception e) {
            throw new VizException(e);
        }

        colorMapParameters.setDataUnit(dataUnit);
        colorMapParameters.setDisplayUnit(displayUnit);

        // TODO: Get from style rules?
        colorMapParameters.setColorMapMin(12900);
        colorMapParameters.setColorMapMax(52700);

        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);
    }

    /**
     * Construct a unique key for the record. Base implementation assumes all
     * records are same channel/region and only uses DataTime as unique key.
     * Object returned will be added to a HashMap so must implement hashCode and
     * equals
     * 
     * @param dataRecord
     * @return
     */
    protected Object getRecordKey(VIIRSDataRecord dataRecord) {
        return dataRecord.getDataTime();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        clearCoverages();
        for (VIIRSFrame frame : frameData.values()) {
            for (VIIRSData data : frame.dataMap.values()) {
                data.image.dispose();
            }
            frame.dataMap.clear();
        }
        frameData.clear();
    }

    private void clearCoverages() {
        for (VIIRSFrame frame : frameData.values()) {
            for (VIIRSData data : frame.dataMap.values()) {
                if (data.tile != null) {
                    data.tile.dispose();
                    data.tile = null;
                }
            }
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
        VIIRSFrame frame = frameData.get(paintProps.getDataTime());
        if (frame != null) {
            ImagingCapability imgCap = getCapability(ImagingCapability.class);
            float brightness = imgCap.getBrightness();
            float contrast = imgCap.getContrast();
            boolean interp = imgCap.isInterpolationState();

            List<DrawableImage> images = new ArrayList<DrawableImage>();
            for (VIIRSData data : frame.dataMap.values()) {
                data.image.setBrightness(brightness);
                data.image.setContrast(contrast);
                data.image.setInterpolated(interp);

                DrawableImage di = new DrawableImage(data.image,
                        data.tile.coverage);
                di.setMode(RasterMode.ASYNCHRONOUS);
                images.add(di);
            }
            target.drawRasters(paintProps,
                    images.toArray(new DrawableImage[images.size()]));
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
    protected synchronized void initInternal(IGraphicsTarget target)
            throws VizException {
        this.target = target;
        try {
            if (records != null) {
                for (VIIRSDataRecord record : records) {
                    addRecord(record);
                }
            }
        } catch (Exception e) {
            throw new VizException(e);
        }
    }

    /**
     * @param frame
     * @param target
     */
    private void calculateMesh(VIIRSData frame, IGraphicsTarget target)
            throws VizException {
        // Rectangle tile = frame.tile.rect;
        // frame.tile.coverage = new PixelCoverage(new
        // Coordinate(tile.getMinX(),
        // tile.getMinY()),
        // new Coordinate(tile.getMaxX(), tile.getMinY()), new Coordinate(
        // tile.getMaxX(), tile.getMaxY()), new Coordinate(
        // tile.getMinX(), tile.getMaxY()));
        // IMesh mesh = target.getExtension(IMapMeshExtension.class)
        // .constructMesh(descriptor);
        // mesh.calculateMesh(frame.tile.coverage, frame.tile,
        // new VIIRSDataMathTransform(frame.projectionData,
        // frame.tile.rect.width, frame.tile.rect.height));
        // frame.projectionData = null;
        // frame.tile.coverage.setMesh(mesh);
        // frame.projectionData = null;
    }

    /**
     * Normalize the DataTime using BinOffset of resourceData if set
     * 
     * @param dt
     * @return
     */
    private DataTime normalizeTime(DataTime dt) {
        BinOffset offset = resourceData.getBinOffset();
        if (offset != null) {
            dt = offset.getNormalizedTime(dt);
        }
        return dt;
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
    public synchronized void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            VIIRSDataRecord[] records = (VIIRSDataRecord[]) object;
            for (VIIRSDataRecord record : records) {
                this.records.add(record);
                if (target != null) {
                    // target is set in initInternal, this means we have already
                    // initialized and can add records freely. If we get updates
                    // before we've initialized, they will be processed in
                    // initInternal
                    try {
                        addRecord(record);
                    } catch (Exception e) {
                        statusHandler.handle(
                                Priority.PROBLEM,
                                "Error adding record from update: "
                                        + e.getLocalizedMessage(), e);
                    }
                }
            }
        }
        issueRefresh();
    }
}
