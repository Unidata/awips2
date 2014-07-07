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
package com.raytheon.viz.lightning;

import java.awt.Font;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.HDF5Util;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataplugin.binlightning.impl.LtgStrikeType;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.ByteDataRecord;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.datastorage.records.IntegerDataRecord;
import com.raytheon.uf.common.datastorage.records.LongDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.BinOffset;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetrieverAndDisposer;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.ICanvasRenderingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;

/**
 * LightningResource
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Aug 6, 2007              chammack    Initial Creation.
 *    Feb 18, 2009             chammack    Refactored to new resource model
 *    June 22, 2010 #4286      bkowal      The resource now uses the magnification
 *                                         capability. The resource also implements
 *                                         IResourceDataChanged now so that it will
 *                                         know when the magnification level has been
 *                                         altered by the user. The size of the &quot;+&quot; and
 *                                         &quot;-&quot; symbols as well as the font size of the
 *                                         text will be updated when the user changes
 *                                         the magnification level now.
 *    Sep 4, 2012  15335       kshresth    Will now display lightning/wind 
 *                                         fields when magnification set to 0
 *    Feb 27, 2013 DCS 152     jgerth/elau Support for WWLLN and multiple sources
 *    Jan 21, 2014  2667       bclement    renamed record's lightSource field to source
 *    Jun 05, 2014  3226       bclement    reference datarecords by LightningConstants
 *    Jun 06, 2014  DR 17367   D. Friedman Fix cache object usage.
 *    Jun 19, 2014  3214       bclement    added pulse and cloud flash support
 *    Jul 07, 2014  3333       bclement    removed lightSource field
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class LightningResource extends
        AbstractVizResource<LightningResourceData, IMapDescriptor> implements
        IResourceDataChanged {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LightningResource.class);

    private static class LightningFrame {

        public LightningFrameMetadata metadata;

        public DataTime frameTime;

        public List<double[]> posLatLonList = new ArrayList<double[]>();

        public List<double[]> negLatLonList = new ArrayList<double[]>();

        public List<double[]> cloudLatLonList = new ArrayList<double[]>();

        public List<double[]> pulseLatLonList = new ArrayList<double[]>();
    }

    private static class LightningFrameMetadata {

        private BinOffset offset;

        private DataTime frameTime;
        
        private List<BinLightningRecord> newRecords = new ArrayList<BinLightningRecord>();

        private List<BinLightningRecord> processed = new ArrayList<BinLightningRecord>();

        public LightningFrameMetadata(DataTime frameTime, BinOffset offset) {
            this.frameTime = frameTime;
            this.offset = offset;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result
                    + ((frameTime == null) ? 0 : frameTime.hashCode());
            result = prime * result
                    + ((offset == null) ? 0 : offset.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            LightningFrameMetadata other = (LightningFrameMetadata) obj;
            if (frameTime == null) {
                if (other.frameTime != null)
                    return false;
            } else if (!frameTime.equals(other.frameTime))
                return false;
            if (offset == null) {
                if (other.offset != null)
                    return false;
            } else if (!offset.equals(other.offset))
                return false;
            return true;
        }

    }

    private static class LightningFrameRetriever implements
            IObjectRetrieverAndDisposer<LightningFrameMetadata, LightningFrame> {

        @Override
        public LightningFrame retrieveObject(LightningFrameMetadata metadata) {
            synchronized (metadata) {
                LightningFrame bundle = new LightningFrame();
                bundle.frameTime = metadata.frameTime;
                bundle.metadata = metadata;
                populateData(metadata, bundle);
                return bundle;
            }
        }

        @Override
        public int getSize(LightningFrame object) {
            int doubleCount = 0;
            if (object != null) {
                synchronized (object) {
                    for (double[] arr : object.posLatLonList) {
                        doubleCount += arr.length;
                    }
                    for (double[] arr : object.negLatLonList) {
                        doubleCount += arr.length;
                    }
                }
            }
            // 8 bytes per double
            return doubleCount * 8;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.core.cache.CacheObject.IObjectRetrieverAndDisposer
         * #disposeObject(java.lang.Object)
         */
        @Override
        public void disposeObject(LightningFrame object) {
            LightningFrameMetadata metadata = object.metadata;
            synchronized (metadata) {
                metadata.newRecords.addAll(metadata.processed);
                metadata.processed.clear();
            }
        }
    }

    private static final LightningFrameRetriever resourceBuilder = new LightningFrameRetriever();

    private Map<DataTime, CacheObject<LightningFrameMetadata, LightningFrame>> cacheObjectMap;

    private DataTime lastPaintedTime;

    private boolean needsUpdate;

    private String resourceName;
    
    private int posAdj;

    private IFont font;

    private List<double[]> currPosList = Collections.emptyList();

    private List<double[]> currNegList = Collections.emptyList();

    private List<double[]> currCloudList = Collections.emptyList();

    private List<double[]> currPulseList = Collections.emptyList();

    public LightningResource(LightningResourceData resourceData,
            LoadProperties loadProperties, int pa) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);

        this.dataTimes = new ArrayList<DataTime>();
        this.cacheObjectMap = new ConcurrentHashMap<DataTime, CacheObject<LightningFrameMetadata, LightningFrame>>();
        this.posAdj = pa;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        cacheObjectMap.clear();

        if (font != null) {
            font.dispose();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return this.resourceName;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#init(com.raytheon.uf
     * .viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        font = target.initializeFont(Font.MONOSPACED, 11,
                new Style[] { Style.BOLD });

        VizApp.runSync(new Runnable() {

            @Override
            public void run() {
                font.setMagnification(getCapability(
                        MagnificationCapability.class).getMagnification()
                        .floatValue());
                font.setScaleFont(false);
            }

        });

        String timeString = "";
        int absTimeInterval = Math.abs(this.resourceData.getBinOffset()
                .getInterval());

        // If a virtual offset is provided, it is aged lightning, so use
        // the virtual offset to provide the "Old" time
        int virtualOffset = this.resourceData.getBinOffset().getVirtualOffset();
        if (virtualOffset != 0) {
            timeString = convertTimeIntervalToString(virtualOffset) + "Old ";
        } else {
            timeString = convertTimeIntervalToString(absTimeInterval);
        }
        if (this.resourceData.isExclusiveForType()) {
            String modifier;
            if (this.resourceData.isHandlingCloudFlashes()) {
                modifier = "Cloud Flash ";
            } else if (this.resourceData.isHandlingNegativeStrikes()) {
                modifier = "Negative ";
            } else if (this.resourceData.isHandlingPositiveStrikes()) {
                modifier = "Positive ";
            } else if (this.resourceData.isHandlingPulses()) {
                modifier = "Pulse ";
            } else {
                /* space to preserve formatting */
                modifier = " ";
            }
            this.resourceName = timeString + modifier;
        } else {
            this.resourceName = timeString;
        }

        HashMap<String, RequestConstraint> metadata = this.resourceData
                .getMetadataMap();
        if (metadata != null && metadata.containsKey(LightningConstants.SOURCE)) {
            this.resourceName += metadata.get(LightningConstants.SOURCE)
                    .getConstraintValue() + " ";
        }

        this.resourceName += "Lightning Plot ";
    }

    private String convertTimeIntervalToString(int time) {
        time = Math.abs(time);
        String timeString = null;
        if (time >= 60 * 60) {
            int hrs = time / (60 * 60);
            timeString = hrs + " Hour ";
        } else if (time < 60 * 60) {
            int mins = time / 60;
            timeString = mins + " Minute ";
        }

        return timeString;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.drawables.IRenderable#paint(com.raytheon.uf.
     * viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (paintProps.getDataTime() == null) {
            // invalid time, can't paint
            return;
        }

        if (!paintProps.getDataTime().equals(this.lastPaintedTime)) {
            needsUpdate = true;
        }

        this.lastPaintedTime = paintProps.getDataTime();

        float magnification = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();
        RGB color = getCapability(ColorableCapability.class).getColor();

        int posCount = 0;
        int negCount = 0;
        int cloudCount = 0;
        int pulseCount = 0;
        
        if (magnification == 0.0) magnification=(float) 0.01;
        
        IExtent extent = paintProps.getView().getExtent();

        CacheObject<LightningFrameMetadata, LightningFrame> cacheObject = cacheObjectMap
                .get(this.lastPaintedTime);

        if (cacheObject != null) {
            synchronized (cacheObject.getMetadata()) {
                LightningFrame bundle = cacheObject.getObjectAsync();
                if (bundle == null) {
                    needsUpdate = true;
                    issueRefresh();
                } else {
                    if (needsUpdate) {
                        needsUpdate = false;
                        if (resourceData.isHandlingNegativeStrikes()) {
                            currNegList = convertToPixel(bundle.negLatLonList);
                        }
                        if (resourceData.isHandlingPositiveStrikes()) {
                            currPosList = convertToPixel(bundle.posLatLonList);
                        }
                        if (resourceData.isHandlingCloudFlashes()) {
                            currCloudList = convertToPixel(bundle.cloudLatLonList);
                        }
                        if (resourceData.isHandlingPulses()) {
                            currPulseList = convertToPixel(bundle.pulseLatLonList);
                        }
                    }

                    if (resourceData.isHandlingPositiveStrikes()) {
                        posCount = drawFilteredPoints(target, magnification,
                                color, PointStyle.CROSS, extent, currPosList);
                    }
                    if (resourceData.isHandlingNegativeStrikes()) {
                        negCount = drawFilteredPoints(target, magnification,
                                color, PointStyle.DASH, extent, currNegList);
                    }
                    if (resourceData.isHandlingCloudFlashes()) {
                        cloudCount = drawFilteredPoints(target, magnification,
                                color, PointStyle.CIRCLE, extent, currCloudList);
                    }
                    if (resourceData.isHandlingPulses()) {
                        pulseCount = drawFilteredPoints(target, magnification,
                                color, PointStyle.PIPE, extent, currPulseList);
                    }
                }
            }
        }

        font.setMagnification(magnification);
        List<DrawableString> strings = new ArrayList<DrawableString>();
        double height = target.getStringsBounds(new DrawableString("Hy", null))
                .getHeight();

        if (this.resourceData.isHandlingPositiveStrikes()) {
            strings.add(createLegendString(color, posCount, " + Strikes",
                    height, 2));
        }
        if (this.resourceData.isHandlingNegativeStrikes()) {
            strings.add(createLegendString(color, negCount, " - Strikes",
                    height, 3));
        }
        if (this.resourceData.isHandlingCloudFlashes()) {
            strings.add(createLegendString(color, cloudCount, " Cloud Flashes",
                    height, 4));
        }
        if (this.resourceData.isHandlingPulses()) {
            strings.add(createLegendString(color, pulseCount, " Pulses",
                    height, 5));
        }
        target.getExtension(ICanvasRenderingExtension.class).drawStrings(
                paintProps, strings.toArray(new DrawableString[0]));

    }

    /**
     * Create lightning legend string in upper left corner of display
     * 
     * @param color
     * @param count
     * @param msg
     * @param height
     * @param verticalOffset
     *            vertical position of legend string (starting at 2)
     * @return
     */
    private DrawableString createLegendString(RGB color, int count, String msg,
            double height, int verticalOffset) {
        DrawableString pos = new DrawableString(count + msg, color);
        pos.setCoordinates(225, height * (verticalOffset + 2 * this.posAdj));
        // jjg above
        pos.font = font;
        pos.verticallAlignment = VerticalAlignment.TOP;
        pos.horizontalAlignment = HorizontalAlignment.RIGHT;
        return pos;
    }

    /**
     * Draw points on target using provided styling. Points are filtered by
     * extent.
     * 
     * @param target
     * @param magnification
     * @param color
     * @param style
     * @param extent
     * @param pixelList
     * @return count of points that matched filter
     * @throws VizException
     */
    private static int drawFilteredPoints(IGraphicsTarget target,
            float magnification, RGB color, PointStyle style, IExtent extent,
            List<double[]> pixelList) throws VizException {
        List<double[]> filtered = new ArrayList<double[]>(pixelList.size());
        for (double[] pxl : pixelList) {
            if (extent.contains(pxl)) {
                filtered.add(pxl);
            }
        }
        target.drawPoints(filtered, color, style, magnification);
        return filtered.size();
    }

    /**
     * convert list of world coordinates to pixel coordinates
     * 
     * @param lonLats
     * @return
     */
    private List<double[]> convertToPixel(List<double[]> lonLats) {
        List<double[]> rval = new ArrayList<double[]>(lonLats.size());
        for (double[] lonLat : lonLats) {
            rval.add(descriptor.worldToPixel(lonLat));
        }
        return rval;
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
        /*
         * Workaround for time matching which does not know about records at the
         * end of a time period that may contain data for the next period. If we
         * are asked to remove the latest data time and there is only one record
         * we know about, return without removing the time.
         */
        if (dataTimes.indexOf(dataTime) == dataTimes.size() - 1) {
            CacheObject<LightningFrameMetadata, LightningFrame> co = cacheObjectMap.get(dataTime);
            if (co != null) {
                LightningFrameMetadata metadata = co.getMetadata();
                synchronized (metadata) {
                    if (metadata.newRecords.size() + metadata.processed.size() < 2) {
                        return;
                    }
                }
            }
        }

        dataTimes.remove(dataTime);
        cacheObjectMap.remove(dataTime);
    }

    protected void addRecords(List<BinLightningRecord> objs) {
        Map<DataTime, List<BinLightningRecord>> recordMap = new HashMap<DataTime, List<BinLightningRecord>>();

        for (BinLightningRecord obj : objs) {
            DataTime time = new DataTime(obj.getStartTime());
            DataTime end = new DataTime(obj.getStopTime());
            time = this.getResourceData().getBinOffset()
                    .getNormalizedTime(time);
            end = this.getResourceData().getBinOffset().getNormalizedTime(end);

            // check for frames in the middle
            // get interval ( in seconds ) between frames
            int interval = this.getResourceData().getBinOffset().getInterval();
            while (end.greaterThan(time) || end.equals(time)) {
                List<BinLightningRecord> records = recordMap.get(time);
                if (records == null) {
                    records = new ArrayList<BinLightningRecord>();
                    recordMap.put(time, records);
                }
                records.add(obj);

                // increment to the next time
                long newTime = time.getRefTime().getTime() + (interval * 1000);
                TimeRange range = new TimeRange(newTime, newTime);
                time = new DataTime(newTime, range);
            }
        }

        for (Map.Entry<DataTime, List<BinLightningRecord>> entry : recordMap
                .entrySet()) {
            DataTime dt = entry.getKey();
            if (dt.equals(lastPaintedTime)) {
                needsUpdate = true;
            }

            List<BinLightningRecord> records = entry.getValue();

            LightningFrameMetadata frame;
            CacheObject<LightningFrameMetadata, LightningFrame> co;
            synchronized (cacheObjectMap) {
                co = cacheObjectMap.get(dt);
                if (co == null) {
                    // New frame
                    LightningFrameMetadata key = new LightningFrameMetadata(dt,
                            resourceData.getBinOffset());
                    co = CacheObject.newCacheObject(key, resourceBuilder);
                    cacheObjectMap.put(dt, co);
                    dataTimes.add(dt);
                }
            }
            frame = co.getMetadata();

            synchronized (frame) {
                // Add as new records
                for (BinLightningRecord record : records) {
                    if (frame.newRecords.contains(record) == false
                            && frame.processed.contains(record) == false) {
                        frame.newRecords.add(record);
                    }
                }

                if (frame.processed.size() > 0 && frame.newRecords.size() > 0) {
                    // if we've already processed some records, request the
                    // new ones now and merge
                    LightningFrame existing = co.getObjectSync();
                    LightningFrame newBundle = resourceBuilder
                            .retrieveObject(frame);
                    existing.posLatLonList.addAll(newBundle.posLatLonList);
                    existing.negLatLonList.addAll(newBundle.negLatLonList);
                }
            }
        }
        issueRefresh();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            List<BinLightningRecord> records = new ArrayList<BinLightningRecord>(
                    pdo.length);
            for (PluginDataObject p : pdo) {
                if (p instanceof BinLightningRecord) {
                    records.add((BinLightningRecord) p);
                }
            }
            addRecords(records);
        } else if (type == ChangeType.CAPABILITY) {
            if (object instanceof DensityCapability
                    || object instanceof MagnificationCapability) {
                this.needsUpdate = true;
            }
        }

        issueRefresh();
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        // force a repaint
        needsUpdate = true;
        issueRefresh();
    }

    private static void populateData(LightningFrameMetadata frame,
            LightningFrame bundle) {
        long t0 = System.currentTimeMillis();
        long strikeCount = 0;
        long dsTime = 0;

        // Bin up requests to the same hdf5
        Map<File, List<BinLightningRecord>> fileMap = new HashMap<File, List<BinLightningRecord>>();

        for (BinLightningRecord record : frame.newRecords) {
            File f = HDF5Util.findHDF5Location(record);
            List<BinLightningRecord> recList = fileMap.get(f);
            if (recList == null) {
                recList = new ArrayList<BinLightningRecord>();
                fileMap.put(f, recList);
            }
            recList.add(record);
            frame.processed.add(record);
        }
        frame.newRecords.clear();

        for (File f : fileMap.keySet()) {
            List<BinLightningRecord> recList = fileMap.get(f);
            String[] groups = new String[recList.size()];
            for (int i = 0; i < recList.size(); i++) {
                groups[i] = recList.get(i).getDataURI();
            }

            // Go fetch data
            try {
                long tDS0 = System.currentTimeMillis();
                IDataStore ds = DataStoreFactory.getDataStore(f);
                IDataRecord[] records = ds.retrieveGroups(groups, Request.ALL);

                long tDS1 = System.currentTimeMillis();
                dsTime += (tDS1 - tDS0);
                // Throw in a map for easy accessibility
                Map<String, List<IDataRecord>> recordMap = createRecordMap(records);

                List<IDataRecord> times = recordMap
                        .get(LightningConstants.TIME_DATASET);
                List<IDataRecord> intensities = recordMap
                        .get(LightningConstants.INTENSITY_DATASET);
                List<IDataRecord> lats = recordMap
                        .get(LightningConstants.LAT_DATASET);
                List<IDataRecord> lons = recordMap
                        .get(LightningConstants.LON_DATASET);
                List<IDataRecord> types = recordMap
                        .get(LightningConstants.STRIKE_TYPE_DATASET);
                List<IDataRecord> pulseIndexes = recordMap
                        .get(LightningConstants.PULSE_INDEX_DATASET);

                int k = 0;
                for (IDataRecord timeRec : times) {
                    if (hasPulseData(pulseIndexes, k)) {
                        populatePulseData(frame, bundle, timeRec.getGroup(), ds);
                    }
                    LongDataRecord time = (LongDataRecord) timeRec;
                    // Now loop through the obs times and intensities and
                    // start categorizing strikes
                    int numRecords = (int) time.getSizes()[0];

                    long[] timeData = time.getLongData();

                    int[] intensityData = ((IntegerDataRecord) intensities
                            .get(k)).getIntData();
                    float[] latitudeData = ((FloatDataRecord) lats.get(k))
                            .getFloatData();
                    float[] longitudeData = ((FloatDataRecord) lons.get(k))
                            .getFloatData();
                    byte[] typeData = ((ByteDataRecord) types.get(k))
                            .getByteData();

                    for (int i = 0; i < numRecords; i++) {

                        DataTime dt = new DataTime(new Date(timeData[i]));
                        dt = frame.offset.getNormalizedTime(dt);
                        List<double[]> list;
                        LtgStrikeType type = LtgStrikeType.getById(typeData[i]);
                        switch(type){
                        case CLOUD_TO_CLOUD:
                            list = bundle.cloudLatLonList;
                            break;
                        default:
                            if (intensityData[i] > 0) {
                                list = bundle.posLatLonList;
                            } else {
                                list = bundle.negLatLonList;
                            }
                            break;
                        }

                        double[] latLon = new double[] { longitudeData[i],
                                latitudeData[i] };

                        // only add the strike to the list if the data time
                        // of the strike matches the data time of the frame
                        if (dt.equals(bundle.frameTime)) {
                            list.add(latLon);
                            strikeCount++;
                        }

                    }
                    k++;
                }
            } catch (StorageException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Storage error retrieving lightning data", e);
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to open lightning file", e);
            }

        }

        long t1 = System.currentTimeMillis();

        System.out.println("Decoded: " + strikeCount + " strikes in "
                + (t1 - t0) + "ms (hdf5 time = " + dsTime + "ms)");
    }

    /**
     * Unpack records into map keyed by record name
     * 
     * @param records
     * @return
     */
    private static Map<String, List<IDataRecord>> createRecordMap(
            IDataRecord[] records) {
        Map<String, List<IDataRecord>> recordMap = new HashMap<String, List<IDataRecord>>();
        for (IDataRecord rec : records) {
            List<IDataRecord> recordList = recordMap.get(rec.getName());
            if (recordList == null) {
                recordList = new ArrayList<IDataRecord>();
                recordMap.put(rec.getName(), recordList);
            }
            recordList.add(rec);
        }
        return recordMap;
    }

    /**
     * Search records and return first found with name
     * 
     * @param records
     * @param name
     * @return null if none found
     */
    private static IDataRecord findDataRecord(IDataRecord[] records, String name) {
        IDataRecord rval = null;
        for (IDataRecord record : records) {
            if (record.getName().equals(name)) {
                rval = record;
                break;
            }
        }
        return rval;
    }

    /**
     * @param pulseIndexes
     * @param recordIndex
     * @return true if any data record in list has a valid pulse index
     */
    private static boolean hasPulseData(List<IDataRecord> pulseIndexes,
            int recordIndex) {
        if (pulseIndexes != null) {
            IDataRecord record = pulseIndexes.get(recordIndex);
            int[] indexData = ((IntegerDataRecord) record).getIntData();
            for (int i = 0; i < indexData.length; ++i) {
                if (indexData[i] >= 0) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Read pulse data from datastore and populate in frame
     * 
     * @param frame
     * @param bundle
     * @param group
     * @param ds
     */
    private static void populatePulseData(LightningFrameMetadata frame,
            LightningFrame bundle, String group, IDataStore ds) {
        try {
            IDataRecord[] records = ds.retrieve(group + DataURI.SEPARATOR
                    + LightningConstants.PULSE_HDF5_GROUP_SUFFIX);
            FloatDataRecord latRecord = (FloatDataRecord) findDataRecord(
                    records, LightningConstants.LAT_DATASET);
            FloatDataRecord lonRecord = (FloatDataRecord) findDataRecord(
                    records, LightningConstants.LON_DATASET);
            if (latRecord == null || lonRecord == null) {
                throw new StorageException(
                        "Missing pulse latitude and/or longitude data", null);
            }
            float[] lats = latRecord.getFloatData();
            float[] lons = lonRecord.getFloatData();
            if (lats.length != lons.length) {
                throw new StorageException(
                        "Mismatched pulse latitude/longitude data", latRecord);
            }
            for (int i = 0; i < lats.length; ++i) {
                bundle.pulseLatLonList.add(new double[] { lons[i], lats[i] });
            }
        } catch (FileNotFoundException e) {
            statusHandler.error("Unable to open lightning file", e);
        } catch (StorageException e) {
            statusHandler.error("Unable to read pulse datasets for group "
                    + group, e);
        }
    }

}
