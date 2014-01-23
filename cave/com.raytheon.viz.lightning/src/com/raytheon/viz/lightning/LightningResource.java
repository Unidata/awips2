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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
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
import com.raytheon.uf.viz.core.HDF5Util;
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
    }

    private static class LightningFrameMetadata {

        private BinOffset offset;

        private DataTime frameTime;
        
        private String lightSource;
        
        private List<BinLightningRecord> newRecords = new ArrayList<BinLightningRecord>();

        private List<BinLightningRecord> processed = new ArrayList<BinLightningRecord>();

        public LightningFrameMetadata(DataTime frameTime, BinOffset offset, String ls) {
            this.frameTime = frameTime;
            this.offset = offset;
            this.lightSource = ls;
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
            if (lightSource == null) {
                if (other.lightSource != null)
                    return false;
            } else if (!lightSource.equals(other.lightSource))
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
    
    private String lightSource;
    
    private int posAdj;

    private IFont font;

    private List<double[]> currPosList = null;

    private List<double[]> currNegList = null;

    public LightningResource(LightningResourceData resourceData,
            LoadProperties loadProperties, String ls, int pa) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);

        this.dataTimes = new ArrayList<DataTime>();
        this.cacheObjectMap = new ConcurrentHashMap<DataTime, CacheObject<LightningFrameMetadata, LightningFrame>>();
        this.lightSource = ls;
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

        if (!this.resourceData.isHandlingNegativeStrikes()) {
            this.resourceName = timeString + "Positive";
        } else if (!this.resourceData.isHandlingPositiveStrikes()) {
            this.resourceName = timeString + "Negative";
        } else {
            this.resourceName = timeString;
        }

        String lightType = " ";
        if (!this.lightSource.isEmpty()) {
        	lightType += this.lightSource + " ";
        }
        this.resourceName += lightType + "Lightning Plot ";
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
                        currNegList = new ArrayList<double[]>(
                                bundle.posLatLonList.size());
                        currPosList = new ArrayList<double[]>(
                                bundle.negLatLonList.size());

                        if (resourceData.isHandlingPositiveStrikes()) {
                            for (double[] pos : bundle.posLatLonList) {
                                currPosList.add(descriptor.worldToPixel(pos));
                            }
                        }
                        if (resourceData.isHandlingNegativeStrikes()) {
                            for (double[] neg : bundle.negLatLonList) {
                                currNegList.add(descriptor.worldToPixel(neg));
                            }
                        }
                    }

                    if (resourceData.isHandlingPositiveStrikes()) {
                        List<double[]> positive = new ArrayList<double[]>(
                                currPosList.size());
                        for (double[] pos : currPosList) {
                            if (extent.contains(pos)) {
                                positive.add(pos);
                            }
                        }
                        posCount = positive.size();

                        target.drawPoints(positive, color, PointStyle.CROSS,
                                magnification);
                    }

                    if (resourceData.isHandlingNegativeStrikes()) {
                        List<double[]> negative = new ArrayList<double[]>(
                                currPosList.size());
                        for (double[] neg : currNegList) {
                            if (extent.contains(neg)) {
                                negative.add(neg);
                            }
                        }
                        negCount = negative.size();

                        target.drawPoints(negative, color, PointStyle.DASH,
                                magnification);
                    }
                }
            }
        }

        font.setMagnification(magnification);
        List<DrawableString> strings = new ArrayList<DrawableString>();
        double height = target.getStringsBounds(new DrawableString("Hy", null))
                .getHeight();

        if (this.resourceData.isHandlingPositiveStrikes()) {
            DrawableString pos = new DrawableString(posCount + " + Strikes",
                    color);
            pos.setCoordinates(225, height * (2 + 2*this.posAdj));
            // jjg above
            pos.font = font;
            pos.verticallAlignment = VerticalAlignment.TOP;
            pos.horizontalAlignment = HorizontalAlignment.RIGHT;
            strings.add(pos);
        }

        if (this.resourceData.isHandlingNegativeStrikes()) {
            DrawableString neg = new DrawableString(negCount + " - Strikes",
                    color);
            neg.setCoordinates(225, height * (3 + 2*this.posAdj));
            // jjg above
            neg.font = font;
            neg.verticallAlignment = VerticalAlignment.TOP;
            neg.horizontalAlignment = HorizontalAlignment.RIGHT;
            strings.add(neg);
        }

        target.getExtension(ICanvasRenderingExtension.class).drawStrings(
                paintProps, strings.toArray(new DrawableString[0]));

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
        dataTimes.remove(dataTime);
        cacheObjectMap.remove(dataTime);
    }

    protected void addRecords(List<BinLightningRecord> objs) {
        Map<DataTime, List<BinLightningRecord>> recordMap = new HashMap<DataTime, List<BinLightningRecord>>();

        for (BinLightningRecord obj : objs) {
        	if (obj.getSource().equals(this.lightSource) || this.lightSource.isEmpty()) {
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
        }

        for (Map.Entry<DataTime, List<BinLightningRecord>> entry : recordMap
                .entrySet()) {
            DataTime dt = entry.getKey();
            if (dt.equals(lastPaintedTime)) {
                needsUpdate = true;
            }

            List<BinLightningRecord> records = entry.getValue();

            CacheObject<LightningFrameMetadata, LightningFrame> co = cacheObjectMap
                    .get(dt);
            LightningFrameMetadata frame;
            if (co == null) {
                // New frame
                frame = new LightningFrameMetadata(dt,
                        resourceData.getBinOffset(), this.lightSource);
                co = CacheObject.newCacheObject(frame, resourceBuilder);
                cacheObjectMap.put(dt, co);
                dataTimes.add(dt);
            } else {
                // Frame exists
                frame = co.getMetadata();
            }

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
                Map<String, List<IDataRecord>> recordMap = new HashMap<String, List<IDataRecord>>();
                // Throw in a map for easy accessibility
                for (IDataRecord rec : records) {
                    List<IDataRecord> recordList = recordMap.get(rec.getName());
                    if (recordList == null) {
                        recordList = new ArrayList<IDataRecord>();
                        recordMap.put(rec.getName(), recordList);
                    }
                    recordList.add(rec);
                }

                List<IDataRecord> times = recordMap.get("obsTime");

                int k = 0;
                for (IDataRecord timeRec : times) {
                    LongDataRecord time = (LongDataRecord) timeRec;
                    // Now loop through the obs times and intensities and
                    // start categorizing strikes
                    int numRecords = (int) time.getSizes()[0];

                    long[] timeData = time.getLongData();
                    int[] intensityData = ((IntegerDataRecord) recordMap.get(
                            "intensity").get(k)).getIntData();
                    float[] latitudeData = ((FloatDataRecord) recordMap.get(
                            "latitude").get(k)).getFloatData();
                    float[] longitudeData = ((FloatDataRecord) recordMap.get(
                            "longitude").get(k)).getFloatData();

                    for (int i = 0; i < numRecords; i++) {

                        DataTime dt = new DataTime(new Date(timeData[i]));
                        dt = frame.offset.getNormalizedTime(dt);
                        List<double[]> list;
                        if (intensityData[i] > 0) {
                            list = bundle.posLatLonList;
                        } else {
                            list = bundle.negLatLonList;
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

}
