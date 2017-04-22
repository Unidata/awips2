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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.cache.CacheObject;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.point.IBulkPointsRenderingExtension;
import com.raytheon.uf.viz.core.drawables.ext.point.IBulkPointsRenderingExtension.IBulkPoints;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.DensityCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.extratext.ExtraTextResourceData;
import com.raytheon.uf.viz.core.rsc.extratext.IExtraTextGeneratingResource;
import com.raytheon.viz.lightning.LightningResourceData.DisplayType;
import com.raytheon.viz.lightning.cache.LightningFrame;
import com.raytheon.viz.lightning.cache.LightningFrameMetadata;
import com.raytheon.viz.lightning.cache.LightningFrameRetriever;

/**
 * Resource for rendering lightning. For each specific data point a symbol is
 * rendered to indicate the location and type of lightning event.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer     Description
 * ------------- -------- ------------ -----------------------------------------
 * Aug 06, 2007  451      chammack     Initial Creation.
 * Feb 18, 2009  1959     chammack     Refactored to new resource model
 * Jun 22, 2010  4286     bkowal       Add support for magnification capability
 * Sep 04, 2012  15335    kshresth     Will now display lightning/wind fields
 *                                     when magnification set to 0
 * Feb 27, 2013  DCS 152  jgerth/elau  Support for WWLLN and multiple sources
 * Jan 21, 2014  2667     bclement     renamed record's lightSource field to
 *                                     source
 * Jun 05, 2014  3226     bclement     reference datarecords by
 *                                     LightningConstants
 * Jun 06, 2014  17367    D. Friedman  Fix cache object usage.
 * Jun 19, 2014  3214     bclement     added pulse and cloud flash support
 * Jul 07, 2014  3333     bclement     removed lightSource field
 * Jul 10, 2014  3333     bclement     moved cache object inner classes to own
 *                                     package moved name formatting to static
 *                                     method
 * Aug 04, 2014  3488     bclement     added sanity check for record bin range
 * Aug 19, 2014  3542     bclement     fixed strike count clipping issue
 * Mar 05, 2015  4233     bsteffen     include source in cache key.
 * Apr 09, 2015  4386     bclement     added updateLightningFrames()
 * Jul 01, 2015  4592     bclement     cloud flashes are now points instead of
 *                                     circles
 * Jul 01, 2015  4597     bclement     reworked resource name using DisplayType
 * Sep 10, 2015  4856     njensen      synchronize in remove(DataTime)
 * Sep 25, 2015  4605     bsteffen     repeat binning
 * Nov 05, 2015  5070     randerso     Adjust font sizes for dpi scaling
 * Apr 26, 2016  5597     bsteffen     Include update interval in legend.
 * May 19, 2016  3253     bsteffen     Use extra text for count display.
 * Jul 26, 2016  5759     njensen      Use IBulkPointsRenderingExtension
 * 
 * </pre>
 * 
 * @author chammack
 */
public class LightningResource extends
        AbstractVizResource<LightningResourceData, IMapDescriptor> implements
        IResourceDataChanged, IExtraTextGeneratingResource {

    private static final long MAX_RECORD_BIN_MILLIS = TimeUtil.MILLIS_PER_DAY;

    private Map<DataTime, CacheObject<LightningFrameMetadata, LightningFrame>> cacheObjectMap;

    private DataTime lastPaintedTime;

    private boolean needsUpdate;

    private String resourceName;

    private IFont font;

    private List<double[]> currPosList = Collections.emptyList();

    private List<double[]> currNegList = Collections.emptyList();

    private List<double[]> currCloudList = Collections.emptyList();

    private List<double[]> currPulseList = Collections.emptyList();

    /**
     * Set the number of elements of each type that are painted.
     */
    private Map<DisplayType, Integer> currCounts = Collections.emptyMap();

    /**
     * Map of DataTime (frame time) to points, organized by DisplayType
     */
    private Map<DataTime, Map<DisplayType, IBulkPoints>> timePointsMap = new HashMap<>();

    public LightningResource(LightningResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);

        this.dataTimes = new ArrayList<>();
        this.cacheObjectMap = new ConcurrentHashMap<>();
    }

    @Override
    protected void disposeInternal() {
        cacheObjectMap.clear();
        synchronized (timePointsMap) {
            for (DataTime time : timePointsMap.keySet()) {
                disposeBulkPoints(time, null);
            }
            timePointsMap.clear();
        }

        if (font != null) {
            font.dispose();
        }
    }

    @Override
    public String getName() {
        return this.resourceName;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        ExtraTextResourceData.addExtraTextResource(descriptor);

        font = target.initializeFont(Font.MONOSPACED, 9,
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
        this.resourceName = formatResourceName(resourceData)
                + "Lightning Plot ";
    }

    /**
     * Create a resource name string from resource data configuration. Includes
     * time interval, cloud flash, pos/neg, pulse and source name.
     * 
     * @param resourceData
     * @return
     */
    public static String formatResourceName(LightningResourceData resourceData) {
        String rval = "";
        int absTimeInterval = Math.abs(resourceData.getRepeatingBinOffset()
                .getInterval());

        int updateInterval = Math
                .abs(resourceData.getBinOffset().getInterval());

        // If a virtual offset is provided, it is aged lightning, so use
        // the virtual offset to provide the "Old" time
        int virtualOffset = resourceData.getBinOffset().getVirtualOffset();
        if (virtualOffset != 0) {
            rval = convertTimeIntervalToString(virtualOffset) + "Old ";
        } else {
            rval = convertTimeIntervalToString(absTimeInterval);
        }
        DisplayType displayType = resourceData.getDisplayType();
        if (!displayType.equals(DisplayType.UNDEFINED)) {
            rval += displayType.label + ' ';
        }

        String source = resourceData.getSource();
        if (source != null) {
            rval += source + ' ';
        }
        if (updateInterval != absTimeInterval) {
            rval += convertTimeIntervalToString(updateInterval) + "Update ";
        }
        return rval;
    }

    /**
     * Format time interval to human readable display string
     * 
     * @param time
     *            in seconds
     * @return
     */
    private static String convertTimeIntervalToString(int time) {
        time = Math.abs(time);
        String timeString = null;
        if (time >= TimeUtil.SECONDS_PER_HOUR) {
            int hrs = time / (TimeUtil.SECONDS_PER_HOUR);
            timeString = hrs + " Hour ";
        } else {
            int mins = time / TimeUtil.SECONDS_PER_MINUTE;
            timeString = mins + " Minute ";
        }

        return timeString;
    }

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

        DataTime paintTime = paintProps.getDataTime();
        this.lastPaintedTime = paintTime;

        float magnification = getCapability(MagnificationCapability.class)
                .getMagnification().floatValue();
        RGB color = getCapability(ColorableCapability.class).getColor();

        int posCount = 0;
        int negCount = 0;
        int cloudCount = 0;
        int pulseCount = 0;

        if (magnification == 0.0) {
            magnification = (float) 0.01;
        }

        /*
         * we only want strikes that are visible so we have to filter any
         * strikes that aren't in both the clipping pane and the view
         */
        IExtent viewExtent = paintProps.getView().getExtent();
        IExtent clipExtent = paintProps.getClippingPane();
        IExtent screenExtent = viewExtent.intersection(clipExtent);

        CacheObject<LightningFrameMetadata, LightningFrame> cacheObject = cacheObjectMap
                .get(paintTime);

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
                            currNegList = convertToPixel(bundle
                                    .getNegLatLonList());
                        }
                        if (resourceData.isHandlingPositiveStrikes()) {
                            currPosList = convertToPixel(bundle
                                    .getPosLatLonList());
                        }
                        if (resourceData.isHandlingCloudFlashes()) {
                            currCloudList = convertToPixel(bundle
                                    .getCloudLatLonList());
                        }
                        if (resourceData.isHandlingPulses()) {
                            currPulseList = convertToPixel(bundle
                                    .getPulseLatLonList());
                        }
                    }

                    if (resourceData.isHandlingPositiveStrikes()) {
                        posCount = drawPoints(target, paintTime, magnification,
                                color, DisplayType.POSITIVE, screenExtent,
                                clipExtent, currPosList);
                    }
                    if (resourceData.isHandlingNegativeStrikes()) {
                        negCount = drawPoints(target, paintTime, magnification,
                                color, DisplayType.NEGATIVE, screenExtent,
                                clipExtent, currNegList);
                    }
                    if (resourceData.isHandlingCloudFlashes()) {
                        cloudCount = drawPoints(target, paintTime,
                                magnification, color, DisplayType.CLOUD_FLASH,
                                screenExtent, clipExtent, currCloudList);
                    }
                    if (resourceData.isHandlingPulses()) {
                        pulseCount = drawPoints(target, paintTime,
                                magnification, color, DisplayType.PULSE,
                                screenExtent, clipExtent, currPulseList);
                    }
                }
            }
        }

        font.setMagnification(magnification);

        Map<DisplayType, Integer> counts = new EnumMap<>(DisplayType.class);

        if (this.resourceData.isHandlingPositiveStrikes()) {
            counts.put(DisplayType.POSITIVE, posCount);
        }
        if (this.resourceData.isHandlingNegativeStrikes()) {
            counts.put(DisplayType.NEGATIVE, negCount);

        }
        if (this.resourceData.isHandlingCloudFlashes()) {
            counts.put(DisplayType.CLOUD_FLASH, cloudCount);

        }
        if (this.resourceData.isHandlingPulses()) {
            counts.put(DisplayType.PULSE, pulseCount);
        }

        this.currCounts = counts;

    }

    /**
     * Draw points on target using provided styling. Points are filtered by the
     * clipping pane's extent for efficiency, and filtered by the screen's
     * extent to produce the count of points currently displayed.
     * 
     * @param target
     * @param time
     * @param magnification
     * @param color
     * @param style
     * @param screenExtent
     * @param clipExtent
     * @param pixelList
     * @return count of points that were within the extent
     * @throws VizException
     */
    private int drawPoints(IGraphicsTarget target, DataTime time,
            float magnification, RGB color, DisplayType type,
            IExtent screenExtent, IExtent clipExtent, List<double[]> pixelList)
            throws VizException {
        List<double[]> points = new ArrayList<>(pixelList.size());
        int displayedCount = 0;
        for (double[] pxl : pixelList) {
            if (screenExtent.contains(pxl)) {
                displayedCount++;
            }
            if (clipExtent.contains(pxl)) {
                points.add(pxl);
            }
        }

        IBulkPointsRenderingExtension ext = target
                .getExtension(IBulkPointsRenderingExtension.class);
        IBulkPoints bulkPoints = null;
        synchronized (timePointsMap) {
            Map<DisplayType, IBulkPoints> bulkMap = timePointsMap.get(time);
            if (bulkMap == null) {
                bulkMap = new HashMap<>();
                timePointsMap.put(time, bulkMap);
            }
            bulkPoints = bulkMap.get(type);
            if (bulkPoints == null || (bulkPoints.size() != points.size())) {
                // haven't handled this frame or it got an update
                if (bulkPoints != null) {
                    bulkPoints.dispose();
                }
                bulkPoints = ext.createBulkPoints(getPointStyle(type));
                bulkPoints.addPoints(points);
                bulkMap.put(type, bulkPoints);
            }
        }
        ext.drawBulkPoints(bulkPoints, color, magnification);

        return displayedCount;
    }

    /**
     * Gets the drawing PointStyle that should be used for a particular
     * DisplayType.
     * 
     * @param type
     * @return
     */
    private static PointStyle getPointStyle(DisplayType type) {
        switch (type) {
        case POSITIVE:
            return PointStyle.CROSS;
        case NEGATIVE:
            return PointStyle.DASH;
        case CLOUD_FLASH:
            return PointStyle.POINT;
        case PULSE:
            return PointStyle.PIPE;
        default:
            throw new IllegalArgumentException(
                    "PointStyle not supported for Display Type: " + type.label);
        }
    }

    /**
     * Disposes of an IBulkPoints from the timePointsMap at a specified time and
     * display type. If type is null, it will dispose of all IBulkPoints at the
     * specified time.
     * 
     * This method assumes the timePointsMap is externally synchronized before
     * being called.
     * 
     * @param time
     *            the DataTime to dispose an IBulkPoints for
     * @param type
     *            the display type to dispose IBulkPoints for at the specified
     *            time, or null to dispose of all IBulkPoints at the specified
     *            time
     */
    private void disposeBulkPoints(DataTime time, DisplayType type) {
        Map<DisplayType, IBulkPoints> displayTypeMap = timePointsMap.get(time);
        if (displayTypeMap != null) {
            if (type == null) {
                // dispose of them all at this time
                for (DisplayType dType : displayTypeMap.keySet()) {
                    IBulkPoints bulkPoints = displayTypeMap.get(dType);
                    if (bulkPoints != null) {
                        bulkPoints.dispose();
                    }
                }
                displayTypeMap.clear();
            } else {
                // dispose only the specified DisplayType's IBulkPoints
                IBulkPoints bulkPoints = displayTypeMap.remove(type);
                if (bulkPoints != null) {
                    bulkPoints.dispose();
                }
            }
        }
    }

    /**
     * convert list of world coordinates to pixel coordinates
     * 
     * @param lonLats
     * @return
     */
    private List<double[]> convertToPixel(List<double[]> lonLats) {
        List<double[]> rval = new ArrayList<>(lonLats.size());
        for (double[] lonLat : lonLats) {
            rval.add(descriptor.worldToPixel(lonLat));
        }
        return rval;
    }

    @Override
    public void remove(DataTime dataTime) {
        synchronized (cacheObjectMap) {
            /*
             * Workaround for time matching which does not know about records at
             * the end of a time period that may contain data for the next
             * period. If we are asked to remove the latest data time and there
             * is only one record we know about, return without removing the
             * time.
             */
            if (dataTimes.indexOf(dataTime) == dataTimes.size() - 1) {
                CacheObject<LightningFrameMetadata, LightningFrame> co = cacheObjectMap
                        .get(dataTime);
                if (co != null) {
                    LightningFrameMetadata metadata = co.getMetadata();
                    synchronized (metadata) {
                        if (metadata.getNewRecords().size()
                                + metadata.getProcessed().size() < 2) {
                            return;
                        }
                    }
                }
            }

            dataTimes.remove(dataTime);
            cacheObjectMap.remove(dataTime);
            synchronized (timePointsMap) {
                disposeBulkPoints(dataTime, null);
                timePointsMap.remove(dataTime);
            }
        }
    }

    protected void addRecords(List<BinLightningRecord> objs) {
        final Map<DataTime, List<BinLightningRecord>> recordMap = new HashMap<>();

        for (BinLightningRecord obj : objs) {
            TimeRange validPeriod = obj.getDataTime().getValidPeriod();
            if (validPeriod.getDuration() > MAX_RECORD_BIN_MILLIS) {
                statusHandler.error("Record bin time larger than maximum "
                        + "supported period. Skipping record: " + obj);
                continue;
            }
            Collection<DataTime> times = resourceData.getRepeatingBinOffset()
                    .getNormalizedTimes(validPeriod);
            for (DataTime time : times) {
                List<BinLightningRecord> records = recordMap.get(time);
                if (records == null) {
                    records = new ArrayList<>();
                    recordMap.put(time, records);
                }
                records.add(obj);
            }
        }

        Job job = new Job("Update Lightning Frames") {
            @Override
            protected IStatus run(IProgressMonitor monitor) {
                updateLightningFrames(recordMap);
                return Status.OK_STATUS;
            }
        };
        job.setSystem(true);
        job.schedule();

    }

    /**
     * Update lightning frames with data from record map. Must be ran as part of
     * an asynchronous job.
     * 
     * @param recordMap
     */
    private void updateLightningFrames(
            final Map<DataTime, List<BinLightningRecord>> recordMap) {
        for (Map.Entry<DataTime, List<BinLightningRecord>> entry : recordMap
                .entrySet()) {
            DataTime dt = entry.getKey();
            if (dt.equals(lastPaintedTime)) {
                needsUpdate = true;
            }

            List<BinLightningRecord> records = entry.getValue();
            LightningFrameRetriever retriever = LightningFrameRetriever
                    .getInstance();
            CacheObject<LightningFrameMetadata, LightningFrame> co = getCachedFrame(
                    dt, retriever);
            retriever.updateAndGet(records, co);
        }

        issueRefresh();
    }

    /**
     * Get lightning frame from cache, creates a new cached object if none found
     * for time
     * 
     * @param dt
     * @param retriever
     * @return
     */
    private CacheObject<LightningFrameMetadata, LightningFrame> getCachedFrame(
            DataTime dt, LightningFrameRetriever retriever) {
        CacheObject<LightningFrameMetadata, LightningFrame> co;
        synchronized (cacheObjectMap) {
            co = cacheObjectMap.get(dt);
            if (co == null) {
                /*
                 * no local reference to cache object, create key and get cache
                 * object which may be new or from another resource
                 */
                LightningFrameMetadata key = new LightningFrameMetadata(
                        resourceData.getSource(), dt,
                        resourceData.getRepeatingBinOffset());
                co = CacheObject.newCacheObject(key, retriever);
                cacheObjectMap.put(dt, co);
                dataTimes.add(dt);
            }
        }
        return co;
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            List<BinLightningRecord> records = new ArrayList<>(pdo.length);
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
        // need to get brand new bulk points for new projection
        synchronized (timePointsMap) {
            for (DataTime time : timePointsMap.keySet()) {
                disposeBulkPoints(time, null);
            }
            timePointsMap.clear();
        }

        // force a repaint
        needsUpdate = true;
        issueRefresh();
    }

    @Override
    public String[] getExtraText(DataTime time) {
        List<String> text = new ArrayList<>(4);
        if (currCounts.containsKey(DisplayType.POSITIVE)) {
            text.add(currCounts.get(DisplayType.POSITIVE) + " + Strikes");
        }
        if (currCounts.containsKey(DisplayType.NEGATIVE)) {
            text.add(currCounts.get(DisplayType.NEGATIVE) + " - Strikes");
        }
        if (currCounts.containsKey(DisplayType.CLOUD_FLASH)) {
            text.add(currCounts.get(DisplayType.CLOUD_FLASH) + " Cloud Flashes");
        }
        if (currCounts.containsKey(DisplayType.PULSE)) {
            text.add(currCounts.get(DisplayType.PULSE) + " Pulses");
        }
        return text.toArray(new String[0]);
    }

}
