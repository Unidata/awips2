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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.binlightning.BinLightningRecord;
import com.raytheon.uf.common.dataplugin.binlightning.LightningConstants;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.PointStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.cache.CacheObject;
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
import com.raytheon.viz.lightning.cache.LightningFrame;
import com.raytheon.viz.lightning.cache.LightningFrameMetadata;
import com.raytheon.viz.lightning.cache.LightningFrameRetriever;

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
 *    Jul 10, 2014  3333       bclement    moved cache object inner classes to own package
 *                                          moved name formatting to static method
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class LightningResource extends
        AbstractVizResource<LightningResourceData, IMapDescriptor> implements
        IResourceDataChanged {

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
        int absTimeInterval = Math.abs(resourceData.getBinOffset()
                .getInterval());

        // If a virtual offset is provided, it is aged lightning, so use
        // the virtual offset to provide the "Old" time
        int virtualOffset = resourceData.getBinOffset().getVirtualOffset();
        if (virtualOffset != 0) {
            rval = convertTimeIntervalToString(virtualOffset) + "Old ";
        } else {
            rval = convertTimeIntervalToString(absTimeInterval);
        }
        if (resourceData.isExclusiveForType()) {
            String modifier;
            if (resourceData.isHandlingCloudFlashes()) {
                modifier = "Cloud Flash ";
            } else if (resourceData.isHandlingNegativeStrikes()) {
                modifier = "Negative ";
            } else if (resourceData.isHandlingPositiveStrikes()) {
                modifier = "Positive ";
            } else if (resourceData.isHandlingPulses()) {
                modifier = "Pulse ";
            } else {
                /* space to preserve formatting */
                modifier = " ";
            }
            rval += modifier;
        }

        HashMap<String, RequestConstraint> metadata = resourceData
                .getMetadataMap();
        if (metadata != null && metadata.containsKey(LightningConstants.SOURCE)) {
            rval += metadata.get(LightningConstants.SOURCE)
                    .getConstraintValue() + " ";
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
                    if (metadata.getNewRecords().size()
                            + metadata.getProcessed().size() < 2) {
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

            LightningFrameRetriever retriever = LightningFrameRetriever
                    .getInstance();
            CacheObject<LightningFrameMetadata, LightningFrame> co;
            synchronized (cacheObjectMap) {
                co = cacheObjectMap.get(dt);
                if (co == null) {
                    /*
                     * no local reference to cache object, create key and get
                     * cache object which may be new or from another resource
                     */
                    LightningFrameMetadata key = new LightningFrameMetadata(dt,
                            resourceData.getBinOffset());
                    co = CacheObject.newCacheObject(key, retriever);
                    cacheObjectMap.put(dt, co);
                    dataTimes.add(dt);
                }
            }

            retriever.updateAndGet(records, co);
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

}
