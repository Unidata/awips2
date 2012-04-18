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

package com.raytheon.viz.radar.rsc.mosaic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.measure.unit.UnitFormat;

import org.eclipse.swt.graphics.RGB;
import org.geotools.geometry.DirectPosition2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IRefreshListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.rsc.BestResResource;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.rsc.AbstractRadarResource;
import com.raytheon.viz.radar.rsc.MosaicPaintProperties;
import com.raytheon.viz.radar.rsc.RadarImageResource;
import com.raytheon.viz.radar.rsc.RadarTextResource.IRadarTextGeneratingResource;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicRendererFactoryExtension;
import com.raytheon.viz.radar.rsc.mosaic.ext.IRadarMosaicRendererFactoryExtension.IRadarMosaicRenderer;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Implements Radar Mosaic display
 * 
 * <pre>
 * 
 *      SOFTWARE HISTORY
 *     
 *      Date         Ticket#     Engineer    Description
 *      ------------ ----------  ----------- --------------------------
 *      Jun 12, 2009 1937        askripsk    Initial creation
 *      21May2009          6309  garmendariz Modified path for Geotools 2.6.4
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1
 */
public class RadarMosaicResource extends
        AbstractVizResource<RadarMosaicResourceData, MapDescriptor> implements
        IResourceDataChanged, IRadarTextGeneratingResource, IRefreshListener {

    private static final RGB DEFAULT_COLOR = new RGB(255, 255, 255);

    private IRadarMosaicRenderer mosaicRenderer;

    private boolean initColorMap = false;

    private String groupName = null;

    private String unitString = null;

    private boolean force = false;

    private DataTime lastTime = null;

    private Map<AbstractVizResource<?, ?>, DataTime[]> timeMatchingMap = new HashMap<AbstractVizResource<?, ?>, DataTime[]>();

    protected RadarMosaicResource(RadarMosaicResourceData rrd,
            LoadProperties loadProps) throws VizException {
        super(rrd, loadProps);
        rrd.addChangeListener(this);

        if (this.getCapability(ColorableCapability.class).getColor() == null) {
            this.getCapability(ColorableCapability.class).setColor(
                    DEFAULT_COLOR);
        }

        dataTimes = new ArrayList<DataTime>();
        // add listener for underlying resources
        for (ResourcePair rp : getResourceList()) {
            if (rp.getResourceData() != null) {
                rp.getResourceData().addChangeListener(this);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getDataTimes()
     */
    @Override
    public DataTime[] getDataTimes() {
        return dataTimes.toArray(new DataTime[0]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        mosaicRenderer.dispose();
        for (ResourcePair rp : getResourceList()) {
            if (rp.getResource() != null) {
                rp.getResource().dispose();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#init(com.raytheon.viz.core.
     * IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {

        mosaicRenderer = target.getExtension(
                IRadarMosaicRendererFactoryExtension.class).createNewRenderer(
                resourceData.getMosaicType());

        // We want to init the most severe resource first so the colormap
        // matches.
        ResourcePair mostSevere = null;
        int severity = -1;
        for (ResourcePair rp : getResourceList()) {
            int recSeverity = getSeverity(rp);
            if (severity < recSeverity) {
                mostSevere = rp;
                severity = recSeverity;
            }

        }
        if (mostSevere != null) {
            mostSevere.getResource().init(target);
            mostSevere.getResource().registerListener(this);
        }
        // add listener for underlying resources
        for (ResourcePair rp : getResourceList()) {
            if (rp.getResource() != null && rp != mostSevere) {
                rp.getResource().init(target);
                rp.getResource().registerListener(this);
            }
        }
    }

    private int getSeverity(ResourcePair rp) {
        int maxSeverity = -1;
        if (rp.getResource() == null) {
            ;
        } else if (rp.getResource() instanceof BestResResource) {
            for (ResourcePair rp1 : ((BestResResource) rp.getResource())
                    .getResourceList()) {
                int severity = getSeverity(rp1);
                if (severity > maxSeverity) {
                    maxSeverity = severity;
                }
            }
        } else if (rp.getResource() instanceof RadarImageResource) {
            RadarImageResource<?> rir = (RadarImageResource<?>) rp
                    .getResource();
            for (RadarRecord rr : rir.getRadarRecords().values()) {
                // this will make 8 bit products take precedence over all
                // 4-bit products
                int severity = rr.getOperationalMode() * rr.getNumLevels();
                if (severity > maxSeverity) {
                    maxSeverity = severity;
                }
            }
        }
        return maxSeverity;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.core.rsc.IVizResource#paint(com.raytheon.viz.core.
     * IGraphicsTarget, com.raytheon.viz.core.PixelExtent, double, float)
     */
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime[] frameTimes = paintProps.getFramesInfo().getTimeMap()
                .get(this);
        if (force || !Arrays.equals(timeMatchingMap.get(this), frameTimes)) {
            redoTimeMatching(
                    !Arrays.equals(timeMatchingMap.get(this), frameTimes),
                    frameTimes);
        }
        List<RadarRecord> recordsToMosaic = constructRecordsToMosaic(target);
        if (recordsToMosaic.isEmpty() == false) {
            DataTime curTime = getTimeForResource(this);
            synchronized (this) {
                force = force || !curTime.equals(lastTime);
                mosaicRenderer.mosaic(target, new MosaicPaintProperties(
                        paintProps, force), this);
                force = false;
                lastTime = curTime;
            }

        }
    }

    /**
     * Creates the buffer containing the data for the mosaic'ed radars.
     * 
     * @param target
     * @param geo
     * @return
     * @throws VizException
     */
    @SuppressWarnings("unchecked")
    private List<RadarRecord> constructRecordsToMosaic(IGraphicsTarget target)
            throws VizException {
        List<RadarRecord> recordsToMosaic = new ArrayList<RadarRecord>();

        // Build list of radarRecords to mosaic
        for (ResourcePair pair : getResourceList()) {
            AbstractVizResource<?, ?> rsc = pair.getResource();
            DataTime time = getTimeForResource(rsc);
            if (rsc instanceof BestResResource) {
                rsc = ((BestResResource) rsc).getBestResResource(time);
            }
            if (rsc instanceof AbstractRadarResource) {
                AbstractRadarResource<MapDescriptor> rr = (AbstractRadarResource<MapDescriptor>) rsc;

                VizRadarRecord rec = rr.getRadarRecord(time);
                if (rec != null) {
                    if (rec.getStoredDataAsync() != null) {
                        recordsToMosaic.add(rec);
                    } else {
                        issueRefresh();
                    }
                }
            }
        }

        if (!recordsToMosaic.isEmpty() && !initColorMap) {

            ColorMapParameters params = null;

            for (ResourcePair rp : getResourceList()) {
                if (rp.getResource() != null
                        && rp.getResource().hasCapability(
                                ColorMapCapability.class)) {
                    params = rp.getResource()
                            .getCapability(ColorMapCapability.class)
                            .getColorMapParameters();
                    break;
                }
            }
            if (params != null && params.getColorMap() != null) {
                for (ResourcePair rp : getResourceList()) {
                    if (rp.getResource() != null) {
                        rp.getResource()
                                .getCapability(ColorMapCapability.class)
                                .setColorMapParameters(params);
                    }
                }
                unitString = UnitFormat.getUCUMInstance().format(
                        params.getDisplayUnit());
                initColorMap = true;
            }
        }

        return recordsToMosaic;
    }

    public DataTime getTimeForResource(AbstractVizResource<?, ?> rsc) {
        DataTime[] dt = timeMatchingMap.get(rsc);
        int idx = descriptor.getFramesInfo().getFrameIndex();

        if (dt == null || dt.length <= idx || idx < 0) {
            return null;
        }

        return dt[idx];
    }

    private void redoTimeMatching(boolean requery, DataTime[] frameTimes)
            throws VizException {
        timeMatchingMap.clear();
        if (frameTimes == null) {
            return;
        }
        List<DataTime> dataTimes = Arrays.asList(frameTimes);
        timeMatchingMap.put(this, frameTimes);
        for (ResourcePair pair : getResourceList()) {
            DataTime[] availableTimes = pair.getResource().getDataTimes();
            if (requery
                    && pair.getResourceData() instanceof AbstractRequestableResourceData) {
                availableTimes = ((AbstractRequestableResourceData) pair
                        .getResourceData()).getAvailableTimes();
            }
            DataTime[] displayTimes = new DataTime[frameTimes.length];
            for (int i = 0; i < frameTimes.length; i++) {
                DataTime frameTime = frameTimes[i];
                if (frameTime == null) {
                    continue;
                }
                if (resourceData.getBinOffset() != null) {
                    frameTime = resourceData.getBinOffset().getNormalizedTime(
                            frameTime);
                    long frameSeconds = frameTime.getMatchValid() / 1000;
                    for (DataTime displayTime : availableTimes) {
                        long dispSeconds = displayTime.getMatchValid() / 1000;
                        // Match at twice the range of binOffset this makes
                        // things much smoother
                        if (Math.abs(dispSeconds - frameSeconds) < resourceData
                                .getBinOffset().getInterval() * 2) {
                            if (displayTimes[i] != null) {
                                long d1 = Math.abs(frameTime.getMatchValid()
                                        - displayTimes[i].getMatchValid());
                                long d2 = Math.abs(frameTime.getMatchValid()
                                        - displayTime.getMatchValid());
                                if (d1 < d2) {
                                    continue;
                                }
                            }
                            displayTimes[i] = displayTime;
                        }
                    }
                } else if (Arrays.asList(availableTimes).contains(frameTime)) {
                    displayTimes[i] = frameTime;
                }
            }
            timeMatchingMap.put(pair.getResource(), displayTimes);
            availableTimes = pair.getResource().getDataTimes();
            // request any new times.
            if (requery
                    && pair.getResourceData() instanceof AbstractRequestableResourceData) {
                AbstractRequestableResourceData arrd = (AbstractRequestableResourceData) pair
                        .getResourceData();
                PluginDataObject[] pdos = arrd.getLatestPluginDataObjects(
                        displayTimes, availableTimes);
                if (pdos.length > 1) {
                    resourceData.update(pdos);
                }
            }
            // remove any extra times
            for (DataTime availableTime : availableTimes) {
                DataTime adjAvailTime = availableTime;
                if (resourceData.getBinOffset() != null) {
                    adjAvailTime = resourceData.getBinOffset()
                            .getNormalizedTime(availableTime);
                }
                if (!dataTimes.contains(adjAvailTime)
                        && !Arrays.asList(displayTimes).contains(availableTime)) {
                    pair.getResourceData().fireChangeListeners(
                            ChangeType.DATA_REMOVE, availableTime);
                }
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.IVizResource#setDescriptor(com.raytheon.viz
     * .core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(MapDescriptor descriptor) {
        super.setDescriptor(descriptor);

        for (ResourcePair rp : this.resourceData.getResourceList()) {
            @SuppressWarnings("unchecked")
            AbstractVizResource<?, MapDescriptor> rsc = (AbstractVizResource<?, MapDescriptor>) rp
                    .getResource();
            if (rsc != null) {
                rsc.setDescriptor(descriptor);
            }
        }
    }

    /**
     * Mosaic inspect method, heavily dependent on RadarResource's interrogate
     * method is string keys
     */
    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {

        Coordinate coord = null;

        try {
            coord = latLon.asLatLon();
        } catch (Exception e) {
            // ignore
        }
        Map<AbstractRadarResource<?>, Map<String, String>> rscInspectMap = new HashMap<AbstractRadarResource<?>, Map<String, String>>();
        AbstractRadarResource<?> highestRsc = null;
        String inspectString = null;
        if (coord != null) {
            List<AbstractRadarResource<?>> resources = new ArrayList<AbstractRadarResource<?>>();
            double minDist = Double.POSITIVE_INFINITY;
            for (ResourcePair rp : getResourceList()) {
                if (rp.getResource() != null) {
                    AbstractVizResource<?, ?> rsc = rp.getResource();
                    DataTime time = getTimeForResource(rsc);
                    if (rsc instanceof BestResResource) {
                        rsc = ((BestResResource) rsc).getBestResResource(time);
                    }
                    if (rsc instanceof AbstractRadarResource) {
                        @SuppressWarnings("unchecked")
                        AbstractRadarResource<MapDescriptor> rr = (AbstractRadarResource<MapDescriptor>) rsc;
                        try {
                            // Everything in this try block is to only sample
                            // records within range
                            RadarRecord md = rr.getRadarRecord(time);
                            if (md == null) {
                                continue;
                            } else {
                                int range = md.getGateResolution()
                                        * md.getNumBins();
                                MathTransform ll2crs = MapUtil
                                        .getTransformFromLatLon(md.getCRS());
                                DirectPosition2D p1 = new DirectPosition2D(
                                        coord.x, coord.y);
                                DirectPosition2D p2 = new DirectPosition2D(
                                        md.getLongitude(), md.getLatitude());
                                ll2crs.transform(p1, p1);
                                ll2crs.transform(p2, p2);
                                if (p1.distance(p2) < minDist) {
                                    highestRsc = rr;
                                    minDist = p1.distance(p2);
                                }
                                if (p1.distance(p2) > range) {
                                    continue;
                                }
                            }
                        } catch (Exception e) {
                            throw new VizException(e);
                        }
                        resources.add(rr);
                    }
                }
            }
            // no on is within range.
            if (resources.isEmpty() && highestRsc != null) {
                resources.add(highestRsc);
            }
            for (AbstractRadarResource<?> rr : resources) {
                Map<String, String> vals = rr.interrogate(coord);
                if (vals != null) {
                    rscInspectMap.put(rr, vals);
                }
            }
            Double highestVal = Double.NEGATIVE_INFINITY;
            // Now loop through and find highest value
            for (AbstractRadarResource<?> rr : rscInspectMap.keySet()) {
                Map<String, String> valMap = rscInspectMap.get(rr);
                String num = valMap.get("numericValue");
                if (num != null) {
                    Double d = Double.parseDouble(num);
                    if (d > highestVal) {
                        highestVal = d;
                        highestRsc = rr;
                    }
                }
            }
        }

        if (highestRsc != null) {
            inspectString = highestRsc.inspect(rscInspectMap.get(highestRsc));
        }
        return inspectString == null ? "NO DATA" : inspectString;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.capabilities.IProjectableResource#project(org
     * .opengis.referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        for (ResourcePair rp : getResourceList()) {
            if (rp.getResource() != null) {
                rp.getResource().project(mapData);
            }
        }
        force = true;
    }

    public ResourceList getResourceList() {
        return this.resourceData.getResourceList();
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
        switch (type) {
        case DATA_UPDATE:
            for (PluginDataObject pdo : (PluginDataObject[]) object) {
                DataTime time = pdo.getDataTime();
                if (resourceData.getBinOffset() != null) {
                    time = resourceData.getBinOffset().getNormalizedTime(time);
                }
                if (!dataTimes.contains(time)) {
                    dataTimes.add(time);
                }
            }
            break;
        case DATA_REMOVE:
            DataTime time = (DataTime) object;
            if (resourceData.getBinOffset() != null) {
                time = resourceData.getBinOffset().getNormalizedTime(time);
            }
            dataTimes.remove(time);
            break;
        }
        synchronized (this) {
            force = true;
        }
        issueRefresh();
    }

    @Override
    public String getName() {
        if (groupName == null) {
            if (unitString != null || (unitString == null && initColorMap)) {
                groupName = resourceData.getProductName();
                if (unitString != null) {
                    if (groupName.contains("{U}")) {
                        groupName = groupName.replace("{U}", unitString);
                    } else {
                        groupName = String.format((groupName + " (%s) "),
                                unitString);
                    }
                }
            } else {
                return resourceData.getProductName();
            }
        }
        return groupName;
    }

    public String[] getUpperText(DataTime time) {
        if (!getResourceData().getMergeUpperText()) {
            return null;
        }
        ResourceList list = getResourceList();
        List<Set<String>> texts = new ArrayList<Set<String>>();
        for (ResourcePair rp : list) {
            if (rp.getResource() != null) {
                AbstractVizResource<?, ?> rsc = rp.getResource();
                time = getTimeForResource(rsc);
                if (rsc instanceof BestResResource) {
                    rsc = ((BestResResource) rsc).getBestResResource(time);
                }
                if (rsc instanceof AbstractRadarResource) {
                    if (rp.getProperties().isVisible()) {
                        String[] text = ((AbstractRadarResource<?>) rsc)
                                .getUpperText(time);
                        if (text == null) {
                            continue;
                        }
                        for (int i = 0; i < text.length; i++) {
                            if (texts.size() < i + 1) {
                                texts.add(new HashSet<String>());
                            }
                            texts.get(i).add(text[i]);
                        }
                    }
                }
            }
        }
        String[] textsArr = new String[texts.size()];
        for (int i = 0; i < texts.size(); i++) {
            textsArr[i] = "";
            for (String s2 : texts.get(i)) {
                String s1 = textsArr[i];
                // If they are equal just go with it
                if (s1.equals(s2)) {
                    continue;
                }
                if (s1.isEmpty()) {
                    textsArr[i] = s2;
                    continue;
                }
                if (s2.isEmpty()) {
                    textsArr[i] = s1;
                    continue;
                }
                // Determine any shared characters making a
                // prefix
                String prefix = "";
                while (s2.startsWith(s1.substring(0, 1))
                        && !Character.isDigit(s1.charAt(0))) {
                    prefix += s1.charAt(0);
                    s1 = s1.substring(1);
                    s2 = s2.substring(1);
                }
                // Determine any shared characters making a
                // sufix
                String suffix = "";
                while (s2.endsWith(s1.substring(s1.length() - 1))
                        && !Character.isDigit(s1.charAt(s1.length() - 1))) {
                    suffix = s1.charAt(s1.length() - 1) + suffix;
                    s1 = s1.substring(0, s1.length() - 1);
                    s2 = s2.substring(0, s2.length() - 1);

                }
                // If this is a max or mean tag try to parse the
                // max or min
                if (prefix.startsWith("MX") || prefix.startsWith("MN")) {
                    try {
                        double d1 = Double.parseDouble(s1);
                        double d2 = Double.parseDouble(s2);
                        if (d2 > d1 && prefix.startsWith("MX")) {
                            textsArr[i] = prefix + s2 + suffix;
                        } else if (d2 < d1 && prefix.startsWith("MN")) {
                            textsArr[i] = prefix + s2 + suffix;
                        }
                        continue;
                    } catch (Exception e) {
                        // Its probably just a parse exception,
                        // give up and do the default
                    }
                }
                // Try merging with only one copy of Prefix and
                // suffix
                textsArr[i] = prefix + s1 + ", " + s2 + suffix;
            }
        }
        return textsArr;
    }

    @Override
    public void refresh() {
        synchronized (this) {
            force = true;
        }
        issueRefresh();
    }

}
