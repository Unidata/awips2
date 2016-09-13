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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarInfoDict;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.cache.CacheObject.ICacheObjectCallback;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendedCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.d2d.core.sampling.ID2DSamplingResource;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.core.time.ID2DTimeMatchingExtension;
import com.raytheon.uf.viz.d2d.core.time.TimeMatcher;
import com.raytheon.viz.radar.DefaultVizRadarRecord;
import com.raytheon.viz.radar.VizRadarRecord;
import com.raytheon.viz.radar.frame.SailsFrameCoordinator;
import com.raytheon.viz.radar.interrogators.IRadarInterrogator;
import com.raytheon.viz.radar.rsc.RadarTextResource.IRadarTextGeneratingResource;
import com.raytheon.viz.radar.textcontributors.IRadarTextContributor;
import com.raytheon.viz.radar.textcontributors.UpperTextSet;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Top level radar resource that contains the code that is shared by all below
 * resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- ------------------------------------------
 * Aug 03, 2010           mnash      Initial creation
 * Mar 05, 2013  15313    kshresth   Added sampling for DMD
 * Apr 11, 2013  16030    dfriedman  Fix NPE.
 * May 05, 2014  17201    dfriedman  Enable same-radar time matching.
 * Jun 11, 2014  2061     bsteffen   Move rangeable methods to radial resource
 * May 13, 2015  4461     bsteffen   Add sails frame coordinator.
 * Sep 03, 2015  4779     njensen    Removed IDataScale
 * Nov 03, 2015  4857     bsteffen   Set volume scan interval in time matcher
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AbstractRadarResource<D extends IDescriptor> extends
        AbstractVizResource<RadarResourceData, D> implements
        IResourceDataChanged, IRadarTextGeneratingResource,
        ICacheObjectCallback<RadarRecord>, ID2DTimeMatchingExtension {

    /*
     * TODO This is dumb that a class with a name starting with Abstract is not
     * actually abstract. Either rename this class or actually apply the
     * abstract modifier.
     */

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractRadarResource.class);

    public enum InspectLabels {
        Mnemonic, Value, Angle, Shear, MSL, AGL, Azimuth, Range, ICAO
    }

    private static final List<InspectLabels> defaultInspectLabels = Arrays
            .asList(InspectLabels.Value, InspectLabels.Shear,
                    InspectLabels.MSL, InspectLabels.AGL, InspectLabels.Range,
                    InspectLabels.Azimuth, InspectLabels.ICAO);

    private static final List<InspectLabels> primaryInspectLabels = Arrays
            .asList(InspectLabels.Mnemonic, InspectLabels.Value,
                    InspectLabels.Shear, InspectLabels.MSL, InspectLabels.AGL,
                    InspectLabels.Range, InspectLabels.Azimuth,
                    InspectLabels.ICAO);

    private static final List<InspectLabels> secondaryInspectLabels = Arrays
            .asList(InspectLabels.Value, InspectLabels.Shear,
                    InspectLabels.ICAO);

    private static final List<InspectLabels> offscreenInspectLabels = Arrays
            .asList(InspectLabels.Mnemonic, InspectLabels.Value,
                    InspectLabels.Angle, InspectLabels.Shear);

    private IRadarInterrogator interrogator;

    public String icao;

    protected DataTime displayedDate;

    protected float displayedLevel = -1;

    protected String actualLevel = "";

    protected Map<DataTime, VizRadarRecord> radarRecords;

    protected Map<DataTime, String[]> upperTextMap = new HashMap<DataTime, String[]>();

    protected Coordinate radarLocation = null;

    protected static final RadarInfoDict infoDict;

    static {
        File radarInfo = PathManagerFactory.getPathManager().getStaticFile(
                "radarInfo.txt");
        if (radarInfo != null) {
            infoDict = RadarInfoDict.getInstance(radarInfo.getParent());
        } else {
            infoDict = null;
        }
    }

    protected AbstractRadarResource(RadarResourceData resourceData,
            LoadProperties loadProperties, IRadarInterrogator interrogator) {
        this(resourceData, loadProperties);
        this.interrogator = interrogator;
    }

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractRadarResource(RadarResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapability(ColorableCapability.class);
        resourceData.addChangeListener(this);

        dataTimes = new ArrayList<DataTime>();
        radarRecords = Collections
                .synchronizedMap(new HashMap<DataTime, VizRadarRecord>());
        icao = "";
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        SailsFrameCoordinator.addToDescriptor(descriptor);
        RadarTextResourceData.addRadarTextResource(descriptor);
    }

    @Override
    protected void disposeInternal() {
        radarRecords.clear();
        upperTextMap.clear();

        RadarTextResourceData.removeRadarTextResource(descriptor);
    }

    /**
     * @param record
     */
    public void addRecord(PluginDataObject record) {
        if (!(record instanceof RadarRecord)) {
            statusHandler.handle(Priority.PROBLEM, ""
                    + this.getClass().getName() + " expected : "
                    + RadarRecord.class.getName() + " Got: " + record);
            return;
        }
        RadarRecord radarRecord = (RadarRecord) record;
        radarRecord.setAddSpatial(!resourceData.latest);
        icao = radarRecord.getIcao();
        if (radarRecord.getLatitude() != null
                && radarRecord.getLongitude() != null && radarLocation == null) {
            radarLocation = new Coordinate(radarRecord.getLongitude(),
                    radarRecord.getLatitude());
        }
        DataTime d = radarRecord.getDataTime();

        VizRadarRecord existing = getRadarRecord(d);
        if (existing != null) {
            if (existing.getNumLevels() != null
                    && !existing.getNumLevels().equals(
                            radarRecord.getNumLevels())) {
                // Use the one with the most levels
                if (existing.getNumLevels().intValue() < radarRecord
                        .getNumLevels().intValue()) {
                    remove(d);
                    existing = null;
                }
            } else if (existing.getGateResolution() != null
                    && !existing.getGateResolution().equals(
                            radarRecord.getGateResolution())) {
                // use the one with the smallest resolution
                if (existing.getGateResolution().intValue() > radarRecord
                        .getGateResolution().intValue()) {
                    remove(d);
                    existing = null;
                }
            } else if (existing.getNumBins() * existing.getNumRadials() != radarRecord
                    .getNumBins() * radarRecord.getNumRadials()) {
                // use the one with the most pixels
                if (existing.getNumBins() * existing.getNumRadials() < radarRecord
                        .getNumBins() * radarRecord.getNumRadials()) {
                    remove(d);
                    existing = null;
                }
            } else if (existing.getInsertTime().getTimeInMillis() < radarRecord
                    .getInsertTime().getTimeInMillis()) {
                // Use the newest one
                remove(d);
                existing = null;
            }
        }
        if (existing == null) {
            existing = createVizRadarRecord(radarRecord);
            radarRecords.put(d, existing);
            synchronized (dataTimes) {
                dataTimes.add(d);
                Collections.sort(dataTimes);
            }
        }
    }

    protected VizRadarRecord createVizRadarRecord(RadarRecord radarRecord) {
        return new DefaultVizRadarRecord(radarRecord);
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        /*
         * TODO if this class actually goes abstract, this method should be
         * deleted and force subclasses to implement it
         */
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdoArr = (PluginDataObject[]) object;
            for (PluginDataObject record : pdoArr) {
                addRecord(record);
            }
        }
        issueRefresh();
    }

    @Override
    public String inspect(ReferencedCoordinate latLon) throws VizException {
        Map<String, String> dataMap;
        if (resourceData.mode.equals("CZ-Pg")) {
            return null;
        }
        // Grab current time
        DataTime displayedDate = descriptor.getTimeForResource(this);

        if (displayedDate == null) {
            FramesInfo fi = descriptor.getFramesInfo();
            DataTime[] times = fi.getTimeMap().get(this);
            int index = fi.getFrameIndex();
            if (times != null && index > 0 && index < times.length) {
                displayedDate = times[index];
            }
        }
        if (displayedDate == null) {
            displayedDate = this.displayedDate;
        }

        try {
            dataMap = interrogate(displayedDate, latLon.asLatLon());
        } catch (Exception e) {
            throw new VizException("Error converting coordinate for hover", e);
        }
        // determine if we are blended, if so are we the primary.
        boolean primary = true;
        if (this.hasCapability(BlendedCapability.class)) {
            int myIndex = this.getCapability(BlendedCapability.class)
                    .getResourceIndex();
            int hiddenIndex = this.getCapability(BlendedCapability.class)
                    .getBlendableResource().getResource()
                    .getCapability(BlendableCapability.class)
                    .getResourceIndex();
            primary = myIndex != hiddenIndex;

        }
        // determine if all pane sampling is enabled
        List<ID2DSamplingResource> samplingResources = descriptor
                .getResourceList().getResourcesByTypeAsType(
                        ID2DSamplingResource.class);
        boolean allPaneSample = false;
        if (!samplingResources.isEmpty()) {
            allPaneSample = samplingResources.get(0).isAllPanelSampling();
        }
        if (allPaneSample) {
            // When all pane sampling is on paint lots of info for the primary
            // on the visible pane, other panes return minimal info
            boolean visible = descriptor.getRenderableDisplay().getContainer()
                    .getActiveDisplayPane().getDescriptor() == descriptor;
            if (visible && primary) {
                return "="
                        + inspect(displayedDate, primaryInspectLabels, dataMap);
            } else {
                return " "
                        + inspect(displayedDate, offscreenInspectLabels,
                                dataMap);
            }
        } else if (primary) {
            return inspect(displayedDate, dataMap);
        } else {
            // The secondary returns slightly less data
            return inspect(displayedDate, secondaryInspectLabels, dataMap);
        }
    }

    public String inspect(DataTime dataTime, Map<String, String> dataMap) {
        return inspect(dataTime, defaultInspectLabels, dataMap);
    }

    /**
     * Given the map of data values, return the inspection string
     * 
     * @param dataMap
     * @return
     */
    public String inspect(DataTime dataTime, List<InspectLabels> labels,
            Map<String, String> dataMap) {
        if (dataMap == null) {
            return "NO DATA";
        }

        StringBuffer displayedData = new StringBuffer();

        if (labels.contains(InspectLabels.Mnemonic)) {
            displayedData.append(dataMap.get("Mnemonic") + " ");
        }

        if (labels.contains(InspectLabels.Value)) {
            displayedData.append(dataMap.get("Value"));
        }

        if (labels.contains(InspectLabels.Angle)) {
            while (displayedData.length() < 15) {
                displayedData.append(" ");
            }
            displayedData.append(dataMap.get("Angle"));
        }

        if (labels.contains(InspectLabels.Shear)
                && dataMap.containsKey("Shear")) {
            displayedData.append(" " + dataMap.get("Shear"));
        }

        if (labels.contains(InspectLabels.MSL) && dataMap.containsKey("MSL")) {
            displayedData.append(" " + dataMap.get("MSL") + "MSL");
            displayedData.append(" " + dataMap.get("AGL") + "AGL");
        }

        if (labels.contains(InspectLabels.Azimuth)
                && dataMap.containsKey("Azimuth")) {
            displayedData.append(" " + dataMap.get("Range"));
            displayedData.append("@" + dataMap.get("Azimuth"));
        }

        if (!"DMD".equalsIgnoreCase(dataMap.get("Mnemonic"))) {
            if (labels.contains(InspectLabels.ICAO)) {
                displayedData.append(' ').append(dataMap.get("ICAO"));
            }
        }

        if (displayedData.toString().contains("null")
                || displayedData.toString().isEmpty()) {
            displayedData.replace(0, displayedData.length(), "NO DATA");
        }

        return displayedData.toString();
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        try {
            DataTime displayedDate = descriptor.getTimeForResource(this);

            if (displayedDate == null) {
                displayedDate = this.displayedDate;
            }
            return new HashMap<String, Object>(interrogate(displayedDate,
                    coord.asLatLon()));
        } catch (TransformException e) {
            throw new VizException(
                    "Transformation error creating lat/lon from referenced coordinate",
                    e);
        } catch (FactoryException e) {
            throw new VizException(
                    "Error creating lat/lon from referenced coordinate", e);
        }
    }

    public Map<String, String> interrogate(DataTime dataTime, Coordinate latLon) {
        if (interrogator == null) {
            return new HashMap<String, String>();
        }
        ColorMapParameters params = null;
        if (hasCapability(ColorMapCapability.class)) {
            params = getCapability(ColorMapCapability.class)
                    .getColorMapParameters();
        }

        VizRadarRecord radarRecord = getRadarRecord(dataTime);
        if (radarRecord != null && radarRecord.getStoredDataAsync() != null) {
            return interrogator.sample(radarRecord, latLon, params);
        }
        return new HashMap<String, String>();
    }

    /**
     * Given the dataTime, returns the upper text info for that time
     * 
     * @param time
     * @return
     */
    @Override
    public String[] getUpperText(DataTime time) {
        VizRadarRecord record = getRadarRecord(time);
        if (record == null) {
            return null;
        }
        String[] result = upperTextMap.get(time);
        if (result == null && upperTextMap.containsKey(time) == false) {
            if (record.getStoredDataAsync() == null) {
                return null;
            }
            List<IRadarTextContributor> lines = UpperTextSet
                    .getContributors(record.getProductCode());
            if (lines != null) {
                result = new String[lines.size()];
                for (int i = 0; i < result.length; i++) {
                    result[i] = lines.get(i).contributeText(record);
                }
                // Remove blank lines from the end.
                while (result[result.length - 1].isEmpty()) {
                    result = Arrays.copyOfRange(result, 0, result.length - 1);
                }
            }
            upperTextMap.put(time, result);
        }
        return result;
    }

    public VizRadarRecord getRadarRecord(DataTime time) {
        return radarRecords.get(time);
    }

    public Map<DataTime, VizRadarRecord> getRadarRecords() {
        return radarRecords;
    }

    public Coordinate getRadarLocation() {
        return radarLocation;
    }

    @Override
    public void remove(DataTime dataTime) {
        synchronized (dataTimes) {
            super.remove(dataTime);
        }
        radarRecords.remove(dataTime);
        upperTextMap.remove(dataTime);
    }

    @Override
    public void objectArrived(RadarRecord object) {
        issueRefresh();
    }

    /**
     * The purpose of this method is to allow TDWR, SAILS, and MESO SAILS data
     * to time match products at different elevation angles correctly.
     * 
     * For example MESO SAILS has a 0.5 elevation product about every 1.5
     * minutes and a 1.5 elevation product every 6 minutes. Normally when the
     * 4th 0.5 product comes in it stops matching the 1.5 product because the
     * 1.5 product is too old. This method determines what the actual interval
     * is between volume scans and sets it in the time matcher so that the time
     * matcher will match all the times within that interval.
     */
    @Override
    public void modifyTimeMatching(D2DTimeMatcher d2dTimeMatcher,
            AbstractVizResource<?, ?> rsc, TimeMatcher timeMatcher) {
        /*
         * In order to use the radar customizations, the time match basis must
         * be an AbstractRadarResource for the same icao. If it is not, return
         * early.
         */
        AbstractVizResource<?, ?> basis = d2dTimeMatcher.getTimeMatchBasis();
        if (!(basis instanceof AbstractRadarResource)) {
            return;
        }
        AbstractRadarResource<?> radarBasis = (AbstractRadarResource<?>) basis;
        RequestConstraint icaoRC = getResourceData().getMetadataMap().get(
                "icao");
        RequestConstraint basisIcaoRC = radarBasis.getResourceData()
                .getMetadataMap().get("icao");
        if (icaoRC == null || !icaoRC.equals(basisIcaoRC)) {
            return;
        }
        /*
         * Gather all the frame times that we can, sorted by elevation number.
         * The time between two frames with the same elevation number is the
         * volume scan interval.
         */
        Set<RadarRecord> records = new HashSet<>();
        records.addAll(this.getRadarRecords().values());
        records.addAll(radarBasis.getRadarRecords().values());
        Map<Integer, SortedSet<Date>> elevationTimeMap = new HashMap<>();
        for (RadarRecord record : records) {
            Integer elevation = record.getElevationNumber();
            SortedSet<Date> times = elevationTimeMap.get(elevation);
            if (times == null) {
                times = new TreeSet<>();
                elevationTimeMap.put(elevation, times);
            }
            times.add(record.getDataTime().getRefTime());
        }
        long minInterval1 = getMinVolumeScanInterval(radarBasis
                .getRadarRecords().values());
        long minInterval2 = getMinVolumeScanInterval(this.getRadarRecords()
                .values());
        long minInteval = Math.min(minInterval1, minInterval2);
        if (minInteval < TimeUtil.MILLIS_PER_HOUR) {
            /*
             * 1 second padding to ensure that consecutive volume scans do not
             * overlap
             */
            minInteval -= TimeUtil.MILLIS_PER_SECOND;
            timeMatcher.setRadarOnRadar(minInteval);
        } else {
            timeMatcher.setRadarOnRadar(5 * TimeUtil.MILLIS_PER_MINUTE);
        }
    }

    /**
     * Determine the minimum interval between volume scans for all the volume
     * scans that are covered by the provided records. If there is not enough
     * records at the same elvation number than this will return
     * {@link Long#MAX_VALUE}.
     */
    private static long getMinVolumeScanInterval(
            Collection<? extends RadarRecord> records) {
        /*
         * Sorting by elevation number ensures that VCP changes and SAILS frames
         * are properly separated and do not invalidate the result.
         */
        Map<Integer, SortedSet<Date>> elevationTimeMap = new HashMap<>();
        for (RadarRecord record : records) {
            Integer elevation = record.getElevationNumber();
            SortedSet<Date> times = elevationTimeMap.get(elevation);
            if (times == null) {
                times = new TreeSet<>();
                elevationTimeMap.put(elevation, times);
            }
            times.add(record.getDataTime().getRefTime());
        }
        long minVolumeScanInterval = Long.MAX_VALUE;
        for (SortedSet<Date> times : elevationTimeMap.values()) {
            if (times.size() > 1) {
                Date prev = null;
                for (Date date : times) {
                    if (prev != null) {
                        long interval = date.getTime() - prev.getTime();
                        minVolumeScanInterval = Math.min(interval,
                                minVolumeScanInterval);
                    }
                    prev = date;
                }

            }
        }
        return minVolumeScanInterval;
    }

}
