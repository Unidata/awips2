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
package com.raytheon.uf.viz.cwa.rsc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.cwa.CWARecord;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.AbstractTimeMatcher;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.time.TimeMatchingJob;
import com.raytheon.viz.pointdata.PointDataRequest;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Resource for Center Weather Advisory
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 4, 2010             jsanchez     Initial creation
 * Jun 10,2011  9744       cjeanbap     Added Magnification, Outline, and Density
 *                                      compabilities.
 * May 11, 2015 4379       nabowle      Display all current CWAs for each frame.
 * Jun 15, 2015 4379       nabowle      Make last frame a live frame.
 * </pre>
 *
 * @author jsanchez
 * @version 1.0
 */
public class CWAResource extends
        AbstractVizResource<CWAResourceData, MapDescriptor> implements
        ISimulatedTimeChangeListener {

    private static final String LATS = "latitudes";

    private static final String LONS = "longitudes";

    private static final String EVENT_ID = "eventId";

    private static final String DIMENSION = "dimension";

    private static final String TEXT = "text";

    private static final String NUM_OF_POINTS = "numOfPoints";

    private static final String CWA_NAME = "Conus Center Weather Advisory";

    private static final String DATA_TIME = "dataTime";

    private static final String REF_TIME = "refTime";

    private static final String END = DATA_TIME + ".validPeriod.end";

    private static final String START = DATA_TIME + ".validPeriod.start";

    protected static RefreshTimerTask refreshTask;

    protected static Timer refreshTimer;

    protected DataTime displayedDataTime;

    private Map<DataTime, CWAFrame> frameMap;

    private CWAFrame liveFrame = new CWAFrame(now());

    private IFont font;

    private class CWAFrame implements IRenderable {
        private DataTime time;

        private PointDataContainer pdc;

        private List<PointDataView> framePdvs;

        private IWireframeShape wfs;

        private List<DrawableString> strings;

        private CWAFrame(DataTime time) {
            this.time = time;
            strings = new ArrayList<DrawableString>();
        }

        @Override
        public void paint(IGraphicsTarget target, PaintProperties paintProps)
                throws VizException {
            synchronized (this) {
                if (wfs == null) {
                    updateFrame(target, paintProps);
                }
            }

            float magnification = getCapability(MagnificationCapability.class)
                    .getMagnification().floatValue();
            font.setMagnification(magnification);

            target.drawWireframeShape(wfs,
                    getCapability(ColorableCapability.class).getColor(),
                    getCapability(OutlineCapability.class).getOutlineWidth(),
                    getCapability(OutlineCapability.class).getLineStyle());

            for (DrawableString string : strings) {
                string.setText(string.getText(),
                        getCapability(ColorableCapability.class).getColor());
            }
            target.drawStrings(strings);
        }

        /**
         * @param target
         * @param paintProps
         */
        private void updateFrame(IGraphicsTarget target,
                PaintProperties paintProps) throws VizException {
            Map<String, RequestConstraint> constraints = new HashMap<>();
            String startTime = TimeUtil.formatToSqlTimestamp(time.getRefTime());

            // Select CWAs whose valid period contains this frame's start time.
            RequestConstraint constraint = new RequestConstraint();
            constraint.setConstraintType(ConstraintType.LESS_THAN_EQUALS);
            constraint.setConstraintValue(startTime);
            constraints.put(START, constraint);

            constraint = new RequestConstraint();
            constraint.setConstraintType(ConstraintType.GREATER_THAN_EQUALS);
            constraint.setConstraintValue(startTime);
            constraints.put(END, constraint);

            // Request the point data
            this.pdc = PointDataRequest
                    .requestPointDataAllLevels((DataTime) null,
                    resourceData.getMetadataMap().get("pluginName")
                            .getConstraintValue(),
                    getParameters(), null, constraints);

            if (wfs != null) {
                wfs.dispose();
            }
            strings.clear();

            wfs = target.createWireframeShape(false, descriptor);

            if (this.pdc == null) {
                // nothing met the query restraints
                this.framePdvs = Collections.emptyList();
            } else {
                this.framePdvs = getPDVs();
            }

            for (PointDataView pdv : this.framePdvs) {
                Coordinate rightMost = new Coordinate(-9999, 0);
                int numOfPoints = pdv.getNumber(NUM_OF_POINTS).intValue();
                Number[] latitudes = pdv.getNumberAllLevels(LATS);
                Number[] longitudes = pdv.getNumberAllLevels(LONS);
                Coordinate[] coordinates = new Coordinate[numOfPoints];
                for (int i = 0; i < numOfPoints; i++) {
                    if (longitudes[i].floatValue() > rightMost.x) {
                        rightMost = new Coordinate(longitudes[i].floatValue(),
                                latitudes[i].floatValue());
                    }
                    coordinates[i] = new Coordinate(longitudes[i].floatValue(),
                            latitudes[i].floatValue());
                }

                if (!coordinates[numOfPoints - 1].equals(coordinates[0])) {
                    Coordinate[] temp = new Coordinate[numOfPoints + 1];
                    for (int j = 0; j < numOfPoints; j++) {
                        temp[j] = coordinates[j];
                    }
                    temp[numOfPoints] = temp[0];
                    coordinates = temp;
                }

                wfs.addLineSegment(coordinates);

                if (numOfPoints > 0) {
                    String eventId = pdv.getString(EVENT_ID);
                    DrawableString string = new DrawableString(eventId,
                            getCapability(ColorableCapability.class).getColor());
                    string.font = font;
                    double[] loc = descriptor.worldToPixel(new double[] {
                            rightMost.x, rightMost.y });
                    string.setCoordinates(loc[0], loc[1]);
                    strings.add(string);
                }
            }
            wfs.compile();
        }

        /**
         * Get the list of PointDataViews. PointDataViews that share an event id
         * will be deduplicated to the most recent record for that event id.
         *
         * @return The list of PointDataViews.
         * @throws VizException
         */
        private List<PointDataView> getPDVs() throws VizException {
            Map<String, PointDataView> eventIdMap = new HashMap<>();
            PointDataView pdv;
            PointDataView pdv2;
            String eventId;
            for (int uriCounter = 0; uriCounter < pdc.getAllocatedSz(); uriCounter++) {
                pdv = pdc.readRandom(uriCounter);
                eventId = pdv.getString(EVENT_ID);

                /*
                 * CWAs may be updated and reissued with the same eventId. In
                 * this case, display only the latest CWA for that eventId for
                 * this frame so we don't end up with overlapping and/or
                 * outdated CWAs. Outdated CWAs will still be the most recent
                 * for their eventId the frame at their reftime.
                 */
                /*
                 * FIXME - outdated CWAs may still be viewable on future frames
                 * if the validTo date was shortened.
                 */
                pdv2 = eventIdMap.get(eventId);
                if (pdv2 == null
                        || pdv.getLong(REF_TIME) > pdv2.getLong(REF_TIME)) {
                    eventIdMap.put(eventId, pdv);
                }
            }

            List<PointDataView> pdvs = new ArrayList<>();
            pdvs.addAll(eventIdMap.values());

            return pdvs;
        }

        public void dispose() {
            if (wfs != null) {
                wfs.dispose();
            }
        }
    }

    protected static class RefreshTimerTask extends TimerTask {

        private final Set<CWAResource> resourceSet = new HashSet<>();

        @Override
        public void run() {
            List<CWAResource> rscs;
            synchronized (resourceSet) {
                rscs = new ArrayList<>(resourceSet);
            }
            for (CWAResource rsc : rscs) {
                rsc.updateLiveFrame(now());
                rsc.issueRefresh();
                rsc.redoTimeMatching();
            }
        }

        public void addResource(CWAResource rsc) {
            synchronized (resourceSet) {
                resourceSet.add(rsc);
            }
        }

        public void removeResource(CWAResource rsc) {
            synchronized (resourceSet) {
                resourceSet.remove(rsc);
            }
        }
    }

    protected CWAResource(CWAResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        frameMap = new HashMap<DataTime, CWAResource.CWAFrame>();
        this.dataTimes = new ArrayList<DataTime>();
    }

    /**
     * Cancel the heart beat timer task
     *
     * @param resource
     */
    protected static void cancelRefreshTask(CWAResource resource) {
        synchronized (RefreshTimerTask.class) {
            if (refreshTask != null) {
                refreshTask.removeResource(resource);
                if (refreshTask.resourceSet.isEmpty()) {
                    refreshTimer.cancel();
                    refreshTimer = null;
                    refreshTask = null;
                }
            }
        }
    }

    /**
     * schedule the heart beat for the next minute
     */
    protected static void scheduleRefreshTask(CWAResource resource) {
        synchronized (RefreshTimerTask.class) {
            if (refreshTask == null) {
                refreshTimer = new Timer(true);
                refreshTask = new RefreshTimerTask();

                Calendar cal = Calendar.getInstance();
                cal.add(Calendar.MINUTE, 1);
                cal.set(Calendar.SECOND, 0);
                cal.set(Calendar.MILLISECOND, 0);

                refreshTimer.scheduleAtFixedRate(refreshTask, cal.getTime(),
                        TimeUtil.MILLIS_PER_MINUTE);
            }
            refreshTask.addResource(resource);
        }
    }

    /**
     * Get the current/simulated time to the minute. Seconds and Milliseconds
     * will be zero so a consistent time can be used each minute when retrieving
     * from the frame map.
     *
     * @return
     */
    public static DataTime now() {
        Calendar cal = Calendar.getInstance();
        cal.setTime(SimulatedTime.getSystemTime().getTime());
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);

        return new DataTime(cal);
    }

    @Override
    public String getName() {
        return CWA_NAME;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        displayedDataTime = paintProps.getDataTime();
        if (displayedDataTime == null) {
            return;
        }

        synchronized (frameMap) {
            CWAFrame frame = frameMap.get(displayedDataTime);
            if (frame != null) {
                frame.paint(target, paintProps);
            }
        }
    }

    @Override
    protected void disposeInternal() {
        cancelRefreshTask(this);
        if (font != null) {
            font.dispose();
            font = null;
        }

        synchronized (frameMap) {
            for (CWAFrame frame : frameMap.values()) {
                frame.dispose();
            }
            frameMap.clear();
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.font = target.initializeFont("Monospace", 11,
                new Style[] { Style.ITALIC });
        updateLiveFrame(now());
        scheduleRefreshTask(this);
        SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(this);
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        CWAFrame frame = frameMap.get(displayedDataTime);
        if (frame == null || frame.pdc == null) {
            return "NO DATA";
        }

        for (PointDataView pdv : frame.framePdvs) {
            Coordinate rightMost = new Coordinate(-9999, 0);
            int numOfPoints = pdv.getNumber(NUM_OF_POINTS).intValue();
            Number[] latitudes = pdv.getNumberAllLevels(LATS);
            Number[] longitudes = pdv.getNumberAllLevels(LONS);
            Coordinate[] coordinates = new Coordinate[numOfPoints];
            for (int i = 0; i < numOfPoints; i++) {
                if (longitudes[i].floatValue() > rightMost.x) {
                    rightMost = new Coordinate(longitudes[i].floatValue(),
                            latitudes[i].floatValue());
                }
                coordinates[i] = new Coordinate(longitudes[i].floatValue(),
                        latitudes[i].floatValue());
            }

            if (!coordinates[numOfPoints - 1].equals(coordinates[0])) {
                Coordinate[] temp = new Coordinate[numOfPoints + 1];
                for (int j = 0; j < numOfPoints; j++) {
                    temp[j] = coordinates[j];
                }
                temp[numOfPoints] = temp[0];
                coordinates = temp;
            }

            /*
             * FIXME - quick fix for 3-point lines since you can't create a
             * linear ring from the line. Adding a fourth point doesn't add
             * value since contains() will return false, or at least did while
             * testing.
             */
            if (coordinates.length < 4) {
                continue;
            }

            // GeometryFactory factory = new GeometryFactory();
            // LinearRing ring = factory.createLinearRing(coordinates);
            // Polygon llPolygon = factory.createPolygon(ring, null);
            // Coordinate[] llCoords = llPolygon.getCoordinates();
            Coordinate[] pixelCoords = new Coordinate[coordinates.length];
            for (int i = 0; i < coordinates.length; i++) {
                double[] pixelCoord = descriptor.worldToPixel(new double[] {
                        coordinates[i].x, coordinates[i].y });
                pixelCoords[i] = new Coordinate(pixelCoord[0], pixelCoord[1]);
            }
            GeometryFactory factory2 = new GeometryFactory();
            LinearRing ring2 = factory2.createLinearRing(pixelCoords);

            Polygon pixelPoly = factory2.createPolygon(ring2, null);
            Point point;
            try {
                point = pixelPoly.getFactory().createPoint(
                        coord.asPixel(descriptor.getGridGeometry()));
            } catch (Exception e) {
                throw new VizException(
                        "Error inspecting Center Weather Advisory", e);
            }

            if (pixelPoly.contains(point)) {
                String returnString = pdv.getString(TEXT).split("=")[0];
                return returnString;
            }
        }
        return "NO DATA";
    }

    @Override
    public void remove(DataTime dataTime) {
        super.remove(dataTime);
        synchronized (frameMap) {
            final CWAFrame frame = frameMap.remove(dataTime);
            if (frame != null) {
                frame.dispose();
            }
        }
    }

    /**
     * Adds a new record to this resource
     *
     * @param obj
     */
    protected void addRecord(CWARecord obj) {
        synchronized (frameMap) {
            DataTime dataTime = obj.getDataTime();
            CWAFrame frame = frameMap.get(dataTime);
            if (frame == null) {
                frame = new CWAFrame(dataTime);
                frameMap.put(dataTime, frame);
            }
        }
    }

    private String[] getParameters() {
        return new String[] { LATS, LONS, EVENT_ID, DIMENSION, TEXT,
                NUM_OF_POINTS, REF_TIME };
    }

    public class CoordinateComparator implements Comparator<Coordinate> {

        @Override
        public int compare(Coordinate o1, Coordinate o2) {
            int result = Double.compare(o1.x, o2.x);
            if (result == 0) {
                result = Double.compare(o1.y, o2.y);
            }
            return result;
        }
    }

    /**
     * Handle Data Updates by adding the record and refreshing the display.
     */
    @Override
    protected void resourceDataChanged(ChangeType type, Object updateObject) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) updateObject;
            for (PluginDataObject p : pdo) {
                if (p instanceof CWARecord) {
                    addRecord((CWARecord) p);
                }
            }
        }
        issueRefresh();
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        frameMap.get(displayedDataTime).wfs.dispose();
        frameMap.get(displayedDataTime).wfs = null;
    }

    /**
     * Update the live frame's time and mapping.
     */
    protected void updateLiveFrame(DataTime time) {
        synchronized (this.frameMap) {
            this.frameMap.remove(this.liveFrame.time);
            this.liveFrame.time = time;
            this.liveFrame.dispose();
            this.liveFrame.wfs = null;
            this.frameMap.put(time, this.liveFrame);
        }
    }

    /**
     * Redo the time matching
     */
    protected void redoTimeMatching() {
        AbstractTimeMatcher timeMatcher = this.getDescriptor().getTimeMatcher();
        if (timeMatcher != null) {
            timeMatcher.redoTimeMatching(this);
            TimeMatchingJob.scheduleTimeMatch(this.getDescriptor());
        }
    }

    /**
     * Updates the live frame to the current time set.
     */
    @Override
    public void timechanged() {
        updateLiveFrame(now());
        issueRefresh();
        redoTimeMatching();
    }
}
