package com.raytheon.viz.warnings.rsc;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.EmergencyType;
import com.raytheon.uf.common.dataplugin.warning.PracticeWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.warnings.DateUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * 
 * Top level watches, warnings, and advisory resource that contains the code
 * that is shared by all below resources
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2011            jsanchez     Initial creation
 * Aug 5, 2011            njensen       Refactored maps
 * Aug 22, 2011  10631   njensen  Major refactor
 * May 31, 2012 DR14992  mgamazaychikov Changed the order of strings in the
 *                                      String array returned from getText method
 * Jun 04, 2012 DR14992  mgamazaychikov Reversed the previous changes
 * Sep 26, 2012          jsanchez       Refactored AbstractWarningResource and AbstractWatchesResource into this class.
 * Apr 11, 2013   1877   jsanchez       Updated conditions for matching a frame.
 * Apr 18, 2013   1877   jsanchez       Had the child classes set the comparator. Fixed a null pointer.
 *                                      Remove frameAltered condition in matchesFrame. It prevented entries from being displayed.
 *                                      Check if geometry is null when inspecting.
 * Jul 22, 2013   2176   jsanchez       Updated the wire frame and text for EMERGENCY warnings.
 * Sep  4, 2013   2176   jsanchez       Made the polygon line width thicker and made regular text not bold.
 * Nov 11, 2013   2439   rferrel        Changes to prevent getting future warning when in DRT mode.
 * Dec  3, 2013   2576   jsanchez       Increased the font size of EMER.
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public abstract class AbstractWWAResource extends
        AbstractVizResource<WWAResourceData, MapDescriptor> implements
        IResourceDataChanged {
    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractWWAResource.class);

    protected class WarningEntry {

        protected AbstractWarningRecord record;

        protected IWireframeShape wireframeShape;

        protected IShadedShape shadedShape;

        /**
         * whether or not the warning has been altered, ie CON, CAN, EXP. a
         * warning can only be altered once with the exception of a partial
         * cancel.
         **/
        protected boolean altered = false;

        protected Date timeAltered;

        /**
         * was the alter a partial cancel? if it was then a matching CON should
         * be processed and added
         */
        protected boolean partialCancel = false;

        /**
         * set to true if paint needs to re-init the shape
         */
        protected boolean project = false;

    }

    protected static GeometryFactory gf = new GeometryFactory();

    protected static PreparedGeometryFactory pgf = new PreparedGeometryFactory();

    /** one hour ahead, entirely arbitrary/magic **/
    private static final long LAST_FRAME_ADJ = TimeUtil.MILLIS_PER_HOUR;

    protected String resourceName;

    /** map of dataURI to a warning entry **/
    protected Map<String, WarningEntry> entryMap;

    protected IFont warningsFont = null;

    protected IFont emergencyFont = null;

    protected RGB color;

    protected DataTime earliestRequested;

    protected final Object paintLock = new Object();

    private static final DataTime[] dataTimes = AbstractVizResource.TIME_AGNOSTIC
            .toArray(new DataTime[0]);

    protected static final SimpleDateFormat DEFAULT_FORMAT = new SimpleDateFormat(
            "HHmm'Z'");

    protected static final SimpleDateFormat LONG_FORMAT = new SimpleDateFormat(
            "HH:mm'Z' EEE ddMMMyy");

    protected static final SimpleDateFormat DAY_FORMAT = new SimpleDateFormat(
            "HH:mm'Z' EEE");

    protected List<AbstractWarningRecord> recordsToLoad;

    protected Comparator<AbstractWarningRecord> comparator;

    protected abstract void updateDisplay(IGraphicsTarget target)
            throws VizException;

    protected abstract void initShape(IGraphicsTarget target,
            AbstractWarningRecord record) throws VizException;

    public AbstractWWAResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        this.recordsToLoad = new ArrayList<AbstractWarningRecord>();
        resourceData.addChangeListener(this);
        getCapability(OutlineCapability.class).setOutlineWidth(2);
        color = getCapability((ColorableCapability.class)).getColor();
        this.entryMap = new ConcurrentHashMap<String, WarningEntry>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getDataTimes()
     */
    @Override
    public DataTime[] getDataTimes() {
        return dataTimes;
    }

    @Override
    public boolean isTimeAgnostic() {
        return true;
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (resourceData.hideSampling) {
            return "";
        }
        // check if we are in the last frame
        boolean lastFrame = false;
        FramesInfo framesInfo = this.descriptor.getFramesInfo();
        int frameIdx = framesInfo.getFrameIndex();
        DataTime[] frameTimes = framesInfo.getFrameTimes();
        if (frameIdx < 0 || frameIdx >= frameTimes.length)
            return "NO DATA";
        DataTime time = frameTimes[frameIdx];

        TimeRange framePeriod = null;
        if (frameIdx + 1 < frameTimes.length) {
            framePeriod = new TimeRange(time.getRefTime(),
                    frameTimes[frameIdx + 1].getRefTime());
        } else {
            framePeriod = getLastFrameTimeRange(time.getRefTime());

            lastFrame = true;
        }

        if (time != null) {
            try {
                Point point = gf.createPoint(coord.asLatLon());

                for (String key : entryMap.keySet()) {

                    WarningEntry entry = entryMap.get(key);
                    AbstractWarningRecord record = entry.record;
                    if (matchesFrame(entry, time, framePeriod, lastFrame)
                            && record.getGeometry() != null) {

                        Geometry recordGeom = record.getGeometry();
                        for (int i = 0; i < recordGeom.getNumGeometries(); i++) {
                            PreparedGeometry prepGeom = pgf.create(recordGeom
                                    .getGeometryN(i));

                            if (prepGeom.contains(point)) {
                                StringBuffer sb = new StringBuffer();
                                String[] textToPrint = getText(record, 0);
                                for (String text : textToPrint) {
                                    if (sb.length() > 0) {
                                        sb.append(" ");
                                    }
                                    sb.append(text);
                                }
                                return sb.toString();
                            }
                        }
                    }
                }
            } catch (Exception e) {
                throw new VizException("Error inspecting resource", e);
            }

        }
        return "NO DATA";
    }

    protected void disposeEntry(final WarningEntry entry) {
        if (entry.wireframeShape != null || entry.shadedShape != null) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    if (entry.shadedShape != null) {
                        entry.shadedShape.dispose();
                    }
                    if (entry.wireframeShape != null) {
                        entry.wireframeShape.dispose();
                    }
                }
            });
        }
    }

    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        synchronized (paintLock) {
            for (Map.Entry<String, WarningEntry> entry : entryMap.entrySet()) {
                WarningEntry warning = entry.getValue();
                // dispose and set to null just to be safe
                if (warning.shadedShape != null) {
                    warning.shadedShape.dispose();
                    warning.shadedShape = null;
                }
                if (warning.wireframeShape != null) {
                    warning.wireframeShape.dispose();
                    warning.wireframeShape = null;
                }
                warning.project = true;
            }
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        FramesInfo info = paintProps.getFramesInfo();
        DataTime[] frames = info.getFrameTimes();
        if (earliestRequested == null) {
            this.requestData(frames[0]);
        } else {
            int frameToRequestedCompare = frames[0]
                    .compareTo(earliestRequested);
            if (frameToRequestedCompare < 0) {
                // we haven't requested data this far back
                this.requestData(frames[0]);
            } else if (frameToRequestedCompare > 0) {
                // the previous earliest frame was removed as updates came in,
                // so
                // the warnings need to be disposed and we need to update the
                // earliestRequested so if they ever went back in time to that
                // again, it would be re-requested
                earliestRequested = frames[0];
                if (paintProps.getDataTime() != null) {
                    cleanupData(paintProps.getDataTime(), frames);
                }
            }
        }
        int index = info.getFrameIndex();
        if (!this.recordsToLoad.isEmpty()) {
            this.updateDisplay(target);
        }

        DataTime thisFrameTime = null;
        if (index > -1 && index < frames.length) {
            thisFrameTime = frames[index];
        }
        if (thisFrameTime == null) {
            return;
        }

        TimeRange framePeriod = null;
        boolean lastFrame = false;
        if (index + 1 < frames.length) {
            framePeriod = new TimeRange(thisFrameTime.getRefTime(),
                    frames[index + 1].getRefTime());
        } else {
            framePeriod = getLastFrameTimeRange(thisFrameTime.getRefTime());
            lastFrame = true;
        }
        synchronized (paintLock) {
            HashMap<String, WarningEntry> candidates = new HashMap<String, WarningEntry>();
            for (WarningEntry entry : entryMap.values()) {
                if (matchesFrame(entry, paintProps.getDataTime(), framePeriod,
                        lastFrame)) {
                    String key = getEventKey(entry);
                    WarningEntry current = candidates.get(key);

                    if (current == null
                            || current.record.getIssueTime().before(
                                    entry.record.getIssueTime())
                            || (current.record.getIssueTime().equals(
                                    entry.record.getIssueTime()) && current.record
                                    .getInsertTime().before(
                                            entry.record.getInsertTime())))
                        candidates.put(key, entry);
                }
            }
            for (WarningEntry entry : candidates.values()) {
                AbstractWarningRecord record = entry.record;

                // check shapes
                if (entry.project) {
                    initShape(target, entry.record);
                    entry.project = false;
                }

                if (entry != null && entry.wireframeShape != null) {
                    LineStyle lineStyle = LineStyle.SOLID;
                    if (record.getProductClass() != null
                            && record.getProductClass().equals("T")) {
                        lineStyle = LineStyle.DASHED;
                    }

                    int outlineWidth = getCapability(OutlineCapability.class)
                            .getOutlineWidth();
                    // Make wire frame outline thicker for EMERGENCY warnings
                    if (EmergencyType.isEmergency(record.getRawmessage())) {
                        outlineWidth *= 3;
                    }

                    target.drawWireframeShape(
                            entry.wireframeShape,
                            getCapability(ColorableCapability.class).getColor(),
                            outlineWidth, lineStyle);
                } else if (entry != null && entry.shadedShape != null) {
                    target.drawShadedShape(entry.shadedShape, 1);
                }

                if (record != null && record.getGeometry() != null) {
                    // Calculate the upper left portion of the polygon
                    Coordinate upperLeft = new Coordinate(180, -90);

                    for (Coordinate c : record.getGeometry().getCoordinates()) {
                        if (c.y - c.x > upperLeft.y - upperLeft.x) {
                            upperLeft = c;
                        }
                    }

                    double[] d = descriptor.worldToPixel(new double[] {
                            upperLeft.x, upperLeft.y });
                    d[0] -= paintProps.getZoomLevel() * 100;

                    double mapWidth = descriptor.getMapWidth()
                            * paintProps.getZoomLevel() / 1000;
                    String[] textToPrint = getText(record, mapWidth);
                    if (warningsFont == null) {
                        warningsFont = target.initializeFont(target
                                .getDefaultFont().getFontName(), 11,
                                new IFont.Style[0]);
                        emergencyFont = target.getDefaultFont().deriveWithSize(
                                14);
                    }
                    // DR14992: reverse the textToPrint array to plot the
                    // strings in correct order
                    String[] textToPrintReversed = new String[textToPrint.length];
                    for (int i = 0; i < textToPrint.length; i++) {
                        textToPrintReversed[i] = textToPrint[textToPrint.length
                                - i - 1];
                    }

                    DrawableString params = new DrawableString(
                            textToPrintReversed, color);
                    params.font = warningsFont;
                    params.setCoordinates(d[0], d[1]);
                    params.textStyle = TextStyle.NORMAL;
                    params.horizontalAlignment = HorizontalAlignment.RIGHT;
                    params.verticallAlignment = VerticalAlignment.BOTTOM;
                    params.magnification = getCapability(
                            MagnificationCapability.class).getMagnification();

                    // Draws the string again to have it appear bolder
                    if (EmergencyType.isEmergency(record.getRawmessage())) {
                        // moves over text to add EMER in a different font
                        textToPrintReversed[2] = String.format("%1$-23" + "s",
                                textToPrintReversed[2]);
                        params.setText(textToPrintReversed, color);

                        DrawableString emergencyString = new DrawableString(
                                params);
                        emergencyString.setCoordinates(d[0],
                                d[1] + (paintProps.getZoomLevel()) * 90);
                        emergencyString.font = emergencyFont;
                        emergencyString.setText(new String[] { "", "",
                                " " + EmergencyType.EMER, "" }, color);
                        target.drawStrings(emergencyString);
                    }

                    target.drawStrings(params);

                }
            }
        }
    }

    abstract protected String getEventKey(WarningEntry entry);

    protected boolean matchesFrame(WarningEntry entry, DataTime paintTime,
            TimeRange framePeriod, boolean lastFrame) {
        TimeRange recordPeriod = new TimeRange(entry.record.getStartTime()
                .getTimeInMillis(), entry.record.getEndTime().getTimeInMillis());
        long diff = entry.record.getEndTime().getTimeInMillis()
                - entry.record.getStartTime().getTimeInMillis();
        Date centerTime = new Date(entry.record.getStartTime()
                .getTimeInMillis() + (diff / 2));
        Date frameTime = framePeriod.getStart();

        Date frameStart = framePeriod.getStart();
        Date refTime = entry.record.getDataTime().getRefTime();

        if (lastFrame) {
            // use current system time to determine what to display
            Date timeToDisplay = TimeUtil.newDate();
            // change frame time
            frameTime = timeToDisplay;
            // point paint time to different time
            paintTime = new DataTime(timeToDisplay);
            framePeriod = getLastFrameTimeRange(frameTime);
        }

        // check if the warning is cancelled
        WarningAction action = WarningAction.valueOf(entry.record.getAct());
        if (action == WarningAction.CAN && refTime.equals(paintTime)) {
            return false;
            // If this entry has been altered/updated, display its pre-altered
            // version only in the frames prior to the time it was altered
        } else if (entry.altered) {
            if (frameStart.getTime() >= refTime.getTime()
                    && frameStart.getTime() < entry.timeAltered.getTime()) {
                return true;
            }
        } else if (refTime.equals(paintTime)
                || recordPeriod.contains(frameTime)
                || (framePeriod.contains(centerTime) && (!lastFrame || !entry.altered))) {
            return true;
        }

        return false;
    }

    protected void cleanupData(DataTime paintTime, DataTime[] descFrameTimes) {
        List<TimeRange> framePeriods = new ArrayList<TimeRange>(
                descFrameTimes.length);
        for (int i = 0; i < descFrameTimes.length; i++) {
            if (i == descFrameTimes.length - 1) {
                framePeriods.add(getLastFrameTimeRange(descFrameTimes[i]
                        .getRefTime()));
            } else {
                framePeriods.add(new TimeRange(descFrameTimes[i].getRefTime(),
                        descFrameTimes[i + 1].getRefTime()));
            }
        }

        int size = framePeriods.size();
        List<String> toRemove = new ArrayList<String>();
        for (String key : entryMap.keySet()) {
            WarningEntry entry = entryMap.get(key);
            boolean found = false;
            for (int i = 0; i < size; i++) {
                TimeRange tr = framePeriods.get(i);
                if (matchesFrame(entry, paintTime, tr, (i == size - 1))) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                toRemove.add(key);
            }
        }

        for (String key : toRemove) {
            WarningEntry entry = entryMap.remove(key);
            System.out.println("removing " + entry.record.getDataURI());
            disposeEntry(entry);
        }
    }

    public synchronized void addRecord(PluginDataObject[] pdos)
            throws VizException {
        for (PluginDataObject pdo : pdos) {
            if (pdo instanceof AbstractWarningRecord) {
                AbstractWarningRecord record = (AbstractWarningRecord) pdo;
                String officeid = record.getOfficeid();
                if (!resourceData.getMetadataMap().containsKey("officeid")
                        || resourceData.getMetadataMap().get("officeid")
                                .getConstraintValue().contains(officeid)) {
                    this.recordsToLoad.add((AbstractWarningRecord) pdo);
                }
            }
        }
    }

    protected PluginDataObject[] sort(PluginDataObject[] pdos) {
        ArrayList<AbstractWarningRecord> sortedWarnings = new ArrayList<AbstractWarningRecord>();
        for (Object o : pdos) {
            if (((PluginDataObject) o) instanceof AbstractWarningRecord) {
                AbstractWarningRecord record = (AbstractWarningRecord) o;
                sortedWarnings.add(record);
            }
        }

        if (comparator != null) {
            Collections.sort(sortedWarnings, comparator);
        }
        return sortedWarnings.toArray(new AbstractWarningRecord[sortedWarnings
                .size()]);
    }

    @SuppressWarnings("unchecked")
    protected void requestData(DataTime earliest) throws VizException {
        System.out.println("requesting data");
        Map<String, RequestConstraint> map = (Map<String, RequestConstraint>) resourceData
                .getMetadataMap().clone();
        if (earliestRequested != null) {
            // don't request data we've already requested
            String[] times = new String[] { earliest.toString(),
                    earliestRequested.toString() };
            RequestConstraint constraint = new RequestConstraint();
            constraint.setConstraintType(ConstraintType.BETWEEN);
            constraint.setBetweenValueList(times);
            map.put("endTime", constraint);
        } else {
            RequestConstraint endConstraint = new RequestConstraint(
                    earliest.toString(), ConstraintType.GREATER_THAN_EQUALS);
            map.put("endTime", endConstraint);
        }

        earliestRequested = earliest;

        PluginDataObject[] pdos = DataCubeContainer.getData(map);
        addRecord(sort(pdos));
    }

    protected String[] getText(AbstractWarningRecord record, double mapWidth) {
        String vid = record.getPhensig();
        String phen = record.getPhen();
        DateUtil du = new DateUtil();
        String[] textToPrint = new String[] { "", "", "", "" };

        textToPrint[0] = record.getProductClass();
        if ((vid != null && phen != null)
                && (vid.equals("TO.A") || vid.equals("SV.A")
                        || phen.equals("FL") || phen.equals("FA"))) {
            textToPrint[0] += "." + vid;
        }
        textToPrint[0] += "." + record.getEtn();
        textToPrint[1] = record.getPil();

        SimpleDateFormat startFormat = DEFAULT_FORMAT;
        SimpleDateFormat endFormat = DEFAULT_FORMAT;
        if (mapWidth == 0) {
            startFormat = LONG_FORMAT;
            endFormat = DAY_FORMAT;
        } else if (mapWidth <= 200) {
            startFormat = DAY_FORMAT;
            endFormat = DAY_FORMAT;
        }

        synchronized (startFormat) {
            textToPrint[2] = "Valid "
                    + du.format(record.getStartTime().getTime(), startFormat);
        }
        synchronized (endFormat) {
            textToPrint[3] = "Thru "
                    + du.format(record.getEndTime().getTime(), endFormat);
        }

        return textToPrint;
    }

    protected PluginDataObject[] getWarningRecordArray() {
        CAVEMode caveMode = CAVEMode.getMode();
        boolean isOperational = (CAVEMode.OPERATIONAL.equals(caveMode)
                || CAVEMode.TEST.equals(caveMode) ? true : false);
        if (isOperational) {
            return resourceData.records
                    .toArray(new AbstractWarningRecord[resourceData.records
                            .size()]);
        } else {
            return resourceData.records
                    .toArray(new PracticeWarningRecord[resourceData.records
                            .size()]);
        }
    }

    @Override
    public String getName() {
        String name = resourceData.name != null ? resourceData.name
                : resourceName;

        DataTime[] times = this.descriptor.getFramesInfo().getFrameTimes();
        int timeIdx = this.descriptor.getFramesInfo().getFrameIndex();

        // handle last frame differently, it should always be the latest time
        boolean lastFrame = false;
        if (timeIdx == times.length - 1) {
            lastFrame = true;
        }
        DataTime time = null;

        // get time to display
        if (lastFrame) {
            time = new DataTime(SimulatedTime.getSystemTime().getTime());
        } else if (timeIdx > -1 && timeIdx < times.length) {
            time = times[timeIdx];
        }

        // add time to legend
        if (time != null) {
            name += " " + time.getLegendString();
        }
        return name;
    }

    /**
     * Determine time range for the last frame. When in simulated time (DRT)
     * keep end of time range the start of the base time's next minute.
     * 
     * @param baseTime
     * @return timeRange
     */
    private TimeRange getLastFrameTimeRange(Date baseTime) {
        TimeRange timeRange = null;
        if (SimulatedTime.getSystemTime().isRealTime()) {
            timeRange = new TimeRange(baseTime, LAST_FRAME_ADJ);
        } else {
            Calendar cal = TimeUtil.newGmtCalendar();
            cal.setTime(baseTime);
            // Make the end time for the last frame the start of the next minute
            // of the base time to prevent getting "future" warnings.
            cal.add(Calendar.MINUTE, 1);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            timeRange = new TimeRange(baseTime, cal.getTime());
        }
        return timeRange;
    }
}
