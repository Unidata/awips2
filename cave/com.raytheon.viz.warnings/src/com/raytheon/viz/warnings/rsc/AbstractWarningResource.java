package com.raytheon.viz.warnings.rsc;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * 
 * TODO njensen: This was a refactored version of AbstractWWAResource. The
 * WatchesResource was not refactored to work with it. Ideally this class and
 * AbstractWWAResource should be combined so it supports both watches and
 * warnings.
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
 * 2012-04-16   DR 14866   D. Friedman Fix sampling error
 * May 3, 2012  DR 14741  porricel      Updated matchesFrame function
 *                                      to make SVS warning updates and
 *                                      original warning display properly
 *                                      in a given display frame
 * Jun 04, 2012 DR14992  mgamazaychikov Reversed the textToPrint array to
 * 										plot the strings in correct order
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public abstract class AbstractWarningResource extends AbstractWWAResource
        implements IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractWarningResource.class);

    private static GeometryFactory gf = new GeometryFactory();

    private static PreparedGeometryFactory pgf = new PreparedGeometryFactory();

    protected static class RepaintHeartbeat extends TimerTask {

        private HashSet<AbstractVizResource<?, ?>> resourceSet = new HashSet<AbstractVizResource<?, ?>>();

        private boolean cancelled = false;

        public RepaintHeartbeat() {

        }

        /**
         * copy resources from old task, just in case some were added after it
         * should have been replaced (threads are fun)
         **/
        public void copyResourceSet(RepaintHeartbeat oldTask) {
            // copy resources, in case one was added after a cancel
            Set<AbstractVizResource<?, ?>> oldResourceSet = oldTask
                    .getResourceSet();
            synchronized (oldResourceSet) {
                for (AbstractVizResource<?, ?> rsc : oldResourceSet) {
                    this.addResource(rsc);
                }
            }
        }

        public Set<AbstractVizResource<?, ?>> getResourceSet() {
            return resourceSet;
        }

        @Override
        public void run() {
            // get the unique displays from all the added resources
            ArrayList<IRenderableDisplay> displaysToRefresh = new ArrayList<IRenderableDisplay>(
                    1);
            synchronized (resourceSet) {
                for (AbstractVizResource<?, ?> rsc : resourceSet) {
                    try {
                        IRenderableDisplay disp = rsc.getDescriptor()
                                .getRenderableDisplay();
                        if (!displaysToRefresh.contains(disp)) {
                            displaysToRefresh.add(disp);
                        }
                    } catch (Exception e) {
                        statusHandler
                                .handle(Priority.PROBLEM,
                                        "Encountered error during Warnings Heartbeat, continuing with other Warnings ",
                                        e);
                    }
                }
            }

            // create an array with final modifier
            final IRenderableDisplay[] refreshList = displaysToRefresh
                    .toArray(new IRenderableDisplay[displaysToRefresh.size()]);

            // execute refersh in UI thread
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    for (IRenderableDisplay disp : refreshList) {
                        disp.refresh();
                    }
                }

            });

            // cancel the task if there are no more resources
            boolean cancel = false;
            synchronized (resourceSet) {
                if (resourceSet.size() < 1) {
                    cancel = true;
                }
            }

            if (cancel) {
                doCancel();
            }
        }

        public void addResource(AbstractVizResource<?, ?> rsc) {
            // if task has no resources then it needs to be started when the
            // first is added
            boolean start = false;
            synchronized (resourceSet) {
                // if this is the first resource added to an empty set start the
                // timer
                if (resourceSet.size() < 1) {
                    start = true;
                }
                resourceSet.add(rsc);
            }
            if (start) {
                AbstractWarningResource.scheduleHeartBeat();
            }
        }

        public void removeResource(AbstractVizResource<?, ?> rsc) {
            synchronized (resourceSet) {
                resourceSet.remove(rsc);
                // cancel the task if there are no more resources
                if (resourceSet.size() < 1) {
                    doCancel();
                }
            }
        }

        private void doCancel() {
            synchronized (heartBeatChangeLock) {
                if (cancelled == false) {
                    cancelled = true;
                    heartBeatTimer.cancel();
                    heartBeatTask = new RepaintHeartbeat();
                    heartBeatTimer = new Timer();
                    heartBeatTask.copyResourceSet(this);
                }
            }
        }
    }

    /** lock when changing heartBeatTask **/
    protected static final Object heartBeatChangeLock = new Object();

    protected static RepaintHeartbeat heartBeatTask = null;

    protected static Timer heartBeatTimer = null;

    /** one hour ahead, entirely arbitrary/magic **/
    private static final long LAST_FRAME_ADJ = (60 * 60 * 1000);

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

    /** map of dataURI to a warning entry **/
    protected Map<String, WarningEntry> entryMap;

    protected IFont warningsFont;

    protected RGB color;

    protected DataTime earliestRequested;

    protected final Object paintLock = new Object();

    /**
     * Constructor
     */
    public AbstractWarningResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        resourceData.addChangeListener(this);
        getCapability(OutlineCapability.class).setOutlineWidth(2);
        color = getCapability((ColorableCapability.class)).getColor();
        this.entryMap = new ConcurrentHashMap<String, WarningEntry>();
    }

    /**
     * schedule the heart beat for the next minute
     */
    protected static void scheduleHeartBeat() {
        // get simulated time
        Date currentTime = SimulatedTime.getSystemTime().getTime();
        // get a calendar
        Calendar now = Calendar.getInstance();
        // set calendar time to simulated time
        now.setTime(currentTime);
        // add one to the minutes field
        now.add(Calendar.MINUTE, 1);
        // reset second and milisecond to 0
        now.set(Calendar.SECOND, 0);
        now.set(Calendar.MILLISECOND, 0);
        // schedule task to fire every minute
        synchronized (heartBeatChangeLock) {
            try {
                if (heartBeatTimer == null) {
                    heartBeatTimer = new Timer();
                }
                // schedule on the minute every minute
                heartBeatTimer.schedule(heartBeatTask, now.getTime(),
                        1 * 60 * 1000);
            } catch (Exception e) {
                try {
                    heartBeatTimer.cancel();
                } catch (Exception e2) {
                    // ignore, we just want to make sure the timer is cancelled
                } finally {
                    // create a new task if there was an error when scheduling
                    heartBeatTask = new RepaintHeartbeat();
                    heartBeatTimer = new Timer();
                }
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Error scheduling warnings heart beat ", e);
            }
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        synchronized (heartBeatChangeLock) {
            if (heartBeatTask == null) {
                heartBeatTask = new RepaintHeartbeat();
            }
            heartBeatTask.addResource(this);
        }
        synchronized (this) {
            try {
                addRecord(getWarningRecordArray());
            } catch (VizException e) {
                e.printStackTrace();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        synchronized (heartBeatChangeLock) {
            heartBeatTask.removeResource(this);
        }
        for (WarningEntry entry : entryMap.values()) {
            if (entry.shadedShape != null) {
                entry.shadedShape.dispose();
            }
            if (entry.wireframeShape != null) {
                entry.wireframeShape.dispose();
            }
        }

        entryMap.clear();
        if (warningsFont != null) {
            warningsFont.dispose();
        }
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
            framePeriod = new TimeRange(time.getRefTime(), LAST_FRAME_ADJ);
            lastFrame = true;
        }

        if (time != null) {
            try {
                Point point = gf.createPoint(coord.asLatLon());

                for (String key : entryMap.keySet()) {

                    WarningEntry entry = entryMap.get(key);
                    AbstractWarningRecord record = entry.record;
                    if (matchesFrame(entry, time, framePeriod, lastFrame)) {

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
                throw new VizException("Error inspecting Warning Resource", e);
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
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            synchronized (AbstractWarningResource.this) {
                {
                    try {
                        addRecord(pdo);
                    } catch (VizException e) {
                        statusHandler.handle(Priority.SIGNIFICANT,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        } else if (type == ChangeType.CAPABILITY) {
            if (color != null
                    && color.equals(getCapability((ColorableCapability.class))
                            .getColor()) == false) {
                color = getCapability((ColorableCapability.class)).getColor();

                // TODO this needs to be fixed to work with watches which are
                // shaded
                // for (String dataUri : entryMap.keySet()) {
                // WarningEntry entry = entryMap.get(dataUri);
                // TODO init a shape somewhere else
                // if (entry.shadedShape != null) {
                // entry.shadedShape.dispose();
                // try {
                // initShape(entry.record);
                // } catch (VizException e) {
                // statusHandler.handle(Priority.PROBLEM,
                // e.getLocalizedMessage(), e);
                // }
                // }
                // }
            }
        }
        issueRefresh();
    }

    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        FramesInfo info = paintProps.getFramesInfo();
        DataTime[] frames = info.getFrameTimes();
        int frameToRequestedCompare = frames[0].compareTo(earliestRequested);
        if (frameToRequestedCompare < 0) {
            // we haven't requested data this far back
            this.requestData(frames[0]);
        } else if (frameToRequestedCompare > 0) {
            // the previous earliest frame was removed as updates came in, so
            // the warnings need to be disposed and we need to update the
            // earliestRequested so if they ever went back in time to that
            // again, it would be re-requested
            earliestRequested = frames[0];
            if (paintProps.getDataTime() != null) {
                cleanupData(paintProps.getDataTime(), frames);
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
            framePeriod = new TimeRange(thisFrameTime.getRefTime(),
                    LAST_FRAME_ADJ);
            lastFrame = true;
        }
        synchronized (paintLock) {
            for (String datauri : entryMap.keySet()) {
                WarningEntry entry = entryMap.get(datauri);
                AbstractWarningRecord record = entry.record;
                if (matchesFrame(entry, paintProps.getDataTime(), framePeriod,
                        lastFrame)) {

                    // check shapes
                    if (entry.project) {
                        initShape(target, entry.record);
                        entry.project = false;
                    }

                    if (entry != null && entry.wireframeShape != null) {
                        LineStyle lineStyle = (record.getProductClass() != null && record
                                .getProductClass().equals("T")) ? LineStyle.DASHED
                                : LineStyle.SOLID;
                        target.drawWireframeShape(entry.wireframeShape,
                                getCapability(ColorableCapability.class)
                                        .getColor(),
                                getCapability(OutlineCapability.class)
                                        .getOutlineWidth(), lineStyle);
                    } else if (entry != null && entry.shadedShape != null) {
                        target.drawShadedShape(entry.shadedShape, 1);
                    }

                    if (record != null && record.getGeometry() != null) {
                        // Calculate the upper left portion of the polygon
                        Coordinate upperLeft = new Coordinate(180, -90);

                        for (Coordinate c : record.getGeometry()
                                .getCoordinates()) {
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
                            warningsFont = target.getDefaultFont()
                                    .deriveWithSize(11);
                        }
                        // DR14992: reverse the textToPrint array to plot the strings in correct order
                        String [] textToPrintReversed = new String[textToPrint.length];
                        for(int i = 0; i < textToPrint.length; i++) {
							textToPrintReversed[i] = textToPrint[textToPrint.length
									- i - 1];
						}
                        
                        DrawableString params = new DrawableString(textToPrintReversed,
                                color);
                        params.font = warningsFont;
                        params.setCoordinates(d[0], d[1]);
                        params.textStyle = TextStyle.NORMAL;
                        params.horizontalAlignment = HorizontalAlignment.RIGHT;
                        params.verticallAlignment = VerticalAlignment.BOTTOM;
                        params.magnification = getCapability(
                                MagnificationCapability.class)
                                .getMagnification();
                        target.drawStrings(params);
                    }
                }
            }
        }
    }

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
            Date timeToDisplay = SimulatedTime.getSystemTime().getTime();
            // change frame time
            frameTime = timeToDisplay;
            // point paint time to different time
            paintTime = new DataTime(timeToDisplay);
            // point framePeriod to new frame
            framePeriod = new TimeRange(frameTime, LAST_FRAME_ADJ);
        }

        // check if the warning is cancelled
        WarningAction action = WarningAction.valueOf(entry.record.getAct());
        if (action == WarningAction.CAN
                && refTime.equals(paintTime)) {
            return false;
        // If this entry has been altered/updated, display its pre-altered version 
        // only in the frames prior to the time it was altered
        } else if (entry.altered){
        	if (frameStart.getTime() >= refTime.getTime() && 
        		    frameStart.getTime() < (entry.timeAltered.getTime()))	
        		return true;
        	if (frameStart.getTime() >= (entry.timeAltered.getTime())) 
        		return false;
        	
        } else if (refTime.equals(paintTime)
                || recordPeriod.contains(frameTime)
                || (framePeriod.contains(centerTime) && (!lastFrame || !entry.altered))) {
            return true;
        }

        return false;
    }

    protected void cleanupData(DataTime paintTime, DataTime[] descFrameTimes) {
        System.out.println("entryMap size " + entryMap.size());
        List<TimeRange> framePeriods = new ArrayList<TimeRange>(
                descFrameTimes.length);
        for (int i = 0; i < descFrameTimes.length; i++) {
            if (i == descFrameTimes.length - 1) {
                framePeriods.add(new TimeRange(descFrameTimes[i].getRefTime(),
                        LAST_FRAME_ADJ));
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

    protected abstract void updateDisplay(IGraphicsTarget target);

    protected abstract void requestData(DataTime earliest) throws VizException;

    protected abstract void initShape(IGraphicsTarget target,
            AbstractWarningRecord record);

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

    @Override
    public String getName() {
        String name = resourceData.name != null ? resourceData.name
                : "Warnings";

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

}
