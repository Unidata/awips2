package com.raytheon.viz.warnings.rsc;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.eclipse.swt.graphics.RGB;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.warning.AbstractWarningRecord;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
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
 * TODO njensen: Ideally this should be refactored to be more in line with the
 * AbstractWarningsResource.
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
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public abstract class AbstractWatchesResource extends AbstractWWAResource
        implements IResourceDataChanged {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractWatchesResource.class);

    private static PreparedGeometryFactory pgf = new PreparedGeometryFactory();

    private static GeometryFactory gf = new GeometryFactory();

    /**
     * 
     * this task calls redoTimeMatching on the resource, it should be scheduled
     * to run for when a warning is set to expire
     * 
     * @author ekladstrup
     * @version 1.0
     */
    protected class WarningExpirationTask extends TimerTask {

        private AbstractWatchesResource rsc = null;

        public WarningExpirationTask(AbstractWatchesResource rsc) {
            this.rsc = rsc;
        }

        @Override
        public void run() {
            // System.err.println("warning expired");
            // some warning has expired
            rsc.redoTimeMatching(this.scheduledExecutionTime());
            rsc.issueRefresh();
        }

    }

    protected class WarningEntry {

        protected AbstractWarningRecord record;

        protected IWireframeShape wireframeShape;

        protected IShadedShape shadedShape;

        protected List<DataTime> times;
    }

    protected IGraphicsTarget target;

    /** map of dataURI to a warning entry **/
    protected Map<String, WarningEntry> entryMap;

    protected Map<DataTime, List<AbstractWarningRecord>> frames;

    protected DataTime displayedDate;

    protected IFont warningsFont;

    protected RGB color;

    private Timer timer;

    private Set<Long> expTaskSet;

    /**
     * Constructor
     */
    public AbstractWatchesResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        resourceData.addChangeListener(this);
        getCapability(OutlineCapability.class).setOutlineWidth(2);
        color = getCapability((ColorableCapability.class)).getColor();
        this.entryMap = new HashMap<String, WarningEntry>();
        this.frames = Collections
                .synchronizedMap(new HashMap<DataTime, List<AbstractWarningRecord>>());
        timer = new Timer();
        expTaskSet = new HashSet<Long>();
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (this.target == null) {
            this.target = target;

            synchronized (this) {
                try {
                    addRecord(getWarningRecordArray());
                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        }
        // force creation of a frame for any currently active warnings, this
        // frame might get displayed in place of the last frame.
        initNewFrame(new DataTime(SimulatedTime.getSystemTime().getTime()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.rsc.IVizResource#dispose()
     */
    @Override
    protected void disposeInternal() {
        timer.cancel();

        for (WarningEntry entry : entryMap.values()) {
            if (entry.shadedShape != null) {
                entry.shadedShape.dispose();
            }
            if (entry.wireframeShape != null) {
                entry.wireframeShape.dispose();
            }
            if (entry.times != null) {
                entry.times.clear();
            }
        }

        entryMap.clear();
        frames.clear();
        if (warningsFont != null) {
            warningsFont.dispose();
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        if (resourceData.hideSampling) {
            return "";
        }

        if (this.displayedDate != null
                && this.frames.containsKey(displayedDate)) {

            try {
                Point point = gf.createPoint(coord.asLatLon());

                synchronized (frames) {
                    for (AbstractWarningRecord record : this.frames
                            .get(displayedDate)) {

                        Date entryIssue = record.getStartTime().getTime();
                        if (entryIssue == null) {
                            entryIssue = record.getIssueTime().getTime();
                        }

                        Date entryEnd = record.getEndTime().getTime();
                        Date ref = displayedDate.getRefTime();

                        if (entryIssue.compareTo(ref) <= 0
                                && entryEnd.compareTo(ref) >= 0) {

                            Geometry recordGeom = record.getGeometry();
                            for (int i = 0; i < recordGeom.getNumGeometries(); i++) {
                                PreparedGeometry prepGeom = pgf
                                        .create(recordGeom.getGeometryN(i));

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
                }

            } catch (Exception e) {
                throw new VizException("Error inspecting Warning Resource", e);
            }
        }
        return "NO DATA";
    }

    protected void removeFrame(DataTime dataTime) {
        synchronized (frames) {
            DataTime[] frameTimes = descriptor.getFramesInfo().getFrameTimes();
            DataTime lastTime = frameTimes[frameTimes.length - 1];
            if (lastTime != null
                    && lastTime.getMatchValid() < dataTime.getMatchValid()) {
                // Don't remove warnings that are newer than the last frame.
                // This allows us to display the latest warnings even if they
                // don't time match. These warnings will be cleaned up again on
                // the next redoTimeMatching. Hopefully we aren't keeping too
                // many around.
                return;
            }
            List<AbstractWarningRecord> warningRecordList = frames
                    .remove(dataTime);
            // remove times can effect stepping through frames
            for (AbstractWarningRecord w : warningRecordList) {
                final WarningEntry entry = entryMap.get(w.getDataURI());
                if (entry != null) {
                    List<DataTime> list = entry.times;
                    if (list != null && list.remove(dataTime)) {
                        if (list.isEmpty()) {
                            disposeEntry(entry);
                        }
                    }
                    if (list == null || list.isEmpty()) {
                        entryMap.remove(w.getDataURI());
                    }
                }
            }
        }

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
        this.frames.clear();
    }

    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.DATA_UPDATE) {
            PluginDataObject[] pdo = (PluginDataObject[]) object;
            synchronized (AbstractWatchesResource.this) {
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

                for (String dataUri : entryMap.keySet()) {
                    WarningEntry entry = entryMap.get(dataUri);
                    if (entry.shadedShape != null) {
                        entry.shadedShape.dispose();
                        try {
                            initShape(entry.record);
                        } catch (VizException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                }
            }
        }
        issueRefresh();
    }

    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        FramesInfo info = paintProps.getFramesInfo();
        DataTime[] frames = info.getFrameTimes();
        cleanupData(frames);
        int index = info.getFrameIndex();
        if (index > -1 && info.getFrameCount() > 0
                && index < info.getFrameCount()) {
            displayedDate = frames[index];
            if (index == info.getFrameCount() - 1) {
                // When we are on the last frame, if there exists a frame with
                // data that is beyond the last frame but within a reasonable
                // amount of time(1 hour) then we will display that frame
                // instead of the last frame. We do this so when a new warning
                // is issued it will display immediately over radar or satellite
                // data, even if the latest frame is a few minutes behind the
                // current time.
                long displayValid = displayedDate.getMatchValid();
                long latestValid = displayValid;
                synchronized (frames) {
                    for (Entry<DataTime, List<AbstractWarningRecord>> entry : this.frames
                            .entrySet()) {
                        if (entry.getValue().isEmpty()) {
                            // do not override the frame time for a time with no
                            // records.
                            continue;
                        }
                        long valid = entry.getKey().getMatchValid();
                        // 3600000 == 1 hour == a magicly random amount of time
                        if (valid > latestValid
                                && latestValid - displayValid < 3600000) {
                            displayedDate = entry.getKey();
                            latestValid = valid;
                        }
                    }
                }
            }
        } else {
            displayedDate = paintProps.getDataTime();
        }
        if (displayedDate == null) {
            return;
        }

        if (!this.recordsToLoad.isEmpty()) {
            this.updateFrames();
        }

        DataTime thisFrameTime = displayedDate;
        if (!this.frames.containsKey(thisFrameTime)) {
            this.initNewFrame(thisFrameTime);
        }

        List<AbstractWarningRecord> records = this.frames.get(thisFrameTime);
        for (AbstractWarningRecord record : records) {
            WarningEntry entry = entryMap.get(record.getDataURI());
            if (entry != null && entry.wireframeShape != null) {
                LineStyle lineStyle = (record.getProductClass() != null && record
                        .getProductClass().equals("T")) ? LineStyle.DASHED
                        : LineStyle.SOLID;
                target.drawWireframeShape(entry.wireframeShape,
                        getCapability(ColorableCapability.class).getColor(),
                        getCapability(OutlineCapability.class)
                                .getOutlineWidth(), lineStyle);
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
                    warningsFont = target.getDefaultFont().deriveWithSize(11);
                }
                DrawableString params = new DrawableString(textToPrint, color);
                params.font = warningsFont;
                params.setCoordinates(d[0], d[1]);
                params.textStyle = TextStyle.NORMAL;
                params.horizontalAlignment = HorizontalAlignment.RIGHT;
                params.verticallAlignment = VerticalAlignment.BOTTOM;
                params.magnification = getCapability(
                        MagnificationCapability.class).getMagnification();
                target.drawStrings(params);
            }
        }
    }

    protected void cleanupData(DataTime[] descFrameTimes) {
        List<DataTime> oldFrames = null;
        synchronized (frames) {
            for (DataTime dt : frames.keySet()) {
                boolean found = false;
                for (DataTime descTime : descFrameTimes) {
                    if (descTime.equals(dt)) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    if (oldFrames == null) {
                        oldFrames = new ArrayList<DataTime>();
                    }
                    oldFrames.add(dt);
                }
            }
        }
        if (oldFrames != null) {
            for (DataTime old : oldFrames) {
                this.removeFrame(old);
            }
        }
    }

    protected synchronized void initNewFrame(DataTime thisFrameTime)
            throws VizException {
        // subclasses should override
    }

    protected synchronized void updateFrames() throws VizException {
        // subclasses should override
    }

    protected void initShape(AbstractWarningRecord record) throws VizException {
        // subclasses should override
    }

    @Override
    public String getName() {
        return null;
    }

    /**
     * Schedules a WarningExpirationTask for the end time of the passing in
     * record
     * 
     * @param rec
     *            a WarningRecord
     */
    protected void scheduleTimer(AbstractWarningRecord rec) {
        // only schedule if record has not expired already
        long now = SimulatedTime.getSystemTime().getTime().getTime();
        long endTime = rec.getEndTime().getTimeInMillis();
        synchronized (expTaskSet) {
            if (endTime > now && !expTaskSet.contains(new Long(endTime))) {
                WarningExpirationTask task = new WarningExpirationTask(this);
                timer.schedule(task, rec.getEndTime().getTime());
                expTaskSet.add(new Long(endTime));
            }
        }
    }

    /**
     * Redo time matching and remove the passed in time from the map of
     * scheduled times
     * 
     * @param triggerTime
     */
    public void redoTimeMatching(long triggerTime) {
        redoTimeMatching();
        Long time = new Long(triggerTime);
        // remove the instance of the trigger time from the map
        synchronized (expTaskSet) {
            if (expTaskSet != null && expTaskSet.contains(time)) {
                expTaskSet.remove(time);
            }
        }
    }

    /**
     * Redo the time matching
     */
    public void redoTimeMatching() {
        try {
            this.getDescriptor().getTimeMatcher().redoTimeMatching(this);
            this.getDescriptor().getTimeMatcher()
                    .redoTimeMatching(this.getDescriptor());
        } catch (VizException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }
}
