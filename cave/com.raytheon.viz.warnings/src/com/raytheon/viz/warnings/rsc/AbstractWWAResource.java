package com.raytheon.viz.warnings.rsc;

import java.text.DateFormat;
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
import com.raytheon.uf.common.dataplugin.warning.util.WarningLookups;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.inventory.exception.DataCubeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.VizApp;
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
import com.raytheon.uf.viz.datacube.DataCubeContainer;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.warnings.ui.DrawingPropertiesDialog;
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
 * May 3, 2011             jsanchez    Initial creation
 * Aug 5, 2011             njensen     Refactored maps
 * Aug 22, 2011  10631     njensen     Major refactor
 * May 31, 2012 DR14992  mgamazaychikov Changed the order of strings in the
 *                                      String array returned from getText method
 * Jun 04, 2012 DR14992  mgamazaychikov Reversed the previous changes
 * Sep 26, 2012            jsanchez    Refactored AbstractWarningResource and AbstractWatchesResource into this class.
 * Apr 11, 2013   1877     jsanchez    Updated conditions for matching a frame.
 * Apr 18, 2013   1877     jsanchez    Had the child classes set the comparator. Fixed a null pointer.
 *                                     Remove frameAltered condition in matchesFrame. It prevented entries from being displayed.
 *                                     Check if geometry is null when inspecting.
 * Jul 22, 2013   2176     jsanchez    Updated the wire frame and text for EMERGENCY warnings.
 * Sep  4, 2013   2176     jsanchez    Made the polygon line width thicker and made regular text not bold.
 * Nov 11, 2013   2439     rferrel     Changes to prevent getting future warning when in DRT mode.
 * Dec  3, 2013   2576     jsanchez    Increased the font size of EMER.
 * Mar 10, 2014   2832     njensen     Moved duplicated subclass's disposeInternal() logic here
 * Aug 14, 2014   3523     mapeters    Updated deprecated {@link DrawableString#textStyle}
 *                                     assignments.
 * Dec 5, 2014   DR14944   jgerth      Only set outline width when there is no existing capability
 * Oct 16, 2015   4971     bsteffen    Do not reverse order of text.
 * Nov 05, 2015   5070     randerso    Adjust font sizes for dpi scaling
 * Aug 22, 2016   5842     dgilling    Remove dependency on viz.texteditor plugin.
 * Dec 19, 2018   ----     mjames@ucar Added phensig color table lookup.
 * Mar 15, 2022			 srcarter@ucar Add support for display settings for outline, fill, text and time displays
 * Jun 24, 2022			 srcarter@ucar Add 'statement/other' display settings, set enabled for only relevant WWA types
 * Jun 28, 2022			 srcarter@ucar Display sampling based on new 'sampling' settings
 *
 * </pre>
 *
 * @author jsanchez
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
    
    /** Whether to display warning outlines by default */
    public static final boolean WARN_OUTLINE_DEFAULT = true;
    /** Whether to display warning fill by default */
    public static final boolean WARN_FILL_DEFAULT = false;
    /** Whether to display warning text by default */
    public static final boolean WARN_TEXT_DEFAULT = true;
    /** Whether to display warning times by default */
    public static final boolean WARN_TIME_DEFAULT = true;
    /** Whether to display watch outlines by default */
    public static final boolean WATCH_OUTLINE_DEFAULT = false;
    /** Whether to display watch fill by default */
    public static final boolean WATCH_FILL_DEFAULT = true;
    /** Whether to display watch text by default */
    public static final boolean WATCH_TEXT_DEFAULT = true;
    /** Whether to display watch time by default */
    public static final boolean WATCH_TIME_DEFAULT = true;
    /** Whether to display advisory outlines by default */
    public static final boolean ADV_OUTLINE_DEFAULT = true;
    /** Whether to display advisory fill by default */
    public static final boolean ADV_FILL_DEFAULT = false;
    /** Whether to display advisory text by default */
    public static final boolean ADV_TEXT_DEFAULT = true;
    /** Whether to display advisory time by default */
    public static final boolean ADV_TIME_DEFAULT = true;
    /** Whether to display statements/other outlines by default */
    public static final boolean OTHER_OUTLINE_DEFAULT = true;
    /** Whether to display statements/other fill by default */
    public static final boolean OTHER_FILL_DEFAULT = true;
    /** Whether to display statements/other text by default */
    public static final boolean OTHER_TEXT_DEFAULT = true;
    /** Whether to display statements/other time by default */
    public static final boolean OTHER_TIME_DEFAULT = true;
    //gui display variables
    private boolean warnOutline = WARN_OUTLINE_DEFAULT;
    private boolean warnFill = WARN_FILL_DEFAULT;
    private boolean warnText = WARN_TEXT_DEFAULT;
    private boolean warnTime = WARN_TIME_DEFAULT;
    private boolean warnSample = true;
    private boolean watchOutline = WATCH_OUTLINE_DEFAULT;
    private boolean watchFill = WATCH_FILL_DEFAULT;
    private boolean watchText = WATCH_TEXT_DEFAULT;
    private boolean watchTime = WATCH_TIME_DEFAULT;
    private boolean watchSample = true;
    private boolean advOutline = ADV_OUTLINE_DEFAULT;
    private boolean advFill = ADV_FILL_DEFAULT;
    private boolean advText = ADV_TEXT_DEFAULT;
    private boolean advTime = ADV_TIME_DEFAULT;
    private boolean advSample = true;
    private boolean otherOutline = OTHER_OUTLINE_DEFAULT;
    private boolean otherFill = OTHER_FILL_DEFAULT;
    private boolean otherText = OTHER_TEXT_DEFAULT;
    private boolean otherTime = OTHER_TIME_DEFAULT;
    private boolean otherSample = true;
    private boolean enableWarnDisplay = false;
    private boolean enableWatchDisplay = false;
    private boolean enableAdvisoryDisplay = false;
    private boolean enableOtherDisplay = false;
    
    // The significance values for WWAs
    private static final String WARN_SIG = "W";
    private static final String WATCH_SIG = "A";
    private static final String ADVISORY_SIG = "Y";
    
    /** The dialog used to change display properties */
    private DrawingPropertiesDialog drawingDialog;
    
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

    protected static final String DEFAULT_FORMAT = "HHmm'Z'";

    protected static final String LONG_FORMAT = "HH:mm'Z' EEE ddMMMyy";

    protected static final String DAY_FORMAT = "HH:mm'Z' EEE";

    protected List<AbstractWarningRecord> recordsToLoad;

    protected Comparator<AbstractWarningRecord> comparator;

    protected abstract void updateDisplay(IGraphicsTarget target)
            throws VizException;

    protected abstract void initShape(IGraphicsTarget target,
            AbstractWarningRecord record) throws VizException;

    public AbstractWWAResource(WWAResourceData data, LoadProperties props) {
        super(data, props);
        this.recordsToLoad = new ArrayList<>();
        resourceData.addChangeListener(this);
        if (!hasCapability(OutlineCapability.class)) {
            getCapability(OutlineCapability.class).setOutlineWidth(2);
        }
        color = getCapability((ColorableCapability.class)).getColor();
        this.entryMap = new ConcurrentHashMap<>();
    }

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
        if (frameIdx < 0 || frameIdx >= frameTimes.length) {
            return "NO DATA";
        }
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
                    String sig = record.getSig();
                    boolean samplingOn = false;
                    System.out.println("start: "+samplingOn+" "+sig);
                    if(sig !=null){ 
	                    if(sig.equals(WATCH_SIG)){
	                    	if(showWatchSampling()){
		                    	samplingOn = true;
	                    	}
	                    }else if(sig.equals(WARN_SIG)){
	                    	if(showWarnSampling()){
	                    		samplingOn = true;
	                    	}
	                    }else if(sig.equals(ADVISORY_SIG)){
	                    	if(showAdvisorySampling()){
	                    		samplingOn = true;
	                    	}
	                    }else{
	                    	if(showOtherSampling()){
	                    		System.out.println("here4");
	                    		samplingOn = true;
	                    	}
	                    }
                    }else{
                    	if(showOtherSampling()){
                    		System.out.println("here5");
                    		samplingOn = true;
                    	}
                    }
                    
                    System.out.println("sampling: "+samplingOn);
                    
                    if (samplingOn && matchesFrame(entry, time, framePeriod, lastFrame)
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
                                sb.append("\n\n");
                                sb.append(record.getOverviewText());
                                sb.append(record.getSegText());
                                return sb.toString();
                            }
                        }
                    }
                }
            } catch (Exception e) {
                throw new VizException("Error inspecting resource", e);
            }

        }
        return null;
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
            HashMap<String, WarningEntry> candidates = new HashMap<>();
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
                                            entry.record.getInsertTime()))) {
                        candidates.put(key, entry);
                    }
                }
            }
            for (WarningEntry entry : candidates.values()) {
                AbstractWarningRecord record = entry.record;
                boolean drawShape = true;
                boolean drawOutline = true;
                boolean drawText = true;
                boolean drawTime = true;
                
                String sig = record.getSig();
                boolean sigRecognized = false;
                if(record != null && record.getSig() != null){
                	
                	//warning
                	if(sig.equalsIgnoreCase(WARN_SIG)){
                		drawShape = warnFill;
                		drawOutline = warnOutline;
                		drawText = warnText;
                		drawTime = warnTime;
                		sigRecognized = true;
                	}
                	//watch
                	else if(sig.equalsIgnoreCase(WATCH_SIG)){
                		drawShape = watchFill;
                		drawOutline = watchOutline;
                		drawText = watchText;
                		drawTime = watchTime;
                		sigRecognized = true;
                	}
                	//advisory
                	else if(sig.equals(ADVISORY_SIG)){
                		drawShape = advFill;
                		drawOutline = advOutline;
                		drawText = advText;
                		drawTime = advTime;
                		sigRecognized = true;
                	}
                }
                
                if(sig == null || !sigRecognized){
                	drawShape = otherFill;
                	drawOutline = otherOutline;
                	drawText = otherText;
                	drawTime = otherTime;
                }

                // check shapes
                if (entry.project) {
                    initShape(target, entry.record);
                    entry.project = false;
                }
                
                RGB displaycolor = color;
                if ( ! record.getPil().equals("SPS")) {
                	displaycolor = RGBColors.getRGBColor(getPhensigColor(record.getPhensig()));
                }
                
                if(entry != null){
                	//draw shape
                	if(drawShape && entry.shadedShape != null){
                		target.drawShadedShape(entry.shadedShape, 1);
                	}
                	//draw outline
                	if(drawOutline && entry.wireframeShape != null){
                		OutlineCapability oc = getCapability(OutlineCapability.class);
                		LineStyle lineStyle = oc.getLineStyle();
                		int outlineWidth = oc.getOutlineWidth();
                		// Make wire frame outline thicker for EMERGENCY warnings
                		if (EmergencyType.isEmergency(record.getRawmessage())) {
                			outlineWidth *= 3;
                		}

                		target.drawWireframeShape(
                              entry.wireframeShape,
                              displaycolor,
                              outlineWidth, lineStyle);
                	}
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
                    String[] fullText = getText(record, mapWidth);
                    
                    String[] textToPrint = {"",""};
                    if(drawText){
                    	textToPrint[0] = fullText[0];
                    }
                    if(drawTime){
                    	textToPrint[1] = fullText[1];
                    }

                    if (warningsFont == null) {
                        warningsFont = target.initializeFont(target
                                .getDefaultFont().getFontName(), 9,
                                new IFont.Style[0]);
                        emergencyFont = target.getDefaultFont().deriveWithSize(
                                12);
                    }

                    DrawableString params = new DrawableString(textToPrint, displaycolor);
                    params.font = warningsFont;
                    params.setCoordinates(d[0], d[1]);
                    params.horizontalAlignment = HorizontalAlignment.RIGHT;
                    params.verticalAlignment = VerticalAlignment.BOTTOM;
                    params.magnification = getCapability(
                            MagnificationCapability.class).getMagnification();

                    // Draws the string again to have it appear bolder
                    if (EmergencyType.isEmergency(record.getRawmessage())) {
                        // moves over text to add EMER in a different font
                        textToPrint[1] = String.format("%1$-23" + "s",
                                textToPrint[1]);
                        params.setText(textToPrint, displaycolor);

                        DrawableString emergencyString = new DrawableString(
                                params);
                        emergencyString.setCoordinates(d[0],
                                d[1] + (paintProps.getZoomLevel()) * 90);
                        emergencyString.font = emergencyFont;
                        emergencyString.setText(new String[] { "", "",
                                " " + EmergencyType.EMER, "" }, displaycolor);
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
        List<TimeRange> framePeriods = new ArrayList<>(
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
        List<String> toRemove = new ArrayList<>();
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
                	
                    AbstractWarningRecord rec = (AbstractWarningRecord) pdo;
                    this.recordsToLoad.add(rec);
                    
                    //set the drawing display for the corresponding significance types
                    // if all settings are on, no need to keep doing it
                    if(rec !=null && (!enableWatchDisplay || !enableWarnDisplay || !enableAdvisoryDisplay || !enableOtherDisplay)){
	                    String sig = rec.getSig();
	                    boolean sigRecognized = false;
	                    if(sig!=null){
		                    if(sig.equals(WARN_SIG)){
		                    	enableWarnDisplay = true;
		                    	sigRecognized = true;
		                    }
		                    else if(sig.equals(WATCH_SIG)){
		                    	enableWatchDisplay = true;
		                    	sigRecognized = true;
		                    }
		                    else if(sig.equals(ADVISORY_SIG)){
		                    	enableAdvisoryDisplay = true;
		                    	sigRecognized = true;
		                    }
	                    }
	                    if(sig == null || !sigRecognized){
	                    	enableOtherDisplay = true;
	                    }
                    }
                    
                    //update display if it already exists
                    if(drawingDialog != null){
                    	drawingDialog.updateControlsEnabled(enableWatchDisplay, enableWarnDisplay, enableAdvisoryDisplay, enableOtherDisplay);
                    }
                }
            }
        }
    }

    protected PluginDataObject[] sort(PluginDataObject[] pdos) {
        ArrayList<AbstractWarningRecord> sortedWarnings = new ArrayList<>();
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

        PluginDataObject[] pdos;
        try {
            pdos = DataCubeContainer.getData(map);
        } catch (DataCubeException e) {
            throw new VizException(e);
        }
        addRecord(sort(pdos));
    }

    protected String getPhensigColor(String phensig){
        WarningLookups lookup = new WarningLookups();
        return lookup.getPhensig(phensig).color;
    }

    protected String getPhensigName(String phensig){
        WarningLookups lookup = new WarningLookups();
        return lookup.getPhensig(phensig).name;
    }

    protected String[] getText(AbstractWarningRecord record, double mapWidth) {
    	
        String[] textToPrint = new String[] { "", "" };

        if ( ! record.getPil().equals("SPS")) {
            textToPrint[0] = getPhensigName(record.getPhensig());
        } else {
        	textToPrint[0] = "Special Weather Statement";
        }
        
        String startFormatString = DEFAULT_FORMAT;
        String endFormatString = DEFAULT_FORMAT;
        if (mapWidth == 0) {
            startFormatString = LONG_FORMAT;
            endFormatString = DAY_FORMAT;
        } else if (mapWidth <= 200) {
            startFormatString = DAY_FORMAT;
            endFormatString = DAY_FORMAT;
        }

        DateFormat startFormat = new SimpleDateFormat(startFormatString);
        DateFormat endFormat = new SimpleDateFormat(endFormatString);

        startFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        endFormat.setTimeZone(TimeUtil.GMT_TIME_ZONE);
        
        textToPrint[1] = startFormat.format(record.getStartTime().getTime()) 
                + "-" + endFormat.format(record.getEndTime().getTime());;
               

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

    @Override
    protected void disposeInternal() {
        for (WarningEntry entry : entryMap.values()) {
            if (entry.shadedShape != null) {
                entry.shadedShape.dispose();
            }
            if (entry.wireframeShape != null) {
                entry.wireframeShape.dispose();
            }

            /*
             * we set this to true and keep the entries around solely in case
             * this resource is being recycled
             */
            entry.project = true;
        }

        if (warningsFont != null) {
            warningsFont.dispose();
            // set font to null for recycle safety
            warningsFont = null;
        }

        if (emergencyFont != null) {
            emergencyFont.dispose();
            // set font to null for recycle safety
            emergencyFont = null;
        }
        
        if(drawingDialog != null){
        	drawingDialog.hide();
        	drawingDialog = null;
        }
    }

	/**
	 * Set whether or not to display the outline for warnings
	 * @param warnOutline If true, will draw warning outlines
	 */
	public void setWarnOutlineDisplay(boolean warnOutline) {
		this.warnOutline = warnOutline;
	}

	/**
	 * Set whether or not to display the fill (shaded shape)
	 * for warnings
	 * @param warnFill  If true, will draw the warning fill
	 */
	public void setWarnFillDisplay(boolean warnFill) {
		this.warnFill = warnFill;
	}

	/**
	 * Set whether or not to display the text for warnings
	 * @param warnText  If true, will draw the warning title
	 */
	public void setWarnTextDisplay(boolean warnText) {
		this.warnText = warnText;
	}

	/**
	 * Set whether or not to display the time for warnings
	 * @param warnTime  If true, will draw the warning time
	 */
	public void setWarnTimeDisplay(boolean warnTime) {
		this.warnTime = warnTime;
	}
	
	/**
	 * Set whether or not to display the sampling for warnings
	 * @param warnSample  If true, will show the sampling output
	 * for warnings, when sampling is enabled
	 */
	public void setWarnSampleDisplay(boolean warnSample) {
		this.warnSample = warnSample;
	}

	/**
	 * Set whether or not to display the outline for watches
	 * @param watchOutline  If true, will draw the watch outline
	 */
	public void setWatchOutlineDisplay(boolean watchOutline) {
		this.watchOutline = watchOutline;
	}

	/**
	 * Set whether or not to display the fill (shaded shape) for watches
	 * @param watchFill  If true, will draw the watch fill
	 */
	public void setWatchFillDisplay(boolean watchFill) {
		this.watchFill = watchFill;
	}

	/**
	 * Set whether or not to display the text for watches
	 * @param watchText  If true, will draw the watch title
	 */
	public void setWatchTextDisplay(boolean watchText) {
		this.watchText = watchText;
	}

	/**
	 * Set whether or not to display the time for watches
	 * @param watchTime  If true, will draw the watch time
	 */
	public void setWatchTimeDisplay(boolean watchTime) {
		this.watchTime = watchTime;
	}
	
	/**
	 * Set whether or not to display the sampling for watches
	 * @param watchSample  If true, will show the sampling output
	 * for watches, when sampling is enabled
	 */
	public void setWatchSampleDisplay(boolean watchSample) {
		this.watchSample = watchSample;
	}

	/**
	 * Set whether or not to display the outline for advisories
	 * @param advOutline  If true, will draw the advisory outline
	 */
	public void setAdvisoryOutlineDisplay(boolean advOutline) {
		this.advOutline = advOutline;
	}

	/**
	 * Set whether or not to display the fill (shaded shape) for
	 * advisories
	 * @param advFill  If true, will draw the advisory fill
	 */
	public void setAdvisoryFillDisplay(boolean advFill) {
		this.advFill = advFill;
	}

	/**
	 * Set whether or not to display the text for advisories
	 * @param advText  If true, will draw the advisory title
	 */
	public void setAdvisoryTextDisplay(boolean advText) {
		this.advText = advText;
	}

	/**
	 * Set whether or not to display the time for advisories
	 * @param advTime  If true, will draw the advisory time
	 */
	public void setAdvisoryTimeDisplay(boolean advTime) {
		this.advTime = advTime;
	}
	
	/**
	 * Set whether or not to display the sampling for advisories
	 * @param advSample  If true, will show the sampling output
	 * for advisories, when sampling is enabled
	 */
	public void setAdvisorySampleDisplay(boolean advSample) {
		this.advSample = advSample;
	}
	
	/**
	 * Set whether or not to display the outline for statements
	 * and other records
	 * @param advOutline  If true, will draw the outline
	 */
	public void setOtherOutlineDisplay(boolean otherOutline) {
		this.otherOutline = otherOutline;
	}

	/**
	 * Set whether or not to display the fill (shaded shape) for
	 * statements and other records
	 * @param otherFill  If true, will draw the fill
	 */
	public void setOtherFillDisplay(boolean otherFill) {
		this.otherFill = otherFill;
	}

	/**
	 * Set whether or not to display the text for statements
	 * and other records
	 * @param otherText  If true, will draw the title
	 */
	public void setOtherTextDisplay(boolean otherText) {
		this.otherText = otherText;
	}

	/**
	 * Set whether or not to display the time for statements 
	 * and other records
	 * @param otherTime  If true, will draw the time
	 */
	public void setOtherTimeDisplay(boolean otherTime) {
		this.otherTime = otherTime;
	}
	
	/**
	 * Set whether or not to display the sampling for statements/
	 * other records
	 * @param otherSample  If true, will show the sampling output
	 * for statements/other records, when sampling is enabled
	 */
	public void setOtherSampleDisplay(boolean otherSample) {
		this.otherSample = otherSample;
	}
	
	/**
	 * @return  True if the warning outline is displayed
	 */
	public boolean showWarnOutline(){
		return warnOutline;
	}
	
	/**
	 * @return  True if the warning fill is displayed
	 */
	public boolean showWarnFill(){
		return warnFill;
	}
	
	/**
	 * @return  True if the warning text is displayed
	 */
	public boolean showWarnText(){
		return warnText;
	}
	
	/**
	 * @return  True if the warning time is displayed
	 */
	public boolean showWarnTime(){
		return warnTime;
	}
	
	/**
	 * @return  True if the warning sampling is to be displayed
	 */
	public boolean showWarnSampling(){
		return warnSample;
	}
	
	/**
	 * @return  True if the watch outline is displayed
	 */
	public boolean showWatchOutline(){
		return watchOutline;
	}
	
	/**
	 * @return  True if the watch fill is displayed
	 */
	public boolean showWatchFill(){
		return watchFill;
	}
	
	/**
	 * @return  True if the watch text is displayed
	 */
	public boolean showWatchText(){
		return watchText;
	}
	
	/**
	 * @return  True if the watch time is displayed
	 */
	public boolean showWatchTime(){
		return watchTime;
	}
	
	/**
	 * @return  True if the watch sampling is to be displayed
	 */
	public boolean showWatchSampling(){
		return watchSample;
	}
	
	/**
	 * @return  True if the advisory outline is displayed
	 */
	public boolean showAdvisoryOutline(){
		return advOutline;
	}
	
	/**
	 * @return  True if the advisory fill is displayed
	 */
	public boolean showAdvisoryFill(){
		return advFill;
	}
	
	/**
	 * @return True if the advisory text is displayed
	 */
	public boolean showAdvisoryText(){
		return advText;
	}
	
	/**
	 * @return True if the advisory time is displayed
	 */
	public boolean showAdvisoryTime(){
		return advTime;
	}
	
	/**
	 * @return  True if the advisory sampling is to be displayed
	 */
	public boolean showAdvisorySampling(){
		return advSample;
	}
	
	/**
	 * @return  True if the statement/other outline is displayed
	 */
	public boolean showOtherOutline(){
		return otherOutline;
	}
	
	/**
	 * @return  True if the statement/other fill is displayed
	 */
	public boolean showOtherFill(){
		return otherFill;
	}
	
	/**
	 * @return True if the statement/other text is displayed
	 */
	public boolean showOtherText(){
		return otherText;
	}
	
	/**
	 * @return True if the statement/other time is displayed
	 */
	public boolean showOtherTime(){
		return otherTime;
	}
	
	/**
	 * @return  True if the other/statement sampling is to be displayed
	 */
	public boolean showOtherSampling(){
		return otherSample;
	}
	
	/**
	 * @return True if the warning display settings are to 
	 * be enabled
	 */
    public boolean enableWarnDisplay() {
		return enableWarnDisplay;
	}

    /**
	 * @return True if the watch display settings are to 
	 * be enabled
	 */
	public boolean enableWatchDisplay() {
		return enableWatchDisplay;
	}

	/**
	 * @return True if the advisory display settings are
	 * to be enabled
	 */
	public boolean enableAdvisoryDisplay() {
		return enableAdvisoryDisplay;
	}
	
	/**
	 * @return True if the warning statement/other settings
	 * are to be enabled
	 */
	public boolean enableOtherDisplay(){
		return enableOtherDisplay;
	}
	
	/**
	 * Set the associated DrawingPropertiesDialog
	 * @param dialog
	 */
	public void setDrawingDialog(DrawingPropertiesDialog dialog){
		drawingDialog = dialog;
	}
	
	/**
	 * @return The dialog used to set the display (drawing) properties
	 */
	public DrawingPropertiesDialog getDrawingDialog(){
		return drawingDialog;
	}
	
}
