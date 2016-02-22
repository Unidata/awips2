/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package com.raytheon.viz.awipstools.ui.layer;

import java.awt.geom.Point2D;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.geotools.referencing.GeodeticCalculator;
import org.opengis.coverage.grid.GridEnvelope;

import com.raytheon.uf.common.geospatial.LocalTimeZone;
import com.raytheon.uf.common.geospatial.SpatialException;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ProgressiveDisclosureProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.raytheon.uf.viz.core.rsc.tools.GenericToolsResourceData;
import com.raytheon.viz.awipstools.common.stormtrack.AbstractStormTrackResource;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackDisplay;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.LabelMode;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.Mode;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.StormCoord;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackUIManager;
import com.raytheon.viz.awipstools.ui.dialog.TimeOfArrivalDialog;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.input.EditableManager;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;

/**
 * Layer which contains a configurable storm track as well as a Time Of
 * Arrival/Lead Time point. The primary purpose is to display the distance and
 * time it would take to reach the lead point following the storm track.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer   Description
 * ------------- -------- ---------- -------------------------------------------
 * Jun 28, 2010  6513     bkowal     Switching the mode of the Time of Arrival /
 *                                   Lead time tool to Polyline before creating
 *                                   a a track will no longer cause the tool to
 *                                   crash.
 * Jul 28, 2010  4518     bkowal     CDT time will now be displayed in the Time
 *                                   of Arrival point label when the scale is
 *                                   WFO or State(s).
 * Aug 19, 2010  4518     bkowal     Instead of CDT time, the users local time
 *                                   will be displayed. Progressive Disclosure
 *                                   Properties are now used to determine
 *                                   whether local time should be displayed or
 *                                   not.
 * Oct 19, 2010  6753     bkowal     Added logic to change the text alignment
 *                                   from left-to-right if there is not enough
 *                                   room for the text to the left of the point.
 * Mar 15, 2013  15693    mgamazay   Added magnification capability.
 * Apr 12, 2013  16032    dfriedman  Make it work in multiple panes.
 * Aug 14, 2014  3523     mapeters   Updated deprecated DrawableString.textStyle
 *                                   assignments.
 * Dec 02, 2015  5150     bsteffen   Calculate correct lead time for Polylines.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public class TimeOfArrivalLayer extends AbstractStormTrackResource {

    public static class LeadTimeState {
        public Coordinate loc;

        public Coordinate mouseLoc;

        public String text = "Drag me to Point of Arrival";

        public double distance;

        public boolean changed;
    }

    /* Thread Dangerous: Must only be used on the "paint" thread. */
    private final DateFormat timeFormat = new SimpleDateFormat("HH:mm");

    /*
     * Thread Dangerous: Must only be used on the "paint" thread.
     * 
     * The time zone should be set based off the end time before each use.
     */
    private final DateFormat localTimeFormat = new SimpleDateFormat("HH:mmz");

    private static final String formatString = "%sZ (%s) %s miles";

    private static final String formatStringIncludeLocal = "%sZ (%s) (%s) %s miles";

    public static final String NAME = "Time Of Arrival / Lead Time";

    private static final int LEAD_TIME_SIZE = 80;

    private static final int PD_MAX_WIDTH = 1999000;

    private static final UnitConverter metersToMiles = SI.METRE
            .getConverterTo(NonSI.MILE);

    private IWireframeShape jazzyExtras;

    private TimeOfArrivalDialog dialog;

    private LeadTimeState leadState;

    private ProgressiveDisclosureProperties pdProps;

    private Shell shell;

    private Cursor movePoint;

    private boolean hovering = false;

    private InputAdapter adapter = new InputAdapter() {
        @Override
        public boolean handleMouseMove(int x, int y) {
            Coordinate mouse = new Coordinate(x, y);
            hovering = false;
            Coordinate loc = leadState.loc;

            if (loc != null) {
                if (StormTrackUIManager.getCoordinateIndex(
                        TimeOfArrivalLayer.this, new Coordinate[] { loc },
                        mouse) > -1) {
                    shell.setCursor(movePoint);
                    hovering = true;
                }
            }

            return super.handleMouseMove(x, y);
        }

        @Override
        public boolean handleMouseDown(int x, int y, int mouseButton) {

            if (mouseButton == 1 && hovering) {
                leadState.mouseLoc = new Coordinate(leadState.loc);
                issueRefresh();
                return true;
            }

            return false;
        }

        @Override
        public boolean handleMouseDownMove(int arg_x, int arg_y, int mouseButton) {
            double x = arg_x;
            double y = arg_y;
            if (mouseButton == 1 && hovering) {

                // check if point of arrival is off the drawn map, draw at
                // closest border if it is
                IDisplayPane pane = getResourceContainer()
                        .getActiveDisplayPane();
                double[] world = pane.screenToGrid(x, y, 0);
                GridEnvelope ge = pane.getDescriptor().getGridGeometry()
                        .getGridRange();
                IExtent extent = new PixelExtent(ge);
                if (world == null) {
                    return true;
                } else if (extent.contains(world) == false) {
                    // snap x coord to closest edge if out of bounds
                    if (world[0] > extent.getMaxX()) {
                        world[0] = extent.getMaxX();
                    } else if (world[0] < extent.getMinX()) {
                        world[0] = extent.getMinX();
                    }

                    // snap y coord to closest edge if out of bounds
                    if (world[1] > extent.getMaxY()) {
                        world[1] = extent.getMaxY();
                    } else if (world[1] < extent.getMinY()) {
                        world[1] = extent.getMinY();
                    }

                    // translate back to screen coords
                    double[] screen = pane.gridToScreen(world);
                    x = screen[0];
                    y = screen[1];
                }

                // translate from screen to lat/lon
                Coordinate c = getResourceContainer().translateClick(x, y);
                if (c != null) {
                    leadState.mouseLoc = c;
                    issueRefresh();
                }
                return true;
            }

            return false;
        }

        @Override
        public boolean handleMouseUp(int x, int y, int mouseButton) {
            if (mouseButton == 1 && hovering) {
                leadState.loc = leadState.mouseLoc;
                leadState.mouseLoc = null;
                leadState.changed = true;
                if (displayState.displayType != DisplayType.POINT) {
                    displayState.geomChanged = true;
                }
                issueRefresh();
                return true;
            }

            return false;
        }
    };

    public TimeOfArrivalLayer(
            GenericToolsResourceData<TimeOfArrivalLayer> resourceData,
            LoadProperties loadProperties, MapDescriptor descriptor) {
        super(resourceData, loadProperties, descriptor);
        // add magnification capability
        getCapabilities().addCapability(new MagnificationCapability());
        this.pdProps = new ProgressiveDisclosureProperties();
        this.pdProps.setMaxDisplayWidth(TimeOfArrivalLayer.PD_MAX_WIDTH);

        timeFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        leadState = new LeadTimeState();

        shell = VizWorkbenchManager.getInstance().getCurrentWindow().getShell();

        Display display = Display.getCurrent();
        movePoint = new Cursor(display, SWT.CURSOR_HAND);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(adapter);
        }
        reopenDialog();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {

        if (displayState.lineOfStormsLength == -1) {
            double widthInMeters = this.getDescriptor().getMapWidth();
            double zoomLevel = paintProps.getZoomLevel();
            double widthInPixels = paintProps.getCanvasBounds().width;
            double metersPerPixel = widthInMeters / widthInPixels;
            double desiredPixelWidth = widthInPixels / 6;
            double distanceThreshold = (paintProps.getView().getExtent()
                    .getWidth() / paintProps.getCanvasBounds().width) * 10;
            displayState.lineOfStormsLength = (zoomLevel * desiredPixelWidth * metersPerPixel)
                    / (distanceThreshold * 2);
        }

        // If geometry changed, reconstruct line
        boolean recreate = displayState.geomChanged
                || (jazzyExtras == null && displayState.displayType == DisplayType.CIRCULAR)
                || leadState.changed;
        super.paintInternal(target, paintProps);

        if (recreate) {
            updateLeadTimeState();

            if (displayState.mode == Mode.TRACK) {
                if (displayState.displayType == DisplayType.CIRCULAR) {
                    constructCircularStuff(target);
                } else if (displayState.displayType == DisplayType.POLY) {
                    constructPolyStuff(target);
                }
            }
        }

        paintLeadTime(target, paintProps);

        if (displayState.displayType != DisplayType.POINT) {
            if (jazzyExtras == null) {
                /* Do Something Here. */
                jazzyExtras = target.createWireframeShape(false, descriptor);
            }
            target.drawWireframeShape(jazzyExtras, displayState.color,
                    displayState.lineWidth, LineStyle.DASHED_LARGE);
        }
    }

    /**
     * Paints the lead time point and text
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintLeadTime(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (leadState.loc == null) {
            GeodeticCalculator gc = new GeodeticCalculator();
            Coordinate loc = displayState.dragMePoint.getCoordinate();
            gc.setStartingGeographicPoint(loc.x, loc.y);
            // 20 km away
            gc.setDirection(45, 20 * 1000);
            Point2D point = gc.getDestinationGeographicPoint();
            leadState.loc = new Coordinate(point.getX(), point.getY());
        }

        RGB color = leadState.mouseLoc == null ? displayState.color
                : StormTrackDisplay.LIGHT_GRAY;

        double[] p1 = descriptor.worldToPixel(new double[] { leadState.loc.x,
                leadState.loc.y });

        double size = paintProps.getZoomLevel() * LEAD_TIME_SIZE;

        DrawableCircle dc1 = new DrawableCircle();
        dc1.basics.x = p1[0];
        dc1.basics.y = p1[1];
        dc1.radius = size;
        dc1.basics.color = color;
        dc1.filled = true;
        dc1.numberOfPoints = 24;
        target.drawCircle(dc1);

        this.drawLeadStateString(target, paintProps, size, p1, color);

        if (leadState.mouseLoc != null) {
            p1 = descriptor.worldToPixel(new double[] { leadState.mouseLoc.x,
                    leadState.mouseLoc.y });

            DrawableCircle dc2 = new DrawableCircle();
            dc2.basics.x = p1[0];
            dc2.basics.y = p1[1];
            dc2.radius = size;
            dc2.basics.color = displayState.color;
            dc2.filled = true;
            dc2.numberOfPoints = 24;
            target.drawCircle(dc2);

            this.drawLeadStateString(target, paintProps, size, p1,
                    this.displayState.color);
        }
    }

    private void drawLeadStateString(IGraphicsTarget target,
            PaintProperties paintProps, double size, double[] p1, RGB color)
            throws VizException {
        /*
         * Assume the text will be left-aligned ...
         */
        double[] labelLoc = target.getPointOnCircle(p1[0], p1[1], 0.0,
                size * 3, 15);
        HorizontalAlignment alignment = HorizontalAlignment.LEFT;
        /*
         * If we want to place the text on the right side of the point, we need
         * to get a starting coordinate for the text on the right side of the
         * circle which requires a different polar angle.
         */
        double[] labelLocRightAlign = target.getPointOnCircle(p1[0], p1[1],
                0.0, size * 3, 165);

        if (!this.willLeadStateTextFitOnLeft(target, paintProps, labelLoc[0])) {
            labelLoc = labelLocRightAlign;
            alignment = HorizontalAlignment.RIGHT;
        }

        // draw the string
        DrawableString ds = new DrawableString(leadState.text, color);
        ds.basics.x = labelLoc[0];
        ds.basics.y = labelLoc[1];
        ds.horizontalAlignment = alignment;
        ds.font = null;
        ds.magnification = getCapability(MagnificationCapability.class)
        .getMagnification().floatValue();
        target.drawStrings(ds);
    }

    private boolean willLeadStateTextFitOnLeft(IGraphicsTarget target,
            PaintProperties paintProps, double xCoord) {

        GridEnvelope ge = getResourceContainer().getActiveDisplayPane()
                .getDescriptor().getGridGeometry().getGridRange();
        IExtent extent = new PixelExtent(ge);

        int canvasWidth = paintProps.getCanvasBounds().width;
        double extentWidth = paintProps.getView().getExtent().getWidth();
        double ratio = canvasWidth / extentWidth;
        // IExtent extent = paintProps.getView().getExtent();

        // Get the width of the text.
        DrawableString ds = new DrawableString(this.leadState.text, new RGB(
                100, 100, 100));
        double width = target.getStringsBounds(ds).getWidth();
        // Get the width in gl space
        double widthInGl = width / ratio;
        if (xCoord + widthInGl > extent.getMaxX()) {
            return false;
        }

        return true;
    }

    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        dialog.close();

        if (jazzyExtras != null) {
            jazzyExtras.dispose();
        }

        IDisplayPaneContainer container = getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(adapter);
        }
    }

    /**
     * Create lines in jazzyExtras that mirror the dragMeLine projected out to
     * intersect the lead point. Should only be used when the display type is
     * {@link DisplayType#POLY}.
     */
    private void constructPolyStuff(IGraphicsTarget target) {
        if (jazzyExtras != null) {
            jazzyExtras.dispose();
        }

        jazzyExtras = target.createWireframeShape(false, descriptor);
        Coordinate startLonLat = getLeadDragMePoint();
        if(startLonLat == null){
            return;
        }
        Coordinate endLonLat = leadState.loc;
        
        double[] startPixel = descriptor.worldToPixel(new double[] {
                startLonLat.x, startLonLat.y });
        double[] endPixel = descriptor.worldToPixel(new double[] {
                endLonLat.x, endLonLat.y });
        
        double dx = endPixel[0] - startPixel[0];
        double dy = endPixel[1] - startPixel[1];
        
        LineString fullLine = displayState.dragMeLine;
        double[][] pixels = new double[fullLine.getNumPoints()][];
        for (int i = 0; i < fullLine.getNumPoints(); i += 1) {
            Coordinate c = fullLine.getCoordinateN(i);
            pixels[i] = descriptor.worldToPixel(new double[] {
                    c.x, c.y });
            pixels[i][0] += dx;
            pixels[i][1] += dy; 
        }
        jazzyExtras.addLineSegment(pixels);
        
    }

    private void constructCircularStuff(IGraphicsTarget target)
            throws VizException {

        StormCoord start = displayState.timePoints[0];
        StormCoord end1 = displayState.timePoints[displayState.timePoints.length - 1];
        StormCoord end2 = displayState.futurePoints[displayState.futurePoints.length - 1];
        StormCoord end3 = new StormCoord(leadState.loc, null);

        // reconstruct the shape using points
        if (jazzyExtras != null) {
            jazzyExtras.dispose();
        }

        jazzyExtras = target.createWireframeShape(false, descriptor);
        JTSCompiler compiler = new JTSCompiler(null, jazzyExtras, descriptor);

        createCircularLine(compiler, start, end1);
        createCircularLine(compiler, start, end2);
        createCircularLine(compiler, start, end3);

        // Draw line from start to end at angle

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(start.coord.x, start.coord.y);
        gc.setDestinationGeographicPoint(end2.coord.x, end2.coord.y);
        double distance = gc.getOrthodromicDistance();

        gc.setDirection(displayState.angle + 23.0, distance);
        Point2D point = gc.getDestinationGeographicPoint();

        Coordinate[] coords = new Coordinate[2];
        coords[0] = new Coordinate(start.coord);
        coords[1] = new Coordinate(point.getX(), point.getY());

        compiler.handle(new GeometryFactory().createLineString(coords));

        gc.setDirection(displayState.angle - 23.0, distance);
        point = gc.getDestinationGeographicPoint();

        coords = new Coordinate[2];
        coords[0] = new Coordinate(start.coord);
        coords[1] = new Coordinate(point.getX(), point.getY());

        compiler.handle(new GeometryFactory().createLineString(coords));

        jazzyExtras.compile();
    }

    private void createCircularLine(JTSCompiler compiler, StormCoord start,
            StormCoord end) throws VizException {
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(start.coord.x, start.coord.y);

        // get distance
        gc.setDestinationGeographicPoint(end.coord.x, end.coord.y);
        double distance = gc.getOrthodromicDistance();

        // one coord per degree
        Coordinate[] coords = new Coordinate[46];
        int startAngle = (int) unadjustAngle(displayState.angle + 23);
        for (int i = 0; i < 46; ++i) {
            double angle = startAngle - i;
            gc.setDirection(angle, distance);
            Point2D point = gc.getDestinationGeographicPoint();

            coords[i] = new Coordinate(point.getX(), point.getY());
        }

        compiler.handle(new GeometryFactory().createLineString(coords));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.common.stormtrack.AbstractStormTrackResource
     * #getResourceName()
     */
    @Override
    protected String getResourceName() {
        // TODO Auto-generated method stub
        return NAME;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.awipstools.common.stormtrack.AbstractStormTrackResource
     * #initializeState
     * (com.raytheon.viz.awipstools.common.stormtrack.StormTrackState)
     */
    @Override
    protected void initializeState(StormTrackState state) {
        FramesInfo info = descriptor.getFramesInfo();
        // Setup the initial state for the storm track
        // Default angle for POINT
        displayState.displayType = StormTrackState.DisplayType.POINT;
        displayState.labelMode = LabelMode.TIME;
        displayState.liveOffsetEndTime = false;
        state.angle = 48;
        state.dragMePoint = null;
        state.dragMeLine = null;
        // default for POLY, calculated usually
        state.lineOfStormsLength = -1;
        state.mode = StormTrackState.Mode.DRAG_ME;
        state.numDragMePoints = 1;
        state.pivotIndex = trackUtil.getCurrentFrame(info);
        state.otherPivotIndex = displayState.pivotIndex > 0 ? 0 : trackUtil
                .getFrameCount(info) - 1;
        state.thingToDragTo = "feature";
    }

    /**
     * Re-opens the dialog if closed
     */
    public void reopenDialog() {
        // Open the dialog
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (dialog == null || dialog.getShell() == null
                        || dialog.getShell().isDisposed()) {
                    dialog = new TimeOfArrivalDialog(VizWorkbenchManager
                            .getInstance().getCurrentWindow().getShell(),
                            TimeOfArrivalLayer.this);
                    dialog.setBlockOnOpen(false);
                    dialog.open();
                }
            }
        });
    }

    /**
     * Determine a point along the dragMeLine that can be combined with the lead
     * point to create a line that is perpendicular to the storm track. This is
     * the point that is used when determining the distance of the lead point.
     * This method will return null if the lead point is too far from the storm
     * track so no point exists. Should only be used when the display type is
     * {@link DisplayType#POLY}.
     */
    private Coordinate getLeadDragMePoint() {
        /*
         * The input and output points are all in Lon/Lat but all math is
         * performed in the descriptor pixel space to minimize the potential for
         * projection related problems.
         */
        Coordinate startLonLat = displayState.timePoints[0].coord;
        Coordinate endLonLat = displayState.timePoints[displayState.timePoints.length - 1].coord;

        double[] startPixel = descriptor.worldToPixel(new double[] {
                startLonLat.x, startLonLat.y });
        double[] endPixel = descriptor.worldToPixel(new double[] { endLonLat.x,
                endLonLat.y });
        double[] leadPixel = descriptor.worldToPixel(new double[] {
                leadState.loc.x, leadState.loc.y });

        double dx = leadPixel[0] - endPixel[0];
        double dy = leadPixel[1] - endPixel[1];

        double[] leadStartPixel = { startPixel[0] + dx, startPixel[1] + dy };

        /*
         * This is a line that is parallel to the storm track and the same
         * distance as the timePoints in the storm track.
         */
        LineSegment leadLine = new LineSegment(leadStartPixel[0],
                leadStartPixel[1], leadPixel[0], leadPixel[1]);

        LineString fullLine = displayState.dragMeLine;
        Coordinate c = fullLine.getCoordinateN(0);
        double[] prevPixel = descriptor.worldToPixel(new double[] { c.x, c.y });
        Coordinate bestIntersection = null;
        for (int i = 1; i < fullLine.getNumPoints(); i += 1) {
            c = fullLine.getCoordinateN(i);
            double[] currPixel = descriptor.worldToPixel(new double[] { c.x,
                    c.y });
            LineSegment segment = new LineSegment(prevPixel[0], prevPixel[1],
                    currPixel[0], currPixel[1]);
            /*
             * The leadLine may not be long enough to reach from the lead point
             * to the dragMeLine so must use lineIntersection to extend both
             * segments infinitely
             */
            Coordinate intersection = segment.lineIntersection(leadLine);
            /*
             * lineIntersection may extends both lines but only an intersection
             * that is actually on the segment is valid so ensure that the
             * intersection point is reasonably close to the segment.
             */
            if (segment.distance(intersection) < 0.0000001) {
                /*
                 * If the dragMeLine is complex thre can be multiple
                 * intersections, only keep the closest one.
                 */
                if (bestIntersection == null) {
                    bestIntersection = intersection;
                } else if (bestIntersection.distance(new Coordinate(
                        leadPixel[0], leadPixel[1])) > intersection
                        .distance(new Coordinate(leadPixel[0], leadPixel[1]))) {
                    bestIntersection = intersection;

                }
            }
            prevPixel = currPixel;
        }
        if (bestIntersection != null) {
            double[] intersectionLonLat = descriptor.pixelToWorld(new double[] {
                    bestIntersection.x, bestIntersection.y });
            return new Coordinate(intersectionLonLat[0], intersectionLonLat[1]);
        }
        return bestIntersection;
    }

    /**
     * Determine the distance(in meters) from the lead point to the current
     * point, line, or front. This is the distance displayed in the lead time
     * text. This method will return Double.NaN if the lead point is
     * unrealistic.
     */
    private double getLeadDistance() {
        GeodeticCalculator gc = new GeodeticCalculator();
        if (displayState.displayType == DisplayType.POLY) {
            Coordinate dragMe = getLeadDragMePoint();
            if (dragMe == null) {
                return Double.NaN;
            }
            gc.setStartingGeographicPoint(dragMe.x, dragMe.y);
            gc.setDestinationGeographicPoint(leadState.loc.x, leadState.loc.y);
            double distance = gc.getOrthodromicDistance();
            double angle = unadjustAngle(gc.getAzimuth());
            double stateAngle = unadjustAngle(displayState.angle);
            if(Math.abs(angle - stateAngle) > 180){
                distance = -1*distance;
            }
            return distance;
        }else{
            StormCoord start = displayState.timePoints[0];
            Coordinate end = displayState.timePoints[displayState.timePoints.length - 1].coord;

            gc.setStartingGeographicPoint(start.coord.x, start.coord.y);
            gc.setDestinationGeographicPoint(end.x, end.y);
            double distFromStartEnd = gc.getOrthodromicDistance();
            
            end = leadState.loc;
            gc.setDestinationGeographicPoint(end.x, end.y);
            double angle = unadjustAngle(gc.getAzimuth());
            double stateAngle = unadjustAngle(displayState.angle);
            double distance = gc.getOrthodromicDistance();

            if (Math.abs(angle - stateAngle) >= 22.5) {
                return Double.NaN;
            }else{
                return distance
                        - distFromStartEnd;
            }
        }
    }

    private void updateLeadTimeState() {
        leadState.changed = false;

        // based on display state and lead time location, figure out distance,
        // duration, and time

        if (displayState.mode != Mode.TRACK) {
            return;
        }

        double distance = getLeadDistance();
        if (Double.isNaN(distance)) {
            leadState.text = "Unrealistic Point of Arrival";
            return;
        }
        StormCoord endStormCoord = displayState.timePoints[displayState.timePoints.length - 1];

        Date refDate = endStormCoord.time.getValidTimeAsDate();
        long timeInSec = (long) (distance / displayState.speed);

        Date date = new Date(refDate.getTime() + (timeInSec * 1000));

        boolean negative = date.getTime() < refDate.getTime();

        String miles = "";
        String hoursMinutesFormat = "";
        if (negative) {
            hoursMinutesFormat += "-";
            miles += "-";
        }

        long timeDiffInMinutes = Math.abs(date.getTime() - refDate.getTime())
                / (60 * 1000);

        long hours = timeDiffInMinutes / 60;
        long minutes = timeDiffInMinutes % 60;
        Object[] args = new Object[2];
        if (hours > 0) {
            hoursMinutesFormat += "%02d hours ";
            args[0] = hours;
            args[1] = minutes;
        } else {
            args[0] = minutes;
        }

        hoursMinutesFormat += "%02d minutes";

        miles += (int) metersToMiles.convert(Math.abs(distance));

        String time = this.timeFormat.format(date);
        int currentDisplayWidth = ((IMapDescriptor) this.descriptor)
                .getMapWidth();

        if (!this.pdProps.isDisclosed(currentDisplayWidth)) {
            // Do not include Local time
            leadState.text = String.format(formatString, time,
                    String.format(hoursMinutesFormat, args), miles);
        } else {
            // Include Local time
            String localTime = "";
            try {
                Coordinate end = endStormCoord.coord;
                localTimeFormat
                        .setTimeZone(LocalTimeZone.getLocalTimeZone(end));
                localTime = localTimeFormat.format(date);
            } catch (SpatialException e) {
                e.printStackTrace();
            }

            leadState.text = String.format(
                    TimeOfArrivalLayer.formatStringIncludeLocal, time,
                    localTime, String.format(hoursMinutesFormat, args), miles);
        }
    }

    public void makeEditableAndReopenDialog() {
        EditableManager.makeEditable(this, true);
        reopenDialog();
    }
}
