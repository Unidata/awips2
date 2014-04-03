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
package com.raytheon.viz.awipstools.common.stormtrack;

import java.awt.geom.Point2D;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.RGB;
import org.geotools.referencing.GeodeticCalculator;

import com.raytheon.uf.common.geospatial.DestinationGeodeticCalculator;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.DrawableCircle;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.awipstools.common.StormTrackData;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.Mode;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.StormCoord;
import com.raytheon.viz.core.rsc.jts.JTSCompiler;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * Working version of StormTrackDisplay, correctly draws and keeps track of
 * track coordinates, speed, and angle. NEEDS to have WarngenLayer rewritten to
 * use this class instead of StormTrackDisplay
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05-28-2010   #6042      bkowal      When an Impossible Track Exception
 *                                     Is Encountered, The Track Will Now
 *                                     Be Reverted Back To Its Previous
 *                                     State. Replaced Impossible Storm
 *                                     Exception With Impossible Track
 *                                     Exception.
 *  06-23-2010  #6468      bkowal      The Tool Will No Longer Generate
 *                                     An Exception When It Is Initially
 *                                     Added To The Map While Looping
 *                                     Is Running.
 *  06-23-2010  #5925      bkowal      The Tool Will No Longer Generate An
 *                                     Exception When The Polyline Mode
 *                                     Is Enabled Even When A Track
 *                                     Has Not Been Generated.
 *  07-14-2010  #6558      bkowal      The tool will no longer generate an
 *                                     exception when it is moved from a
 *                                     pane with a larger frame count to a
 *                                     pane with a smaller frame count. 
 *                                     The track will now always be centered around
 *                                     the initial location of the drag-me point
 *                                     when the track was created.
 *  10-27-2010  #6964      bkowal      The LineStyle is now passed as a parameter to
 *                                     the IGraphicsTarget drawWireframeShape method.
 *  15Mar2013	15693	mgamazaychikov Made sure that magnification capability works.
 *  06-11-2013  DR 16234   D. Friedman Fix pivot index when frames count is reduced.
 *  06-24-2013  DR 16317   D. Friedman Handle "motionless" track.
 *  01-28-2014  DR16465 mgamazaychikov Fixed the problem with anchor point when frame 
 *                                     count changes; made line width configurable.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class StormTrackDisplay implements IRenderable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(StormTrackDisplay.class);

    private static final GeometryFactory gf = new GeometryFactory();

    private static final double SQRT_2 = Math.sqrt(2);

    public static final RGB LIGHT_GRAY = new RGB(200, 200, 200);

    private static final UnitConverter mpsToKts = SI.METERS_PER_SECOND
            .getConverterTo(NonSI.KNOT);

    private static final UnitConverter ktsToMps = mpsToKts.inverse();

    private static final int editableCircleSize = 45;

    private static final int unEditableCircleSize = 25;

    private static final String dragTextFormat = "Drag %s to %s";

    private static final String speedFormat = "%dkts@%03d";

    private ToolsDataManager dataManager = ToolsDataManager.getInstance();

    private final DateFormat timeFormat = new SimpleDateFormat("HH:mm");

    private IMapDescriptor descriptor;

    private StormTrackUIManager manager;

    private IWireframeShape cachedTrack;

    private int lastFrame = -1;

    private DataTime[] currentDisplayedTimes;

    private Coordinate theAnchorPoint = null;

    private int theAnchorIndex = -1;

    private StormTrackUtil trackUtil;

    private double currentTickLen;

    public StormTrackDisplay(IMapDescriptor descriptor,
            StormTrackUIManager manager) {
        this.descriptor = descriptor;
        this.timeFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        this.manager = manager;
        trackUtil = this.manager.getTrackUtil();
    }

    public void setDescriptor(MapDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    public void dispose() {
        if (cachedTrack != null) {
            cachedTrack.dispose();
        }
    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {
        paint(target, (StormTrackProperties) paintProps);
    }

    private void paint(IGraphicsTarget target, StormTrackProperties paintProps)
            throws VizException {
        StormTrackState currentState = paintProps.getState();

        DataTime[] times = trackUtil.getDataTimes(paintProps.getFramesInfo());

        // short circuit if no data times to paint
        if (times.length == 0) {
            return;
        }

        int currentFrame = trackUtil
                .getCurrentFrame(paintProps.getFramesInfo());

        // If the state's time points are set and the times don't quite match up
        if (currentState.timePoints != null
                && (currentState.timePoints.length != times.length || !times[currentFrame]
                        .equals(currentState.timePoints[currentFrame].time,
                                true))) {

            int oldPivot = currentState.pivotIndex;
            trackUtil.setPivotIndexes(paintProps.getFramesInfo(), currentState);

            if (currentState.otherPivotIndex == currentState.pivotIndex) {
                if (currentState.displayedPivotIndex == oldPivot) {
                    currentState.pivotIndex = 0;
                } else {
                    currentState.otherPivotIndex = 0;
                }
            }

            if (currentState.displayedPivotIndex == oldPivot) {
                currentState.displayedPivotIndex = currentState.pivotIndex;
            }

            if (currentFrame == currentState.displayedPivotIndex) {
                if (currentState.displayedPivotIndex == currentState.pivotIndex) {
                    currentState.displayedPivotIndex = currentState.otherPivotIndex;
                } else {
                    currentState.displayedPivotIndex = currentState.pivotIndex;
                }
            }

            if (currentState.displayedPivotIndex >= times.length) {
                currentState.displayedPivotIndex = Math.max(0,
                        currentFrame != times.length - 1 ?
                        times.length - 1 : times.length - 2);
            }

            currentState.geomChanged = true;
            target.setNeedsRefresh(true);
        } else if (lastFrame != -1 && lastFrame != currentFrame) {
            // This is a newly painted frame, update the display if we need to
            if (currentState.nextPivotIndex != -1) {
                // we acquired a new pivot index while painting the last frame
                currentState.pivotIndex = currentState.nextPivotIndex;
                currentState.otherPivotIndex = currentState.displayedPivotIndex;
                currentState.displayedPivotIndex = currentState.pivotIndex;
                currentState.nextPivotIndex = -1;
            } else if (currentFrame == currentState.displayedPivotIndex) {
                if (currentState.displayedPivotIndex == currentState.pivotIndex) {
                    currentState.displayedPivotIndex = currentState.otherPivotIndex;
                } else {
                    currentState.displayedPivotIndex = currentState.pivotIndex;
                }
            } else if (currentFrame != currentState.displayedPivotIndex) {
                if (currentState.displayedPivotIndex == currentState.otherPivotIndex) {
                    currentState.displayedPivotIndex = currentState.pivotIndex;
                }
            }

            if (currentState.timePoints != null) {
                currentState.dragMePoint = gf
                        .createPoint(currentState.timePoints[currentFrame].coord);
            }
            if (currentState.dragMeLine != null
                    && currentState.dragMePoint != null) {
                currentState.dragMeLine = manager.figureLineFromPoint(
                        currentState.dragMeLine, currentState.dragMePoint);
            }
            currentState.geomChanged = true;
            target.setNeedsRefresh(true);
        }

        switch (currentState.displayType) {
        case CIRCULAR: {
            paintPointMode(target, paintProps);
            break;
        }
        case POINT: {
            paintPointMode(target, paintProps);
            break;
        }
        case POLY: {
            paintPolyMode(target, paintProps);
            break;
        }
        }

        lastFrame = currentFrame;
    }

    /**
     * Paints the storm track display for Point mode
     * 
     * @param target
     * @param paintProps
     */
    private void paintPointMode(IGraphicsTarget target,
            StormTrackProperties paintProps) throws VizException {
        StormTrackState currentState = paintProps.getState();

        switch (currentState.mode) {
        case DRAG_ME: {
            paintDragMePoint(target, paintProps);
            paintDragMeText(target, paintProps, currentState.dragMePoint);
            break;
        }
        case TRACK: {
            paintTrack(target, paintProps);
            paintDragMePoint(target, paintProps);
            // if (trackUtil.getDataTimes().length == 1) {
            // paintDragMeText(target, paintProps, currentState.dragMePoint);
            // }
            if (currentState.isInitiallyMotionless() && ! currentState.isNonstationary()) {
                int currentFrame = trackUtil
                        .getCurrentFrame(paintProps.getFramesInfo());
                if (currentFrame != currentState.intialFrame) {
                    paintDragMeText(target, paintProps, currentState.dragMePoint);
                }
            }
            break;
        }
        }
        currentState.dragMeGeom = currentState.dragMePoint;
    }

    /**
     * Paints the storm track display for Poly mode
     * 
     * @param target
     * @param paintProps
     */
    private void paintPolyMode(IGraphicsTarget target,
            StormTrackProperties paintProps) throws VizException {
        StormTrackState currentState = paintProps.getState();

        switch (currentState.mode) {
        case DRAG_ME: {
            paintDragMeLine(target, paintProps);
            paintDragMeText(target, paintProps, currentState.dragMeLine);
            break;
        }
        case TRACK: {
            paintTrack(target, paintProps);
            paintDragMeLine(target, paintProps);
            if (trackUtil.getDataTimes(paintProps.getFramesInfo()).length == 1) {
                paintDragMeText(target, paintProps, currentState.dragMeLine);
            }
            break;
        }
        }
        currentState.dragMeGeom = currentState.dragMeLine;
    }

    /**
     * Like paintDragMePoint, this will paint the drag line which is a little
     * different than drawing a line then drawing points. If editable, points
     * will look like drag me points but it not, will be smaller filled in
     * circle
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintDragMeLine(IGraphicsTarget target,
            StormTrackProperties paintProps) throws VizException {
        StormTrackState state = paintProps.getState();

        final double circleSize = state.isEditable() ? editableCircleSize
                * paintProps.getZoomLevel() : unEditableCircleSize
                * paintProps.getZoomLevel();

        LineString line = state.dragMeLine;
        if (line == null) {
            GeodeticCalculator gc = new GeodeticCalculator(descriptor.getCRS());
            int numPoints = state.numDragMePoints;
            double[] worldPixel = descriptor.pixelToWorld(paintProps.getView()
                    .getExtent().getCenter());
            double lengthInMeters = state.distanceThreshold
                    * state.lineOfStormsLength;
            gc.setStartingGeographicPoint(worldPixel[0], worldPixel[1]);
            double startAngle = state.angle;
            double firstAngle = Double.NaN;
            double secondAngle = Double.NaN;
            if (Double.isNaN(startAngle)) {
                firstAngle = 180;
                secondAngle = 360;
            } else {
                firstAngle = startAngle - 90;
                secondAngle = startAngle + 90;
            }
            gc.setDirection(adjustAngle(firstAngle), lengthInMeters);
            Coordinate c1 = new Coordinate(gc.getDestinationGeographicPoint()
                    .getX(), gc.getDestinationGeographicPoint().getY());

            gc.setStartingGeographicPoint(worldPixel[0], worldPixel[1]);
            gc.setDirection(adjustAngle(secondAngle), lengthInMeters);
            Coordinate c3 = new Coordinate(gc.getDestinationGeographicPoint()
                    .getX(), gc.getDestinationGeographicPoint().getY());

            Coordinate[] coords = null;
            // unless explicitly set to 2, assume 3 points
            if (numPoints == 2) {
                coords = new Coordinate[] { c1, c3 };
            } else {
                coords = new Coordinate[] { c1,
                        new Coordinate(worldPixel[0], worldPixel[1]), c3 };
            }
            line = new GeometryFactory().createLineString(coords);

            if (state.dragMePoint != null) {
                state.dragMeLine = manager.figureLineFromPoint(line,
                        state.dragMePoint);
                line = state.dragMeLine;
            } else {
                state.dragMeLine = line;
            }
        }

        // Time to paint the line. Again, need to handle
        // if mouseDownGeom set or not
        RGB lineColor = state.mouseDownGeom != null ? LIGHT_GRAY : state.color;
        // Paint the non dragging geom
        paintLine(target, line.getCoordinates(), lineColor, state.lineWidth, state.isEditable(),
                circleSize, LineStyle.SOLID);

        if (state.mouseDownGeom != null) {
            // Paint the dragging geom
            paintLine(target, state.mouseDownGeom.getCoordinates(),
                    state.color, state.lineWidth, state.isEditable(), circleSize,
                    LineStyle.DOTTED);
            lineColor = LIGHT_GRAY;
        }

        if (state.mode == Mode.TRACK && state.futurePoints != null) {
            LineString endLine = manager
                    .figureLineFromPoint(
                            line,
                            gf.createPoint(state.futurePoints[state.futurePoints.length - 1].coord));
            if (endLine != null) {
                paintLine(target, endLine.getCoordinates(), lineColor, state.lineWidth, false,
                        0, LineStyle.DASHED_LARGE);
            }
        }

    }

    /**
     * Paints a line with points at each vertex in the line on the screen
     * 
     * @param target
     * @param coords
     *            coordinates in the line
     * @param color
     *            color of the line
     * @param lineWidth 
     * @param editable
     *            if the line is editable
     * @throws VizException
     */
    private void paintLine(IGraphicsTarget target, Coordinate[] coords,
            RGB color, float lineWidth, boolean editable, double circleSize, LineStyle style)
            throws VizException {
        DrawableCircle circle = new DrawableCircle();
        circle.basics.color = color;
        circle.radius = circleSize;
        circle.filled = true;

        Coordinate lastCoord = null;
        for (int i = 0; i < coords.length; ++i) {
            Coordinate currCoord = coords[i];
            if (currCoord != null) {
                // paint point
                if (editable) {
                    paintPoint(target, currCoord, color, circleSize);
                } else {
                    double[] p1 = descriptor.worldToPixel(new double[] {
                            currCoord.x, currCoord.y });
                    circle.setCoordinates(p1[0], p1[1]);
                    target.drawCircle(circle);
                }

                // paint line if lastCoord not null
                if (lastCoord != null) {
                    double[] p1 = descriptor.worldToPixel(new double[] {
                            currCoord.x, currCoord.y });
                    double[] p2 = descriptor.worldToPixel(new double[] {
                            lastCoord.x, lastCoord.y });

                    target.drawLine(p1[0], p1[1], 0.0, p2[0], p2[1], 0.0,
                            color, lineWidth, style);
                }
            }
            lastCoord = currCoord;
        }
    }

    /**
     * Paints the drag me point on the screen, if dragMeGeom null in
     * StormTrackState, will use center of screen and set the point
     * 
     * @param target
     * @param paintProps
     */
    private void paintDragMePoint(IGraphicsTarget target,
            StormTrackProperties paintProps) throws VizException {
        final double circleSize = editableCircleSize
                * paintProps.getZoomLevel();

        StormTrackState state = paintProps.getState();

        Coordinate point = state.dragMePoint == null ? null : state.dragMePoint
                .getCoordinate();
        if (point == null) {
            // Calculate point
            if (state.dragMeLine != null) {
                manager.figurePointFromLine();
            } else {
                double[] p = descriptor.pixelToWorld(paintProps.getView()
                        .getExtent().getCenter());
                point = new Coordinate(p[0], p[1]);
                state.dragMePoint = new GeometryFactory().createPoint(point);
            }
        }

        RGB pointColor = state.mouseDownGeom != null ? LIGHT_GRAY : state.color;
        paintPoint(target, point, pointColor, circleSize);
        if (state.mouseDownGeom != null) {
            paintPoint(target, state.mouseDownGeom.getCoordinate(),
                    state.color, circleSize);
            pointColor = LIGHT_GRAY;
        }
    }

    /**
     * 
     * @param target
     * @param point
     * @param color
     * @param circleSize
     * @throws VizException
     */
    private void paintPoint(IGraphicsTarget target, Coordinate point,
            RGB color, double circleSize) throws VizException {
        if (point == null) {
            return;
        }
        // Paint the point
        double[] p1 = descriptor
                .worldToPixel(new double[] { point.x, point.y });

        if (p1 == null) {
            return;
        }

        DrawableCircle circle = new DrawableCircle();
        circle.basics.color = color;
        circle.setCoordinates(p1[0], p1[1]);
        circle.radius = 2 * circleSize;
        target.drawCircle(circle);

        circle.filled = true;
        circle.radius = circleSize;
        target.drawCircle(circle);
    }

    /**
     * Paint the drag me text
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintDragMeText(IGraphicsTarget target,
            StormTrackProperties paintProps, Geometry dragMeGeom)
            throws VizException {
        final double circleSize = editableCircleSize
                * paintProps.getZoomLevel();

        StormTrackState state = paintProps.getState();
        // get the magnification from the state
        float magnification = state.magnification;

        String text = String.format(dragTextFormat, state.displayType.dragWhat,
                state.thingToDragTo);
        Geometry geom = dragMeGeom;
        Geometry mouseDownGeom = state.mouseDownGeom;
        RGB geomColor = mouseDownGeom != null ? LIGHT_GRAY : state.color;

        // draw text using this drag geom
        Coordinate[] coords = geom.getCoordinates();
        Coordinate toUse = null;
        if (coords.length == 1) {
            toUse = coords[0];
        } else if (coords.length > 1) {
            // pick half way between last 2
            Coordinate last = coords[coords.length - 1];
            Coordinate secondLast = coords[coords.length - 2];

            toUse = new Coordinate(last.x - (last.x - secondLast.x) / 2, last.y
                    - (last.y - secondLast.y) / 2);
        }
        paintTextAtPoint(target, text, toUse, geomColor, circleSize * 3, 15.0, magnification);

        if (mouseDownGeom != null) {
            // draw text using mouse geom
            coords = mouseDownGeom.getCoordinates();
            toUse = null;
            if (coords.length == 1) {
                toUse = coords[0];
            } else if (coords.length > 1) {
                // pick half way between last 2
                Coordinate last = coords[coords.length - 1];
                Coordinate secondLast = coords[coords.length - 2];

                toUse = new Coordinate(last.x - (last.x - secondLast.x) / 2,
                        last.y - (last.y - secondLast.y) / 2);
            }
            paintTextAtPoint(target, text, toUse, state.color, circleSize * 3,
                    15.0, magnification);
        }

    }

    /**
     * Paints text at an offset from a point with left alignment
     * 
     * @param target
     * @param text
     * @param point
     * @param color
     * @param radiusFromPoint
     * @param angle
     * @throws VizException
     */
    private void paintTextAtPoint(IGraphicsTarget target, String text,
            Coordinate point, RGB color, double radiusFromPoint, double angle, float magnification)
            throws VizException {
        paintTextAtPoint(target, text, point, color, radiusFromPoint, angle,
                HorizontalAlignment.LEFT, VerticalAlignment.BOTTOM, magnification);
    }

    /**
     * Paints text at an offset from a point
     * 
     * @param target
     * @param text
     * @param point
     * @param color
     * @param radiusFromPoint
     * @param angle
     * @param hAlignment
     *            horizontal alignment
     * @param vAlignment
     *            vertical alignment
     * 
     * @throws VizException
     */
    private void paintTextAtPoint(IGraphicsTarget target, String text,
            Coordinate point, RGB color, double radiusFromPoint, double angle,
            HorizontalAlignment hAlignment, VerticalAlignment vAlignment, float magnification)
            throws VizException {
        // get screen location of point
        double[] p1 = descriptor
                .worldToPixel(new double[] { point.x, point.y });

        // get screen location of where to draw text using point
        double[] labelLoc = target.getPointOnCircle(p1[0], p1[1], 0.0,
                radiusFromPoint, angle);
        DrawableString str = new DrawableString(text, color);
        str.horizontalAlignment = hAlignment;
        str.verticallAlignment = vAlignment;
        // set the string magnification
        str.magnification = magnification;
        str.setCoordinates(labelLoc[0], labelLoc[1]);

        // draw the string
        target.drawStrings(str);
    }

    /**
     * Calculates and paints the track, this function will also post the points
     * location to the ToolsDataManager
     * 
     * @param target
     * @param paintProps
     * @throws VizException
     */
    private void paintTrack(IGraphicsTarget target,
            StormTrackProperties paintProps) throws VizException {
        StormTrackState state = paintProps.getState();

        if (!state.trackVisible
                || trackUtil.getDataTimes(paintProps.getFramesInfo()).length == 1) {
            return;
        }

        if (state.geomChanged) {
            if (cachedTrack != null) {
                cachedTrack.dispose();
            }
            generateTrackInfo(state, paintProps);
            if (state.mode == Mode.TRACK) {
                createTrack(target, paintProps);
            }
            state.geomChanged = false;
        }
        if (state.mode == Mode.TRACK
                && (!state.isInitiallyMotionless() || state.isNonstationary())) {
            target.drawWireframeShape(cachedTrack, state.color,
                    state.lineWidth, state.lineStyle);
            paintLabels(target, paintProps);
        }
    }

    /**
     * Generates the start track info from the state
     * 
     * @param currentState
     */
    private void generateTrackInfo(StormTrackState currentState,
            PaintProperties paintProps) throws VizException {
        int frameCount = trackUtil.getFrameCount(paintProps.getFramesInfo());
        int currFrame = trackUtil.getCurrentFrame(paintProps.getFramesInfo());
        try {
            boolean moved = false;
            boolean update = isUpdatedDataTimes(paintProps);
            if (currentState.timePoints == null
                    || currentState.timePoints.length != frameCount
                    || currentState.newDuration != -1 || update) {
                if (currentState.timePoints != null
                        && currentState.timePoints.length != frameCount) {
                    // need to set theAnchorPoint and theAnchorIndex here
                    // because timePoints get erased before we get to updateAnchorPoint
                    DataTime frameTime = paintProps.getDataTime();
                    for (int j=0;j<currentState.timePoints.length;j++){
                        if (frameTime.equals(currentState.timePoints[j].time)) {
                            theAnchorPoint = currentState.timePoints[j].coord;
                            theAnchorIndex = j;
                        }
                    }
                    currentState.timePoints = null;
                }

                if (currentState.newDuration != -1) {
                    currentState.duration = currentState.newDuration;
                    currentState.newDuration = -1;
                }

                if (currentState.endTime != null) {
                    DataTime[] frameTimes = trackUtil.getDataTimes(paintProps
                            .getFramesInfo());
                    Calendar lastFrame = frameTimes[frameTimes.length - 1]
                            .getValidTime();
                    int newDuration = (int) (currentState.endTime
                            .getTimeInMillis() - lastFrame.getTimeInMillis())
                            / (60 * 1000);
                    currentState.duration = newDuration;
                }

                if (this.theAnchorPoint == null || currentState.resetAnchor) {
                    if (currentState.dragMePoint == null) {
                        double[] p = descriptor.pixelToWorld(paintProps
                                .getView().getExtent().getCenter());
                        currentState.dragMePoint = new GeometryFactory()
                                .createPoint(new Coordinate(p[0], p[1]));
                    }
                    theAnchorPoint = currentState.dragMePoint.getCoordinate();
                    currentState.dragMePoint = gf
                            .createPoint(this.theAnchorPoint);
                    currentState.timePoints = null;
                    currentState.pointMoved = false;
                    currentState.resetAnchor = false;
                } else if (update && currentState.timePoints != null) {
                    theAnchorPoint = updateAnchorPoint(currentState, paintProps);
                    currentState.timePoints = null;
                }

                generateNewTrackInfo(currentState, currFrame, paintProps);
                currentDisplayedTimes = trackUtil.getDataTimes(paintProps
                        .getFramesInfo());
                generateTrackInfo(currentState, paintProps);
            } else {

                if (currentState.pointMoved) {
                    this.theAnchorPoint = currentState.dragMePoint
                            .getCoordinate();
                    generateExistingTrackInfo(currentState, paintProps);
                    currentState.pointMoved = false;
                    currentState.originalTrack = false;
                    moved = true;
                }

                currentDisplayedTimes = trackUtil.getDataTimes(paintProps
                        .getFramesInfo());
                if (moved) {
                    this.theAnchorIndex = currFrame;
                    this.theAnchorPoint = currentState.timePoints[theAnchorIndex].coord;
                }
            }

        } catch (ImpossibleTrackException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Track produced an impossible scenario", e);
        }

        if (currentState.timePoints != null) {
            currentState.dragMePoint = gf
                    .createPoint(currentState.timePoints[currFrame].coord);
            if (currentState.dragMeLine != null) {
                currentState.dragMeLine = manager.figureLineFromPoint(
                        currentState.dragMeLine, currentState.dragMePoint);
            }
        } else {
            currentState.mode = StormTrackState.Mode.DRAG_ME;
            currentState.dragMePoint = gf.createPoint(this.theAnchorPoint);
        }
    }

    private void generateExistingTrackInfo(StormTrackState state,
            PaintProperties paintProps) throws ImpossibleTrackException {
        int moveIndex = this.trackUtil.getCurrentFrame(paintProps
                .getFramesInfo());
        int pivotIndex = state.displayedPivotIndex;

        double angle;
        double oppositeAngle;
        double speed;
        int startCoordIndex;
        int endCoordIndex;

        DataTime[] dataTimes = trackUtil.getDataTimes(paintProps
                .getFramesInfo());
        state.timePoints[moveIndex].coord = state.dragMePoint.getCoordinate();

        boolean hasMotion = ! state.isInitiallyMotionless() || state.isNonstationary()
                || moveIndex != state.intialFrame;
        if (hasMotion) {
            startCoordIndex = pivotIndex < moveIndex ? pivotIndex : moveIndex;
            endCoordIndex = pivotIndex < moveIndex ? moveIndex : pivotIndex;
        } else {
            startCoordIndex = endCoordIndex = moveIndex;
        }

        StormCoord startCoord = state.timePoints[startCoordIndex];
        StormCoord endCoord = state.timePoints[endCoordIndex];

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(startCoord.coord.x, startCoord.coord.y);
        gc.setDestinationGeographicPoint(endCoord.coord.x, endCoord.coord.y);

        if (hasMotion) {
            // get speed and angle
            angle = gc.getAzimuth();
            oppositeAngle = adjustAngle(angle + 180);

            speed = gc.getOrthodromicDistance()
                    / trackUtil
                            .timeBetweenDataTimes(startCoord.time, endCoord.time);

            // Tempory fix to prevent a resource error because
            // the time between data times is 0.
            if (Double.isNaN(speed)) {
                return;
            }
        } else {
            angle = 0;
            oppositeAngle = 0;
            speed = 0;
        }
        StormCoord[] timePoints = new StormCoord[state.timePoints.length];

        timePoints[startCoordIndex] = startCoord;
        timePoints[endCoordIndex] = endCoord;

        // First, use startCoord and start at end coord -1 and come towards
        // startCoord
        DestinationGeodeticCalculator dc = new DestinationGeodeticCalculator();
        dc.setStartingGeographicPoint(startCoord.coord.x, startCoord.coord.y);

        for (int i = timePoints.length - 1; i >= 0; --i) {
            if (i == startCoordIndex || i == endCoordIndex)
                continue;

            DataTime coordTime = state.timePoints[i].time;
            double distance = speed
                    * trackUtil
                            .timeBetweenDataTimes(startCoord.time, coordTime);
            if (i > startCoordIndex) {
                dc.setDirection(angle, distance);
            } else if (i < startCoordIndex) {
                dc.setDirection(oppositeAngle, distance);
            }
            Point2D point = dc.getDestinationGeographicPoint();
            timePoints[i] = new StormCoord(new Coordinate(point.getX(),
                    point.getY()), coordTime);
        }

        int minIntervalInSeconds = trackUtil.minIntervalInSeconds(dataTimes);
        if (minIntervalInSeconds < 60) {
            minIntervalInSeconds = 60;
        }

        if (state.duration == -1) {
            state.duration = trackUtil.timeBetweenDataTimes(dataTimes[0],
                    dataTimes[dataTimes.length - 1]) / 60;
        }

        int interval = (int) Math.round(minIntervalInSeconds / 60 + 0.4d);
        int additionalDivisions = 1;
        int remainder = 0;

        if (state.duration >= 0) {
            additionalDivisions = state.duration / interval;
            remainder = state.duration % (minIntervalInSeconds / 60);
        }

        StormCoord[] futurePoints = new StormCoord[1 + additionalDivisions
                + (remainder == 0 ? 0 : 1)];

        // Now do same thing with future points, only have to go forward in
        // time.
        futurePoints[0] = timePoints[timePoints.length - 1];

        long timeInMillis = futurePoints[0].time.getMatchValid();

        // duration is negative when the last frame time is older than the end
        // time of a follow up warning. if the last frame is older than the end
        // time, the arrow of the pathcast is drawn behind the last frame
        if (state.duration >= 0) {
            for (int i = 1; i < futurePoints.length - (remainder == 0 ? 0 : 1); ++i) {
                timeInMillis += minIntervalInSeconds * 1000;
                DataTime time = new DataTime(new Date(timeInMillis));

                double distance = speed
                        * trackUtil.timeBetweenDataTimes(startCoord.time, time);
                dc.setDirection(angle, distance);
                Point2D point = dc.getDestinationGeographicPoint();
                futurePoints[i] = new StormCoord(new Coordinate(point.getX(),
                        point.getY()), time);
            }

            if (remainder != 0) {
                timeInMillis += (remainder * 60 * 1000);
                DataTime time = new DataTime(new Date(timeInMillis));

                double distance = speed
                        * trackUtil.timeBetweenDataTimes(startCoord.time, time);
                dc.setDirection(angle, distance);
                Point2D point = dc.getDestinationGeographicPoint();
                futurePoints[futurePoints.length - 1] = new StormCoord(
                        new Coordinate(point.getX(), point.getY()), time);
            }
        } else {
            timeInMillis -= Math.abs(state.duration) * 60 * 1000;
            DataTime time = new DataTime(new Date(timeInMillis));
            double distance = speed
                    * trackUtil
                            .timeBetweenDataTimes(futurePoints[0].time, time);
            gc.setDirection(oppositeAngle, distance);
            Point2D point = gc.getDestinationGeographicPoint();
            futurePoints[1] = new StormCoord(new Coordinate(point.getX(),
                    point.getY()), time);
        }

        state.timePoints = timePoints;
        state.futurePoints = futurePoints;

        state.angle = angle;
        state.speed = speed;

        postData(state);
    }

    private void generateNewTrackInfo(StormTrackState state, int anchorIndex,
            PaintProperties paintProps) throws ImpossibleTrackException {
        double speed, angle, oppositeAngle;
        int frameCount = trackUtil.getFrameCount(paintProps.getFramesInfo());
        if (state.timePoints != null) {
            theAnchorIndex = frameCount
                    - (state.timePoints.length - theAnchorIndex);
            if (theAnchorIndex >= 0 && theAnchorIndex < frameCount) {
                anchorIndex = theAnchorIndex;
            } else {
                theAnchorIndex = 0;
            }
            theAnchorPoint = state.timePoints[state.timePoints.length
                    - (frameCount - anchorIndex)].coord;
        } else {
            theAnchorIndex = anchorIndex;
        }

        // If uninitialized, grab from tools data manager
        if (Double.isNaN(state.speed) || Double.isNaN(state.angle)) {
            StormTrackData std = dataManager.getStormTrackData();
            speed = ktsToMps.convert(std.getMotionSpeed());
            if (state.displayType == DisplayType.POLY) {
                // calculate based on poly line
                GeodeticCalculator gc = new GeodeticCalculator();
                Coordinate[] coords = state.dragMeLine.getCoordinates();
                gc.setStartingGeographicPoint(coords[0].x, coords[0].y);
                gc.setDestinationGeographicPoint(coords[coords.length - 1].x,
                        coords[coords.length - 1].y);
                angle = adjustAngle(gc.getAzimuth() - 90);
            } else {
                angle = adjustAngle(std.getMotionDirection());
            }
            state.angle = angle;
            state.speed = speed;
        } else {
            speed = state.speed;
            angle = state.angle;
        }
        oppositeAngle = adjustAngle(angle + 180);

        DataTime[] dataTimes = trackUtil.getDataTimes(paintProps
                .getFramesInfo());

        StormCoord[] timePoints = new StormCoord[frameCount];

        StormCoord anchor = new StormCoord(this.theAnchorPoint,
                dataTimes[anchorIndex]);

        timePoints[anchorIndex] = anchor;

        DestinationGeodeticCalculator gc = new DestinationGeodeticCalculator();
        gc.setStartingGeographicPoint(anchor.coord.x, anchor.coord.y);

        if (state.timePoints == null || isUpdatedDataTimes(paintProps)) {
            for (int i = timePoints.length - 1; i >= 0; --i) {
                if (i == anchorIndex)
                    continue;

                DataTime coordTime = dataTimes[i];
                double distance = speed
                        * trackUtil
                                .timeBetweenDataTimes(anchor.time, coordTime);

                if (i > anchorIndex) {
                    gc.setDirection(angle, distance);
                } else if (i < anchorIndex) {
                    gc.setDirection(oppositeAngle, distance);
                }
                Point2D point = gc.getDestinationGeographicPoint();
                timePoints[i] = new StormCoord(new Coordinate(point.getX(),
                        point.getY()), coordTime);
            }
        } else {
            timePoints = state.timePoints;
        }

        int minIntervalInSeconds = trackUtil.minIntervalInSeconds(dataTimes);

        if (minIntervalInSeconds < 60) {
            minIntervalInSeconds = 60;
        }

        if (state.duration == -1) {
            state.duration = trackUtil.timeBetweenDataTimes(dataTimes[0],
                    dataTimes[dataTimes.length - 1]) / 60;
        }
        int interval = (int) Math.round(minIntervalInSeconds / 60 + 0.4d);
        int additionalDivisions = 1;
        int remainder = 0;

        if (state.duration >= 0) {
            additionalDivisions = state.duration / interval;
            remainder = state.duration % (minIntervalInSeconds / 60);
        }

        StormCoord[] futurePoints = new StormCoord[1 + additionalDivisions
                + (remainder == 0 ? 0 : 1)];

        // Now do same thing with future points, only have to go forward in
        // time.
        futurePoints[0] = timePoints[timePoints.length - 1];

        long timeInMillis = futurePoints[0].time.getMatchValid();
        // duration is negative when the last frame time is older than the end
        // time of a follow up warning. if the last frame is older than the end
        // time, the arrow of the pathcast is drawn behind the last frame
        if (state.duration >= 0) {
            for (int i = 1; i < futurePoints.length - (remainder == 0 ? 0 : 1); ++i) {
                timeInMillis += minIntervalInSeconds * 1000;
                DataTime time = new DataTime(new Date(timeInMillis));

                double distance = speed
                        * trackUtil.timeBetweenDataTimes(anchor.time, time);
                gc.setDirection(angle, distance);
                Point2D point = gc.getDestinationGeographicPoint();
                futurePoints[i] = new StormCoord(new Coordinate(point.getX(),
                        point.getY()), time);
            }

            if (remainder != 0) {
                timeInMillis += (remainder * 60 * 1000);
                DataTime time = new DataTime(new Date(timeInMillis));

                double distance = speed
                        * trackUtil.timeBetweenDataTimes(anchor.time, time);
                gc.setDirection(angle, distance);
                Point2D point = gc.getDestinationGeographicPoint();
                futurePoints[futurePoints.length - 1] = new StormCoord(
                        new Coordinate(point.getX(), point.getY()), time);
            }
        } else {
            timeInMillis -= Math.abs(state.duration) * 60 * 1000;
            DataTime time = new DataTime(new Date(timeInMillis));
            double distance = speed
                    * trackUtil
                            .timeBetweenDataTimes(futurePoints[0].time, time);
            gc.setDirection(oppositeAngle, distance);
            Point2D point = gc.getDestinationGeographicPoint();
            futurePoints[1] = new StormCoord(new Coordinate(point.getX(),
                    point.getY()), time);
        }

        state.timePoints = timePoints;
        state.futurePoints = futurePoints;

        postData(state);
    }

    private Coordinate updateAnchorPoint(StormTrackState currentState,
            PaintProperties paintProps) {
        int currFrame = trackUtil.getCurrentFrame(paintProps.getFramesInfo());
        StormCoord sc = currentState.timePoints[currFrame];
        DataTime[] dataTimes = trackUtil.getDataTimes(paintProps
                .getFramesInfo());
        double distance = currentState.speed
                * trackUtil.timeBetweenDataTimes(sc.time, dataTimes[currFrame]);
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(sc.coord.x, sc.coord.y);
        gc.setDirection(currentState.angle, distance);
        Point2D point = gc.getDestinationGeographicPoint();
        return new Coordinate(point.getX(), point.getY());
    }

    private boolean isUpdatedDataTimes(PaintProperties paintProps) {
        DataTime[] uniqueTimes = trackUtil.getDataTimes(paintProps
                .getFramesInfo());
        if (currentDisplayedTimes == null) {
            return false;
        }
        int len = currentDisplayedTimes.length;
        return uniqueTimes.length != len
                || (!uniqueTimes[0].equals(currentDisplayedTimes[0]) && !uniqueTimes[len - 1]
                        .equals(currentDisplayedTimes[len - 1]));
    }

    /**
     * Creates the track based on line/point geometries
     * 
     * @param target
     * @param paintProps
     */
    private void createTrack(IGraphicsTarget target,
            StormTrackProperties paintProps) throws VizException {
        StormTrackState state = paintProps.getState();

        DataTime[] dataTimes = trackUtil.getDataTimes(paintProps
                .getFramesInfo());

        int minIntervalInSeconds = trackUtil.minIntervalInSeconds(dataTimes);
        if (minIntervalInSeconds < 60) {
            minIntervalInSeconds = 60;
        }
        int shortestDistance = (int) Math.round(minIntervalInSeconds
                * state.speed);

        int tickLengthInMeters = (int) Math.round(shortestDistance) / 2;

        // Create track
        Coordinate[] coords = new Coordinate[state.timePoints.length
                + state.futurePoints.length - 1];
        Coordinate[] orig = new Coordinate[coords.length];
        int index = 0;
        for (int i = 0; i < state.timePoints.length; ++i, ++index) {
            coords[index] = new Coordinate(state.timePoints[i].coord);
            orig[index] = new Coordinate(coords[index]);
        }
        for (int i = 1; i < state.futurePoints.length; ++i, ++index) {
            coords[index] = new Coordinate(state.futurePoints[i].coord);
            orig[index] = new Coordinate(coords[index]);
        }

        cachedTrack = target.createWireframeShape(false, descriptor);
        JTSCompiler compiler = new JTSCompiler(null, cachedTrack, descriptor);

        compiler.handle(gf.createLineString(coords));

        double angle = state.angle;
        // Draw ticks, X and Arrow
        for (int i = 0; i < orig.length - 1; ++i) {
            GeodeticCalculator gc = new GeodeticCalculator();
            gc.setStartingGeographicPoint(orig[i].x, orig[i].y);
            gc.setDestinationGeographicPoint(orig[i + 1].x, orig[i + 1].y);

            angle = gc.getAzimuth();

            if (i == state.displayedPivotIndex) {
                double tickLen = SQRT_2 * tickLengthInMeters;
                // Draw X
                double plus45 = adjustAngle(angle + 45);
                double plus90plus45 = adjustAngle(angle + 90 + 45);
                double minus45 = adjustAngle(angle - 45);
                double minus90minus45 = adjustAngle(angle - 90 - 45);

                gc.setDirection(plus45, tickLen);
                Point2D point = gc.getDestinationGeographicPoint();
                Coordinate a = new Coordinate(point.getX(), point.getY());
                gc.setDirection(minus90minus45, tickLen);
                point = gc.getDestinationGeographicPoint();
                Coordinate b = new Coordinate(point.getX(), point.getY());
                compiler.handle(gf.createLineString(new Coordinate[] { a, b }));

                gc.setDirection(minus45, tickLen);
                point = gc.getDestinationGeographicPoint();
                a = new Coordinate(point.getX(), point.getY());
                gc.setDirection(plus90plus45, tickLen);
                point = gc.getDestinationGeographicPoint();
                b = new Coordinate(point.getX(), point.getY());
                compiler.handle(gf.createLineString(new Coordinate[] { a, b }));

            } else {
                // Draw tick
                double minus90 = adjustAngle(angle - 90);
                double plus90 = adjustAngle(angle + 90);

                gc.setDirection(minus90, tickLengthInMeters);
                Point2D point = gc.getDestinationGeographicPoint();
                Coordinate a = new Coordinate(point.getX(), point.getY());
                gc.setDirection(plus90, tickLengthInMeters);
                point = gc.getDestinationGeographicPoint();
                Coordinate b = new Coordinate(point.getX(), point.getY());

                compiler.handle(gf.createLineString(new Coordinate[] { a, b }));
            }
        }

        GeodeticCalculator gc = new GeodeticCalculator();
        Coordinate start = new Coordinate(orig[orig.length - 1]);
        gc.setStartingGeographicPoint(start.x, start.y);

        // Draw arrow
        double tickLen = SQRT_2 * tickLengthInMeters;
        double plus90plus45 = adjustAngle(angle + 90 + 45);
        double minus90minus45 = adjustAngle(angle - 90 - 45);

        if (state.duration < 0) {
            plus90plus45 = adjustAngle(angle + 45);
            minus90minus45 = adjustAngle(angle - 45);
        }

        gc.setDirection(minus90minus45, tickLen);
        Point2D point = gc.getDestinationGeographicPoint();
        Coordinate a = new Coordinate(point.getX(), point.getY());
        gc.setDirection(plus90plus45, tickLen);
        point = gc.getDestinationGeographicPoint();
        Coordinate b = new Coordinate(point.getX(), point.getY());
        compiler.handle(gf.createLineString(new Coordinate[] { a, start, b }));

        cachedTrack.compile();

        // store tickLen in gl coords
        gc.setDirection(0, tickLengthInMeters);
        point = gc.getStartingGeographicPoint();
        start = new Coordinate(point.getX(), point.getY());
        point = gc.getDestinationGeographicPoint();
        Coordinate end = new Coordinate(point.getX(), point.getY());
        double[] loc1 = descriptor
                .worldToPixel(new double[] { start.x, start.y });
        double[] loc2 = descriptor.worldToPixel(new double[] { end.x, end.y });

        double xDist = Math.abs(loc1[0] - loc2[0]);
        double yDist = Math.abs(loc1[1] - loc2[1]);
        currentTickLen = Math.sqrt(Math.pow(xDist, 2) + Math.pow(yDist, 2));
    }

    /**
     * Paints the time or speed labels
     * 
     * @param target
     * @param paintProps
     */
    private void paintLabels(IGraphicsTarget target,
            StormTrackProperties paintProps) throws VizException {
        StormTrackState state = paintProps.getState();
        // get the magnification from the state
        float magnification = state.magnification;
        // find a nice looking radius
        double radius = Math.max(currentTickLen, editableCircleSize * 2
                * paintProps.getZoomLevel());

        // find the angle in screen space for label placement calculations.
        double[] p1 = descriptor.worldToPixel(new double[] {
                state.timePoints[0].coord.x, state.timePoints[0].coord.y });
        double[] p2 = descriptor.worldToPixel(new double[] {
                state.futurePoints[state.futurePoints.length - 1].coord.x,
                state.futurePoints[state.futurePoints.length - 1].coord.y });
        double screenAngle = Math.toDegrees(Math.atan2(p2[0] - p1[0], p1[1]
                - p2[1]));
        // Find a good alignment for a point at the end
        VerticalAlignment vEnd = null;
        HorizontalAlignment hEnd = null;

        if (screenAngle > 30 && screenAngle < 120) {
            hEnd = HorizontalAlignment.LEFT;
        } else if (screenAngle < -30 && screenAngle > -120) {
            hEnd = HorizontalAlignment.RIGHT;
        } else {
            hEnd = HorizontalAlignment.CENTER;
        }

        if (Math.abs(screenAngle) > 120) {
            vEnd = VerticalAlignment.TOP;
        } else if (Math.abs(screenAngle) < 30) {
            vEnd = VerticalAlignment.BOTTOM;
        } else {
            vEnd = VerticalAlignment.MIDDLE;
        }

        // Find a good alignment for a point in the middle
        VerticalAlignment vMid = null;
        HorizontalAlignment hMid = null;
        if (Math.abs(screenAngle) > 100) {
            hMid = HorizontalAlignment.LEFT;
        } else if (Math.abs(screenAngle) < 80) {
            hMid = HorizontalAlignment.RIGHT;
        } else {
            hMid = HorizontalAlignment.CENTER;
        }

        if (screenAngle > 10 && screenAngle < 170) {
            vMid = VerticalAlignment.BOTTOM;
        } else if (screenAngle < -10 && screenAngle > -170) {
            vMid = VerticalAlignment.TOP;
        } else {
            vMid = VerticalAlignment.MIDDLE;
        }

        switch (state.labelMode) {
        case SPEED: {

            int speedInKts = (int) mpsToKts.convert(state.speed);
            int angle = (int) unadjustAngle(state.angle);
            if (angle == 0) {
                angle = 360;
            }
            String text = String.format(speedFormat, speedInKts, angle);

            paintTextAtPoint(target, text, state.dragMePoint.getCoordinate(),
                    state.color, radius, screenAngle + 180, hMid, vMid, magnification);

            paintTextAtPoint(target, text,
                    state.futurePoints[state.futurePoints.length - 1].coord,
                    state.color, radius, -90 + screenAngle, hEnd, vEnd, magnification);
            break;
        }

        case TIME: {

            // First time:
            String time = this.timeFormat.format(new Date(
                    state.timePoints[0].time.getMatchValid()));
            paintTextAtPoint(target, time, state.timePoints[0].coord,
                    state.color, radius, 180 + screenAngle, hMid, vMid, magnification);

            time = this.timeFormat.format(new Date(state.futurePoints[0].time
                    .getMatchValid()));
            paintTextAtPoint(target, time, state.futurePoints[0].coord,
                    state.color, radius, 180 + screenAngle, hMid, vMid, magnification);

            // End time:
            Calendar currentTime = Calendar.getInstance();
            currentTime.setTime(SimulatedTime.getSystemTime().getTime());
            long delta = state.futurePoints[state.futurePoints.length - 1].time
                    .getMatchValid()
                    - state.futurePoints[0].time.getMatchValid();
            time = this.timeFormat.format(new Date(currentTime
                    .getTimeInMillis() + delta));
            paintTextAtPoint(target, time,
                    state.futurePoints[state.futurePoints.length - 1].coord,
                    state.color, radius, -90 + screenAngle, hEnd, vEnd, magnification);
            break;
        }
        }
    }

    /**
     * Adjusts the angle from -360/360 to be between -180/180
     * 
     * @param angle
     * @return
     */
    public static double adjustAngle(double angle) {
        double newVal = angle % 360;
        if (newVal > 180) {
            newVal -= 360;
        } else if (newVal < -180) {
            newVal += 360;
        }
        return newVal;
    }

    /**
     * Adjusts the angle from -180/180 to be between 0/360
     * 
     * @param angle
     * @return
     */
    private double unadjustAngle(double angle) {
        return (360 + angle) % 360;
    }

    private void postData(StormTrackState state) {
        StormTrackData data = dataManager.getStormTrackData();
        Coordinate[] coords = new Coordinate[state.timePoints.length];
        for (int i = 0; i < coords.length; ++i) {
            coords[i] = new Coordinate(state.timePoints[i].coord);
        }
        data.setCoordinates(coords);
        data.setMotionDirection(state.angle);
        data.setMotionSpeed((int) mpsToKts.convert(state.speed));
        dataManager.setStormTrackData(data);
    }
}
