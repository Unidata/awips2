package gov.noaa.nws.mdl.viz.boundaryTool.common.boundary;

import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.Mode;
import gov.noaa.nws.mdl.viz.boundaryTool.common.boundary.BoundaryState.UserAction;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.IAction;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.input.InputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * 
 * @author Mamoudou ba
 * @version 1.0
 * 
 *          Substantially modified of A2 "StormTrackUIManager" class
 */

public class BoundaryUIManager extends InputAdapter {

    private enum MoveType {
        SINGLE_POINT, ALL_POINTS;
    }

    private AbstractBoundaryResource controller;

    private Cursor movePolygon;

    private Cursor movePoint;

    private MoveType moveType = null;

    private int movePointIndex = -1;

    private int lastX, lastY;

    private boolean pointCreated = false;

    private boolean handle = true;

    private BoundaryUtil trackUtil;

    public static final double point_coord = 3.0;

    public BoundaryUIManager(AbstractBoundaryResource controller) {
        this.controller = controller;

        IDisplayPaneContainer container = controller.getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(this);
        }

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                Display display = getShell().getDisplay();
                movePolygon = display.getSystemCursor(SWT.CURSOR_SIZEALL);
                movePoint = display.getSystemCursor(SWT.CURSOR_HAND);
                display.getSystemCursor(SWT.CURSOR_ARROW);
            }
        });
    }

    public void dispose() {
        IDisplayPaneContainer container = controller.getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(this);
        }
    }

    /**
     * @param draw
     */
    public void setHandleInput(boolean handle) {
        this.handle = handle;
        if (!handle) {
            getShell().setCursor(null);
        }
    }

    public boolean isHandleInput() {
        if (handle) {
            if (controller.displayState.mode != Mode.NONE) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {

        IDisplayPaneContainer container = controller.getResourceContainer();
        lastX = x;
        lastY = y;
        if (!isHandleInput()) {
            return super.handleMouseMove(x, y);
        }
        moveType = null;
        movePointIndex = -1;
        pointCreated = false;
        Coordinate mouse = new Coordinate(x, y);
        BoundaryState state = controller.getBoundaryState();
        if (state.isEditable() == false) {
            return super.handleMouseMove(x, y);
        }
        Geometry dragme = state.dragMeGeom;
        Cursor toUse = null;
        if (dragme != null) {
            Coordinate[] coords = dragme.getCoordinates();
            int idx = getCoordinateIndex(controller, coords, mouse);
            if (idx > -1) {
                toUse = movePoint;
                moveType = MoveType.SINGLE_POINT;
                /*
                 * In AWIPS I, When Looping Is Enabled Before A Track Has Been
                 * Created, Looping Will Automatically Be Disabled When TheMouse
                 * Cursor Comes Into The Proximity Of The Tool.
                 */
                if (state.mode == BoundaryState.Mode.DRAG_ME) {
                    if (container.getLoopProperties().isLooping()) {
                        container.getLoopProperties().setLooping(false);
                        state.loopingWasOn = true;
                    }
                }
                movePointIndex = idx;
            }

            if (moveType == null) {
                if (state.userAction == UserAction.INSERT_BOUNDARY
                        || state.userAction == UserAction.EDIT_BOUNDARY) {
                    if (closeToLine(controller, coords, mouse)) {
                        toUse = movePolygon;
                        moveType = MoveType.ALL_POINTS;
                    }
                }
            }
        }
        getShell().setCursor(toUse);
        return super.handleMouseMove(x, y);
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        if (controller.getBoundaryState().dragingLineNotAllowed) {
            return false;
        }
        if (!isHandleInput()) {
            return super.handleMouseDown(x, y, mouseButton);
        }
        lastX = x;
        lastY = y;
        pointCreated = false;
        BoundaryState state = controller.getBoundaryState();
        if (state.isEditable() == false) {
            return super.handleMouseDown(x, y, mouseButton);
        }
        if (mouseButton == 1 && moveType != null) {
            Geometry moveGeom = (Geometry) state.dragMeGeom.clone();
            state.mouseDownGeom = moveGeom;
            return true;
        } else if (mouseButton == 2 && moveType != null) {
            return false;
        }

        moveType = null;
        movePointIndex = -1;
        return false;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        if (controller.getBoundaryState().dragingLineNotAllowed) {
            return false;
        }
        if (controller.getBoundaryState().movingEdited) {
            return false;
        }
        if (!isHandleInput()) {
            return super.handleMouseDownMove(x, y, mouseButton);
        }
        boolean move = false;
        BoundaryState state = controller.getBoundaryState();
        if (state.isEditable() == false) {
            return super.handleMouseDownMove(x, y, mouseButton);
        }
        if (mouseButton == 1 && moveType != null) {
            move = true;
        } else if (mouseButton == 2
                && moveType != null
                && controller.getBoundaryState().userAction == UserAction.INSERT_BOUNDARY
                && controller.getBoundaryState().lineIsMoving == false) {
            if (!pointCreated) {
                state = controller.getBoundaryState();
                pointCreated = true;
                movePointIndex = addVertex(lastX, lastY);
                state.mouseDownGeom = (Geometry) state.dragMeGeom.clone();
                moveType = MoveType.SINGLE_POINT;
            }
            move = true;
        }

        if (move) {
            move(x, y);
            return true;
        }

        return super.handleMouseDownMove(x, y, mouseButton);
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        if (controller.getBoundaryState().dragingLineNotAllowed) {
            controller.getBoundaryState().dragingLineNotAllowed = false;
            return false;
        }
        if (!isHandleInput()) {
            super.handleMouseUp(x, y, mouseButton);
        }
        BoundaryState state = controller.getBoundaryState();
        boolean rval = false;
        if (((mouseButton == 1) || (mouseButton == 2 && pointCreated && controller
                .getBoundaryState().lineIsMoving == false)) && moveType != null) {
            state.dragMeGeom = state.mouseDownGeom;
            state.mouseDownGeom = null;
            if (state.mode == Mode.DRAG_ME) {
                state.mode = Mode.TRACK;
            }
            state.lineMovedMap.put(state.boundaryId, true);
            FramesInfo info = controller.getDescriptor().getFramesInfo();
            trackUtil.setPivotIndexes(info, state);

            // This code is duplicated from StormTrackDisplay.paint().
            if (state.displayedPivotIndex == trackUtil.getCurrentFrame(info)) {
                if (state.displayedPivotIndex == state.pivotIndex
                        && state.otherPivotIndex >= 0) {
                    state.displayedPivotIndex = state.otherPivotIndex;
                } else if (state.pivotIndex >= 0) {
                    state.displayedPivotIndex = state.pivotIndex;
                }
            }
            rval = true;
        } else if (mouseButton == 2 && !pointCreated) {
            rval = true;
            Coordinate[] coords = state.dragMeGeom.getCoordinates();
            Coordinate mouse = new Coordinate(x, y);
            int idxToDelete = getCoordinateIndex(controller, coords, mouse);
            if (idxToDelete >= 0) {
                if (!controller.getBoundaryState().lineIsMoving)
                    deleteVertex(idxToDelete);
            } else if (closeToLine(controller, coords, mouse)) {
                if (!controller.getBoundaryState().lineIsMoving) {
                    addVertex(lastX, lastY);
                }
            } else {
                rval = false;
            }
        }

        if (rval) {
            state.geomChanged = true;

            state.dragMeLine = (LineString) state.dragMeGeom;
            state.boundariesMap.put(state.boundaryId, state.dragMeLine);

            updateNumberOfPointsForLine();
            controller.issueRefresh();
        }
        pointCreated = false;
        return rval;
    }

    public static int getCoordinateIndex(AbstractBoundaryResource resource,
            Coordinate[] coords, Coordinate mouse) {
        return getCoordinateIndex(resource.getResourceContainer(), coords,
                mouse, 15.0);
    }

    public static int getCoordinateIndex(IDisplayPaneContainer container,
            Coordinate[] coords, Coordinate mouse, double threshold) {
        int rval = -1;
        int i = 0;
        for (Coordinate c : coords) {
            double[] screen = container.translateInverseClick(c);
            if (screen != null) {
                c = new Coordinate(screen[0], screen[1]);
                if (c.distance(mouse) <= threshold) {
                    rval = i;
                    break;
                }
            }
            ++i;
        }
        return rval;
    }

    public static boolean closeToLine(AbstractBoundaryResource controller,
            Coordinate[] coords, Coordinate mouse) {
        IDisplayPaneContainer container = controller.getResourceContainer();
        if (coords.length < 2) {
            return false;
        }

        GeometryFactory gf = new GeometryFactory();
        List<Coordinate> inverted = new ArrayList<Coordinate>(coords.length);

        for (int i = 0; i < coords.length; ++i) {
            double[] vals = container.translateInverseClick(coords[i]);

            // handle offscreen coords
            // TODO: use nearest point on line that is on screen?
            if (vals != null) {
                inverted.add(new Coordinate(vals[0], vals[1]));
            }
        }

        if (inverted.size() > 1) {
            return (gf.createLineString(
                    inverted.toArray(new Coordinate[inverted.size()]))
                    .distance(gf.createPoint(mouse)) <= point_coord);
        }

        return false;
    }

    private void move(int x, int y) {
        BoundaryState state = controller.getBoundaryState();
        IDisplayPaneContainer container = controller.getResourceContainer();

        int changeX = x - lastX;
        int changeY = y - lastY;
        switch (moveType) {
        case ALL_POINTS: {
            Coordinate[] coords = state.mouseDownGeom.getCoordinates();
            for (Coordinate c : coords) {
                double[] vals = container.translateInverseClick(c);
                vals[0] += changeX;
                vals[1] += changeY;

                Coordinate tmp = container.translateClick(vals[0], vals[1]);
                if (tmp != null) {
                    c.x = tmp.x;
                    c.y = tmp.y;
                }
            }
            break;
        }
        case SINGLE_POINT: {
            if (state.mouseDownGeom != null) {
                Coordinate[] coords = state.mouseDownGeom.getCoordinates();
                Coordinate c = coords[movePointIndex];
                double[] vals = container.translateInverseClick(c);
                vals[0] += changeX;
                vals[1] += changeY;

                Coordinate tmp = container.translateClick(vals[0], vals[1]);
                if (tmp != null) {
                    c.x = tmp.x;
                    c.y = tmp.y;
                }
            }
            break;
        }
        }
        lastX = x;
        lastY = y;
        controller.issueRefresh();
    }

    public int addVertex(int x, int y) {
        BoundaryState state = controller.getBoundaryState();
        IDisplayPaneContainer container = controller.getResourceContainer();
        double bestDistance = Double.MAX_VALUE;
        int insertionPoint = 0;
        Coordinate[] coords = state.dragMeGeom.getCoordinates();
        Coordinate mouse = container.translateClick(x, y);
        for (int i = 1; i < coords.length; i++) {
            LineSegment segment = new LineSegment(coords[i - 1], coords[i]);
            double distance = segment.distance(mouse);
            if (distance < bestDistance) {
                insertionPoint = i;
                bestDistance = distance;
            }
        }

        Coordinate[] newLine = new Coordinate[coords.length + 1];
        for (int i = 0; i < newLine.length; i++) {
            if (i < insertionPoint) {
                newLine[i] = coords[i];
            } else if (i > insertionPoint) {
                newLine[i] = coords[i - 1];
            } else {
                newLine[i] = mouse;
            }
        }

        state.dragMeGeom = new GeometryFactory().createLineString(newLine);
        state.geomChanged = true;
        state.numberPointChanged = true;
        return insertionPoint;
    }

    public void deleteVertex(int index) {
        BoundaryState state = controller.getBoundaryState();
        Coordinate[] coords = state.dragMeGeom.getCoordinates();
        if (coords.length > 2) {
            Coordinate[] newCoords = new Coordinate[coords.length - 1];
            int j = 0;
            for (int i = 0; i < coords.length; ++i) {
                if (i != index) {
                    newCoords[j++] = coords[i];
                }
            }

            state.dragMeGeom = new GeometryFactory()
                    .createLineString(newCoords);
            state.geomChanged = true;
            state.numberPointChanged = true;
        }

    }

    public void updateNumberOfPointsForLine() {
        BoundaryState state = controller.getBoundaryState();
        if (state.timePointsMap.get(state.boundaryId) != null)
            state.dragMePointMap.remove(state.boundaryId);
        state.dragMePointMap.put(state.boundaryId,
                state.boundariesMap.get(state.boundaryId));
        Coordinate[] coords = state.dragMePointMap.get(state.boundaryId)
                .getCoordinates();
        state.dragMePoint = new Point[coords.length];
        GeometryFactory gf = new GeometryFactory();
        for (int k = 0; k < coords.length; k++) {
            state.dragMePoint[k] = gf.createPoint(coords[k]);
        }

        // Check if a vertex is added or deleted in the boundary;
        // if so, update the polyline accordingly along the track
        state.timePoints = state.timePointsMap.get(state.boundaryId);
        if (state.timePoints != null) {

            for (int i = 0; i < state.timePoints.length; i++) {
                Point point = getPointFromLine(state.timePoints[i].polyline);
                state.timePoints[i].polyline = figureLineFromPoint(
                        state.dragMePointMap.get(state.boundaryId), point);
                state.numberPointChanged = false;
            }
            state.timePointsMap.put(state.boundaryId, state.timePoints);
        }

    }

    public static Point getPointFromLine(LineString line) {
        Coordinate[] coords = line.getCoordinates();
        if (coords.length % 2 == 1) {
            return new GeometryFactory()
                    .createPoint(coords[(coords.length - 1) / 2]);
        } else {
            Coordinate middleLeft = coords[coords.length / 2];
            Coordinate middleRight = coords[(coords.length - 2) / 2];
            return new GeometryFactory().createPoint(new Coordinate(
                    middleLeft.x + ((middleRight.x - middleLeft.x) / 2),
                    middleLeft.y + ((middleRight.y - middleLeft.y) / 2)));
        }
    }

    public static Point getPointFromLine(LineString line, int indexVertex) {
        Coordinate[] coords = line.getCoordinates();
        return new GeometryFactory().createPoint(coords[indexVertex]);

    }

    public LineString figureLineFromPoint(LineString line, Point pointGeom) {
        IDisplayPaneContainer container = controller.getResourceContainer();
        Coordinate point = pointGeom.getCoordinate();
        Coordinate linePoint = getPointFromLine(line).getCoordinate();

        double[] dest = container.translateInverseClick(point);
        double[] start = container.translateInverseClick(linePoint);

        double changeX = dest[0] - start[0];
        double changeY = dest[1] - start[1];

        Coordinate[] coords = line.getCoordinates();
        Coordinate[] newLineCoords = new Coordinate[coords.length];

        for (int i = 0; i < coords.length; ++i) {
            double[] vals = container.translateInverseClick(coords[i]);
            newLineCoords[i] = container.translateClick(vals[0] + changeX,
                    vals[1] + changeY);
        }
        return new GeometryFactory().createLineString(newLineCoords);
    }

    /**
     * Will return null if the line or point are offscreen.
     * 
     * @param line
     * @param pointGeom
     * @return
     */
    public LineString figureLineFromPoint(LineString line, Point[] pointGeom)
            throws ImpossibleTrackException {
        IDisplayPaneContainer container = controller.getResourceContainer();

        IDisplayPane pane = container.getActiveDisplayPane();

        Coordinate[] coords = line.getCoordinates();
        Coordinate[] newLineCoords = new Coordinate[coords.length];

        for (int i = 0; i < coords.length; ++i) {
            Coordinate point = pointGeom[i].getCoordinate();
            Coordinate linePoint = getPointFromLine(line, i).getCoordinate();

            double[] dest = container.translateInverseClick(point);
            double[] start = container.translateInverseClick(linePoint);
            if (dest != null && start != null) {
                double changeX = dest[0] - start[0];
                double changeY = dest[1] - start[1];
                double[] vals = container.translateInverseClick(coords[i]);
                double[] translated = pane.getDescriptor().pixelToWorld(
                        pane.screenToGrid(vals[0] + changeX, vals[1] + changeY,

                        0));
                newLineCoords[i] = new Coordinate(translated[0], translated[1]);
            } else {
                return null;
            }
        }
        return new GeometryFactory().createLineString(newLineCoords);
    }

    /**
     * @return
     */
    public boolean closeToPoint() {
        LineString line = null;
        for (Integer boundaryId : controller.getBoundaryState().boundariesMap
                .keySet()) {
            if (boundaryId == controller.getBoundaryState().boundaryId) {
                line = controller.getBoundaryState().boundariesMap
                        .get(boundaryId);
                break;
            }

        }
        return getCoordinateIndex(controller, line.getCoordinates(),
                new Coordinate(lastX, lastY)) > -1;
    }

    /**
     * @return
     */
    public boolean closeToLine() {
        LineString line = null;
        for (Integer boundaryId : controller.getBoundaryState().boundariesMap
                .keySet()) {
            if (boundaryId == controller.getBoundaryState().boundaryId) {
                line = controller.getBoundaryState().boundariesMap
                        .get(boundaryId);
                break;
            }
        }
        return closeToLine(controller, line.getCoordinates(), new Coordinate(
                lastX, lastY));
    }

    /**
     * @return
     */
    public IAction getDeleteAction() {
        return new DeleteVertexAction();
    }

    /**
     * @return
     */
    public IAction getAddAction() {
        return new AddVertexAction();
    }

    private class DeleteVertexAction extends AbstractRightClickAction {
        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            BoundaryState state = controller.getBoundaryState();
            if (!controller.getBoundaryState().lineIsMoving)
                deleteVertex(getCoordinateIndex(controller, state.boundariesMap
                        .get(state.boundaryId).getCoordinates(),
                        new Coordinate(lastX, lastY)));
            state.dragMeLine = (LineString) state.dragMeGeom;
            state.geomChanged = true;
            updateNumberOfPointsForLine();
            controller.issueRefresh();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return controller.getDeleteVertexText();
        }

    }

    private class AddVertexAction extends AbstractRightClickAction {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {
            BoundaryState state = controller.getBoundaryState();
            if (!controller.getBoundaryState().lineIsMoving)
                addVertex(lastX, lastY);
            state.dragMeLine = (LineString) state.dragMeGeom;
            state.geomChanged = true;
            updateNumberOfPointsForLine();
            controller.issueRefresh();
        }

        /*
         * (non-Javadoc)B
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return controller.getAddVertexText();
        }
    }

    /**
     * @param trackUtil
     */
    public void setTrackUtil(BoundaryUtil trackUtil) {
        this.trackUtil = trackUtil;
    }

    public BoundaryUtil getTrackUtil() {
        return trackUtil;
    }

    private Shell getShell() {
        IDisplayPaneContainer container = controller.getResourceContainer();
        if (container instanceof IWorkbenchPart) {
            return ((IWorkbenchPart) container).getSite().getShell();
        } else {
            return VizWorkbenchManager.getInstance().getCurrentWindow()
                    .getShell();
        }
    }
}
