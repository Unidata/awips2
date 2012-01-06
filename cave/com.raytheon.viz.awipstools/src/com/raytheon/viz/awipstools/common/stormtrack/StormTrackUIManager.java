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
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.DisplayType;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.Mode;
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
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06-23-2010   #6468      bkowal      Looping Will Now Be Stopped,
 *                                     If It Was Running, When The
 *                                     Mouse Cursor Is Moved Into
 *                                     The Proximity Of The Drag-Me-Point
 *                                     Before A Track Has Been Created.
 * 07-14-2010   #6558      bkowal      An indicator will be set to inform
 *                                     the Storm Track Display that it
 *                                     needs to update the track because
 *                                     the point has been moved.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class StormTrackUIManager extends InputAdapter {

    private enum MoveType {
        SINGLE_POINT, ALL_POINTS;
    }

    private AbstractStormTrackResource controller;

    private Cursor movePolygon;

    private Cursor movePoint;

    private Cursor arrow;

    private MoveType moveType = null;

    private int movePointIndex = -1;

    private int lastX, lastY;

    private boolean pointCreated = false;

    private boolean handle = true;

    private StormTrackUtil trackUtil;

    public StormTrackUIManager(AbstractStormTrackResource controller) {
        this.controller = controller;

        IDisplayPaneContainer container = controller.getResourceContainer();
        if (container != null) {
            container.registerMouseHandler(this);
        }

        Display display = Display.getCurrent();

        movePolygon = new Cursor(display, SWT.CURSOR_SIZEALL);
        movePoint = new Cursor(display, SWT.CURSOR_HAND);
        arrow = new Cursor(display, SWT.CURSOR_ARROW);
    }

    public void dispose() {
        IDisplayPaneContainer container = controller.getResourceContainer();
        if (container != null) {
            container.unregisterMouseHandler(this);
        }

        movePolygon.dispose();
        movePoint.dispose();
        arrow.dispose();
    }

    /**
     * @param draw
     */
    public void setHandleInput(boolean handle) {
        this.handle = handle;
        if (!handle) {
            getShell().setCursor(arrow);
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
        StormTrackState state = controller.getStormTrackState();
        if (state.isEditable() == false) {
            return super.handleMouseMove(x, y);
        }
        Geometry dragme = state.dragMeGeom;
        Cursor toUse = arrow;
        if (dragme != null) {
            Coordinate[] coords = dragme.getCoordinates();
            int idx = getCoordinateIndex(controller, coords, mouse);
            if (idx > -1) {
                toUse = movePoint;
                moveType = MoveType.SINGLE_POINT;
                /*
                 * In AWIPS I, When Looping Is Enabled Before A Track Has Been
                 * Created, Looping Will Automatically Be Disabled When The
                 * Mouse Cursor Comes Into The Proximity Of The Tool.
                 */
                if (state.mode == StormTrackState.Mode.DRAG_ME) {
                    if (container.getLoopProperties().isLooping()) {
                        container.getLoopProperties().setLooping(false);
                    }
                }
                movePointIndex = idx;
            }

            if (moveType == null) {
                if (state.displayType == DisplayType.POLY) {
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
        if (!isHandleInput()) {
            return super.handleMouseDown(x, y, mouseButton);
        }
        lastX = x;
        lastY = y;
        pointCreated = false;
        StormTrackState state = controller.getStormTrackState();
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
        if (!isHandleInput()) {
            return super.handleMouseDownMove(x, y, mouseButton);
        }
        boolean move = false;
        StormTrackState state = controller.getStormTrackState();
        if (state.isEditable() == false) {
            return super.handleMouseDownMove(x, y, mouseButton);
        }
        if (mouseButton == 1 && moveType != null) {
            move = true;
        } else if (mouseButton == 2
                && moveType != null
                && controller.getStormTrackState().displayType == DisplayType.POLY) {
            if (!pointCreated) {
                state = controller.getStormTrackState();
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
        if (!isHandleInput()) {
            super.handleMouseUp(x, y, mouseButton);
        }
        StormTrackState state = controller.getStormTrackState();
        boolean rval = false;
        if (((mouseButton == 1) || (mouseButton == 2 && pointCreated))
                && moveType != null) {
            state.dragMeGeom = state.mouseDownGeom;
            state.mouseDownGeom = null;
            if (state.mode == Mode.DRAG_ME) {
                state.mode = Mode.TRACK;
            }
            state.pointMoved = true;
            FramesInfo info = controller.getDescriptor().getFramesInfo();
            trackUtil.setPivotIndexes(info, state);
            state.nextPivotIndex = trackUtil.getCurrentFrame(info);
            controller.issueRefresh();
            rval = true;
        } else if (mouseButton == 2 && !pointCreated) {
            rval = true;
            Coordinate[] coords = state.dragMeGeom.getCoordinates();
            Coordinate mouse = new Coordinate(x, y);
            int idxToDelete = getCoordinateIndex(controller, coords, mouse);
            if (idxToDelete >= 0) {
                deleteVertex(idxToDelete);
            } else if (closeToLine(controller, coords, mouse)) {
                addVertex(lastX, lastY);
                state.resetAnchor = true;
                state.timePoints = null;
            } else {
                rval = false;
            }
        }

        if (rval) {
            state.geomChanged = true;
            switch (state.displayType) {
            case CIRCULAR:
            case POINT: {
                state.dragMePoint = (Point) state.dragMeGeom;
                figureLineFromPoint();
                break;
            }
            case POLY: {
                state.dragMeLine = (LineString) state.dragMeGeom;
                figurePointFromLine();
                break;
            }
            }
            controller.issueRefresh();
        }
        pointCreated = false;
        return rval;
    }

    public static int getCoordinateIndex(AbstractStormTrackResource resource,
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

    public static boolean closeToLine(AbstractStormTrackResource controller,
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
                    .distance(gf.createPoint(mouse)) <= 3.0);
        }

        return false;
    }

    private void move(int x, int y) {
        StormTrackState state = controller.getStormTrackState();
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
        StormTrackState state = controller.getStormTrackState();
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
        return insertionPoint;
    }

    public void deleteVertex(int index) {
        StormTrackState state = controller.getStormTrackState();
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
        }

    }

    public void figurePointFromLine() {
        StormTrackState state = controller.getStormTrackState();
        state.dragMePoint = getPointFromLine(state.dragMeLine);
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

    private void figureLineFromPoint() {
        StormTrackState state = controller.getStormTrackState();

        if (state.dragMeLine == null) {
            return;
        }

        state.dragMeLine = figureLineFromPoint(state.dragMeLine,
                state.dragMePoint);
    }

    /**
     * Will return null if the line or point are offscreen.
     * 
     * @param line
     * @param pointGeom
     * @return
     */
    public LineString figureLineFromPoint(LineString line, Point pointGeom) {
        IDisplayPaneContainer container = controller.getResourceContainer();
        IDisplayPane pane = container.getActiveDisplayPane();
        Coordinate point = pointGeom.getCoordinate();
        Coordinate linePoint = getPointFromLine(line).getCoordinate();

        double[] dest = container.translateInverseClick(point);
        double[] start = container.translateInverseClick(linePoint);

        if (dest != null && start != null) {
            double changeX = dest[0] - start[0];
            double changeY = dest[1] - start[1];

            Coordinate[] coords = line.getCoordinates();
            Coordinate[] newLineCoords = new Coordinate[coords.length];

            for (int i = 0; i < coords.length; ++i) {
                double[] vals = container.translateInverseClick(coords[i]);
                // Can't use container.translateClick(...) as it will return
                // null if point is off of valid world range
                double[] translated = pane.getDescriptor().pixelToWorld(
                        pane.screenToGrid(vals[0] + changeX, vals[1] + changeY,
                                0));
                newLineCoords[i] = new Coordinate(translated[0], translated[1]);
            }
            return new GeometryFactory().createLineString(newLineCoords);
        }
        return null;
    }

    /**
     * @return
     */
    public boolean closeToPoint() {
        return getCoordinateIndex(controller,
                controller.getStormTrackState().dragMeLine.getCoordinates(),
                new Coordinate(lastX, lastY)) > -1;

    }

    /**
     * @return
     */
    public boolean closeToLine() {
        return closeToLine(controller,
                controller.getStormTrackState().dragMeLine.getCoordinates(),
                new Coordinate(lastX, lastY));
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
            StormTrackState state = controller.getStormTrackState();
            deleteVertex(getCoordinateIndex(controller,
                    state.dragMeLine.getCoordinates(), new Coordinate(lastX,
                            lastY)));
            state.dragMeLine = (LineString) state.dragMeGeom;
            state.geomChanged = true;
            figurePointFromLine();
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
            StormTrackState state = controller.getStormTrackState();
            addVertex(lastX, lastY);
            state.dragMeLine = (LineString) state.dragMeGeom;
            state.geomChanged = true;
            state.resetAnchor = true;
            state.timePoints = null;
            figurePointFromLine();
            controller.issueRefresh();
        }

        /*
         * (non-Javadoc)
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
    public void setTrackUtil(StormTrackUtil trackUtil) {
        this.trackUtil = trackUtil;
    }

    public StormTrackUtil getTrackUtil() {
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
