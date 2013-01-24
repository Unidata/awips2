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
package com.raytheon.viz.warngen.gui;

import java.awt.geom.Point2D;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.geotools.math.Line;

import com.raytheon.uf.common.dataplugin.warning.WarningRecord.WarningAction;
import com.raytheon.uf.common.dataplugin.warning.util.GeometryUtil;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackState.Mode;
import com.raytheon.viz.awipstools.common.stormtrack.StormTrackUIManager;
import com.raytheon.viz.ui.VizWorkbenchManager;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.raytheon.viz.ui.input.InputAdapter;
import com.raytheon.viz.warngen.Activator;
import com.raytheon.viz.warngen.gis.PolygonUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 7, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class WarngenUIManager extends InputAdapter {

    private enum MoveType {
        SINGLE_POINT, ALL_POINTS;
    }

    private MoveType moveType = null;

    private boolean pointCreated = false;

    private boolean pointDeleted = false;

    private int movePointIndex = -1;

    private final WarngenLayer warngenLayer;

    // Cursor
    private final Cursor movePolygon;

    private final Cursor movePoint;

    private final Cursor arrow;

    /** The last mouse position - x */
    private int lastMouseX;

    /** The last mouse position - y */
    private int lastMouseY;

    private boolean handleInput = true;

    private boolean menuMove = false;

    public WarngenUIManager(WarngenLayer layer) {
        warngenLayer = layer;

        warngenLayer.getResourceContainer().registerMouseHandler(this);

        Display display = Display.getCurrent();

        movePolygon = new Cursor(display, SWT.CURSOR_SIZEALL);
        movePoint = new Cursor(display, SWT.CURSOR_HAND);
        arrow = new Cursor(display, SWT.CURSOR_ARROW);
    }

    public void dispose() {
        warngenLayer.getResourceContainer().unregisterMouseHandler(this);
        movePolygon.dispose();
        movePoint.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.ui.IInputHandler#handleMouseDown(int, int,
     * int)
     */
    @Override
    public boolean handleMouseDown(int x, int y, int button) {
        warngenLayer.showDialog(true);
        lastMouseX = x;
        lastMouseY = y;
        if (!handleInput || warngenLayer.isBoxEditable() == false) {
            return super.handleMouseDown(x, y, button);
        }
        boolean rval = false;
        if (button == 1 && moveType != null) {
            return true;
        } else if (button == 2 && menuMove == false) {
            /** Try to add vertex */
            new AddVertexAction().run();
            if (pointCreated) {
                moveType = MoveType.SINGLE_POINT;
                movePointIndex = StormTrackUIManager.getCoordinateIndex(
                        warngenLayer, warngenLayer.getPolygon()
                                .getCoordinates(), new Coordinate(lastMouseX,
                                lastMouseY));
                return true;
            } else {
                new DeleteVertexAction().run();
                rval = pointDeleted;
                warngenLayer.issueRefresh();
            }
        }

        moveType = null;
        movePointIndex = -1;
        return rval;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        if (!handleInput
                || warngenLayer.getStormTrackState().mouseDownGeom != null
                || warngenLayer.getStormTrackState().mode == Mode.DRAG_ME) {
            return super.handleMouseDownMove(x, y, mouseButton);
        }
        boolean move = false;

        if (pointDeleted) {
            warngenLayer.issueRefresh();
            return true;
        }

        if (warngenLayer.isBoxEditable() == false) {
            return super.handleMouseDownMove(x, y, mouseButton);
        }
        if (mouseButton == 1 && moveType != null) {
            move = true;
        } else if (mouseButton == 2 && moveType != null) {
            if (!pointCreated) {
                pointCreated = true;
                movePointIndex = addVertex(lastMouseX, lastMouseY);
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
        IDisplayPaneContainer container = warngenLayer.getResourceContainer();
        if (!handleInput
                || warngenLayer.getStormTrackState().mouseDownGeom != null
                || (warngenLayer.getStormTrackState().mode == Mode.DRAG_ME && mouseButton != 3)) {
            return super.handleMouseUp(x, y, mouseButton);
        }

        if (mouseButton == 3 && menuMove == false) {
            Coordinate c = container.translateClick(x, y);
            WarngenUIState state = warngenLayer.getWarngenState();
            boolean geomsEqual = true;
            if (state.getWarningArea() == null) {
                if (state.getOldWarningArea() != null)
                    geomsEqual = false;
            } else if (!GeometryUtil.equals(state.getWarningArea(),
                    state.getOldWarningArea()))
                geomsEqual = false;
            if (state.getWarningPolygon() == null) {
                if (state.getOldWarningPolygon() != null)
                    geomsEqual = false;
            } else if (!GeometryUtil.equals(state.getWarningPolygon(),
                    state.getOldWarningPolygon()))
                geomsEqual = false;

            // if state.rightClickSelected is true, always try
            // warngenLayer.featureProduct()
            //
            // even if rightClickSelected is false, right click should only
            // toggle interior counties when making a continuation so try
            // featureProduct() if making a continuation
            if ((warngenLayer.getStormTrackState().mode != Mode.TRACK
                    || warngenLayer.state.followupData != null || warngenLayer.state.rightClickSelected)
                    && (geomsEqual || warngenLayer.state.rightClickSelected || (warngenLayer.state.followupData != null && WarningAction
                            .valueOf(warngenLayer.state.followupData.getAct()) == WarningAction.CON))
                    && warngenLayer.featureProduct(c)) {
                warngenLayer.state.rightClickSelected = true;
                return true;
            } else if (warngenLayer.isPolygonLocked() == false
                    && warngenLayer.getStormTrackState().mode == Mode.TRACK) {
                // Add or remove county right-clicked on.
                warngenLayer.addOrRemoveCounty(c);
                warngenLayer.getWarngenState().geometryChanged = true;
                warngenLayer.issueRefresh();
                return true;
            }
        }

        boolean rval = false;

        if (moveType != null || pointDeleted || pointCreated || menuMove) {
            try {
                if (moveType == MoveType.ALL_POINTS) {
                    WarngenUIState state = warngenLayer.getWarngenState();
                    Coordinate[] coordinates = state.getWarningPolygon()
                            .getCoordinates();
                    PolygonUtil.truncate(coordinates, 2);

                    GeometryFactory gf = new GeometryFactory();
                    LinearRing lr = gf.createLinearRing(coordinates);
                    state.setWarningPolygon(gf.createPolygon(lr, null));
                }
                warngenLayer.updateWarnedAreas(true, true);
            } catch (VizException e) {
                e.printStackTrace();
            }
            warngenLayer.issueRefresh();
            rval = true;
        }
        pointDeleted = false;
        pointCreated = false;
        menuMove = false;
        return rval;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        if (menuMove) {
            move(x, y);
            warngenLayer.getWarngenState().geometryChanged = true;
            return true;
        }

        moveType = null;
        movePointIndex = -1;
        pointCreated = false;

        // TODO: Figure out if we are handling input
        if (warngenLayer.isBoxEditable() == false || !handleInput) {
            return super.handleMouseMove(x, y);
        }

        Coordinate mouse = new Coordinate(x, y);

        Polygon dragme = warngenLayer.getPolygon();
        Cursor toUse = getShell().getCursor();
        if (dragme != null) {
            Coordinate[] coords = dragme.getCoordinates();
            int idx = StormTrackUIManager.getCoordinateIndex(warngenLayer,
                    coords, mouse);
            if (idx > -1) {
                toUse = movePoint;
                moveType = MoveType.SINGLE_POINT;
                movePointIndex = idx;
            }

            if (moveType == null) {
                if (StormTrackUIManager
                        .closeToLine(warngenLayer, coords, mouse)) {
                    toUse = movePolygon;
                    moveType = MoveType.ALL_POINTS;
                }
            }
        }
        getShell().setCursor(toUse);
        return super.handleMouseMove(x, y);
    }

    public void setPointDeleted(boolean val) {
        this.pointDeleted = val;
    }

    public void setPointCreated(boolean val) {
        this.pointCreated = val;
    }

    public int addVertex(int x, int y) {
        IDisplayPaneContainer container = warngenLayer.getResourceContainer();
        WarngenUIState state = warngenLayer.getWarngenState();
        double bestDistance = Double.MAX_VALUE;
        int insertionPoint = 0;
        Coordinate[] coords = state.getWarningPolygon().getCoordinates();
        Coordinate mouse = container.translateClick(x, y);
        PolygonUtil.truncate(mouse, 2);
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
        GeometryFactory gf = new GeometryFactory();

        state.setWarningPolygon(new GeometryFactory().createPolygon(
                gf.createLinearRing(newLine), null));
        warngenLayer.state.rightClickSelected = false;
        warngenLayer.issueRefresh();
        return insertionPoint;
    }

    private void move(int x, int y) {
        IDisplayPaneContainer container = warngenLayer.getResourceContainer();
        WarngenUIState state = warngenLayer.getWarngenState();

        Coordinate c2 = container.translateClick(x, y);
        Coordinate c1 = container.translateClick(lastMouseX, lastMouseY);

        switch (moveType) {
        case ALL_POINTS: {
            if (c2 != null && c1 != null) {
                Coordinate delta = new Coordinate(c2.x - c1.x, c2.y - c1.y);
                warngenLayer.translatePolygon(delta);
            }
            break;
        }
        case SINGLE_POINT: {
            PolygonUtil.truncate(c2, 2);
            if (warngenLayer.isModifiedVertexNeedsToBeUpdated()) {
                int i = StormTrackUIManager.getCoordinateIndex(warngenLayer,
                        state.getWarningPolygon().getCoordinates(), c2);
                if (i != -1) {
                    this.movePointIndex = i;
                }
                warngenLayer.setModifiedVertexNeedsToBeUpdated(false);
            }
            movePointIndex = warngenLayer.translatePolygonVertex(
                    this.movePointIndex, c2, false);
            break;
        }
        }
        lastMouseX = x;
        lastMouseY = y;
        warngenLayer.state.rightClickSelected = false;
        warngenLayer.issueRefresh();
    }

    private class DeleteVertexAction extends AbstractRightClickAction {
        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#run()
         */
        @Override
        public void run() {

            if (warngenLayer.isPolygonLocked()
                    || warngenLayer.getPolygon() == null || !handleInput) {
                return;
            }
            // Short Circuit
            if (warngenLayer.getPolygon().getNumPoints() <= 4) {
                return;
            }

            Coordinate[] coords = warngenLayer.getPolygon().getCoordinates();

            int idx = StormTrackUIManager.getCoordinateIndex(warngenLayer,
                    coords, new Coordinate(lastMouseX, lastMouseY));

            if (idx >= 0) {
                pointDeleted = true;
                Coordinate toRemove = (Coordinate) coords[idx].clone();
                GeometryFactory gf = new GeometryFactory();
                List<Coordinate> coordList = new ArrayList<Coordinate>();
                List<Coordinate> alreadyRemoved = new ArrayList<Coordinate>();

                for (int i = 0; i < coords.length; ++i) {
                    Coordinate toAdd = (Coordinate) coords[i].clone();
                    if (!toAdd.equals(toRemove)
                            || alreadyRemoved.contains(toAdd)) {
                        coordList.add(toAdd);
                    } else {
                        alreadyRemoved.add(toAdd);
                    }
                }

                if (coordList.get(0)
                        .equals(coordList.get(coordList.size() - 1)) == false) {
                    coordList.add(coordList.get(0));
                }

                LinearRing lr = gf.createLinearRing(coordList
                        .toArray(new Coordinate[coordList.size()]));
                Polygon newPoly = gf.createPolygon(lr, null);

                if (newPoly.isValid() == false) {
                    return;
                }

                warngenLayer.getWarngenState().setWarningPolygon(newPoly);
                try {
                    warngenLayer.updateWarnedAreas(true, true);
                } catch (VizException e) {
                    Status s = new Status(Status.ERROR, Activator.PLUGIN_ID,
                            "Error updating warned area", e);
                    Activator.getDefault().getLog().log(s);
                    ErrorDialog.openError(
                            Display.getCurrent().getActiveShell(),
                            "Error updating warned areas",
                            "Error updating warned areas", s);
                }
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return "Delete Vertex";
        }

    }

    private class SelectLocationAction extends AbstractRightClickAction {

        @Override
        public void run() {
            if (warngenLayer.getStormTrackState().mode == Mode.DRAG_ME
                    || !warngenLayer.isBoxEditable()) {
                return;
            }
            Coordinate c = warngenLayer.getResourceContainer().translateClick(
                    lastMouseX, lastMouseY);
            warngenLayer.addOrRemoveCounty(c);
            warngenLayer.getWarngenState().geometryChanged = true;
            warngenLayer.issueRefresh();
        }

        @Override
        public String getText() {
            return "Select Location";
        }
    }

    private class MoveVertexAction extends AbstractRightClickAction {
        @Override
        public void run() {
            moveType = MoveType.SINGLE_POINT;
            movePointIndex = StormTrackUIManager.getCoordinateIndex(
                    warngenLayer, warngenLayer.getPolygon().getCoordinates(),
                    new Coordinate(lastMouseX, lastMouseY));
            menuMove = true;
        }

        @Override
        public String getText() {
            return "Move Vertex";
        }
    }

    private class MoveElementAction extends AbstractRightClickAction {
        @Override
        public void run() {
            moveType = MoveType.ALL_POINTS;
            movePointIndex = StormTrackUIManager.getCoordinateIndex(
                    warngenLayer, warngenLayer.getPolygon().getCoordinates(),
                    new Coordinate(lastMouseX, lastMouseY));
            menuMove = true;
        }

        @Override
        public String getText() {
            return "Move Entire Element";
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
            IDisplayPaneContainer container = warngenLayer
                    .getResourceContainer();
            if (warngenLayer.isPolygonLocked()
                    || warngenLayer.getPolygon() == null || !handleInput) {
                return;
            }

            Coordinate c = new Coordinate(lastMouseX, lastMouseY);
            Polygon poly = warngenLayer.getPolygon();

            if (StormTrackUIManager.getCoordinateIndex(warngenLayer,
                    poly.getCoordinates(), c) > -1) {
                // we are close to a point, don't create
                return;
            }

            GeometryFactory gf = new GeometryFactory();
            // Check if it is on the polygon
            Line line = new Line();

            LineString ls = poly.getExteriorRing();
            Coordinate[] coords = ls.getCoordinates();
            for (int i = 1; i < coords.length; i++) {
                double[] vals = container.translateInverseClick(coords[i - 1]);
                Coordinate a = new Coordinate(vals[0], vals[1]);
                vals = container.translateInverseClick(coords[i]);
                Coordinate b = new Coordinate(vals[0], vals[1]);
                line.setLine(new Point2D.Double(a.x, a.y), new Point2D.Double(
                        b.x, b.y));

                LineString lineSegment = gf.createLineString(new Coordinate[] {
                        a, b });
                if (lineSegment.isWithinDistance(gf.createPoint(c), 3.0)) {
                    Point2D coLinearPoint = line
                            .nearestColinearPoint(new Point2D.Double(c.x, c.y));

                    Coordinate coLinearCoord = container.translateClick(
                            coLinearPoint.getX(), coLinearPoint.getY());
                    Coordinate[] coords2 = new Coordinate[coords.length + 1];
                    int k = 0;
                    for (k = 0; k < i; k++) {
                        coords2[k] = coords[k];
                    }
                    coords2[k] = coLinearCoord;
                    for (k += 1; k < coords2.length - 1; k++) {
                        coords2[k] = coords[k - 1];
                    }
                    coords2[coords2.length - 1] = coords2[0];

                    LinearRing newLs = gf.createLinearRing(coords2);
                    Polygon newPoly = gf.createPolygon(newLs, null);
                    warngenLayer.getWarngenState().setWarningPolygon(newPoly);
                    try {
                        warngenLayer.updateWarnedAreas(true, true);
                    } catch (VizException e) {
                        Status s = new Status(Status.ERROR,
                                Activator.PLUGIN_ID,
                                "Error updating warned area", e);
                        Activator.getDefault().getLog().log(s);
                        ErrorDialog.openError(Display.getCurrent()
                                .getActiveShell(),
                                "Error updating warned areas",
                                "Error updating warned areas", s);
                    }
                    pointCreated = true;
                    break;
                }
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.Action#getText()
         */
        @Override
        public String getText() {
            return "Add Vertex";
        }
    }

    public DeleteVertexAction getDeleteAction() {
        return new DeleteVertexAction();
    }

    public SelectLocationAction getSelectLocationAction() {
        return new SelectLocationAction();
    }

    public MoveVertexAction getMoveAction() {
        return new MoveVertexAction();
    }

    public MoveElementAction getMoveElementAction() {
        return new MoveElementAction();
    }

    public AddVertexAction getAddAction() {
        return new AddVertexAction();
    }

    public boolean closeToPoint() {
        if (warngenLayer.getPolygon() == null) {
            return false;
        }
        return StormTrackUIManager.getCoordinateIndex(warngenLayer,
                warngenLayer.getPolygon().getCoordinates(), new Coordinate(
                        lastMouseX, lastMouseY)) > -1;
    }

    public boolean closeToLine() {
        if (warngenLayer.getPolygon() == null) {
            return false;
        }
        return StormTrackUIManager.closeToLine(warngenLayer, warngenLayer
                .getPolygon().getCoordinates(), new Coordinate(lastMouseX,
                lastMouseY));
    }

    public void setHandleInput(boolean handle) {
        handleInput = handle;
        if (!handle) {
            getShell().setCursor(arrow);
        }
    }

    private Shell getShell() {
        IDisplayPaneContainer container = warngenLayer.getResourceContainer();
        if (container instanceof IWorkbenchPart) {
            return ((IWorkbenchPart) container).getSite().getShell();
        } else {
            return VizWorkbenchManager.getInstance().getCurrentWindow()
                    .getShell();
        }
    }
}
