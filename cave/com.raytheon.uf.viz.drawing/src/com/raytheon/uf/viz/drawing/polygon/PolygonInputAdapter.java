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
package com.raytheon.uf.viz.drawing.polygon;

import org.eclipse.swt.SWT;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.drawing.RscInputAdapter;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.AffineTransformation;

/**
 * An input adapter for editing a drawn polygon. This enables dragging a vertex,
 * dragging the entire polygon, inserting a vertex, and removing a vertex.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2015  3974      njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PolygonInputAdapter extends RscInputAdapter<PolygonLayer<?>> {

    /**
     * a fudge factor in screen coordinates to detect if the coordinate is
     * pretty close to an edge, instead of precisely on top of an edge
     */
    protected static final double EDGE_PROXIMITY = 4.0;

    /** Last mouse x/y location */
    protected int lastX, lastY;

    /** Current cursor type */
    protected int currentCursor = SWT.CURSOR_ARROW;

    /** original vertex being dragged */
    protected int draggedVertexIndex = -1;

    /** is the entire polygon being dragged */
    protected boolean draggingPolygon = false;

    public PolygonInputAdapter(PolygonLayer<?> layer) {
        super(layer);
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        boolean blockOtherHandlers = false;
        if (availableForInput()) {
            int vertexIdx = pointOnVertex(x, y);
            if (mouseButton == 1) {
                lastX = x;
                lastY = y;
                if (vertexIdx > -1) {
                    // user started dragging a vertex
                    draggedVertexIndex = vertexIdx;
                } else if (pointOnPolygon(x, y)) {
                    // user started dragging the entire polygon
                    draggingPolygon = true;
                }
                blockOtherHandlers = isDragging();
            } else if (mouseButton == 2) {
                if (vertexIdx > -1) {
                    removeVertex(vertexIdx);
                    blockOtherHandlers = true;
                } else if (pointOnEdge(x, y)) {
                    addVertex(x, y);
                    blockOtherHandlers = true;
                }
            }
        }
        return blockOtherHandlers;
    }

    @Override
    public boolean handleMouseDownMove(int x, int y, int mouseButton) {
        boolean blockOtherHandlers = false;
        if (availableForInput()) {
            if (mouseButton == 1) {
                blockOtherHandlers = isDragging();
                if (isDragging() && withinDisplayBounds(x, y)) {
                    int diffX = x - lastX;
                    int diffY = y - lastY;
                    lastX = x;
                    lastY = y;
                    if (draggedVertexIndex > -1) {
                        dragVertex(draggedVertexIndex, x, y);
                    } else if (draggingPolygon) {
                        dragPolygon(diffX, diffY);
                    }
                }
            }
        }
        return blockOtherHandlers;
    }

    @Override
    public boolean handleMouseUp(int x, int y, int mouseButton) {
        boolean blockOtherHandlers = false;
        if (availableForInput()) {
            if (mouseButton == 1) {
                // user done dragging
                blockOtherHandlers = isDragging();
                if (draggedVertexIndex > -1) {
                    int index = draggedVertexIndex;
                    draggedVertexIndex = -1;
                    dragVertex(index, x, y);
                } else if (draggingPolygon) {
                    int diffX = x - lastX;
                    int diffY = y - lastY;
                    draggingPolygon = false;
                    dragPolygon(diffX, diffY);
                }
            }
        }

        return blockOtherHandlers;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        if (availableForInput()) {
            int mouseCursor = SWT.CURSOR_ARROW;
            int idx = pointOnVertex(x, y);
            if (idx > -1) {
                // We are hovering over a vertex
                mouseCursor = SWT.CURSOR_HAND;
            } else if (pointOnEdge(x, y)) {
                // do nothing, don't change the cursor
            } else if (pointOnPolygon(x, y)) {
                /*
                 * We are hovering over the polygon but not the vertices or
                 * edges
                 */
                mouseCursor = SWT.CURSOR_SIZEALL;
            }

            if (mouseCursor != currentCursor) {
                // Update the mouse
                updateCursor(mouseCursor);
                currentCursor = mouseCursor;
            }

        }
        return false;
    }

    /**
     * Checks if there is some kind of dragging operation related to the polygon
     * 
     * @return if there is some kind of dragging operation currently going on
     */
    public boolean isDragging() {
        return (draggedVertexIndex > -1 || draggingPolygon);
    }

    protected int pointOnVertex(int screenX, int screenY) {
        IDisplayPaneContainer container = getContainer();
        Coordinate screenCoord = new Coordinate(screenX, screenY);
        int rval = -1;
        int i = 0;
        Polygon poly = rsc.getPolygon();
        if (poly != null) {
            Coordinate[] coords = poly.getExteriorRing().getCoordinates();
            for (Coordinate c : coords) {
                double[] screen = container.translateInverseClick(c);
                if (screen != null) {
                    c = new Coordinate(screen[0], screen[1]);
                    if (c.distance(screenCoord) <= PolygonLayer.VERTEX_RADIUS) {
                        rval = i;
                        break;
                    }
                }
                ++i;
            }
        }
        return rval;
    }

    /**
     * Checks if the screen coordinate is on the polygon
     * 
     * @param screenX
     * @param screenY
     * @return
     */
    protected boolean pointOnPolygon(int screenX, int screenY) {
        boolean overPolygon = false;
        Coordinate worldCoord = screenToLatLon(screenX, screenY);
        if (worldCoord != null) {
            overPolygon = PolygonUtil.FACTORY.createPoint(worldCoord).within(
                    rsc.getPolygon());
        }
        return overPolygon;
    }

    /**
     * Checks if the specified x and y screen coordinates are over or very close
     * to an edge (aka exterior ring) of the polygon
     * 
     * @param screenX
     * @param screenY
     * @return
     */
    protected boolean pointOnEdge(int screenX, int screenY) {
        boolean overEdge = false;
        Polygon screenPolygon = (Polygon) rsc.getPolygon().clone();
        screenPolygon.apply(latlonToScreen);
        if (latlonToScreen.success()) {
            Coordinate screenCoord = new Coordinate(screenX, screenY);
            overEdge = screenPolygon.getExteriorRing().distance(
                    PolygonUtil.FACTORY.createPoint(screenCoord)) <= EDGE_PROXIMITY;
        }

        return overEdge;
    }

    /**
     * Drags the vertex at the specified index to the specified screen
     * coordinates.
     * 
     * @param pointIndex
     * @param screenX
     * @param screenY
     */
    protected void dragVertex(int pointIndex, int screenX, int screenY) {
        Coordinate newLoc = screenToLatLon(screenX, screenY);
        if (newLoc != null) {
            Coordinate[] c = rsc.getPolygon().getCoordinates();
            for (int i = 0; i < c.length; i++) {
                if (i == pointIndex) {
                    c[i] = newLoc;
                    if (i == 0) {
                        // it's both the first and last point, need to update
                        // both
                        c[c.length - 1] = newLoc;
                    }
                    break;
                }
            }
            rsc.resetPolygon(c);
        }
    }

    /**
     * Drags/shifts the entire polygon by screen x and y differences
     * 
     * @param diffX
     * @param diffY
     */
    protected void dragPolygon(int diffX, int diffY) {
        // clone in case something goes wrong
        Polygon movedPolygon = (Polygon) rsc.getPolygon().clone();

        /*
         * shift the coordinates of the polygon to the screen space the user is
         * viewing, then afterwards shift back to latlon coordinate space
         */
        movedPolygon.apply(latlonToScreen);
        if (latlonToScreen.success()) {
            AffineTransformation transform = AffineTransformation
                    .translationInstance(diffX, diffY);
            movedPolygon.apply(transform);
            movedPolygon.apply(screenToLatLon);
            if (screenToLatLon.success()) {
                rsc.resetPolygon(movedPolygon.getCoordinates());
            }
        }
    }

    /**
     * Adds a vertex at the specified screen coordinates.
     * 
     * @param screenX
     * @param screenY
     */
    protected void addVertex(int screenX, int screenY) {
        Coordinate vertex = screenToLatLon(screenX, screenY);
        LinearRing ring = (LinearRing) PolygonUtil.addVertex(rsc.getPolygon()
                .getExteriorRing(), vertex);
        rsc.resetPolygon(ring.getCoordinates());
    }

    /**
     * Removes a vertex at the specified index.
     * 
     * @param index
     */
    protected void removeVertex(int index) {
        if (index > -1) {
            LineString line = PolygonUtil.removeVertex(rsc.getPolygon()
                    .getExteriorRing(), index);
            rsc.resetPolygon(line.getCoordinates());
        }
    }

    @Override
    protected boolean availableForInput() {
        return rsc != null && rsc.polygon != null && super.availableForInput();
    }

}
