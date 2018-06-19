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
import com.vividsolutions.jts.geom.Point;
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
 * Mar 04, 2015  4194      njensen     Block other input on middle click drag on edges
 * Jun 03, 2015  4375      dgilling    Add methods for adding/deleting polygons, 
 *                                     support multiple polygons in PolygonLayer.
 * Jun 18, 2015  4354      dgilling    Changed visibility of pointOnPolygon.
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

    /** index of polygon if dragging a vertex */
    protected int draggedVertexPolygonIndex = -1;

    /** Index of the polygon being dragged */
    protected int draggedPolygonIndex = -1;

    /** is the mouse middle clicking on a vertex or edge */
    protected boolean middleClicking = false;

    public PolygonInputAdapter(PolygonLayer<?> layer) {
        super(layer);
    }

    @Override
    public boolean handleMouseDown(int x, int y, int mouseButton) {
        boolean blockOtherHandlers = false;
        if (availableForInput()) {
            int[] onVertexIndices = pointOnVertex(x, y);
            boolean isOnVertex = (onVertexIndices != null);
            if (mouseButton == 1) {
                lastX = x;
                lastY = y;
                if (isOnVertex) {
                    int polygonIdx = onVertexIndices[0];
                    int vertexIdx = onVertexIndices[1];
                    // user started dragging a vertex
                    draggedVertexIndex = vertexIdx;
                    draggedVertexPolygonIndex = polygonIdx;
                } else {
                    int polygonIdx = pointOnPolygon(x, y);
                    boolean isPointOnPolygon = (polygonIdx > -1);
                    if (isPointOnPolygon) {
                        // user started dragging the entire polygon
                        draggedPolygonIndex = polygonIdx;
                    }
                }
                blockOtherHandlers = isDragging();
            } else if (mouseButton == 2) {
                if (isOnVertex) {
                    int polygonIdx = onVertexIndices[0];
                    int vertexIdx = onVertexIndices[1];
                    removeVertex(polygonIdx, vertexIdx);
                    blockOtherHandlers = true;
                } else {
                    int polygonIdx = pointOnEdge(x, y);
                    boolean isPointOnEdge = (polygonIdx > -1);
                    if (isPointOnEdge) {
                        addVertex(x, y, polygonIdx);
                        blockOtherHandlers = true;
                    }
                }
                middleClicking = blockOtherHandlers;
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
                    if ((draggedVertexIndex > -1)
                            && (draggedVertexPolygonIndex > -1)) {
                        /*
                         * Asking the PolygonLayer to move the polygon's vertex
                         * will force it to the top of the stack. So, we must
                         * update the polygon index to 0 whenever processing a
                         * vertex drag.
                         */
                        int polygonIdx = draggedVertexPolygonIndex;
                        draggedVertexPolygonIndex = 0;
                        dragVertex(polygonIdx, draggedVertexIndex, x, y);
                    } else if (draggedPolygonIndex > -1) {
                        /*
                         * Asking the PolygonLayer to move the currently
                         * selected polygon will force it to the top of the
                         * stack. So, we must update the draggedPolygonIndex to
                         * 0 whenever processing a drag.
                         */
                        int polygonIdx = draggedPolygonIndex;
                        draggedPolygonIndex = 0;
                        dragPolygon(polygonIdx, diffX, diffY);
                    }
                }
            } else if (mouseButton == 2) {
                /*
                 * this can occur if the user meant to middle click and then
                 * accidentally dragged a bit, so let's treat it as just a
                 * middle click if it originally was on an edge or vertex
                 */
                blockOtherHandlers = middleClicking;
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
                if ((draggedVertexIndex > -1)
                        && (draggedVertexPolygonIndex > -1)) {
                    int vertexIdx = draggedVertexIndex;
                    int polygonIdx = draggedVertexPolygonIndex;
                    draggedVertexIndex = -1;
                    draggedVertexPolygonIndex = -1;
                    dragVertex(polygonIdx, vertexIdx, x, y);
                } else if (draggedPolygonIndex > -1) {
                    int diffX = x - lastX;
                    int diffY = y - lastY;
                    int polygonIdx = draggedPolygonIndex;
                    draggedPolygonIndex = -1;
                    dragPolygon(polygonIdx, diffX, diffY);
                }
            } else if (mouseButton == 2) {
                middleClicking = false;
            }
        }

        return blockOtherHandlers;
    }

    @Override
    public boolean handleMouseMove(int x, int y) {
        if (availableForInput()) {
            int mouseCursor = SWT.CURSOR_ARROW;
            if (isPointOnVertex(x, y)) {
                // We are hovering over a vertex
                mouseCursor = SWT.CURSOR_HAND;
            } else if (isPointOnEdge(x, y)) {
                // do nothing, don't change the cursor
            } else if (isPointOnPolygon(x, y)) {
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
        return ((draggedVertexIndex > -1 && draggedVertexPolygonIndex > -1) || (draggedPolygonIndex > -1));
    }

    /**
     * Checks if the screen coordinate is on or near a vertex of any polygons in
     * the layer.
     * 
     * @param screenX
     * @param screenY
     * @return
     */
    protected boolean isPointOnVertex(int screenX, int screenY) {
        return (pointOnVertex(screenX, screenY) != null);
    }

    /**
     * Checks if the screen coordinate is on or near a vertex of any polygons in
     * the layer and returns the indices of the polygon and vertex this screen
     * coordinate is near.
     * 
     * @param screenX
     * @param screenY
     * @return An int array containing the polygon index as the first element
     *         and the vertex index as the second element. If no match is found,
     *         {@code null} is returned.
     */
    protected int[] pointOnVertex(int screenX, int screenY) {
        IDisplayPaneContainer container = getContainer();
        Coordinate clickScreenCoord = new Coordinate(screenX, screenY);

        int polygonIdx = 0;
        for (Polygon polygon : rsc.getPolygons()) {
            int vertexIdx = 0;
            Coordinate[] coords = polygon.getExteriorRing().getCoordinates();
            for (Coordinate c : coords) {
                double[] screen = container.translateInverseClick(c);
                if (screen != null) {
                    Coordinate vertexScreenCoord = new Coordinate(screen[0],
                            screen[1]);
                    if (vertexScreenCoord.distance(clickScreenCoord) <= PolygonLayer.VERTEX_RADIUS) {
                        return new int[] { polygonIdx, vertexIdx };
                    }
                }

                vertexIdx++;
            }

            polygonIdx++;
        }

        return null;
    }

    /**
     * Checks if the screen coordinate is on a polygon.
     * 
     * @param screenX
     * @param screenY
     * @return
     */
    protected boolean isPointOnPolygon(int screenX, int screenY) {
        return pointOnPolygon(screenX, screenY) >= 0;
    }

    /**
     * Checks if the screen coordinate is on a polygon and return the index if
     * it is.
     * 
     * @param screenX
     * @param screenY
     * @return Index of the polygon this screen point is on or -1 if it is not
     *         on any polygons.
     */
    public int pointOnPolygon(int screenX, int screenY) {
        int polygonIdx = -1;
        Coordinate worldCoord = screenToLatLon(screenX, screenY);
        if (worldCoord != null) {
            int i = 0;
            Point point = PolygonUtil.FACTORY.createPoint(worldCoord);
            for (Polygon polygon : rsc.getPolygons()) {
                boolean overPolygon = point.within(polygon);
                if (overPolygon) {
                    polygonIdx = i;
                    break;
                }
                i++;
            }
        }

        return polygonIdx;
    }

    /**
     * Checks if the specified x and y screen coordinates are over or very close
     * to an edge (aka exterior ring) of a polygon
     * 
     * @param screenX
     * @param screenY
     * @return
     */
    protected boolean isPointOnEdge(int screenX, int screenY) {
        return pointOnEdge(screenX, screenY) >= 0;
    }

    /**
     * Checks if the specified x and y screen coordinates are over or very close
     * to an edge (aka exterior ring) of a polygon and returns the index of that
     * polygon.
     * 
     * @param screenX
     * @param screenY
     * @return The index of the polygon or -1 if not near the edge of any
     *         polygon.
     */
    protected int pointOnEdge(int screenX, int screenY) {
        int polygonIdx = -1;

        Coordinate screenCoord = new Coordinate(screenX, screenY);
        Point screenPoint = PolygonUtil.FACTORY.createPoint(screenCoord);

        int i = 0;
        for (Polygon polygon : rsc.getPolygons()) {
            Polygon screenPolygon = (Polygon) polygon.clone();
            screenPolygon.apply(latlonToScreen);
            if (latlonToScreen.success()) {
                boolean overEdge = screenPolygon.getExteriorRing().distance(
                        screenPoint) <= EDGE_PROXIMITY;
                if (overEdge) {
                    polygonIdx = i;
                    break;
                }
            }

            i++;
        }

        return polygonIdx;
    }

    /**
     * Drags the vertex at the specified index to the specified screen
     * coordinates.
     * 
     * @param polygonIndex
     *            Index of the polygon having its vertex dragged.
     * @param pointIndex
     *            The index of the vertex to drag.
     * @param screenX
     *            new X-coordinate of the vertex in screen space.
     * @param screenY
     *            new Y-coordinate of the vertex in screen space.
     */
    protected void dragVertex(int polygonIndex, int pointIndex, int screenX,
            int screenY) {
        Coordinate newLoc = screenToLatLon(screenX, screenY);
        if (newLoc != null) {
            Coordinate[] c = rsc.getPolygon(polygonIndex).getCoordinates();
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
            rsc.resetPolygon(polygonIndex, c);
        }
    }

    /**
     * Drags/shifts the entire polygon by screen x and y differences
     * 
     * @param polygonIndex
     *            Index of the polygon to move.
     * @param diffX
     *            number of screen space units to move in the X direction
     * @param diffY
     *            number of screen space units to move in the Y direction
     */
    protected void dragPolygon(int polygonIndex, int diffX, int diffY) {
        // clone in case something goes wrong
        Polygon movedPolygon = (Polygon) rsc.getPolygon(polygonIndex).clone();

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
                rsc.resetPolygon(polygonIndex, movedPolygon.getCoordinates());
            }
        }
    }

    /**
     * Adds a vertex to the specified polygon at the specified screen
     * coordinates.
     * 
     * @param screenX
     *            X-coordinate of the new vertex in screen space.
     * @param screenY
     *            Y-coordinate of the new vertex in screen space.
     * @param polygonIdx
     *            Index of the polygon to add the vertex to.
     */
    protected void addVertex(int screenX, int screenY, int polygonIdx) {
        Coordinate vertex = screenToLatLon(screenX, screenY);
        LinearRing ring = (LinearRing) PolygonUtil.addVertex(
                rsc.getPolygon(polygonIdx).getExteriorRing(), vertex);
        rsc.resetPolygon(polygonIdx, ring.getCoordinates());
    }

    /**
     * Removes a vertex at the specified index.
     * 
     * @param polygonIndex
     *            Index of the polygon to remove the vertex from.
     * @param vertexIndex
     *            Index of the vertex to remove.
     */
    protected void removeVertex(int polygonIndex, int vertexIndex) {
        if ((vertexIndex > -1) && (polygonIndex > -1)) {
            LineString line = PolygonUtil
                    .removeVertex(rsc.getPolygon(polygonIndex)
                            .getExteriorRing(), vertexIndex);
            rsc.resetPolygon(polygonIndex, line.getCoordinates());
        }
    }

    protected void addPolygon(int screenX, int screenY) {
        Coordinate pixelCoords = screenToPixel(screenX, screenY);
        Polygon newPolygon = PolygonUtil.makePolygon(getContainer()
                .getActiveDisplayPane().getRenderableDisplay(), pixelCoords);
        rsc.addPolygon(newPolygon.getExteriorRing().getCoordinates());
    }

    protected void removePolygon(int index) {
        rsc.deletePolygon(index);
    }

    protected Coordinate screenToPixel(int screenX, int screenY) {
        Coordinate worldCoords = screenToLatLon(screenX, screenY);
        double[] pixelCoords = getContainer().getActiveDisplayPane()
                .getDescriptor()
                .worldToPixel(new double[] { worldCoords.x, worldCoords.y });
        return new Coordinate(pixelCoords[0], pixelCoords[1]);
    }

    @Override
    protected boolean availableForInput() {
        return rsc != null && super.availableForInput();
    }

}
