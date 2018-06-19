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

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineSegment;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Various utilities for making or changing {@link Polygon} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2015  3974      njensen     Initial creation
 * Mar 04, 2015  4194      njensen     Fix removing first vertex of a LinearRing
 * Jun 03, 2015  4375      dgilling    Add method to create polygon centered
 *                                     around arbitrary screen point.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PolygonUtil {

    public static final GeometryFactory FACTORY = new GeometryFactory();

    private PolygonUtil() {

    }

    /**
     * Makes a rectangular polygon based on the display aspect ratio
     * 
     * @param display
     * @return a basic rectangle
     */
    public static Polygon makeDefaultPolygon(IRenderableDisplay display) {
        IExtent extent = display.getExtent();
        double[] center = extent.getCenter();
        return makePolygon(display, new Coordinate(center[0], center[1]));
    }

    /**
     * Makes a rectangular polygon centered on the specified point and sized
     * according to the display aspect ratio.
     * 
     * @param display
     * @param center
     *            A {@code Coordinate} instance in pixel coordinates that will
     *            act as the center of the new polygon.
     * @return a basic rectangle
     */
    public static Polygon makePolygon(IRenderableDisplay display,
            Coordinate center) {
        IExtent extent = display.getExtent();
        IDescriptor descriptor = display.getDescriptor();

        IExtent clone = extent.clone();
        clone.scaleAndBias(.2, center.x, center.y);
        Coordinate[] square = new Coordinate[5];

        double[] pxUL = new double[] { clone.getMinX(), clone.getMaxY() };
        double[] cUL = descriptor.pixelToWorld(pxUL);
        square[0] = new Coordinate(cUL[0], cUL[1]);

        double[] pxLL = new double[] { clone.getMinX(), clone.getMinY() };
        double[] cLL = descriptor.pixelToWorld(pxLL);
        square[1] = new Coordinate(cLL[0], cLL[1]);

        double[] pxLR = new double[] { clone.getMaxX(), clone.getMinY() };
        double[] cLR = descriptor.pixelToWorld(pxLR);
        square[2] = new Coordinate(cLR[0], cLR[1]);

        double[] pxUR = new double[] { clone.getMaxX(), clone.getMaxY() };
        double[] cUR = descriptor.pixelToWorld(pxUR);
        square[3] = new Coordinate(cUR[0], cUR[1]);

        square[4] = (Coordinate) square[0].clone();

        return FACTORY.createPolygon(square);
    }

    /**
     * Adds a vertex to a LineString at the specified coordinate, attempting to
     * find the closest segment to determine where in the LineString the vertex
     * should go.
     * 
     * This function operates on the assumption that the LineString and the
     * Coordinate are in the same CRS. It does not bother to check that the
     * coordinate is near the line string, calling code should determine if the
     * proximity to the line is acceptable.
     * 
     * Note that this does not change the original reference, instead returning
     * a new LineString.
     * 
     * 
     * @param line
     * @param vertexToAdd
     * @return a new line string with an extra vertex inserted at the coordinate
     */
    public static LineString addVertex(LineString line, Coordinate vertexToAdd) {
        Coordinate[] coords = line.getCoordinates();
        double bestDistance = Double.MAX_VALUE;
        int index = -1;
        for (int i = 0; i < coords.length - 1; i++) {
            LineSegment segment = new LineSegment(coords[i], coords[i + 1]);
            double distance = segment.distance(vertexToAdd);
            if (distance < bestDistance) {
                index = i + 1;
                bestDistance = distance;
            }
        }

        if (index > -1) {
            Coordinate[] newLine = new Coordinate[coords.length + 1];
            System.arraycopy(coords, 0, newLine, 0, index);
            newLine[index] = vertexToAdd;
            System.arraycopy(coords, index, newLine, index + 1, coords.length
                    - index);
            if (newLine[0].equals(newLine[newLine.length - 1])) {
                return FACTORY.createLinearRing(newLine);
            } else {
                return FACTORY.createLineString(newLine);
            }
        } else {
            /*
             * TODO contemplate throwing exception instead of returning original
             * line
             */
            return line;
        }
    }

    /**
     * Removes a vertex from a LineString at the specified coordinate,
     * attempting to find the closest vertex to remove and connecting the two
     * points on either side of it.
     * 
     * This function operates on the assumption that the LineString and the
     * Coordinate are in the same CRS. It does not bother to check that the
     * coordinate is near the line string, calling code should determine if the
     * proximity to the line is acceptable.
     * 
     * Note that this does not change the original reference, instead returning
     * a new LineString.
     * 
     * 
     * @param line
     * @param vertexToRemove
     * @return a new line string with the vertex nearest the coordinate removed
     */
    public static LineString removeVertex(LineString line,
            Coordinate vertexToRemove) {
        Coordinate[] coords = line.getCoordinates();
        double bestDistance = Double.MAX_VALUE;
        int index = -1;
        for (int i = 0; i < coords.length - 1; i++) {
            double distance = coords[i].distance(vertexToRemove);
            if (distance < bestDistance) {
                index = i;
                bestDistance = distance;
            }
        }

        if (index > -1) {
            return removeVertex(line, index);
        } else {
            /*
             * TODO contemplate throwing exception instead of returning original
             * line
             */
            return line;
        }
    }

    /**
     * Removes a vertex from a LineString at the specified index. If the
     * LineString is a LinearRing, it will not remove the vertex if there are
     * not enough points to have a LinearRing after removing the vertex.
     * 
     * Note that this does not change the original reference, instead returning
     * a new LineString.
     * 
     * @param line
     * @param index
     * @return a new line string with the vertex nearest the coordinate removed
     */
    public static LineString removeVertex(LineString line, int index) {
        Coordinate[] coords = line.getCoordinates();
        boolean isRing = (line instanceof LinearRing);
        if (index > -1 && (!isRing || coords.length > 4)) {
            Coordinate[] newLine = new Coordinate[coords.length - 1];
            System.arraycopy(coords, 0, newLine, 0, index);
            System.arraycopy(coords, index + 1, newLine, index, newLine.length
                    - index);
            if (isRing) {
                if (index == 0) {
                    /*
                     * first point and last point in a ring always match, so if
                     * we removed the first point we need to update the last to
                     * be the new first point
                     */
                    newLine[newLine.length - 1] = new Coordinate(newLine[0].x,
                            newLine[0].y, newLine[0].z);
                }
                return FACTORY.createLinearRing(newLine);
            } else {
                return FACTORY.createLineString(newLine);
            }
        } else {
            /*
             * TODO contemplate throwing exception instead of returning original
             * line
             */
            return line;
        }
    }

}
