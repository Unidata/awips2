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
package com.raytheon.uf.edex.plugin.cwa.util;

import org.geotools.referencing.GeodeticCalculator;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Ported from DepictUtility.C
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 3, 2010            jsanchez     Initial creation
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class Utility {
    private final static GeodeticCalculator gc = new GeodeticCalculator();

    /**
     * Given a point, return a circle points around it having diameter.
     * Calculates 40 vertices around points, starting with due north (0
     * degrees). Each vertex is 1/2 diameter distance away from point.
     * 
     * @param point
     *            - point mid point of circle
     * @param diameter
     *            - in units of meters
     * @returns lat/lons of area
     */
    public static Coordinate[] makeArea(Coordinate point, double diameter) {
        int numPoints = 40;
        double direction = 0;
        Coordinate[] vertices = new Coordinate[numPoints];

        gc.setStartingGeographicPoint(point.x, point.y);

        for (int i = 0; i < numPoints; i++) {
            if (direction > 180) {
                direction -= 360;
            }
            gc.setDirection(direction, diameter / 2);
            vertices[i] = new Coordinate(gc.getDestinationGeographicPoint()
                    .getX(), gc.getDestinationGeographicPoint().getY());
            direction = rotate(direction, (double) 360 / numPoints);
        }

        return vertices;
    }

    /**
     * Returns a new bearing that is the old bearing rotated 'degrees' degrees.
     */
    private static double rotate(double bearing, double degrees) {
        double newBearing = bearing + degrees;
        if (newBearing >= 360) {
            newBearing -= 360;
        }
        return newBearing;
    }

    /**
     * Given a polyline, return an area around it that is 'width' wide. Assumes
     * width is in METERS. Returns lat/lons of area. For each line segment in
     * 'polyline', calculate four points that define a rectangle around the line
     * segment, such that two sides of the rectangle are parallel to the line
     * segment. Then with each rectangle, calculate the intersection of the
     * corners with the corners of the subsequent rectangle, to determine coords
     * of resulting polygon at vertices of the polyline. At each end of the
     * polyline just use the appropriate rectangle corners.
     */
    public static Coordinate[] makeArea(Coordinate[] polyLine, double width) {
        Coordinate vertices[];
        int numPoints = polyLine.length;
        // If there aren't at least two points, return an empty list
        if (numPoints < 2) {
            return new Coordinate[0];
        }
        // We've got at least two points, start initializing
        int numNewPoints = numPoints * 2;
        vertices = new Coordinate[numNewPoints];

        Coordinate p1, p2, intersectPoint;
        Coordinate[] perpLine1, perpLine2;
        Coordinate[] prevLine1, prevLine2, parallelLine1, parallelLine2;
        prevLine1 = new Coordinate[2];
        prevLine2 = new Coordinate[2];
        parallelLine1 = new Coordinate[2];
        parallelLine2 = new Coordinate[2];

        double meterWidth = width;

        // Define line p1p2
        p1 = polyLine[0];
        p2 = polyLine[1];
        // Get a line perpendicular to p1p2 through the first point of input
        // 'polyline'
        perpLine1 = linePerpendicularThruP1(p1, p2, meterWidth);
        // Keep these two endpoints as part of the final polygon
        vertices[0] = perpLine1[0];
        vertices[numNewPoints - 1] = perpLine1[1];

        // Get a line perpendicular to p1p2 through the second point of
        // 'polyline'
        perpLine2 = linePerpendicularThruP1(p2, p1, meterWidth);
        // Using these new endpoints, determine the 2 parallel lines that
        // straddle p1p2
        // Since the linePerpendicularThruP1 method always rotates in a
        // clockwise
        // direction, perpLine2 endpoints will be rotated 180 degrees wrt
        // perpLine1 endpoints
        prevLine1[0] = perpLine1[0];
        prevLine1[1] = perpLine2[1];
        prevLine2[0] = perpLine1[1];
        prevLine2[1] = perpLine2[0];

        for (int i = 2; i < numPoints; i++) {
            // Define p1p2
            p1 = polyLine[i - 1];
            p2 = polyLine[i];
            // Calculate 2 end lines perpendicular to p1p2 that are 'kmWidth'
            // long
            perpLine1 = linePerpendicularThruP1(p1, p2, meterWidth);
            perpLine2 = linePerpendicularThruP1(p2, p1, meterWidth);
            // Calculate 2 lines parallel to p1p2 that are 'kmWidth' apart
            parallelLine1[0] = perpLine1[0];
            parallelLine1[1] = perpLine2[1];
            parallelLine2[0] = perpLine1[1];
            parallelLine2[1] = perpLine2[0];

            // Compute the intersections
            intersectPoint = intersection(parallelLine1, prevLine1);
            vertices[i - 1] = intersectPoint;
            intersectPoint = intersection(parallelLine2, prevLine2);
            vertices[numNewPoints - i] = intersectPoint;

            // Now stash the current lines as the previous lines for future
            // calcs.
            prevLine1[0] = parallelLine1[0];
            prevLine1[1] = parallelLine1[1];
            prevLine2[0] = parallelLine2[0];
            prevLine2[1] = parallelLine2[1];
        }
        // Now tack on the last perpendicular line
        vertices[numNewPoints / 2 - 1] = perpLine2[1];
        vertices[numNewPoints / 2] = perpLine2[0];
        return vertices;
    }

    /**
     * Returns a line perpendicular to the line defined by p1p2 having length
     * 'length'.
     */
    public static Coordinate[] linePerpendicularThruP1(Coordinate p1,
            Coordinate p2, double length) {
        // MapDepictor mapDep(MapDepictor::AZIMUTH_RANGE, p1);

        gc.setStartingGeographicPoint(p1.x, p1.y);
        gc.setDestinationGeographicPoint(p2.x, p2.y);
        double bearing = gc.getAzimuth();
        // Rotate the bearing 90 degrees
        bearing += 90;
        if (bearing >= 360) {
            bearing -= 360;
        }
        return lineHavingMidPoint(p1, length, bearing);
    }

    /**
     * Returns intersection point of two lines in lat/lon space.
     */
    public static Coordinate intersection(Coordinate[] line1, Coordinate[] line2) {

        if (line1.length != 2 || line2.length != 2) {
            return new Coordinate(0, 0);
        }
        float x1 = (float) line1[0].y;
        float y1 = (float) line1[0].x;
        float x2 = (float) line1[1].y;
        float y2 = (float) line1[1].x;
        float x3 = (float) line2[0].y;
        float y3 = (float) line2[0].x;
        float x4 = (float) line2[1].y;
        float y4 = (float) line2[1].x;
        float ua = (((x4 - x3) * (y1 - y3)) - ((y4 - y3) * (x1 - x3)))
                / (((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1)));
        float y = x1 + ua * (x2 - x1);
        float x = y1 + ua * (y2 - y1);
        return new Coordinate(x, y);
    }

    /**
     * Given a point 'point', returns a line having length 'length' with bearing
     * 'bearing' whose midpoint is 'point'.
     */
    public static Coordinate[] lineHavingMidPoint(Coordinate point,
            double length, double bearing) {
        double x = 0;
        double y = 0;
        Coordinate[] vertices = new Coordinate[2];
        gc.setStartingGeographicPoint(point.x, point.y);

        // Now get one point halfwidth distance away along new bearing
        double newBearing = bearing;
        if (newBearing > 180) {
            newBearing -= 360;
        }
        gc.setDirection(newBearing, length / 2);
        x = gc.getDestinationGeographicPoint().getX();
        y = gc.getDestinationGeographicPoint().getY();
        vertices[0] = new Coordinate(x, y);

        // Now rotate bearing 180 degrees and get other point halfwidth distance
        // away
        newBearing = bearing + 180;
        if (newBearing >= 360) {
            newBearing -= 360;
        }
        if (newBearing > 180) {
            newBearing -= 360;
        }
        gc.setDirection(newBearing, length / 2);
        x = gc.getDestinationGeographicPoint().getX();
        y = gc.getDestinationGeographicPoint().getY();
        vertices[1] = new Coordinate(x, y);

        return vertices;
    }
}
