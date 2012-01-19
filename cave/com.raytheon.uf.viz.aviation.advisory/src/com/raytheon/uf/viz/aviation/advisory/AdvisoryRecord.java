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
package com.raytheon.uf.viz.aviation.advisory;

import java.awt.geom.Point2D;
import java.util.Arrays;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.geotools.referencing.GeodeticCalculator;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Polygon;


/**
 * 
 * A class containing all parameters necessary for an outline resource to be
 * rendered.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 2, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AdvisoryRecord {
    
    private static GeometryFactory FACTORY = new GeometryFactory();

    protected static final UnitConverter NM_TO_METERS = NonSI.NAUTICAL_MILE
            .getConverterTo(SI.METER);

    protected static final int NUM_VERTICES = 40;

    protected static final double ANGLE_STEP = 360.0 / NUM_VERTICES;
    
    public enum AdvisoryResourceType {
        LINE, ISOL, AREA, TEXT
    }

    private AdvisoryResourceType type;
    
    private Polygon polygon;

    private Coordinate[] line;

    private Coordinate labelLoc;
    
    private String label;

    private String inspectString;

    public AdvisoryRecord(Coordinate labelLoc, String label) {
        this.type = AdvisoryResourceType.TEXT;
        this.labelLoc = labelLoc;
        this.label = label;
    }

    public AdvisoryRecord(Coordinate[] polygon, String label,
            String inspectString) {
        this.type = AdvisoryResourceType.AREA;
        this.labelLoc = polygon[0];
        if (polygon.length > 0
                && !polygon[0].equals(polygon[polygon.length - 1])) {
            polygon = Arrays.copyOf(polygon, polygon.length + 1);
            polygon[polygon.length - 1] = polygon[0];
        }
        LinearRing ring = FACTORY.createLinearRing(polygon);
        this.polygon = FACTORY.createPolygon(ring, null);
        this.label = label;
        this.inspectString = inspectString;
    }
    
    public AdvisoryRecord(Coordinate[] line, double diameter, String label,
            String inspectString) {
        this.type = AdvisoryResourceType.LINE;
        this.line = line;
        this.labelLoc = line[0];
        // Use 5 nautical miles for minimum
        if (diameter <= 5) {
            diameter = 5;
        }
        Coordinate[] lastLine1 = null;
        Coordinate[] lastLine2 = null;
        Coordinate[] polygon = new Coordinate[line.length * 2 + 1];
        double radius = NM_TO_METERS.convert(diameter) / 2;
        for (int i = 0; i < line.length - 1; i++) {
            Coordinate thisPoint = line[i];
            Coordinate nextPoint = line[i + 1];
            double angle = Math.atan2(nextPoint.y - thisPoint.y, thisPoint.x
                    - nextPoint.x);
            angle = Math.toDegrees(angle);
            Coordinate[] thisLine1 = getLine(thisPoint, nextPoint, angle,
                    radius);
            Coordinate[] thisLine2 = getLine(thisPoint, nextPoint, angle + 180,
                    radius);
            if (i == 0) {
                polygon[line.length * 2 - 1 - i] = thisLine2[0];
                polygon[i] = thisLine1[0];
            } else {
                polygon[line.length * 2 - 1 - i] = getIntersection(thisLine2,
                        lastLine2);
                polygon[i] = getIntersection(thisLine1, lastLine1);
            }
            lastLine1 = thisLine1;
            lastLine2 = thisLine2;

        }
        // Enter the points for the last coordinate
        polygon[line.length - 1] = lastLine1[1];
        polygon[line.length] = lastLine2[1];
        // Make start and endpointt he same
        polygon[polygon.length - 1] = polygon[0];
        LinearRing ring = FACTORY.createLinearRing(polygon);
        this.polygon = FACTORY.createPolygon(ring, null);
        this.label = label;
        this.inspectString = inspectString;
    }
    
    public AdvisoryRecord(Coordinate center, double diameter, String label,
            String inspectString) {
        this.type = AdvisoryResourceType.ISOL;
        this.labelLoc = center;
        double radius = NM_TO_METERS.convert(diameter / 2);

        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(center.x, center.y);
        Coordinate polygon[] = new Coordinate[NUM_VERTICES + 1];
        for (int i = 0; i < NUM_VERTICES; i++) {
            double azimuth = -180.0 + i * ANGLE_STEP;
            gc.setDirection(azimuth, radius);
            Point2D p = gc.getDestinationGeographicPoint();
            polygon[i] = new Coordinate(p.getX(), p.getY());
        }
        polygon[NUM_VERTICES] = polygon[0];
        LinearRing ring = FACTORY.createLinearRing(polygon);
        this.polygon = FACTORY.createPolygon(ring, null);
        this.type = AdvisoryResourceType.ISOL;
        this.label = label;
        this.inspectString = inspectString;
    }
    
    /**
     * Convert a pair of Lat Lon coords to a pair of pixel coords in a certain
     * direction and a certain distance away
     * 
     * @param point1
     *            the first endpoint
     * @param point2
     *            the second endpoint
     * @param angle
     *            the angle to place the new line relative to the existing line
     * @param radius
     *            the distance in meters between the original line and the new
     *            line
     * @return a set of screen coordinates for the new line
     */
    private Coordinate[] getLine(Coordinate point1, Coordinate point2,
            double angle, double radius) {
        Coordinate p1 = getPointOnCircle(point1, radius, angle);
        Coordinate p2 = getPointOnCircle(point2, radius, angle);
        return new Coordinate[] { p1, p2 };
    }

    /**
     * Get a Coordinate that is correct distance and angle from coord
     * 
     * @param center
     *            the Lat Lon coordinate representing the center of a circle
     * @param distance
     *            the distance in meters between the new point and the center
     * @param angle
     *            the angle to put the new point at
     * @return a new Coordinate at the correct location
     */
    public static Coordinate getPointOnCircle(Coordinate center,
            double distance,
            double angle) {
        while (angle > 180) {
            angle -= 360;
        }
        while (angle < -180) {
            angle += 360;
        }
        GeodeticCalculator gc = new GeodeticCalculator();
        gc.setStartingGeographicPoint(center.x, center.y);
        gc.setDirection(angle, distance);
        Point2D p = gc.getDestinationGeographicPoint();
        return new Coordinate(p.getX(), p.getY());
    }

    /**
     * Find the intersection point between two lines
     * 
     * @param line1
     *            a 2x2 array representing two endpoints
     * @param line2
     *            a 2x2 array representing two endpoints
     * @return an array of length 2 representing the intersection point of the
     *         two lines
     */
    private Coordinate getIntersection(Coordinate[] line1, Coordinate[] line2) {
        Coordinate intersectPoint = new Coordinate();
        double x1 = line1[0].x;
        double y1 = line1[0].y;
        double x2 = line1[1].x;
        double y2 = line1[1].y;
        double x3 = line2[0].x;
        double y3 = line2[0].y;
        double x4 = line2[1].x;
        double y4 = line2[1].y;
        double ua = (((x4 - x3) * (y1 - y3)) - ((y4 - y3) * (x1 - x3)))
                / (((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1)));
        intersectPoint.x = x1 + ua * (x2 - x1);
        intersectPoint.y = y1 + ua * (y2 - y1);
        return intersectPoint;
    }

    public AdvisoryResourceType getType() {
        return type;
    }

    public Polygon getPolygon() {
        return polygon;
    }

    public Coordinate[] getLine() {
        return line;
    }

    public Coordinate getLabelLoc() {
        return labelLoc;
    }

    public String getLabel() {
        return label;
    }

    public String getInspectString() {
        return inspectString;
    }

    
}
